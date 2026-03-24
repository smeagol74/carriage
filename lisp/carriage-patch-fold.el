;;; carriage-patch-fold.el --- Fold applied begin_patch blocks with overlays -*- lexical-binding: t; -*-

;; Hide contents of applied #+begin_patch ... #+end_patch blocks (header has :applied t)
;; Show a compact one-line placeholder:
;;   ✓ <file> — <description|result>
;; No auto-reveal by cursor: applied patches stay folded by default.
;; User toggles visibility only via TAB on the placeholder.
;;
;; Integration:
;; - Enabled automatically when carriage-mode is active and
;;   carriage-mode-hide-applied-patches is non-nil.
;; - Can be called manually: (carriage-patch-fold-enable) / (carriage-patch-fold-disable)

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-patch-fold nil
  "Overlay-based folding of applied #+begin_patch blocks."
  :group 'applications
  :prefix "carriage-patch-fold-")

(defface carriage-patch-fold-placeholder-face
  '((t :inherit shadow :slant italic :height 0.95))
  "Face for the folded placeholder line."
  :group 'carriage-patch-fold)

(defcustom carriage-patch-fold-invisible-symbol 'carriage-patch-fold
  "Symbol used in buffer-invisibility-spec for folded patch overlays."
  :type 'symbol :group 'carriage-patch-fold)


(defvar-local carriage-patch-fold--overlays nil
  "List of overlays currently managed by carriage-patch-fold in this buffer.")

(defvar-local carriage-patch-fold--refresh-timer nil
  "Idle timer used to coalesce rescans after edits.")

(defvar-local carriage-patch-fold--enabled nil
  "Non-nil when patch folding is enabled in the current buffer.")

(defvar-local carriage-patch-fold--active-overlay nil
  "Currently revealed applied-patch overlay under point, or nil.")

(defun carriage-patch-fold--org-hide-at (beg)
  "Hide the Org block at BEG using org-fold/org-hide (best-effort)."
  (when (and (numberp beg) (derived-mode-p 'org-mode))
    (ignore-errors
      (require 'org)
      (save-excursion
        (goto-char beg)
        (cond
         ((fboundp 'org-fold-hide-drawer-or-block)
          (org-fold-hide-drawer-or-block t))
         ((fboundp 'org-hide-block-toggle)
          (org-hide-block-toggle t))
         ((featurep 'org-fold)
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end t))))
         (t nil))))))

(defun carriage-patch-fold--org-show-at (beg)
  "Show the Org block at BEG (undo folding) best-effort."
  (when (and (numberp beg) (derived-mode-p 'org-mode))
    (ignore-errors
      (require 'org)
      (save-excursion
        (goto-char beg)
        (cond
         ((fboundp 'org-fold-show-drawer-or-block)
          (org-fold-show-drawer-or-block))
         ((featurep 'org-fold)
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end nil))))
         ((fboundp 'org-hide-block-toggle)
          (org-hide-block-toggle nil))
         (t nil))
        ;; Org may leave its own ellipsis/placeholder overlays behind after reveal.
        ;; Purge them in the block range so hover reveal shows the full block body.
        (save-excursion
          (goto-char beg)
          (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (carriage-patch-fold--org-suppress-placeholders
             beg (line-end-position)))
          ;; If some Org overlay still keeps the region invisible, force-show it.
          (dolist (ov (overlays-in beg (or (and (re-search-backward "^[ \t]*#\\+begin_patch\\b" nil t)
                                                (save-excursion
                                                  (goto-char beg)
                                                  (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                                                    (line-end-position))))
                                           beg)))
            (when (overlayp ov)
              (let ((inv (overlay-get ov 'invisible))
                    (cat (overlay-get ov 'category)))
                (when (or (eq inv 'org-fold)
                          (eq inv 'org-hide-block)
                          (eq cat 'org-fold)
                          (eq cat 'org-hide-block))
                  (overlay-put ov 'before-string nil)
                  (overlay-put ov 'after-string nil)
                  (overlay-put ov 'display nil)
                  (overlay-put ov 'invisible nil))))))))))

(defun carriage-patch-fold--org-suppress-placeholders (beg end)
  "Best-effort: keep Org folding but suppress Org's own ellipsis/placeholder overlays.

When we fold applied patches we want *one* visible placeholder (Carriage's overlay).
Org folding may also add its own ellipsis via overlay `before-string'/`display'.
This function removes those presentation strings while keeping invisibility."
  (when (and (numberp beg) (numberp end) (< beg end)
             (derived-mode-p 'org-mode))
    (ignore-errors
      (dolist (ov (overlays-in beg end))
        (when (overlayp ov)
          (let ((inv (overlay-get ov 'invisible))
                (cat (overlay-get ov 'category)))
            (when (or (eq inv 'org-fold)
                      (eq inv 'org-hide-block)
                      (eq cat 'org-fold)
                      (eq cat 'org-hide-block))
              (overlay-put ov 'before-string nil)
              (overlay-put ov 'after-string nil)
              (overlay-put ov 'display nil))))))))

(defun carriage-patch-fold--overlay-near (pos)
  "Return a managed applied-patch overlay at or near POS, or nil.

We first check overlays exactly at POS and one char to the right to avoid the
\"stuck at boundary\" case when an overlay starts exactly at POS+1. As a
fallback, scan a tiny neighborhood."
  (let* ((p (or pos (point)))
         (hit nil))
    ;; 1) Exact position
    (dolist (ov (overlays-at p))
      (when (and (overlayp ov)
                 (eq (overlay-get ov 'category) 'carriage-patch-fold))
        (setq hit ov)))
    ;; 2) Right-adjacent position (overlay starting at POS+1)
    (when (and (null hit) (< p (point-max)))
      (dolist (ov (overlays-at (1+ p)))
        (when (and (overlayp ov)
                   (eq (overlay-get ov 'category) 'carriage-patch-fold))
          (setq hit ov))))
    ;; 3) Small neighborhood scan with a right edge strictly beyond start.
    (when (null hit)
      (let* ((lo (max (point-min) (1- p)))
             (hi (min (point-max) (+ p 2))))
        (dolist (ov (overlays-in lo hi))
          (when (and (overlayp ov)
                     (eq (overlay-get ov 'category) 'carriage-patch-fold))
            (setq hit ov)))))
    hit))

(defun carriage-patch-fold--parse-block-at (beg)
  "At BEG (beginning of line), return plist (:beg :end :plist) for a patch block, or nil."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t))
      (when (looking-at "^[ \t]*#\\+begin_patch\\b")
        (let* ((rb (line-beginning-position))
               (hdr-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (pl (carriage-patch-fold--parse-header-plist hdr-line)))
          (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (list :beg rb :end (line-end-position) :plist pl)))))))




(defun carriage-patch-fold--parse-header-plist (line)
  "Return plist parsed from begin_patch LINE or nil."
  (let ((case-fold-search t))
    (when (string-match "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" line)
      (let* ((sexp (match-string 1 line)))
        (condition-case _e
            (let ((obj (car (read-from-string sexp))))
              (and (listp obj) obj))
          (error nil))))))

(defun carriage-patch-fold--block-bounds-at (start)
  "Given point START at beginning of a begin_patch line, return cons (BEG . END) for the block."
  (save-excursion
    (goto-char start)
    (let ((beg (line-beginning-position))
          (case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
        (cons beg (line-end-position))))))

(defun carriage-patch-fold--scan-applied ()
  "Scan buffer for applied begin_patch blocks; return list of cells:
  ((:beg BEG :end END :plist PLIST) ...)"
  (let ((acc '())
        (case-fold-search t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
          (let* ((hdr-line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                 (pl (carriage-patch-fold--parse-header-plist hdr-line))
                 (applied (and (listp pl) (plist-get pl :applied))))
            (when applied
              (let* ((b (line-beginning-position))
                     (rg (carriage-patch-fold--block-bounds-at b)))
                (when (consp rg)
                  (push (list :beg (car rg) :end (cdr rg) :plist pl) acc))))))))
    (nreverse acc)))

(defun carriage-patch-fold--toggle-keymap ()
  "Return keymap for toggling applied-patch visibility with TAB."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'carriage-patch-fold-toggle-at)
    (define-key map [tab] #'carriage-patch-fold-toggle-at)
    map))

(defun carriage-patch-fold--placeholder (pl)
  "Build placeholder string from header PL."
  (let* ((tick (if (display-graphic-p) "✓" "OK"))
         (path (or (plist-get pl :path) (plist-get pl :file) "-"))
         (desc (or (plist-get pl :description)
                   (plist-get pl :result)
                   "Applied"))
         (txt (format "%s %s — %s" tick path desc))
         (s (propertize txt 'face 'carriage-patch-fold-placeholder-face)))
    (let ((map (carriage-patch-fold--toggle-keymap)))
      (add-text-properties 0 (length s)
                           (list 'local-map map
                                 'help-echo "Toggle visibility (TAB)")
                           s))
    s))

(defun carriage-patch-fold--make (beg end pl)
  "Create overlay covering BEG..END (inclusive line bounds) with header PL."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'category 'carriage-patch-fold)
    (overlay-put ov 'carriage-patch-plist pl)
    (overlay-put ov 'carriage-patch-revealed nil)
    (overlay-put ov 'carriage-patch-auto-revealed nil)
    (overlay-put ov 'before-string (carriage-patch-fold--placeholder pl))
    (overlay-put ov 'invisible carriage-patch-fold-invisible-symbol)
    (add-to-invisibility-spec carriage-patch-fold-invisible-symbol)
    (push ov carriage-patch-fold--overlays)
    ov))

(defun carriage-patch-fold--clear-overlays ()
  "Delete all managed overlays in current buffer.

Also purges any stale overlays left behind due to older bugs or lost bookkeeping,
to guarantee a single placeholder per applied patch block."
  ;; First, delete overlays we know about.
  (when (listp carriage-patch-fold--overlays)
    (dolist (ov carriage-patch-fold--overlays)
      (when (overlayp ov) (delete-overlay ov))))
  (setq carriage-patch-fold--overlays nil)
  (
   setq carriage-patch-fold--active-overlay nil)
  ;; Defensive cleanup: if overlays list was stale/cleared incorrectly, remove all
  ;; patch-fold overlays still present in the buffer.
  (ignore-errors
    (save-restriction
      (widen)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlayp ov)
                   (overlay-buffer ov)
                   (or (overlay-get ov 'carriage-patch-plist)
                       (eq (overlay-get ov 'category) 'carriage-patch-fold)))
          (delete-overlay ov))))))

(defun carriage-patch-fold--point-inside-ov-p (ov)
  "Return non-nil when point is inside OV.
Treat overlay end as inside too, to make hover reveal stable at EOL."
  (when (overlayp ov)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov))
          (pt (point)))
      (and (number-or-marker-p beg)
           (number-or-marker-p end)
           (<= beg pt)
           (<= pt end)))))

(defun carriage-patch-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for applied patches.

Invariant for applied patches:
- Carriage placeholder overlay is the only visible fold marker.
- We do NOT re-run Org block hiding on every refresh/post-change, because repeated
  invisibility churn during cursor motion/redisplay can destabilize vertical motion
  (`next-line'/`previous-line', visual-line navigation).

To avoid duplicate ellipses/placeholders, we only suppress Org's own presentation
strings that may already exist inside the folded region, leaving Carriage's
placeholder visible."
  (let ((pt (point))
        (reveal-range nil))
    (when (overlayp carriage-patch-fold--active-overlay)
      (setq reveal-range
            (cons (overlay-start carriage-patch-fold--active-overlay)
                  (overlay-end carriage-patch-fold--active-overlay))))
    (carriage-patch-fold--clear-overlays)
    (when (derived-mode-p 'org-mode)
      (dolist (cell (carriage-patch-fold--scan-applied))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end))
              (pl  (plist-get cell :plist)))
          (when (and (numberp beg) (numberp end) (< beg end))
            (carriage-patch-fold--org-suppress-placeholders beg end)
            (let ((ov (carriage-patch-fold--make beg end pl)))
              (when (and (consp reveal-range)
                         (= beg (car reveal-range))
                         (= end (cdr reveal-range))
                         (<= beg pt) (<= pt end))
                (carriage-patch-fold--org-show-at beg)
                (overlay-put ov 'before-string nil)
                (overlay-put ov 'invisible nil)
                (overlay-put ov 'carriage-patch-revealed t)
                (setq carriage-patch-fold--active-overlay ov)))))))))

(defun carriage-patch-fold--schedule-refresh (&optional delay)
  "Schedule debounced refresh with optional DELAY seconds (default 0.1)."
  (when (timerp carriage-patch-fold--refresh-timer)
    (cancel-timer carriage-patch-fold--refresh-timer))
  (let* ((d (or delay 0.1))
         (buf (current-buffer)))
    (setq carriage-patch-fold--refresh-timer
          (run-at-time d nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (setq carriage-patch-fold--refresh-timer nil)
                             (ignore-errors (carriage-patch-fold--refresh-now)))))))))

(defun carriage-patch-fold--after-change (_beg _end _len)
  "After-change hook to coalesce refresh."
  (carriage-patch-fold--schedule-refresh 0.1))

(defun carriage-patch-fold--post-command ()
  "Reveal applied patch overlay under point; fold it back after point leaves.
Touches at most one active overlay per command to keep vertical motion stable."
  (when (and carriage-patch-fold--enabled
             (derived-mode-p 'org-mode)
             (get-buffer-window (current-buffer) t))
    (let ((active carriage-patch-fold--active-overlay))
      ;; Fold previously revealed overlay when point left it.
      (when (overlayp active)
        (if (carriage-patch-fold--point-inside-ov-p active)
            nil
          (carriage-patch-fold--org-hide-at (overlay-start active))
          (carriage-patch-fold--org-suppress-placeholders
           (overlay-start active) (overlay-end active))
          (overlay-put active 'before-string
                       (carriage-patch-fold--placeholder
                        (overlay-get active 'carriage-patch-plist)))
          (overlay-put active 'invisible carriage-patch-fold-invisible-symbol)
          (overlay-put active 'carriage-patch-revealed nil)
          (setq carriage-patch-fold--active-overlay nil)))

      ;; If nothing is active, reveal overlay exactly under/near point.
      (unless (overlayp carriage-patch-fold--active-overlay)
        (let ((ov (carriage-patch-fold--overlay-near (point))))
          (when (and (overlayp ov)
                     (carriage-patch-fold--point-inside-ov-p ov))
            (carriage-patch-fold--org-show-at (overlay-start ov))
            (overlay-put ov 'before-string nil)
            (overlay-put ov 'invisible nil)
            (overlay-put ov 'carriage-patch-revealed t)
            (setq carriage-patch-fold--active-overlay ov)))))))

;;; Public API

;;;###autoload
(defun carriage-patch-fold-refresh-now (&optional buffer)
  "Public: refresh applied patch overlays now in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-patch-fold--refresh-now)))

(defun carriage-patch-fold-refresh (&optional buffer)
  "Backward-compatible alias for `carriage-patch-fold-refresh-now'."
  (carriage-patch-fold-refresh-now buffer))

(defun carriage-patch-fold-toggle-at (pos)
  "Toggle fold state of an overlay covering POS."
  (interactive "d")
  (let ((hit nil))
    (dolist (ov carriage-patch-fold--overlays)
      (when (and (overlayp ov)
                 (>= pos (overlay-start ov))
                 (<= pos (overlay-end ov)))
        (setq hit ov)))
    (when (overlayp hit)
      (if (overlay-get hit 'carriage-patch-revealed)
          (progn
            ;; Manual fold
            (carriage-patch-fold--org-hide-at (overlay-start hit))
            (carriage-patch-fold--org-suppress-placeholders
             (overlay-start hit) (overlay-end hit))
            (overlay-put hit 'carriage-patch-auto-revealed nil)
            (overlay-put hit 'before-string (carriage-patch-fold--placeholder
                                             (overlay-get hit 'carriage-patch-plist)))
            (overlay-put hit 'invisible carriage-patch-fold-invisible-symbol)
            (overlay-put hit 'carriage-patch-revealed nil))
        ;; Manual reveal
        (carriage-patch-fold--org-show-at (overlay-start hit))
        (overlay-put hit 'carriage-patch-auto-revealed nil)
        (overlay-put hit 'before-string nil)
        (overlay-put hit 'invisible nil)
        (overlay-put hit 'carriage-patch-revealed t)))))

;;;###autoload
(defun carriage-patch-fold-enable (&optional buffer)
  "Enable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-patch-fold--enabled t)
    (setq carriage-patch-fold--active-overlay nil)
    (add-hook 'after-change-functions #'carriage-patch-fold--after-change nil t)
    (add-hook 'post-command-hook #'carriage-patch-fold--post-command nil t)
    (when (derived-mode-p 'org-mode)
      (dolist (cell (carriage-patch-fold--scan-applied))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end)))
          (when (and (numberp beg) (numberp end) (< beg end))
            (carriage-patch-fold--org-hide-at beg)
            (carriage-patch-fold--org-suppress-placeholders beg end)))))
    (carriage-patch-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-patch-fold-disable (&optional buffer)
  "Disable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-patch-fold--enabled nil)
    (setq carriage-patch-fold--active-overlay nil)
    (remove-hook 'after-change-functions #'carriage-patch-fold--after-change t)
    (remove-hook 'post-command-hook #'carriage-patch-fold--post-command t)
    (when (timerp carriage-patch-fold--refresh-timer)
      (cancel-timer carriage-patch-fold--refresh-timer))
    (setq carriage-patch-fold--refresh-timer nil)
    (carriage-patch-fold--clear-overlays)
    t))

;; Auto-enable for carriage-mode when user preferences demand.
(with-eval-after-load 'carriage-mode
  (add-hook 'carriage-mode-hook
            (lambda ()
              (when (and (boundp 'carriage-mode-hide-applied-patches)
                         carriage-mode-hide-applied-patches)
                (ignore-errors (carriage-patch-fold-enable))))))

(provide 'carriage-patch-fold)
;;; carriage-patch-fold.el ends here
