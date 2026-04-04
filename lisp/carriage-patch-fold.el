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

(defvar-local carriage-patch-fold--revealed-ranges nil
  "List of revealed applied-patch ranges as cons cells (BEG . END).")

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

(defun carriage-patch-fold--make (beg end pl &optional revealed)
  "Create overlay covering BEG..END (inclusive line bounds) with header PL.
When REVEALED is non-nil, create a visible overlay without placeholder."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'priority 1001)
    (overlay-put ov 'category 'carriage-patch-fold)
    (overlay-put ov 'carriage-patch-plist pl)
    (overlay-put ov 'carriage-patch-revealed (and revealed t))
    (if revealed
        (progn
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'display nil)
          (overlay-put ov 'after-string nil)
          (overlay-put ov 'invisible nil))
      (overlay-put ov 'before-string (carriage-patch-fold--placeholder pl))
      (overlay-put ov 'display nil)
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'invisible carriage-patch-fold-invisible-symbol)
      (add-to-invisibility-spec carriage-patch-fold-invisible-symbol))
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



(defun carriage-patch-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for applied patches.

Invariant for applied patches:
- Carriage overlay is the only owner of fold state.
- Folded state = placeholder + invisible on the patch overlay.
- Revealed state = same overlay without placeholder/invisibility.
- Refresh preserves only manually revealed blocks tracked by range."
  (let ((revealed-ranges
         (or carriage-patch-fold--revealed-ranges
             (delq nil
                   (mapcar (lambda (ov)
                             (when (and (overlayp ov)
                                        (overlay-get ov 'carriage-patch-revealed))
                               (cons (overlay-start ov) (overlay-end ov))))
                           carriage-patch-fold--overlays)))))
    (carriage-patch-fold--clear-overlays)
    (setq carriage-patch-fold--revealed-ranges nil)
    (when (derived-mode-p 'org-mode)
      (dolist (cell (carriage-patch-fold--scan-applied))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end))
              (pl  (plist-get cell :plist))
              (revealed nil))
          (when (and (numberp beg) (numberp end) (< beg end))
            (setq revealed (member (cons beg end) revealed-ranges))
            (carriage-patch-fold--make beg end pl revealed)
            (when revealed
              (push (cons beg end) carriage-patch-fold--revealed-ranges))))))))

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



;;; Public API

;;;###autoload
(defun carriage-patch-fold-refresh-now (&optional buffer)
  "Public: refresh applied patch overlays now in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-patch-fold--refresh-now)))

(defun carriage-patch-fold-refresh (&optional buffer)
  "Backward-compatible alias for `carriage-patch-fold-refresh-now'."
  (carriage-patch-fold-refresh-now buffer))

(defun carriage-patch-fold--overlay-at-or-near (pos)
  "Return patch-fold overlay at POS or immediately adjacent to it."
  (let ((best nil))
    (dolist (ov (reverse carriage-patch-fold--overlays))
      (when (and (overlayp ov)
                 (overlay-buffer ov)
                 (eq (overlay-get ov 'category) 'carriage-patch-fold))
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (when (and (numberp beg) (numberp end))
            (cond
             ((and (>= pos beg) (<= pos end))
              (setq best ov)
              (cl-return))
             ((= pos end)
              (setq best ov))
             ((= pos (1+ end))
              (setq best ov))
             ((= pos beg)
              (setq best ov)))))))
    best))

(defun carriage-patch-fold-toggle-at (pos)
  "Toggle fold state of a patch-fold overlay at or near POS."
  (interactive "d")
  (let ((hit (carriage-patch-fold--overlay-at-or-near pos)))
    (when (overlayp hit)
      (let ((range (cons (overlay-start hit) (overlay-end hit))))
        (if (overlay-get hit 'carriage-patch-revealed)
            (progn
              (overlay-put hit 'before-string
                           (carriage-patch-fold--placeholder
                            (overlay-get hit 'carriage-patch-plist)))
              (overlay-put hit 'display nil)
              (overlay-put hit 'after-string nil)
              (overlay-put hit 'invisible carriage-patch-fold-invisible-symbol)
              (add-to-invisibility-spec carriage-patch-fold-invisible-symbol)
              (overlay-put hit 'priority 1001)
              (move-overlay hit (overlay-start hit) (overlay-end hit))
              (overlay-put hit 'carriage-patch-revealed nil)
              (setq carriage-patch-fold--revealed-ranges
                    (delete range carriage-patch-fold--revealed-ranges)))
          (overlay-put hit 'before-string nil)
          (overlay-put hit 'display nil)
          (overlay-put hit 'after-string nil)
          (overlay-put hit 'invisible nil)
          (overlay-put hit 'priority 1001)
          (move-overlay hit (overlay-start hit) (overlay-end hit))
          (overlay-put hit 'carriage-patch-revealed t)
          (cl-pushnew range carriage-patch-fold--revealed-ranges :test #'equal))))))

;;;###autoload
(defun carriage-patch-fold-enable (&optional buffer)
  "Enable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-patch-fold--enabled t)
    (setq carriage-patch-fold--revealed-ranges nil)
    (add-hook 'after-change-functions #'carriage-patch-fold--after-change nil t)
    (carriage-patch-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-patch-fold-disable (&optional buffer)
  "Disable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-patch-fold--enabled nil)
    (setq carriage-patch-fold--revealed-ranges nil)
    (remove-hook 'after-change-functions #'carriage-patch-fold--after-change t)
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
