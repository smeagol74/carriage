;;; carriage-ui-patches.el --- Patch count caching and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Patch block count caching and range tracking.
;; Independent module for performance.

;;; Code:

(defvar-local carriage-ui--patch-count-cache nil
  "Cached count of #+begin_patch blocks in the current buffer.")
(defvar-local carriage-ui--patch-count-tick nil
  "Buffer tick corresponding to `carriage-ui--patch-count-cache'.")
(defvar-local carriage-ui--patch-count-cache-time 0
  "Timestamp (float seconds) of the last patch count recomputation.")

(defcustom carriage-ui-patch-count-refresh-interval 0.4
  "Minimum seconds between patch-count recomputations in a buffer."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--patch-refresh-timer nil
  "Timer object for a scheduled patch-count refresh (buffer-local).")

(defvar-local carriage-ui--patch-last-refresh-time 0.0
  "Timestamp (float seconds) of the last completed patch-count refresh in this buffer.")

(defvar-local carriage-ui--patch-ranges nil
  "Cached list of (BEG . END) ranges for #+begin_patch blocks in the current buffer.")
(defvar-local carriage-ui--patch-ranges-tick nil)
(defvar-local carriage-ui--last-patch-range nil)

(defun carriage-ui--patch-refresh-now (&optional buffer)
  "Recompute patch ranges/count for BUFFER and refresh modeline."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (ignore-errors (carriage-ui--get-patch-ranges))
      (setq carriage-ui--patch-last-refresh-time (float-time))
      (setq carriage-ui--patch-count-cache-time carriage-ui--patch-last-refresh-time)
      (when (fboundp 'carriage-ui--invalidate-ml-cache)
        (carriage-ui--invalidate-ml-cache))
      (force-mode-line-update t))
    t))

(defun carriage-ui--patch-schedule-refresh (&optional delay)
  "Schedule a throttled patch-count refresh for the current buffer."
  (when (timerp carriage-ui--patch-refresh-timer)
    (ignore-errors (cancel-timer carriage-ui--patch-refresh-timer)))
  (let* ((buf (current-buffer))
         (now (float-time))
         (interval (max 0.05 (or carriage-ui-patch-count-refresh-interval 0.4)))
         (age (- now (or carriage-ui--patch-last-refresh-time 0.0)))
         (d (cond
             ((numberp delay) delay)
             ((>= age interval) 0.05)
             (t (max 0.05 (- interval age))))))
    (setq carriage-ui--patch-refresh-timer
          (run-at-time
           (max 0.01 (float d)) nil
           (lambda (b)
             (when (buffer-live-p b)
               (with-current-buffer b
                 (setq carriage-ui--patch-refresh-timer nil)
                 (ignore-errors (carriage-ui--patch-refresh-now b)))))
           buf)))
  t)

(defun carriage-ui--patch-after-change (_beg _end _len)
  "After-change hook: coalesce patch-count refresh requests."
  (when (derived-mode-p 'org-mode)
    (carriage-ui--patch-schedule-refresh)))

(defun carriage-ui--get-patch-ranges ()
  "Return cached list of patch block ranges as cons cells (BEG . END)."
  (if (not (derived-mode-p 'org-mode))
      nil
    (let ((tick (buffer-chars-modified-tick)))
      (if (and carriage-ui--patch-ranges
               carriage-ui--patch-ranges-tick
               (= tick carriage-ui--patch-ranges-tick))
          carriage-ui--patch-ranges
        (let ((ranges '()))
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search t))
              (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
                (let ((beg (match-beginning 0)))
                  (if (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                      (let ((end (match-beginning 0)))
                        (push (cons beg end) ranges))
                    (push (cons beg (point-max)) ranges))))))
          (setq ranges (nreverse ranges))
          (setq carriage-ui--patch-ranges ranges
                carriage-ui--patch-ranges-tick tick
                carriage-ui--patch-count-cache (length ranges)
                carriage-ui--patch-count-tick tick)
          ranges)))))

(provide 'carriage-ui-patches)
;;; carriage-ui-patches.el ends here