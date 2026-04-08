;;; carriage-mode-streaming.el --- Streaming and preloader for Carriage  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Streaming insertion state, preloader, and streaming performance helpers.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(defvar-local carriage--undo-change-group nil)

(defun carriage--undo-group-start ()
  "Start a change group for streaming if supported and not already active."
  (when (and (fboundp 'prepare-change-group)
             (fboundp 'activate-change-group)
             (null carriage--undo-change-group))
    (let ((cg (prepare-change-group)))
      (setq carriage--undo-change-group cg)
      (activate-change-group cg)
      cg)))

(defun carriage--undo-group-accept ()
  "Accept the active change group for streaming and insert a boundary."
  (when carriage--undo-change-group
    (when (fboundp 'accept-change-group)
      (accept-change-group carriage--undo-change-group))
    (setq carriage--undo-change-group nil)
    (when (fboundp 'undo-boundary)
      (ignore-errors (undo-boundary)))
    t))

(defun carriage--undo-group-cancel ()
  "Cancel the active change group for streaming."
  (when carriage--undo-change-group
    (when (fboundp 'cancel-change-group)
      (cancel-change-group carriage--undo-change-group))
    (setq carriage--undo-change-group nil)
    t))

(defcustom carriage-mode-reasoning-log-verbose nil
  "When non-nil, emit verbose reasoning begin/end logs to *carriage-log*."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-preloader-enabled t
  "When non-nil, show a lightweight preloader spinner at the insertion point before streaming starts."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-preloader-interval 0.2
  "Update interval, in seconds, for the buffer preloader spinner."
  :type 'number :group 'carriage)

(defcustom carriage-mode-preloader-face 'mode-line-emphasis
  "Face used to render the in-buffer preloader spinner."
  :type 'face :group 'carriage)

(defcustom carriage-mode-preloader-window-local nil
  "When non-nil, show the spinner only in the window where streaming started."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-preloader-follow-point nil
  "When non-nil, keep point after the spinner during streaming."
  :type 'boolean :group 'carriage)

(defconst carriage--preloader-frames-unicode
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames for the buffer preloader (Unicode).")

(defconst carriage--preloader-frames-ascii
  ["." ".." "..."]
  "Spinner frames for the buffer preloader (ASCII fallback).")

(defvar-local carriage--preloader-overlay nil)
(defvar-local carriage--preloader-timer nil)
(defvar-local carriage--preloader-index 0)

(defun carriage--preloader-frames ()
  "Return vector of frames appropriate for current display."
  (if (display-graphic-p)
      carriage--preloader-frames-unicode
    carriage--preloader-frames-ascii))

(defun carriage--preloader--newline-hidden-p (nlpos)
  "Return non-nil when newline at NLPOS is visually hidden by an overlay."
  (when (and (numberp nlpos)
             (>= nlpos (point-min))
             (< nlpos (point-max))
             (eq (char-after nlpos) ?\n))
    (or (get-char-property nlpos 'invisible)
        (get-char-property nlpos 'display)
        (cl-some (lambda (ov)
                   (or (overlay-get ov 'invisible)
                       (overlay-get ov 'display)))
                 (overlays-at nlpos)))))

(defun carriage--preloader--render (pos)
  "Render preloader at POS, updating overlay text."
  (unless (and (boundp 'carriage--preloader-overlay)
               (overlayp carriage--preloader-overlay))
    (setq carriage--preloader-overlay (make-overlay pos pos)))
  (let* ((frames (carriage--preloader-frames))
         (n (length frames))
         (i (mod (or carriage--preloader-index 0) (max 1 n)))
         (frame (aref frames i))
         (need-leading-nl
          (and (numberp pos)
               (> pos (point-min))
               (eq (char-before pos) ?\n)
               (carriage--preloader--newline-hidden-p (1- pos))))
         (prefix (if need-leading-nl "\n" "")))
    (when (overlayp carriage--preloader-overlay)
      (overlay-put carriage--preloader-overlay 'after-string nil)
      (overlay-put carriage--preloader-overlay 'before-string
                   (propertize (concat prefix frame "\n")
                               'face carriage-mode-preloader-face))
      (let ((w (get-buffer-window (current-buffer) t)))
        (when (window-live-p w)
          (force-window-update w))))))

(defun carriage--preloader-start (pos)
  "Start the preloader spinner at POS."
  (when carriage-mode-preloader-enabled
    (carriage--preloader-stop)
    (setq carriage--preloader-index 0)
    (setq carriage--preloader-timer
          (run-at-time
           0 carriage-mode-preloader-interval
           (lambda ()
             (when (and (buffer-live-p (get-buffer (buffer-name)))
                        carriage-mode-preloader-enabled)
               (with-current-buffer (buffer-name)
                 (cl-incf carriage--preloader-index)
                 (carriage--preloader--render
                  (if carriage-mode-preloader-window-local
                      (if (get-buffer-window (current-buffer) t)
                          (window-start (get-buffer-window (current-buffer) t))
                        (point-max))
                    (point-max))))))))))

(defun carriage--preloader-stop ()
  "Stop the preloader spinner."
  (when (timerp carriage--preloader-timer)
    (cancel-timer carriage--preloader-timer))
  (setq carriage--preloader-timer nil)
  (when (overlayp carriage--preloader-overlay)
    (delete-overlay carriage--preloader-overlay))
  (setq carriage--preloader-overlay nil)
  (setq carriage--preloader-index 0))

(defvar-local carriage--stream-beg-marker nil)
(defvar-local carriage--stream-end-marker nil)
(defvar-local carriage--stream-origin-marker nil)

(defcustom carriage-stream-flush-interval 0.03
  "Interval (seconds) to coalesce incoming stream chunks before inserting into the buffer."
  :type 'number
  :group 'carriage)

(defcustom carriage-stream-flush-bytes-threshold 8192
  "Flush pending stream queue early when accumulated bytes reach this threshold."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'carriage)

(defcustom carriage-stream-redisplay-interval 0.05
  "Interval (seconds) for streaming redisplay updates."
  :type 'number
  :group 'carriage)

(defcustom carriage-stream-perf-mode nil
  "When non-nil, enable streaming performance optimizations."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-stream-coalesce-char-threshold 128
  "Minimum chars to coalesce before forcing flush (perf mode)."
  :type 'integer
  :group 'carriage)

(defcustom carriage-stream-coalesce-time-threshold 0.05
  "Maximum time to wait before flushing pending stream (perf mode)."
  :type 'number
  :group 'carriage)

(defcustom carriage-stream-silence-logs nil
  "When non-nil, suppress streaming logs in *carriage-log*."
  :type 'boolean
  :group 'carriage)

(defvar-local carriage--stream-accumulator nil)
(defvar-local carriage--stream-flush-timer nil)
(defvar-local carriage--stream-redisplay-timer nil)
(defvar-local carriage--stream-redisplay-pending nil)
(defvar-local carriage--stream-last-chunk-time nil)

(provide 'carriage-mode-streaming)
;;; carriage-mode-streaming.el ends here