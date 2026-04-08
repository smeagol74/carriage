;;; carriage-mode-send-helpers.el --- Send buffer/subtree helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Send buffer and send subtree logic helpers.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(defun carriage--send-buffer--prepare-stream (origin-marker)
  "Prepare carriage streaming environment at ORIGIN-MARKER."
  (carriage-stream-reset origin-marker)
  (when (fboundp 'carriage-begin-iteration)
    (ignore-errors (carriage-begin-iteration)))
  (when (fboundp 'carriage-insert-send-separator)
    (ignore-errors (carriage-insert-send-separator)))
  (when (fboundp 'carriage-insert-inline-fingerprint-now)
    (ignore-errors (carriage-insert-inline-fingerprint-now)))
  (when (fboundp 'carriage--preloader-start)
    (ignore-errors (carriage--preloader-start))))

(defun carriage--send-buffer--deferred-dispatch (srcbuf origin-marker prefix generation)
  "Deferred LLM send prep/dispath for carriage-send-buffer."
  (let ((current-prefix-arg prefix))
    (when (buffer-live-p srcbuf)
      (with-current-buffer srcbuf
        (if (carriage--send-generation-stale-p generation srcbuf)
            (progn
              (carriage-log "send-buffer: deferred tick dropped as stale")
              (setq carriage--send-in-flight nil)
              (setq carriage--send-dispatch-scheduled nil))
          (progn
            (carriage-log "send-buffer: deferred tick enter")
            (condition-case err
                (carriage--send-prepare-and-dispatch
                 'buffer srcbuf carriage-mode-backend carriage-mode-model
                 carriage-mode-intent carriage-mode-suite
                 (or (and (markerp carriage--stream-origin-marker)
                          (buffer-live-p (marker-buffer carriage--stream-origin-marker))
                          carriage--stream-origin-marker)
                     origin-marker))
              (quit
               (carriage-log "send-buffer: deferred tick quit")
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'idle)))
              (error
               (carriage-log "send-buffer: deferred tick ERROR: %s" (error-message-string err))
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'error))
               (unless (bound-and-true-p noninteractive)
                 (message "Carriage: send-buffer crashed: %s" (error-message-string err)))))))))))

(defun carriage--send-buffer--calc-origin-marker ()
  "Compute stream origin marker for carriage-send-buffer."
  (copy-marker
   (progn
     (when (or (not (bolp))
               (save-excursion
                 (beginning-of-line)
                 (re-search-forward "[^ \t]" (line-end-position) t)))
       (end-of-line)
       (insert "\n"))
     (point))
    t))

(defun carriage--send-subtree--calc-origin-marker ()
  "Compute stream origin marker for carriage-send-subtree."
  (copy-marker
   (progn
     (when (or (not (bolp))
               (save-excursion
                 (beginning-of-line)
                 (re-search-forward "[^ \t]" (line-end-position) t)))
       (end-of-line)
       (insert "\n"))
     (point))
    t))

(defun carriage--send-subtree--prepare-stream (origin-marker)
  "Prepare carriage streaming environment at ORIGIN-MARKER for send-subtree."
  (carriage-stream-reset origin-marker)
  (when (fboundp 'carriage-begin-iteration)
    (ignore-errors (carriage-begin-iteration)))
  (when (fboundp 'carriage-insert-inline-fingerprint-now)
    (ignore-errors (carriage-insert-inline-fingerprint-now)))
  (when (fboundp 'carriage--preloader-start)
    (ignore-errors (carriage--preloader-start))))

(defun carriage--send-subtree--deferred-dispatch (srcbuf origin-marker prefix generation)
  "Deferred LLM send prep/dispath for carriage-send-subtree."
  (let ((current-prefix-arg prefix))
    (when (buffer-live-p srcbuf)
      (with-current-buffer srcbuf
        (if (carriage--send-generation-stale-p generation srcbuf)
            (progn
              (carriage-log "send-subtree: deferred tick dropped as stale")
              (setq carriage--send-in-flight nil)
              (setq carriage--send-dispatch-scheduled nil))
          (progn
            (carriage-log "send-subtree: deferred tick enter")
            (condition-case err
                (carriage--send-prepare-and-dispatch
                 'subtree srcbuf carriage-mode-backend carriage-mode-model
                 carriage-mode-intent carriage-mode-suite
                 (or (and (markerp carriage--stream-origin-marker)
                          (buffer-live-p (marker-buffer carriage--stream-origin-marker))
                          carriage--stream-origin-marker)
                     origin-marker))
              (quit
               (carriage-log "send-subtree: deferred tick quit")
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'idle)))
              (error
               (carriage-log "send-subtree: deferred tick ERROR: %s" (error-message-string err))
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'error))
               (unless (bound-and-true-p noninteractive)
                 (message "Carriage: send-subtree crashed: %s" (error-message-string err)))))))))))

(provide 'carriage-mode-send-helpers)
;;; carriage-mode-send-helpers.el ends here