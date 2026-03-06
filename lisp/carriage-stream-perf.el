;;; carriage-stream-perf.el --- Streaming perf guards (silent inserts, hooks off) -*- lexical-binding: t; -*-

;; Lightweight performance helpers for streaming:
;; - wrap low-level stream insert in with-silent-modifications and inhibit-modification-hooks
;; - keep implementation separate to avoid touching carriage-mode core

(require 'subr-x)

(defun carriage--stream-perf--around-insert (orig-fn &rest args)
  "Around advice for `carriage--stream-insert-at-end' to reduce overhead during streaming."
  (let ((inhibit-modification-hooks t))
    (with-silent-modifications
      (apply orig-fn args))))

(with-eval-after-load 'carriage-mode
  (when (fboundp 'carriage--stream-insert-at-end)
    (unless (or (advice-member-p #'carriage--stream-perf--around-insert 'carriage--stream-insert-at-end)
                (advice-member-p #'carriage--stream-silence--around-insert 'carriage--stream-insert-at-end))
      (advice-add 'carriage--stream-insert-at-end :around #'carriage--stream-perf--around-insert))))

(provide 'carriage-stream-perf)
;;; carriage-stream-perf.el ends here
