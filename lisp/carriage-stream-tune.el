;;; carriage-stream-tune.el --- Streaming path tunings (quiet inserts) -*- lexical-binding: t; -*-

;; Lightweight, low-risk tunings around the streaming inserter to reduce
;; jit-lock/overlay churn on each flush. Installed as advices after carriage-mode
;; is loaded. No behavior change; only wraps the final insert in a quiet section.

(require 'subr-x)

(defun carriage--stream-tune--around-insert (orig-fn s)
  "Wrap streaming insert S with quiet modifications to reduce hooks/redisplay churn."
  (let ((inhibit-modification-hooks t))
    (with-silent-modifications
      (funcall orig-fn s))))

(with-eval-after-load 'carriage-mode
  (when (fboundp 'carriage--stream-insert-at-end)
    (unless (or (advice-member-p #'carriage--stream-tune--around-insert 'carriage--stream-insert-at-end)
                (advice-member-p #'carriage--stream-silence--around-insert 'carriage--stream-insert-at-end))
      (advice-add 'carriage--stream-insert-at-end :around #'carriage--stream-tune--around-insert))))

(provide 'carriage-stream-tune)
;;; carriage-stream-tune.el ends here
