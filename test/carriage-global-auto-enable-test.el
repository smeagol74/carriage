;;; carriage-global-auto-enable-test.el --- Auto-enable modeline test -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)
(require 'carriage)

(defun carriage--ml-has-segment-p ()
  "Detect if mode-line-format contains our (:eval (carriage-ui--modeline)) segment."
  (seq-some (lambda (e)
              (and (consp e)
                   (eq (car e) :eval)
                   (equal (cadr e) '(carriage-ui--modeline))))
            mode-line-format))

(ert-deftest carriage-global-auto-enables-modeline ()
  "When a buffer contains CARRIAGE_STATE requesting :CAR_MODE t, opening it should enable carriage-mode and install modeline." 
  (with-temp-buffer
    (let ((noninteractive nil)
          (carriage-mode-show-header-line t)
          (carriage-mode-show-mode-line-ui t))
      (org-mode)
      ;; Insert a canonical CARRIAGE_STATE property requesting CAR_MODE
      (goto-char (point-min))
      (insert "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t)\n")
      ;; Simulate the find-file/org-mode hook behavior that tries to auto-enable
      (carriage-global-mode--maybe-auto-enable-doc-state)
      ;; Ensure any deferred modeline ensure runs; call explicitly as the real
      ;; startup also schedules a zero-delay timer.
      (when (fboundp 'carriage-mode--modeline-ensure-once)
        (ignore-errors (carriage-mode--modeline-ensure-once (current-buffer))))
      (unwind-protect
          (progn
            (should (bound-and-true-p carriage-mode))
            (should (carriage--ml-has-segment-p)))
        (when (bound-and-true-p carriage-mode) (carriage-mode -1))))))

(provide 'carriage-global-auto-enable-test)
;;; carriage-global-auto-enable-test.el ends here
