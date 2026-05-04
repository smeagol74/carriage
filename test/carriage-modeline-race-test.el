;;; carriage-modeline-race-test.el --- Modeline race/regression tests -*- lexical-binding: t; -*-

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

(ert-deftest carriage-modeline-recovers-from-late-rewrite ()
  "Simulate an external late rewrite of mode-line-format and ensure Carriage can recover via ensure-once." 
  (with-temp-buffer
    (let ((noninteractive nil)
          (carriage-mode-show-mode-line-ui t))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            (should (carriage--ml-has-segment-p))
            ;; Simulate another package overwriting the modeline after our install
            (setq-local mode-line-format '("EXTERNAL"))
            (should-not (carriage--ml-has-segment-p))
            ;; Call ensure-once to attempt recovery (what our idle retry does)
            (when (fboundp 'carriage-mode--modeline-ensure-once)
              (ignore-errors (carriage-mode--modeline-ensure-once (current-buffer))))
            (should (carriage--ml-has-segment-p)))
        (when (bound-and-true-p carriage-mode) (carriage-mode -1)))))

(ert-deftest carriage-modeline-ensure-idempotent ()
  "Ensure repeated ensures don't duplicate the modeline construct." 
  (with-temp-buffer
    (let ((noninteractive nil)
          (carriage-mode-show-mode-line-ui t))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            (should (carriage--ml-has-segment-p))
            ;; Call ensure multiple times
            (dotimes (_ 3)
              (when (fboundp 'carriage-mode--modeline-ensure-once)
                (ignore-errors (carriage-mode--modeline-ensure-once (current-buffer)))))
            ;; Count occurrences of our construct in mode-line-format
            (let ((cnt (seq-count (lambda (e)
                                   (and (consp e) (eq (car e) :eval) (equal (cadr e) '(carriage-ui--modeline))))
                                 mode-line-format)))
              (should (= cnt 1))))
        (when (bound-and-true-p carriage-mode) (carriage-mode -1)))))

(ert-deftest carriage-modeline-restores-original-on-disable ()
  "Ensure original buffer-local mode-line-format is restored on disable." 
  (with-temp-buffer
    (let ((noninteractive nil)
          (orig-ml '("ORIG")))
      (setq-local mode-line-format orig-ml)
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            (should (carriage--ml-has-segment-p))
            (carriage-mode -1)
            (should (equal mode-line-format orig-ml)))
        (when (bound-and-true-p carriage-mode) (carriage-mode -1))))))

(provide 'carriage-modeline-race-test)
;;; carriage-modeline-race-test.el ends here
