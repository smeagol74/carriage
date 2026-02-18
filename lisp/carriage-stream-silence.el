;;; carriage-stream-silence.el --- Silent, low-overhead stream inserts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: performance, streaming

;;; Commentary:
;; Reduce per-chunk overhead during streaming by performing the final, coalesced
;; insert "silently":
;;  - with-silent-modifications (avoid redisplay churn/jit-lock)
;;  - inhibit-modification-hooks / inhibit-point-motion-hooks
;;  - avoid growing undo history for stream flushes (let ((buffer-undo-list t)) …)
;;
;; This is implemented as an around-advice for `carriage--stream-insert-at-end'
;; (the low-level inserter used by the streaming coalescer).
;;
;; Notes:
;; - The coalescing/flush cadence remains controlled by carriage-stream-* modules.
;; - GC guard is already handled by transport begin/complete; we do not touch it here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-stream-silence nil
  "Silent, low-overhead stream inserts for Carriage."
  :group 'carriage)

(defun carriage--stream-silence--around-insert (orig-fn &rest args)
  "Around advice for `carriage--stream-insert-at-end' to minimize hot-path overhead.
Calls ORIG-FN with ARGS under silent insert guards."
  (let ((inhibit-modification-hooks t)
        (inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        ;; Do not record undo entries for streaming flush chunks
        (buffer-undo-list t))
    (with-silent-modifications
      (apply orig-fn args))))

(defun carriage-stream-silence--install ()
  "Install silent-insert advice if the target function is available."
  (when (fboundp 'carriage--stream-insert-at-end)
    (advice-add 'carriage--stream-insert-at-end
                :around #'carriage--stream-silence--around-insert)
    t))

(defun carriage-stream-silence--maybe-install ()
  "Idempotently install advice after relevant modules load."
  ;; Try common providers of the streaming inserter.
  (with-eval-after-load 'carriage-stream-perf
    (ignore-errors (carriage-stream-silence--install)))
  (with-eval-after-load 'carriage-mode
    (ignore-errors (carriage-stream-silence--install))))

;; Attempt eager install (if function is already present), then set up hooks.
(ignore-errors (carriage-stream-silence--install))
(carriage-stream-silence--maybe-install)

(provide 'carriage-stream-silence)
;;; carriage-stream-silence.el ends here
