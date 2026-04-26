;;; carriage-transport-core.el --- Core transport helpers  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;; Small file with helper utilities for the transport layer. These are
;; defined only when not already present to make the split safe and
;; incremental.
;;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;###autoload
(unless (fboundp 'carriage-transport--short-backtrace)
  (defun carriage-transport--short-backtrace (&optional max-lines)
    "Return a short backtrace string (best-effort) limited to MAX-LINES.
Never signals."
    (let ((max-lines (or max-lines 18)))
      (condition-case _e
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (backtrace))
            (goto-char (point-min))
            (let ((i 0))
              (while (and (< i max-lines) (not (eobp)))
                (forward-line 1)
                (setq i (1+ i)))
              (buffer-substring-no-properties (point-min) (point))))
        (error nil)))))

(unless (fboundp 'carriage-transport--rid-next)
  (defvar carriage-transport--request-counter 0
    "Monotonic counter for transport request ids (rid).")
  (defun carriage-transport--rid-next ()
    "Generate a new transport request id (rid)."
    (setq carriage-transport--request-counter (1+ (or carriage-transport--request-counter 0)))
    (format "rid-%d" carriage-transport--request-counter)))

(unless (fboundp 'carriage-transport--send-entry-current-p)
  (defun carriage-transport--send-entry-current-p (entry-id &optional buffer)
    "Return non-nil when ENTRY-ID is still the current send owner for BUFFER.
This is a small helper used by the transport layer to ignore stale callbacks."
    (with-current-buffer (or buffer (current-buffer))
      (and (stringp entry-id)
           (boundp 'carriage--current-send-entry-id)
           (stringp carriage--current-send-entry-id)
           (string= entry-id carriage--current-send-entry-id)))))

(unless (fboundp 'carriage-transport-invalidate-send-owner)
  (defun carriage-transport-invalidate-send-owner (&optional buffer)
    "Invalidate current send owner token for BUFFER.
This prevents stale callbacks from resurrecting a finished send lifecycle."
    (with-current-buffer (or buffer (current-buffer))
      (when (boundp 'carriage--current-send-entry-id)
        (setq carriage--current-send-entry-id nil))
      t)))

(unless (fboundp 'carriage-transport-current-request-id)
  (defun carriage-transport-current-request-id (&optional buffer)
    "Return current transport request id for BUFFER (or current buffer)."
    (with-current-buffer (or buffer (current-buffer))
      (and (boundp 'carriage-transport--request-id) carriage-transport--request-id))))

(provide 'carriage-transport-core)
;;; carriage-transport-core.el ends here
