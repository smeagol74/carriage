;;; carriage-mode-fingerprint.el --- Fingerprint and iteration tracking  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Per-send fingerprint (inline, near iteration marker) and iteration tracking.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(defcustom carriage-mode-insert-fingerprint t
  "When non-nil, Send commands insert an inline fingerprint line at the stream origin."
  :type 'boolean
  :group 'carriage)

(defvar-local carriage--fingerprint-inline-inserted nil
  "Non-nil when an inline CARRIAGE_FINGERPRINT line has been inserted for the current stream.")

(defvar-local carriage--fingerprint-line-marker nil
  "Marker pointing to the beginning of the current request's inline fingerprint line, or nil.")

(defvar-local carriage--separator-inserted nil
  "Non-nil when a visual separator '-----' was already inserted for the current stream.")

(defvar-local carriage--last-iteration-has-patches nil
  "Non-nil when the current \"last iteration\" region contains at least one begin_patch block.")

(defvar-local carriage--stream-detect-tail ""
  "Small tail buffer used to detect begin_patch tokens split across stream chunks.")

(defun carriage--fingerprint-plist ()
  "Return a sanitized fingerprint plist for the current buffer."
  (list
   :CAR_TS (float-time)
   :CAR_INTENT (and (boundp 'carriage-mode-intent) carriage-mode-intent)
   :CAR_SUITE  (and (boundp 'carriage-mode-suite) carriage-mode-suite)
   :CAR_BACKEND  (and (boundp 'carriage-mode-backend) carriage-mode-backend)
   :CAR_PROVIDER (and (boundp 'carriage-mode-provider) carriage-mode-provider)
   :CAR_MODEL    (and (boundp 'carriage-mode-model) carriage-mode-model)
   :CAR_CTX_DOC     (and (boundp 'carriage-mode-include-doc-context)
                         carriage-mode-include-doc-context)
   :CAR_CTX_GPTEL   (and (boundp 'carriage-mode-include-gptel-context)
                         carriage-mode-include-gptel-context)
   :CAR_CTX_VISIBLE (and (boundp 'carriage-mode-include-visible-context)
                         carriage-mode-include-visible-context)
   :CAR_CTX_PLAIN  (and (boundp 'carriage-mode-include-plain-text-context)
                        carriage-mode-include-plain-text-context)
   :CAR_CTX_PATCHED (and (boundp 'carriage-mode-include-patched-files)
                         carriage-mode-include-patched-files)
   :CAR_CTX_MAP (and (boundp 'carriage-mode-include-project-map)
                     carriage-mode-include-project-map)
   :CAR_DOC_CTX_SCOPE (or (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
                          (and (boundp 'carriage-mode-doc-context-scope) carriage-mode-doc-context-scope))
   :CAR_CTX_PROFILE   (or (and (boundp 'carriage-context-profile) carriage-context-profile)
                          (and (boundp 'carriage-mode-context-profile) carriage-mode-context-profile))
   :CAR_CTX_MAX_FILES (and (boundp 'carriage-mode-context-max-files)
                           carriage-mode-context-max-files)
   :CAR_CTX_MAX_BYTES (and (boundp 'carriage-mode-context-max-total-bytes)
                           carriage-mode-context-max-total-bytes)
   :CAR_CTX_INJECTION (and (boundp 'carriage-mode-context-injection)
                           carriage-mode-context-injection)))

(provide 'carriage-mode-fingerprint)
;;; carriage-mode-fingerprint.el ends here