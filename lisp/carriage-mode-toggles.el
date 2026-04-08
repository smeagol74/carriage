;;; carriage-mode-toggles.el --- Toggles and helpers for Carriage  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Toggle functions and UI accessibility helpers.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

;;;###autoload
(defvar-local carriage-mode-auto-open-report t
  "When non-nil, automatically open report after dry-run.")

;;;###autoload
(defvar-local carriage-mode-confirm-apply-all nil
  "When non-nil, require confirmation before applying all blocks.")

;;;###autoload
(defvar-local carriage-mode-include-plain-text-context t
  "When non-nil, include plain text (outside typed blocks) in payload.")

;;;###autoload
(defun carriage-toggle-include-plain-text-context ()
  "Toggle inclusion of plain text (outside typed blocks) in payload for this buffer."
  (interactive)
  (setq-local carriage-mode-include-plain-text-context
              (not carriage-mode-include-plain-text-context))
  (message "Carriage: plain text %s"
           (if carriage-mode-include-plain-text-context "ON" "OFF"))
  (when (fboundp 'carriage-ui--ctx-invalidate)
    (ignore-errors (carriage-ui--ctx-invalidate)))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-auto-open-report ()
  "Toggle auto-opening report after dry-run."
  (interactive)
  (setq-local carriage-mode-auto-open-report (not carriage-mode-auto-open-report))
  (message "Auto-open report: %s" (if carriage-mode-auto-open-report "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-show-diffs ()
  "Toggle requirement to show diffs before apply."
  (interactive)
  (setq-local carriage-mode-show-diffs (not carriage-mode-show-diffs))
  (message "Show diffs before apply: %s" (if carriage-mode-show-diffs "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-confirm-apply-all ()
  "Toggle confirmation before applying all blocks."
  (interactive)
  (setq-local carriage-mode-confirm-apply-all (not carriage-mode-confirm-apply-all))
  (message "Confirm apply-all: %s" (if carriage-mode-confirm-apply-all "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-use-icons ()
  "Toggle using icons in the UI (requires all-the-icons)."
  (interactive)
  (setq-local carriage-mode-use-icons (not carriage-mode-use-icons))
  (message "Use icons: %s" (if carriage-mode-use-icons "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-gptel-context ()
  "Toggle including gptel-context (buffers/files) into the request context."
  (interactive)
  (setq-local carriage-mode-include-gptel-context (not carriage-mode-include-gptel-context))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include gptel-context: %s" (if carriage-mode-include-gptel-context "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-doc-context ()
  "Toggle including file contents from the nearest #+begin_context block in the document."
  (interactive)
  (setq-local carriage-mode-include-doc-context (not carriage-mode-include-doc-context))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include #+begin_context files: %s" (if carriage-mode-include-doc-context "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-visible-context ()
  "Toggle including visible buffers (current frame) into the request context."
  (interactive)
  (setq-local carriage-mode-include-visible-context
              (not (and (boundp 'carriage-mode-include-visible-context)
                        carriage-mode-include-visible-context)))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include visible buffers: %s"
           (if (and (boundp 'carriage-mode-include-visible-context)
                    carriage-mode-include-visible-context)
               "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-project-map ()
  "Toggle including a gitignore-aware repository file tree (begin_map) into the request context."
  (interactive)
  (setq-local carriage-mode-include-project-map
              (not (and (boundp 'carriage-mode-include-project-map)
                        carriage-mode-include-project-map)))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include project map: %s"
           (if (and (boundp 'carriage-mode-include-project-map)
                    carriage-mode-include-project-map)
               "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-intent ()
  "Toggle intent cycle: Ask -> Code -> Hybrid -> Ask."
  (interactive)
  (setq carriage-mode-intent
        (pcase carriage-mode-intent
          ('Ask 'Code)
          ('Code 'Hybrid)
          (_ 'Ask)))
  (message "Intent: %s" carriage-mode-intent)
  (force-mode-line-update t))

(provide 'carriage-mode-toggles)
;;; carriage-mode-toggles.el ends here