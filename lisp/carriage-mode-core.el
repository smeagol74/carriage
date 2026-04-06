;;; carriage-mode-core.el --- Core mode definition and keymap for Carriage  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Core mode definition, keymap and buffer-local variables.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom carriage-mode-default-intent 'Ask
  "Default Intent for Carriage: 'Ask | 'Code | 'Hybrid."
  :type '(choice (const Ask) (const Code) (const Hybrid))
  :group 'carriage)

(defcustom carriage-mode-default-suite 'aibo
  "Default Suite: one of 'sre, 'aibo or 'udiff."
  :type '(choice (const sre) (const aibo) (const udiff))
  :group 'carriage)

(defcustom carriage-mode-default-model "gptel-default"
  "Default model name for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-default-backend 'gptel
  "Default backend for Carriage."
  :type 'symbol
  :group 'carriage)

(defcustom carriage-mode-state-file ".context/carriage/carriage-state.el"
  "State file path for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-report-open-policy 'on-error
  "When to automatically open apply report."
  :type '(choice (const on-error) (const always) (const never))
  :group 'carriage)

(defcustom carriage-mode-show-diffs t
  "Show diffs after apply."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-confirm-apply nil
  "Prompt for confirmation before apply."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-use-icons t
  "Use icons in mode-line/header-line."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-spinner-interval 0.7
  "Spinner animation interval."
  :type 'number
  :group 'carriage)

(defcustom carriage-mode-include-gptel-context nil
  "Include gptel context."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-include-doc-context t
  "Include document context."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-include-visible-context nil
  "Include visible buffer context."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-context-max-files 200
  "Max files in context."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-context-max-total-bytes 16777216
  "Max total bytes in context."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-wip-branch "carriage/WIP"
  "WIP branch name."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-show-header-line t
  "Show header-line."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-show-mode-line-ui t
  "Show mode-line UI."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-allow-patch-binary nil
  "Allow binary patches."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-require-patch-description t
  "Require patch description."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-hide-applied-patches t
  "Hide applied patches."
  :type 'boolean
  :group 'carriage)

(defvar-local carriage-mode-intent carriage-mode-default-intent
  "Current Intent for this buffer.")

(defvar-local carriage-mode-suite carriage-mode-default-suite
  "Current Suite for this buffer.")

(defvar-local carriage-mode-model carriage-mode-default-model
  "Current model for this buffer.")

(defvar-local carriage-mode-backend carriage-mode-default-backend
  "Current backend for this buffer.")

(defvar-local carriage-mode-provider nil
  "Current provider for this buffer.")

(defvar carriage-mode-map (make-sparse-keymap)
  "Keymap for Carriage mode.")

(defvar carriage-mode nil
  "Non-nil if Carriage mode is active.")

(make-variable-buffer-local 'carriage-mode)

(provide 'carriage-mode-core)
;;; carriage-mode-core.el ends here
