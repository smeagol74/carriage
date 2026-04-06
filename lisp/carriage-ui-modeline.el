;;; carriage-ui-modeline.el --- Modeline segment builders for Carriage UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Modeline segment builders for Carriage UI.
;; Extracted from carriage-ui.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-ui--icons-available-p "carriage-ui" ())
(declare-function carriage-ui--icon "carriage-ui" (key &optional fallback))
(declare-function carriage-ui--icon-gap "carriage-ui" (&optional pixels))
(declare-function carriage-ui--ml-button "carriage-ui" (label fn help))

(defcustom carriage-ui-modeline-blocks nil
  "Which segments to include in mode-line. Default uses all."
  :type '(repeat symbol)
  :group 'carriage-ui)

(defvar-local carriage-ui--modeline-default-blocks
  '(intent suite model engine state apply-status context branch patch all diff ediff abort report save
    toggle-ctx toggle-files toggle-patched toggle-map toggle-visible toggle-plain toggle-typed
    doc-scope-all doc-scope-last settings doc-cost req-cost))

(defvar-local carriage-ui--ml-cache nil)
(defvar-local carriage-ui--ml-cache-key nil)
(defvar-local carriage-ui--ml-cache-valid-tick nil)

(defun carriage-ui--ml-seg-intent ()
  "Build Intent segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (label
          (if uicons
              (pcase intent
                ('Ask   (or (carriage-ui--icon 'ask) "Ask"))
                ('Code  (or (carriage-ui--icon 'patch) "Code"))
                (_      (or (carriage-ui--icon 'hybrid) "Hybrid")))
            (format "%s" (pcase intent ('Ask "Ask") ('Code "Code") (_ "Hybrid"))))))
    (carriage-ui--ml-button label #'carriage-ui-toggle-intent "Toggle Ask/Code/Hybrid intent")))

(defun carriage-ui--ml-seg-suite ()
  "Build Suite segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (suite-str (cond ((symbolp suite) (symbol-name suite))
                         ((stringp suite) suite) (t "udiff")))
         (icon (and uicons (carriage-ui--icon 'suite)))
         (label
          (if icon
              icon
            (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :suite) "Suite")))
              (format "%s: %s" name suite-str))))
         (help0 (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                    (carriage-i18n :suite-tooltip)
                  "Select Suite (sre|udiff)"))
         (help (format "%s\nТекущее значение: %s" help0 suite-str)))
    (carriage-ui--ml-button label #'carriage-select-suite help)))

(defun carriage-ui--ml-seg-model ()
  "Build Model segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (model-val (and (boundp 'carriage-mode-model) carriage-mode-model))
         (raw-model (cond
                     ((stringp model-val) model-val)
                     ((symbolp model-val) (symbol-name model-val))
                     ((null model-val) "")
                     (t (format "%s" model-val))))
         (resolved (ignore-errors (carriage-llm-resolve-model carriage-mode-backend carriage-mode-provider raw-model)))
         (display-source (if (and (stringp resolved) (not (string-empty-p resolved)))
                                resolved
                              raw-model))
         (display-name (or (and (stringp display-source)
                                (ignore-errors (carriage-llm-display-name display-source)))
                           "-"))
         (bm-text (format "%s" (if (and (stringp display-name)
                                        (not (string-empty-p display-name)))
                                    display-name
                                  "-")))
         (ic (and uicons (carriage-ui--icon 'model)))
         (full-id
          (let* ((candidate (and (stringp resolved) resolved))
                 (has-colon (and candidate (string-match-p ":" candidate))))
            (cond
             (has-colon candidate)
             (t (or (and (fboundp 'carriage-llm-make-full-id)
                         (carriage-llm-make-full-id carriage-mode-backend carriage-mode-provider (or display-source raw-model)))
                    (and (stringp display-source) display-source)
                    (and (stringp raw-model) raw-model)
                    "-")))))
         (btn (carriage-ui--ml-button bm-text
                                       #'carriage-select-model
                                       (format "Модель: %s (клик — выбрать)"
                                               (or full-id "-")))))
    (if (and ic (stringp btn))
        (concat ic (carriage-ui--icon-gap) btn)
      (or btn "-"))))

(defun carriage-ui--ml-seg-engine ()
  "Build Engine segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (engine-str
          (let ((e (and (boundp 'carriage-apply-engine) carriage-apply-engine)))
            (cond
             ((null e) "git")
             ((eq e 'git)
              (let ((pol (and (boundp 'carriage-git-branch-policy)
                              carriage-git-branch-policy)))
                (format "git:%s" (if (symbolp pol) (symbol-name pol) ""))))
             ((symbolp e) (symbol-name e))
             ((stringp e) e)
             (t "git"))))
         (icon (and uicons (carriage-ui--icon 'engine)))
         (eng (and (boundp 'carriage-apply-engine) carriage-apply-engine))
         (policy-sym (and (eq eng 'git)
                          (boundp 'carriage-git-branch-policy)
                          carriage-git-branch-policy))
         (policy-str (if (symbolp policy-sym) (symbol-name policy-sym) ""))
         (help0 (cond
                 ((and (eq eng 'git)
                       (featurep 'carriage-i18n) (fboundp 'carriage-i18n)
                       (stringp policy-str))
                  (carriage-i18n :engine-tooltip-branch engine-str policy-str))
                 ((and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                  (carriage-i18n :engine-tooltip))
                 (t "Select apply engine")))
         (help (format "%s\nТекущее значение: %s" (or help0 "") engine-str))
         (label
          (if icon
              icon
            (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :engine-label)
                          "Engine")))
              (format "%s: %s" (or name "Engine") engine-str)))))
    (carriage-ui--ml-button (or label "Engine") #'carriage-select-apply-engine (or help ""))))

(defun carriage-ui--ml-seg-state ()
  "Build State segment with spinner and face."
  (let* ((st (let ((s (and (boundp 'carriage--ui-state) carriage--ui-state)))
               (if (symbolp s) s 'idle)))
         (http-code (and (boundp 'carriage--last-http-status) carriage--last-http-status))
         (http-text (and (boundp 'carriage--last-http-status-text) carriage--last-http-status-text))
         (be-err (and (boundp 'carriage--last-backend-error) carriage--last-backend-error))
         (mid (and (boundp 'carriage--last-model-id) carriage--last-model-id))
         (ctx-limited (and (boundp 'carriage--last-context-limited) carriage--last-context-limited))
         (ctx-omitted (and (boundp 'carriage--last-context-omitted) carriage--last-context-omitted))
         (ctx-ic (when ctx-limited
                   (or (and (carriage-ui--icons-available-p)
                            (carriage-ui--icon 'ctx-limit))
                       "⚠")))
         (err-detail
          (cond
           ((and (stringp http-code) (not (string-empty-p http-code))) http-code)
           ((and (boundp 'carriage--last-error-detail)
                 (stringp carriage--last-error-detail)
                 (not (string-empty-p carriage--last-error-detail)))
            carriage--last-error-detail)
           ((and (boundp 'carriage--last-error-class)
                 (symbolp carriage--last-error-class))
            (pcase carriage--last-error-class
              ('LLM_E_TIMEOUT "timeout")
              ('LLM_E_BACKEND "backend")
              ('LLM_E_AUTH "auth")
              ('LLM_E_RATE "rate")
              ('LLM_E_UNKNOWN "error")
              (_ (format "%s" carriage--last-error-class))))
           ((and (stringp be-err) (not (string-empty-p be-err))) be-err)
           (t nil)))
         (spinner (when (memq st '(sending streaming waiting reasoning dispatch))
                    (carriage-ui--spinner-char)))
         (face
          (pcase st
            ('idle 'carriage-ui-state-idle-face)
            ('sending 'carriage-ui-state-sending-face)
            ('streaming 'carriage-ui-state-sending-face)
            ('dispatch 'carriage-ui-state-active-face)
            ('waiting 'carriage-ui-state-active-face)
            ('reasoning 'carriage-ui-state-active-face)
            ('done 'carriage-ui-state-success-face)
            ('error 'carriage-ui-state-error-face)
            (_ 'carriage-ui-state-idle-face)))
         (label
          (concat (or spinner "")
                  (pcase st
                    ('idle (if http-text (format "idle:%s" http-text) "idle"))
                    ('sending "sending")
                    ('streaming "streaming")
                    ('dispatch "dispatch")
                    ('waiting "waiting")
                    ('reasoning "reasoning")
                    ('done (if http-text (format "done:%s" http-text) "done"))
                    ('error (or err-detail "error"))
                    (t "idle")))))
    (propertize label 'face face)))

(provide 'carriage-ui-modeline)
;;; carriage-ui-modeline.el ends here
