;;; carriage-ui-modeline-cache.el --- Modeline cache key computation  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Modeline cache key computation helpers.
;; Independent module.

;;; Code:

(defvar-local carriage-ui--ml-cache nil)
(defvar-local carriage-ui--ml-cache-key nil)

(defun carriage-ui--ml-cache-key ()
  "Compute cache key for the modeline string based on current UI environment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (blocks (or (and (listp carriage-ui-modeline-blocks) carriage-ui-modeline-blocks)
                     carriage-ui--modeline-default-blocks))
         (state (and (boundp 'carriage--ui-state) carriage--ui-state))
         (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
         (apply-ver (and (memq 'apply-status blocks) (or carriage-ui--apply-badge-version 0)))
         (doc-cost-ver (and (memq 'doc-cost blocks) (or carriage-ui--doc-cost-version 0)))
         (state-tt-ver (and (memq 'state blocks)
                            (or (and (boundp 'carriage-ui--state-tooltip-version)
                                     carriage-ui--state-tooltip-version)
                                0)))
         (http-code (and (memq 'state blocks)
                         (boundp 'carriage--last-http-status)
                         carriage--last-http-status))
         (http-text (and (memq 'state blocks)
                         (boundp 'carriage--last-http-status-text)
                         carriage--last-http-status-text))
         (be-err (and (memq 'state blocks)
                      (boundp 'carriage--last-backend-error)
                      carriage--last-backend-error))
         (err-class (and (memq 'state blocks)
                         (boundp 'carriage--last-error-class)
                         carriage--last-error-class))
         (err-detail (and (memq 'state blocks)
                          (boundp 'carriage--last-error-detail)
                          carriage--last-error-detail))
         (ctx-limited (and (memq 'state blocks)
                           (boundp 'carriage--last-context-limited)
                           carriage--last-context-limited))
         (ctx-omitted (and (memq 'state blocks)
                          (boundp 'carriage--last-context-omitted)
                          carriage--last-context-omitted))
         (req-cost-key
          (and (memq 'req-cost blocks)
               (list
                (and (boundp 'carriage--last-cost)
                     (listp carriage--last-cost)
                     (plist-get carriage--last-cost :cost-total-u))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :tokens-in))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :tokens-out))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :bytes-in))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :bytes-out))
                (and (boundp 'carriage--last-model-id)
                     carriage--last-model-id))))
         (patch-count (and (memq 'patch blocks)
                        (not (memq state '(sending streaming dispatch waiting reasoning)))
                        (numberp carriage-ui--patch-count-cache)
                        carriage-ui--patch-count-cache))
         (has-last (and (memq 'all blocks)
                        (carriage-ui--last-iteration-present-p)))
         (spin (and carriage-ui-enable-spinner
                    (memq state '(sending streaming dispatch waiting reasoning))
                    (carriage-ui--spinner-char)))
         (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
         (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
    (list uicons
          state spin state-tt-ver http-code http-text be-err err-class err-detail
          ctx-limited ctx-omitted
          ctx-ver apply-ver doc-cost-ver req-cost-key
          patch-count has-last abortp blocks
          (and (boundp 'carriage-mode-intent) carriage-mode-intent)
          (and (boundp 'carriage-mode-suite) carriage-mode-suite)
          (and (boundp 'carriage-mode-model) carriage-mode-model)
          (and (boundp 'carriage-mode-backend) carriage-mode-backend)
          (and (boundp 'carriage-mode-provider) carriage-mode-provider)
          (and (boundp 'carriage-apply-engine) carriage-apply-engine)
          (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
          branch-t
          (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
          (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
          (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
          (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
          (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope))))

(provide 'carriage-ui-modeline-cache)
;;; carriage-ui-modeline-cache.el ends here