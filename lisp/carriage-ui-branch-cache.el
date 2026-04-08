;;; carriage-ui-branch-cache.el --- Branch name caching  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Branch name caching for modeline.
;; Independent module - no heavy dependencies.

;;; Code:

(defcustom carriage-ui-branch-cache-ttl 30.0
  "TTL in seconds for cached VCS branch name used in the modeline."
  :type '(choice (const :tag "Unlimited" nil) number)
  :group 'carriage-ui)

(defcustom carriage-ui-branch-async-refresh t
  "When non-nil, fetch Git branch name asynchronously off the redisplay path."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-branch-refresh-delay 0.07
  "Idle delay (seconds) before attempting an asynchronous branch refresh."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--branch-cache-string nil)
(defvar-local carriage-ui--branch-cache-time 0)

(defun carriage-ui--branch-name-cached ()
  "Return VCS branch name for the current buffer using a lightweight cache."
  (let* ((ttl carriage-ui-branch-cache-ttl)
         (now (float-time))
         (valid (or (null ttl)
                   (< (- now (or carriage-ui--branch-cache-time 0)) (or ttl 0)))))
    (if (and carriage-ui--branch-cache-string valid)
        carriage-ui--branch-cache-string
      (when (not (and (fboundp 'carriage-git--repo-present-p)
                      (carriage-git--repo-present-p default-directory)))
        (setq carriage-ui--branch-cache-string :no-git
              carriage-ui--branch-cache-time now)
        (cl-return-from carriage-ui--branch-name-cached :no-git))
      (let* ((br
              (or
               (let* ((s (and (boundp 'vc-mode) vc-mode)))
                 (when (stringp s)
                   (cond
                    ((string-match "[: -]\\([^: -]+\\)\\'" s) (match-string 1 s))
                    (t nil))))
               (when (require 'vc-git nil t)
                 (ignore-errors
                   (when (fboundp 'vc-git--symbolic-branch)
                     (vc-git--symbolic-branch default-directory))))
               (condition-case _e
                   (carriage-git-current-branch default-directory)
                 (error nil)))))
        (setq carriage-ui--branch-cache-string br
              carriage-ui--branch-cache-time now)
        br))))

(provide 'carriage-ui-branch-cache)
;;; carriage-ui-branch-cache.el ends here