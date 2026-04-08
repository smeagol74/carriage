;;; carriage-ui-button-builders.el --- Modeline button builders  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Modeline button builders and clickable elements.

;;; Code:

(defvar-local carriage-ui--button-cache nil)

(defun carriage-ui--ml-button (label fn help)
  "Return a clickable LABEL that invokes FN, preserving LABEL's text properties."
  (let* ((kprops (and (stringp label) (text-properties-at 0 label)))
         (key (list label help fn kprops))
         (cache (or carriage-ui--button-cache
                    (setq carriage-ui--button-cache (make-hash-table :test 'equal))))
         (hit (gethash key cache)))
    (if (stringp hit)
        hit
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] fn)
        (let* ((s (copy-sequence (or label "")))
               (len (length s)))
          (when (> len 0)
            (add-text-properties
             0 len
             (list 'mouse-face 'mode-line-highlight
                   'help-echo help
                   'local-map map)
             s))
          (puthash key s cache)
          s)))))

(defun carriage-ui--toggle-icon (key onp)
  "Return colored icon for toggle KEY based on ONP, with caching."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (ckey (list 'toggle key (if onp t nil)))
           (hit (gethash ckey cache)))
      (if (stringp hit)
          hit
        (let* ((face (if onp
                          (pcase key
                            ('auto    'carriage-ui-accent-blue-face)
                            ('diffs   'carriage-ui-accent-orange-face)
                            ('confirm 'carriage-ui-accent-purple-face)
                            ('icons   'carriage-ui-accent-cyan-face)
                            ('ctx     'carriage-ui-accent-blue-face)
                            ('files   'carriage-ui-accent-purple-face)
                            (_        'carriage-ui-accent-blue-face))
                        'carriage-ui-muted-face))
               (fg (carriage-ui--accent-hex face))
               (fplist (list :inherit nil :foreground fg))
               (res
                (pcase key
                  ('auto
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "autorenew"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('diffs
                   (cond
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "diff"
                                            :height carriage-mode-icon-height
                                            :v-adjust carriage-mode-icon-v-adjust
                                            :face fplist))
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "difference"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist))
                    (t nil)))
                  ('confirm
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "done_all"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('icons
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "image"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('ctx
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "toc"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('files
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "folder"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  (_ nil))))
          (puthash ckey res cache)
          res)))))

(defun carriage-ui--toggle (key label onp fn help)
  "Build a toggle button with KEY, LABEL, ONP state, FN and HELP."
  (let* ((icon (carriage-ui--toggle-icon key onp))
         (content (carriage-ui--hl-build-seg label icon)))
    (carriage-ui--ml-button content fn help)))

(defvar-local carriage-ui--report-hist nil)

(defun carriage-ui--maybe-in-report-buffer ()
  "Check if current buffer is a report buffer with selected row."
  (and (boundp 'carriage-report--history)
       carriage-report--history
       (eq (current-buffer) (get-buffer "*carriage-report*"))
       (string= (buffer-name) "*carriage-report*")))

(defun carriage-ui--diff-button ()
  "Open Diff for report item if available."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-show-diff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then press RET or [Diff]"))))
    (error
     (message "Нет доступного отчёта для Diff"))))

(defun carriage-ui--ediff-button ()
  "Open Ediff for report item if available."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-ediff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then e or [Ediff]"))))
    (error
     (message "Нет доступного отчёта для Ediff"))))

(provide 'carriage-ui-button-builders)
;;; carriage-ui-button-builders.el ends here