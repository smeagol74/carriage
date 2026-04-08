;;; carriage-ui-header-helpers.el --- Header-line helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Header-line helper functions.
;; Requires carriage-ui functions at runtime.

;;; Code:

(defvar-local carriage-ui--rendering-window nil
  "Window currently being rendered for header-line (:eval).")

(defun carriage-ui--hl-sep ()
  "Separator used between header-line segments."
  " › ")

(defun carriage-ui--left-pad (&optional pixels)
  "Return a left padding spacer of PIXELS (default 3) using a display property."
  (let ((px (or pixels 3)))
    (propertize " " 'display (list 'space :width (cons 'pixels px)))))

(defun carriage-ui--icon-gap (&optional pixels)
  "Return a small fixed-size gap between an icon and its label."
  (propertize " " 'display (list 'space :width (cons 'pixels (or pixels 4)))))

(defun carriage-ui--hl-build-seg (label icon)
  "Build a segment from LABEL and optional ICON."
  (if icon (concat icon (carriage-ui--icon-gap) label) label))

(defun carriage-ui--hl-mute-tail (s)
  "Apply muted face to text after the icon separator."
  (if (not (stringp s)) s
    (let* ((start (let ((m (string-match "[^ ]" s))) (if (integerp m) m 0)))
           (idx (string-match " " s (1+ start))))
      (if (and (integerp idx) (< idx (length s)))
          (let ((head (substring s 0 (1+ idx)))
                (tail (substring s (1+ idx))))
            (concat head (propertize tail 'face 'carriage-ui-muted-face)))
        (propertize (copy-sequence s) 'face 'carriage-ui-muted-face)))))

(defun carriage-ui--hl-clickable-outline (s)
  "Make outline string S clickable to jump to the heading."
  (let* ((omap (let ((m (make-sparse-keymap)))
                  (define-key m [header-line mouse-1] #'carriage-ui-goto-outline)
                  m)))
    (propertize (or s "")
                'mouse-face 'mode-line-highlight
                'help-echo "Перейти к заголовку (mouse-1)"
                'local-map omap)))

(defun carriage-ui--hl-show-outline-p (tty avail)
  "Return non-nil when outline should be shown given TTY flag and AVAIL width."
  (and carriage-mode-headerline-show-outline (not tty) (> avail 30)))

(defun carriage-ui--hl-fit (pseg bseg oseg show-outline avail sep)
  "Truncate segments to fit AVAIL width. Returns list (P B O)."
  (let* ((base (concat pseg sep bseg))
         (full (if show-outline (concat base sep oseg) base)))
    (when (> (length full) avail)
      (when show-outline
        (let* ((ol-max (max 10 (- avail (length base) (length sep)))))
          (setq oseg (carriage-ui--truncate-middle oseg ol-max))
          (setq full (concat base sep oseg))))
      (when (> (length full) avail)
        (let* ((buf-max (max 10 (- avail (length pseg) (length sep)))))
          (setq bseg (carriage-ui--truncate-middle bseg buf-max))
          (setq base (concat pseg sep bseg))
          (setq full (if show-outline (concat base sep oseg) base))))
      (when (> (length full) avail)
        (setq pseg (carriage-ui--truncate-middle pseg (max 5 (- avail (length sep) (length bseg)))))
        (setq base (concat pseg sep bseg))
        (setq full (if show-outline (concat base sep oseg) base))))
    (list pseg bseg oseg)))

(provide 'carriage-ui-header-helpers)
;;; carriage-ui-header-helpers.el ends here