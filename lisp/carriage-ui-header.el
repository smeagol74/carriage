;;; carriage-ui-header.el --- Header-line for Carriage UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Header-line rendering for Carriage UI.
;; Extracted from carriage-ui.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-ui--project-name "carriage-ui" ())
(declare-function carriage-ui--icons-available-p "carriage-ui" ())
(declare-function carriage-ui--icon "carriage-ui" (key &optional fallback))
(declare-function carriage-ui--left-pad "carriage-ui" ())

(defvar-local carriage-ui--rendering-window nil)
(defvar-local carriage-ui--hl-cache nil)
(defvar-local carriage-ui--hl-cache-key nil)
(defvar-local carriage-ui--hl-last-outline-path-str nil)

(defun carriage-ui--hl-sep ()
  "Return header-line separator."
  (if (carriage-ui--icons-available-p) " › " " > "))

(defun carriage-ui--hl-show-outline-p (tty avail)
  "Determine whether to show outline based on TTY and available width AVAIL."
  (not (or tty (< avail 30))))

(defun carriage-ui--hl-build-seg (text icon)
  "Build header-line segment with TEXT and ICON."
  (if icon
      (concat icon (carriage-ui--icon-gap) text)
    text))

(defun carriage-ui--hl-fit (pseg bseg oseg show-outline avail sep)
  "Fit segments to available width."
  (list pseg bseg oseg))

(defun carriage-ui--hl-mute-tail (seg)
  "Mute tail of segment for visual hierarchy."
  (if (and (stringp seg) (> (length seg) 0))
      (propertize seg 'face 'carriage-ui-muted-face)
    seg))

(defun carriage-ui--header-line-for (win)
  "Wrapper to render header-line for WIN."
  (let ((carriage-ui--rendering-window (and (window-live-p win) win)))
    (carriage-ui--header-line)))

(defun carriage-ui--header-line ()
  "Build header-line: [icon] project › [icon] buffer › org-outline-path."
  (let* ((project (carriage-ui--project-name))
         (bufname (buffer-name))
         (win (or carriage-ui--rendering-window (selected-window)))
         (use-icons (carriage-ui--icons-available-p))
         (icon-env (and use-icons (carriage-ui--icon-cache-env-current)))
         (sep (carriage-ui--hl-sep))
         (w (or (ignore-errors
                  (and (window-live-p win) (window-total-width win)))
                80))
         (maxw (or (and (boundp 'carriage-mode-headerline-max-width)
                        carriage-mode-headerline-max-width)
                   w))
         (reserve 10)
         (avail (max 0 (- maxw reserve)))
         (tty (not (display-graphic-p)))
         (show-outline (carriage-ui--hl-show-outline-p tty avail))
         (outline (and show-outline (or carriage-ui--hl-last-outline-path-str "")))
         (cache-key (list avail show-outline project bufname outline use-icons icon-env)))
    (if (and (equal cache-key carriage-ui--hl-cache-key)
             (stringp carriage-ui--hl-cache))
        carriage-ui--hl-cache
      (let* ((p-icon (and use-icons (carriage-ui--icon 'project)))
             (f-icon (and use-icons (carriage-ui--icon 'file)))
             (pseg (concat (carriage-ui--left-pad) (carriage-ui--hl-build-seg project p-icon)))
             (bseg (carriage-ui--hl-build-seg bufname f-icon))
             (oseg (or outline "")))
        (cl-destructuring-bind (p1 b1 o1)
            (carriage-ui--hl-fit pseg bseg oseg show-outline avail sep)
          (let* ((p2 (if show-outline
                         (carriage-ui--hl-mute-tail p1)
                       (carriage-ui--hl-mute-tail p1)))
                 (b2 (carriage-ui--hl-mute-tail b1)))
            (let* ((base (concat p2 sep b2))
                   (full (if show-outline
                             (concat base sep o1)
                           base)))
              (setq carriage-ui--hl-cache full)
              (setq carriage-ui--hl-cache-key cache-key)
              full)))))))

(provide 'carriage-ui-header)
;;; carriage-ui-header.el ends here
