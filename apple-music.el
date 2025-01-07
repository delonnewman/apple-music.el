;;; apple-music.el --- Control Apple Music from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Delon Newman

;; Author: Delon Newman <contact@delonnewman.name>
;; Keywords: multimedia
;;

(require 'dash)

(defun applescript--compile-list (list)
  (concat "{" (s-join ", " (--map (applescript-compile it) list)) "}"))

(defun applescript--taggedp (form)
  (and (listp form) (symbolp (car form))))

(defun applescript--compile-item (form)
  (concat "item " (applescript-compile (cadr form))))

(defun applescript--compile-of (form)
  (concat (applescript-compile (cadr form)) " of " (applescript-compile (caddr form))))


;; set, of
(defun applescript-compile (form)
  (cond ((numberp form) (number-to-string form))
        ((stringp form) (concat "\"" form "\""))
        ((symbolp form) (symbol-name form))
        ((listp form)
         (cl-case (first form)
           (item (applescript--compile-item form))
           (of (applescript--compile-of form))
           (t (applescript--compile-list form))))
        ))

;; (applescript-compile '(of (item 1) (1 2 3)))


(defvar apple-music--script-evaluator "osascript")

(let ((buffer nil))
  (defun apple-music--get-buffer ()
    (if (and buffer (buffer-live-p buffer))
        buffer
      (setq buffer (generate-new-buffer "*Apple Music Log*")))))


(defvar apple-music--activate-script "tell application \"Music\" to activate")
(defun apple-music-open ()
  (interactive)
  (start-process "Music" (apple-music--get-buffer) apple-music--script-evaluator "-e" apple-music--activate-script))


(defvar apple-music--play-items '(playlist))


(defvar apple-music--play-script "tell application \"Music\" to play%s")
(defun apple-music-play (&optional specifier)
  (interactive)
  (let ((param (if specifier (concat " " specifier) "")))
    (start-process "Music" (apple-music--get-buffer) apple-music--script-evaluator "-e" (format apple-music--play-script param))))


(defvar apple-music--pause-script "tell application \"Music\" to pause")
(defun apple-music-pause ()
  (interactive)
  (start-process "Music" (apple-music--get-buffer) apple-music--script-evaluator "-e" apple-music--pause-script))

;; (apple-music-open)
;; (apple-music-pause)
;; (apple-music-play)
(apple-music-play "After The Rain")
