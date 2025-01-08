;;; apple-music.el --- Control Apple Music from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Delon Newman

;; Author: Delon Newman <contact@delonnewman.name>
;; Keywords: multimedia
;;

(require 'dash)
(require 's)

(defun applescript--compile-boolean (boolean)
  (if boolean "true" "false"))

(defun applescript--compile-symbol (symbol)
  (s-join " " (s-split "-" (symbol-name symbol))))

(defun applescript--compile-item (form)
  (concat "item " (applescript-compile (cadr form))))

(defun applescript--compile-of (form)
  (concat (applescript-compile (cadr form)) " of " (applescript-compile (caddr form))))

(defun applescript--compile-set (form)
  (concat "set " (applescript-compile (cadr form)) " to " (applescript-compile (caddr form))))

(defun applescript--compile-tell (subject &rest body)
  (if (= 1 (length body))
    (concat "tell " (applescript-compile subject) " to " (applescript-compile (car body)))
    (concat "tell " (applescript-compile subject) "\n"
            (s-join "\n" (--map (concat "  " (applescript-compile it)) body))
            "\nend tell")))
;; (applescript--compile-tell '(application "Music") '(set suffle-enabled t) '(play (playlist "Piano Chill")))

(defun applescript--compile-list (list)
  (concat "{" (s-join ", " (--map (applescript-compile it) list)) "}"))

(defun applescript--compile-application (form)
  ;; may want to provide a lookup for valid names
  (concat (symbol-name (car form)) " " (s-join " " (-map #'applescript-compile (cdr form)))))

(defun applescript-compile (form)
  "Compile FORM to AppleScript code."
  (cond
   ((numberp form) (number-to-string form))
   ((stringp form) (concat "\"" form "\""))
   ((booleanp form) (applescript--compile-boolean form))
   ((symbolp form) (applescript--compile-symbol form))
   ((listp form)
    (if (not (symbolp (car form)))
        (applescript--compile-list form)
        (cl-case (first form)
          (item (applescript--compile-item form))
          (of (applescript--compile-of form))
          (set (applescript--compile-set form))
          (tell (apply #'applescript--compile-tell (cdr form)))
          (t (applescript--compile-application form)))))))

(defvar apple-music--script-evaluator "osascript")

(defun applescript-eval (form)
  (let ((code (applescript-compile form)))
    (set-buffer (apple-music--get-buffer))
    (insert "Executing:\n" code)
    (start-process "AppleScript" (apple-music--get-buffer) apple-music--script-evaluator "-e" code)))

;; (applescript-compile '(of (item 1) (1 2 3)))
;; (applescript-compile '(set shuffle-enabled t))
;; (applescript-compile '(tell (application "Music")
;;                        (activate)
;;                        (set suffle-enabled t)
;;                        (play (playlist "Piano Chill"))))

;; (applescript-eval '(tell (application "Music")
;;                        (activate)
;;                        (set suffle-enabled t)
;;                        (play (playlist "Piano Chill"))))


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
;; (apple-music-play "After The Rain")
