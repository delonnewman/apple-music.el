;;; apple-music.el --- Control Apple Music from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Delon Newman

;; Author: Delon Newman <contact@delonnewman.name>
;; Keywords: multimedia
;;

(require 'applescript)

(defun apple-music-open ()
  (interactive)
  (applescript (tell (application "Music") (activate))))


(cl-defun apple-music-play (&optional specifier &key shuffle)
  (interactive)
  (applescript-eval
   `(tell (application "Music")
     (activate)
     ,(when shuffle '(set shuffle-enabled t))
     ,(if specifier `(play ,specifier) '(play)))))


(defun apple-music-pause ()
  (interactive)
  (applescript (tell (application "Music") (activate) (pause))))
