;;; apple-music.el --- Control Apple Music from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Delon Newman

;; Author: Delon Newman <contact@delonnewman.name>
;; Keywords: multimedia
;;

(defvar apple-music--activate-script "tell application \"Music\" to activate")
(defvar apple-music--script-evaluator "osascript")

(defun apple-music-open ()
  (interactive)
  (start-process "Music" "*Apple Music Log*" apple-music--script-evaluator "-e" apple-music--activate-script))
