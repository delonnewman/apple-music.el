;;; applescript-eval.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Delon Newman
;;
;; Author: Delon Newman <contact@delonnewman.name>
;; Maintainer: Delon Newman <contact@delonnewman.name>
;; Created: January 15, 2025
;; Modified: January 15, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/delonnewman/applescript-eval
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)
(require 's)
(require 'applescript-mode)


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
    (concat "tell " (applescript-compile subject) "\r"
            (s-join "\r" (--map (concat "  " (applescript-compile it)) body))
            "\rend tell")))


(defun applescript--compile-list (list)
  (concat "{" (s-join ", " (--map (applescript-compile it) list)) "}"))


(defun applescript--compile-application (form)
  ;; may want to provide a lookup for valid names
  (concat (symbol-name (car form)) " " (s-join " " (-map #'applescript-compile (cdr form)))))


(defun applescript-compile (form)
  "Compile FORM to AppleScript code."
  (cond
   ((eq nil form) "")
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


(defun applescript-eval (form)
  (let ((code (applescript-compile form)))
    (unless (s-blank-str? code)
      (as-execute-string (as-string-to-sjis-string-with-escape code)))))


(defmacro applescript (form)
  `(applescript-eval (quote ,form)))

;; (macroexpand '(applescript (tell (application "Music") (activate))))
;; (applescript (tell (application "Music") (activate)))

(provide 'applescript)
;;; applescript.el ends here
