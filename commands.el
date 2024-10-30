;;; commands.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jose Mazzarelli
;;
;; Author: Jose Mazzarelli <mazzarelli@gmail.com>
;; Maintainer: Jose Mazzarelli <mazzarelli@gmail.com>
;; Created: October 29, 2024
;; Modified: October 29, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/joey/commands
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

Symbol’s value as variable is void: %

;;; commands.el ends here
;;; Commands for Aider

(defun aider-add-current-file ()
  "Add the current file to the Aider system.")
(defun aider-add-files-in-current-window ()
  "Add files in the current window to the Aider system.")
(defun aider-batch-add-dired-marked-files ()
  "Batch add files marked in Dired to the Aider system.")
(defun aider-repo-find-name-dired ()
  "Find a name in the Aider repo using Dired.")
(defun aider-git-repo-root-dired ()
  "Open the Git repo root in Dired.")

(defun aider-current-file-read-only ()
  "Open the current file in read-only mode.")
(defun aider-send-line-under-cursor ()
  "Send the line under the cursor to Aider.")
(defun aider-send-paragraph ()
  "Send the current paragraph to Aider.")

(defun aider-code-change ()
  "Change code in the current buffer.")
(defun aider-region-refactor ()
  "Refactor the selected region.")
(defun aider-undo-last-change ()
  "Undo the last change made in the buffer.")

(defun aider-ask-question ()
  "Ask a question in the Aider system.")
(defun aider-architect-discussion ()
  "Start an architectural discussion in Aider.")
(defun aider-region-explain ()
  "Explain the selected region in Aider.")
(defun aider-debug-exception ()
  "Debug the last exception in Aider.")

(defun aider-general-command ()
  "Execute a general command in Aider.")
(defun aider-help ()
  "Display help for Aider commands.")
(defun aider-magit-show-last-commit ()
  "Show the last commit in Magit.")

(defun aider-exit ()
  "Exit the Aider mode.")
(provide 'aider-commands)
