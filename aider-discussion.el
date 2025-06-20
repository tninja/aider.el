;;; aider-discussion.el --- Discussion operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides discussion functionality for the Aider package.

;;; Code:

(require 'which-func)

(require 'aider-core)
(require 'aider-file)

;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question (&optional prefix)
  "Ask aider question about specific code.
Focuses on understanding, analyzing, improving the selected code or function.
With a prefix argument PREFIX, calls `aider-general-question` instead."
  (interactive "P")
  (if prefix
      (aider-general-question)
    ;; Dispatch to general question if in aider buffer
    (let* ((function-name (which-function))
           (region-active (region-active-p))
           (region-in-function (and region-active function-name))
           (prompt (cond
                    (region-in-function (format "Question for the selected region in function '%s': " function-name))
                    (function-name (format "About function '%s': " function-name))
                    (region-active "Question for the selected region: ")
                    (t "Question: ")))
           (candidate-list '("What kind of question do you have about this code?"
                             "What is your suggestion on the most important thing to do"
                            "Carefully review this file and suggest improvements"
                            "What does this code do? Explain the logic of this code step by step"
                            "What are the inputs and outputs of this code?"
                            "Are there any edge cases not handled in this code?"
                            "How could this code be simplified?"
                            "What's the time/space complexity of this algorithm?"
                            "Is there a more efficient way to implement this?"
                            "What design patterns are used here?"
                            "How does this code handle errors?"))
           (raw-question (aider-read-string prompt nil candidate-list))
           (question (if function-name
                         (concat prompt raw-question)
                       raw-question))
           (region-text (and (region-active-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))))
           (question-context (if region-text
                                 (format "%s: %s" question region-text)
                               question)))
      (when (aider-current-file-command-and-switch "/ask " question-context)
        (message "Question about code sent to Aider")))))

;;;###autoload
(defun aider-general-question ()
  "Ask aider question without context."
  (interactive)
  (let* ((candidate-list '("Explain the purpose and functionality of these files"
                          "What are the key functions/methods in these files?"
                          "What is your suggestion on the most important thing to do in these files?"
                          "Identify potential bugs or issues in these files"
                          "What design patterns are used in these files?"
                          "How could we optimize the performance of this code?"
                          "Are there any security concerns in these files?"
                          "How could we make this code more maintainable?"
                          "Explain the overall architecture of this codebase"))
         (question (aider-read-string "Enter general question to ask: " nil candidate-list)))
    (let ((command (format "/ask %s" question)))
      (aider--send-command command t))))

;;;###autoload
(defun aider-copy-to-clipboard ()
  "Copy the last assistant message to the clipboard via Aider."
  (interactive)
  (aider--send-command "/copy" t)
  ;; (message "Last assistant message copied to the clipboard")
  )

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aider-debug-exception ()
  "Ask Aider to investigate an exception."
  (interactive)
  (let ((command (aider-read-string "Enter exception details (can be multiple lines): ")))
    (aider--send-command (concat "/ask Investigate the following exception, using the added files as context:\n" command) t))) ;; Add newline for clarity

;;;###autoload
(defun aider-open-history ()
  "Open the Aider history file (.aider.chat-history.md under repo git root).
If the history file does not exist, notify the user."
  (interactive)
  (let ((git-root (magit-toplevel)))
    (unless git-root
      (user-error "Not inside a git repository"))
    (let ((history-file (expand-file-name ".aider.chat.history.md" git-root)))
      (if (file-exists-p history-file)
          (find-file-other-window history-file)
        (message "History file does not exist: %s" history-file)))))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aider-help (&optional homepage)
  "Ask aider with help.
With prefix argument HOMEPAGE, open the Aider home page in a browser."
  (interactive "P")
  (if homepage
      (aider-open-aider-home)
    (let ((command (aider-read-string "Enter help question about Aider usage: ")))
      (aider-current-file-command-and-switch "/help " command))))

;;;###autoload
(defun aider-open-aider-home ()
  "Open the Aider home page in the default browser."
  (interactive)
  (browse-url "https://aider.chat"))

(provide 'aider-discussion)

;;; aider-discussion.el ends here
