;;; aider-discussion.el --- Discussion operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides discussion functionality for the Aider package.

;;; Code:

(require 'which-func)

(require 'aider-core)
(require 'aider-file)

(defun aider--get-question-candidates ()
  "Return standard list of question candidates."
  '("What kind of question do you have about this code?"
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

(defun aider--build-region-question-context (prompt)
  "Build question context for selected region with PROMPT."
  (let* ((raw-question (aider-read-string prompt nil (aider--get-question-candidates)))
         (question (concat prompt raw-question))
         (region-text (buffer-substring-no-properties (region-beginning) (region-end))))
    (format "%s: %s" question region-text)))

(defun aider--ask-about-region (function-name)
  "Ask question about selected region, optionally in FUNCTION-NAME."
  (let* ((prompt (if function-name 
                     (format "Question for the selected region in function '%s': " function-name)
                   "Question for the selected region: "))
         (question-context (aider--build-region-question-context prompt)))
    (when (aider-current-file-command-and-switch "/ask " question-context)
      (message "Question about code sent to Aider"))))

(defun aider--ask-about-function (function-name)
  "Ask question about specific FUNCTION-NAME."
  (let* ((prompt (format "About function '%s': " function-name))
         (raw-question (aider-read-string prompt nil (aider--get-question-candidates)))
         (question (concat prompt raw-question)))
    (when (aider-current-file-command-and-switch "/ask " question)
      (message "Question about code sent to Aider"))))

(defun aider--ask-with-function-choice (function-name)
  "Let user choose between function-specific or general question for FUNCTION-NAME."
  (let ((choice (completing-read 
                 "Choose question type: "
                 `(,(format "Question about function '%s'" function-name)
                   "General question")
                 nil t)))
    (if (string-prefix-p "Question about function" choice)
        (aider--ask-about-function function-name)
      (aider-general-question))))

;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question ()
  "Ask aider question about specific code.
Focuses on understanding, analyzing, improving the selected code or function.
If there is selected region, ask question about the region.
If cursor is not in a function, ask general question.
If cursor is in a function, let user choose between function-specific or general question."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p)))
    (cond
     (region-active (aider--ask-about-region function-name))
     ((not function-name) (aider-general-question))
     (t (aider--ask-with-function-choice function-name)))))

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

(defvar aider-run-file-history nil
  "History list for aider-run-current-file commands.")

;;;###autoload
(defun aider-run-current-file ()
  "Generate command to run current script file (.py or .sh).
Let user modify the command before running it in a compile buffer.
Maintains a dedicated history list for this command."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (file-ext (when current-file (file-name-extension current-file)))
         (file-name (when current-file (file-name-nondirectory current-file)))
         (last-command (when aider-run-file-history (car aider-run-file-history)))
         (default-command (cond
                          ;; Check if current file is in the last run command
                          ((and last-command file-name 
                                (string-match-p (regexp-quote file-name) last-command))
                           last-command)
                          ;; Generate default command based on file extension
                          ((string= file-ext "py")
                           (format "python %s" file-name))
                          ((string= file-ext "sh")
                           (format "bash %s" file-name))
                          (t nil))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (unless default-command
      (user-error "Current file is not a .py or .sh file"))
    (let ((command (read-string 
                   (format "Run command for %s: " file-name)
                   default-command
                   'aider-run-file-history)))
      (let ((default-directory (file-name-directory current-file)))
        (compile command)))))

(provide 'aider-discussion)

;;; aider-discussion.el ends here
