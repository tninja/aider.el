;;; aider-discussion.el --- Discussion operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;;; Commentary:
;; This file provides discussion functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question (&optional no-context)
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/ask \".
If a region is active, append the region text to the question.
If cursor is inside a function, include the function name as context."
  (interactive "P")
  ;; Dispatch to general question if in aider buffer
  (if (or no-context
       (string= (buffer-name) (aider-buffer-name)))
    (aider-general-question)
    (let* ((function-name (which-function))
           (region-active (region-active-p))
           (prompt (cond
                    (function-name (format "About function '%s': " function-name))
                    (region-active "Question for the selected region: ")
                    (t "Question: ")))
           (raw-question (aider-read-string prompt))
           (question (if function-name
                         (concat prompt raw-question)
                       raw-question))
           (region-text (and (region-active-p) 
                             (buffer-substring-no-properties (region-beginning) (region-end))))
           (question-context (if region-text
                                 (format "%s: %s" question region-text)
                               question)))
      (aider-current-file-command-and-switch "/ask " question-context)
      )))

;;;###autoload
(defun aider-general-question ()
  "Prompt the user for a general question and send it to the corresponding aider comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((question (aider-read-string "Enter general question to ask: ")))
    (let ((command (format "/ask %s" question)))
      (aider--send-command command t))))

;;;###autoload
(defun aider-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aider comint buffer."
  (interactive)
  (aider--send-command "go ahead" t))

;; New function to explain the code in the selected region
;;;###autoload
(defun aider-region-explain ()
  "Get a command from the user and send it to the corresponding aider comint buffer based on the selected region.
The command will be formatted as \"/ask \" followed by the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (processed-region-text region-text)
             (command (if function-name
                          (format "/ask in function %s, explain the following code block: %s"
                                  function-name
                                  processed-region-text)
                        (format "/ask explain the following code block: %s"
                                processed-region-text))))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No region selected.")))

;; New function to ask Aider to explain the function under the cursor
;;;###autoload
(defun aider-function-explain ()
  "Ask Aider to explain the function under the cursor.
Prompts user for specific questions about the function."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((prefix (format "explain %s: " function-name))
             (prompt (format "Enter your question to %s" prefix))
             (user-question (aider-read-string prompt))
             (command (format "/ask %s%s" prefix user-question)))
        (aider-current-file-command-and-switch "/ask " (concat prefix user-question)))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-function-or-region-explain ()
  "Call aider-function-explain when no region is selected, otherwise call aider-region-explain."
  (interactive)
  (if (region-active-p)
      (aider-region-explain)
    (aider-function-explain)))

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aider-debug-exception ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/debug \",
replacing all newline characters except for the one at the end."
  (interactive)
  (let ((command (aider-read-string "Enter exception, can be multiple lines: ")))
    (aider--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aider-help (&optional homepage)
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/help \"."
  (interactive "P")
  (if homepage
      (aider-open-aider-home) 
    (let ((command (aider-read-string "Enter help question: ")))
      (aider-current-file-command-and-switch "/help " command))
      ))

;;;###autoload
(defun aider-open-aider-home ()
  "Open the Aider home page in the default browser."
  (interactive)
  (browse-url "https://aider.chat"))

(provide 'aider-discussion)

;;; aider-discussion.el ends here
