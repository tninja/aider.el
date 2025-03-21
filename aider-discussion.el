;;; aider-discussion.el --- Discussion operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides discussion functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question ()
  "Ask aider question about specific code.
Focuses on understanding, analyzing, and improving the selected code or function."
  (interactive)
  ;; Dispatch to general question if in aider buffer
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-in-function (and region-active function-name))
         (prompt (cond
                  (region-in-function (format "Question for the selected region in function '%s': " function-name))
                  (function-name (format "About function '%s': " function-name))
                  (region-active "Question for the selected region: ")
                  (t "Question: ")))
         ;; 重新设计的候选提示，更专注于代码理解和分析
         (candidate-list '("What does this code do?"
                          "Explain the logic of this code step by step"
                          "What are the inputs and outputs of this code?"
                          "Are there any edge cases not handled in this code?"
                          "How could this code be simplified?"
                          "What's the time/space complexity of this algorithm?"
                          "Is there a more efficient way to implement this?"
                          "What design patterns are used here?"
                          "How does this code handle errors?"
                          "What assumptions does this code make?"))
         (raw-question (aider-read-string prompt nil candidate-list))
         (question (if function-name
                       (concat prompt raw-question)
                     raw-question))
         (region-text (and (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         (question-context (if region-text
                               (format "%s: %s" question region-text)
                             question)))
    (aider-current-file-command-and-switch "/ask " question-context)
    (message "Question about code sent to Aider")))

;;;###autoload
(defun aider-general-question ()
  "Ask aider question without context."
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
  "Ask Aider to explain the selected region of code."
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
             (user-question (aider-read-string prompt)))
        (aider-current-file-command-and-switch "/ask " (concat prefix user-question)))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-function-or-region-explain ()
  "Call `aider-function-explain` when no region is selected.
otherwise call `aider-region-explain`."
  (interactive)
  (if (region-active-p)
      (aider-region-explain)
    (aider-function-explain)))

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aider-debug-exception ()
  "Ask Aider to investigate an exception."
  (interactive)
  (let ((command (aider-read-string "Enter exception, can be multiple lines: ")))
    (aider--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aider-help (&optional homepage)
  "Ask aider with help.
With prefix argument HOMEPAGE, open the Aider home page in a browser."
  (interactive "P")
  (if homepage
      (aider-open-aider-home)
    (let ((command (aider-read-string "Enter help question: ")))
      (aider-current-file-command-and-switch "/help " command))))

;;;###autoload
(defun aider-open-aider-home ()
  "Open the Aider home page in the default browser."
  (interactive)
  (browse-url "https://aider.chat"))

(provide 'aider-discussion)

;;; aider-discussion.el ends here
