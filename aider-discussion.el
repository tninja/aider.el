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
(defun aider-ask-question (&optional no-context)
  "Ask aider question.
If NO-CONTEXT is non-nil, send the question to the general aider comint buffer.
Otherwise, send the question to the corresponding aider comint buffer."
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
      (aider-current-file-command-and-switch "/ask " question-context))))

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
