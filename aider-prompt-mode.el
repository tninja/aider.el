;;; aider-prompt-mode.el --- Aider prompt mode for editing aider prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing aider prompt files.

;;; Code:

(require 'org)
(require 'aider-core)

;;;###autoload
(defcustom aider-prompt-file-name ".aider.prompt.org"
  "File name that will automatically enable aider-prompt-mode when opened.
This is the file name without path."
  :type 'string
  :group 'aider)

;; Define the keymap for Aider Prompt Mode
;;;###autoload
(defvar aider-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'aider-send-line-or-region)
    (define-key map (kbd "C-c C-c") 'aider-send-block-or-region)
    (define-key map (kbd "C-c C-z") 'aider-switch-to-buffer)
    (define-key map (kbd "C-c C-i") 'aider-prompt-insert-file-path)
    map)
  "Keymap for Aider Prompt Mode.")

;;;###autoload
(defun aider-send-line-or-region ()
  "Send text to the Aider buffer.
If region is active, send the selected region line by line.
Otherwise, send the line under cursor."
  (interactive)
  (if (region-active-p)
      (aider-send-region-by-line)
    (let ((line (thing-at-point 'line t)))
      (aider--send-line-with-code-syntax line))))

(defun aider--send-line-with-code-syntax (line)
  "Trim LINE and send it to the Aider buffer.
If command contains a filename, open that file in a temp buffer,
inherit syntax highlighting, then close the temporary buffer."
  (let ((trimmed-line (string-trim line))
        (filename-buffer nil))
    (unwind-protect
        (progn
          ;; Check if the command contains a filename
          (let ((filename (aider--extract-filename-from-command trimmed-line)))
            (when filename
              ;; If filename found, open it in a temporary buffer
              (setq filename-buffer (find-file-noselect filename))))
          ;; Send the command
          (aider--send-command trimmed-line nil)
          ;; Switch to aider buffer with syntax highlighting from filename-buffer
          (aider-switch-to-buffer filename-buffer))
      ;; Clean up: kill the temporary buffer if it exists
      (when (and filename-buffer (buffer-live-p filename-buffer))
        (kill-buffer filename-buffer)))))

(defun aider--extract-filename-from-command (command-str)
  (let ((filename nil))
    (when (string-match "^/[a-z]+ +\\([^ ]+\\)" (string-trim command-str))
      (setq filename (match-string 1 command-str))
      (if (file-exists-p filename)
          filename
        nil))))

;;;###autoload
(defun aider-send-region-by-line ()
  "Get the text of the current selected region, split them into lines,
strip the newline character from each line,
for each non-empty line, send it to aider session.
If no region is selected, show a message."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties 
                          (region-beginning) 
                          (region-end))))
        (mapc (lambda (line)
                (unless (string-empty-p line)
                  (aider--send-line-with-code-syntax line)))
              (split-string region-text "\n" t)))
    (message "No region selected.")))

;;;###autoload
(defun aider-send-block-or-region ()
  "Send the current active region text or, if no region is active, send the current paragraph content to the aider session.
When sending paragraph content, preserve cursor position and deactivate mark afterwards."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t)))
    (save-excursion  ; preserve cursor position
      (let ((region-text
             (progn
               (mark-paragraph)  ; mark paragraph
               (buffer-substring-no-properties (region-beginning) (region-end)))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t))
        (deactivate-mark)))))  ; deactivate mark after sending

;;;###autoload
(defun aider-open-prompt-file ()
  "Open aider prompt file under git repo root.
If file doesn't exist, create it with command binding help and sample prompt."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (prompt-file (when git-root
                       (expand-file-name aider-prompt-file-name git-root))))
    (if prompt-file
        (progn
          (find-file-other-window prompt-file)
          (unless (file-exists-p prompt-file)
            ;; Insert initial content for new file
            (insert "# Aider Prompt File - Command Reference:\n")
            (insert "# Edit command:\n")
            (insert "#   C-c C-i: Insert file path under cursor\n")
            (insert "# Command to interact with aider session:\n")
            (insert "#   C-c C-n: Single line prompt: Send current line or selected region line by line as multiple prompts\n")
            (insert "#   C-c C-c: Multi-line prompt: Send current block or selected region as a single prompt\n")
            (insert "#   C-c C-z: Switch to aider buffer\n")
            (insert "# If you have yasnippet installed, use these keybindings to access snippet functions in aider-prompt-mode:\n")
            (insert "#   yas-describe-tables: Show available snippets\n")
            (insert "#   yas-insert-snippet: Insert a snippet\n")
            (insert "#   yas-expand: Expand snippet at cursor\n")
            (insert "# Aider command reference: https://aider.chat/docs/usage/commands.html\n\n")
            (insert "* Sample task:\n\n")
            (insert "/ask explain to me what this repo is about?\n")
            (save-buffer)))
      (message "Not in a git repository"))))

(defun aider--setup-snippets ()
  "Setup YASnippet directories for aider-prompt-mode."
  (when (featurep 'yasnippet)
    (let ((snippet-dir (expand-file-name "snippets"
                                       (file-name-directory (file-truename (locate-library "aider"))))))
      (add-to-list 'yas-snippet-dirs snippet-dir t)
      (yas-load-directory snippet-dir))))

(defun aider-prompt-mode-setup-font-lock ()
  "Setup custom font lock for aider commands."
  (let ((green-commands '("/add" "/read-only" "/architect" "/ask" "/copy" "/copy-context"
                           "/drop" "/paste" "/help" "/chat-mode" "/diff" "/editor" "/git"
                           "/load" "/ls" "/map" "/map-refresh" "/model" "/models"
                           "/multiline-mode" "/report" "/run" "/save" "/settings" "/test"
                           "/tokens" "/voice" "/web" "go ahead"))
        (red-commands '("/clear" "/code" "/commit" "/exit" "/quit" "/reset" "/undo" "/lint")))
    ;; Append custom font lock keywords to org-mode's defaults
    (font-lock-add-keywords nil
     `((,(regexp-opt green-commands) . font-lock-type-face)
       (,(regexp-opt red-commands) . font-lock-warning-face)))
    ;; Force font lock refresh
    (when (fboundp 'font-lock-flush)
      (font-lock-flush))
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure))))

;;;###autoload
(defun aider-prompt-insert-file-path ()
  "Prompt for a file path with completion and insert the selected file name at point.
The user is presented with a find-file–like interface. Only existing files can be selected.
The path inserted will be relative to the git repository root."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (file (read-file-name "Select file: " git-root nil t)))
    (if (and file (file-exists-p file))
        (let ((relative-path (if git-root
                                (file-relative-name file git-root)
                              file)))
          (insert relative-path))
      (message "No valid file selected."))))

;; Insert command completion functions for aider-prompt-mode

;; Define the Aider Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode aider-prompt-mode org-mode "Aider Prompt"
  "Major mode derived from org-mode for editing aider prompt files.
Special commands:
\\{aider-prompt-mode-map}"
  ;; Basic setup
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  ;; Setup font lock
  (aider-prompt-mode-setup-font-lock)
  (setq-local truncate-lines nil)  ; Disable line truncation, allowing lines to wrap
  ;; YASnippet support
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (aider--setup-snippets))
  ;; Automatically add command completion for common commands.
  (add-hook 'completion-at-point-functions #'aider-core--command-completion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-command-completion nil t)
  ;; Automatically trigger file path insertion for file-related commands
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-file-path-insertion nil t))

;;;###autoload
(add-to-list 'auto-mode-alist 
             `(,(concat "/" (regexp-quote aider-prompt-file-name) "\\'") . aider-prompt-mode))

(provide 'aider-prompt-mode)

;;; aider-prompt-mode.el ends here
