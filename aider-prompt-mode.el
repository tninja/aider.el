;;; aider-prompt-mode.el --- Aider prompt mode for editing aider prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing aider prompt files.

;;; Code:

(require 'org)
(require 'aider-core)

(defvar yas-snippet-dirs)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function evil-define-key* "evil" (state map key def))

;;;###autoload
(defcustom aider-prompt-file-name ".aider.prompt.org"
  "File name that will automatically enable `aider-prompt-mode` when opened.
This is the file name without path."
  :type 'string
  :group 'aider)

;; Define the keymap for Aider Prompt Mode
;;;###autoload
(defvar aider-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'aider-send-line-or-region)
    (define-key map (kbd "C-c C-c") #'aider-send-block-or-region)
    (define-key map (kbd "C-c C-b") #'aider-send-block-by-line)
    (define-key map (kbd "C-c C-z") #'aider-switch-to-buffer)
    (define-key map (kbd "C-c C-f") #'aider-prompt-insert-file-path)
    (define-key map (kbd "C-c C-i") #'aider-core-insert-prompt)
    (define-key map (kbd "C-c C-y") #'aider-prompt-cycle-file-command)
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
      (aider--send-command (string-trim line) t))))

(defun aider--extract-filename-from-command (command-str)
  "Extract filename from COMMAND-STR if it matches an aider command pattern.
The function looks for patterns like '/command filename' and checks if the
extracted filename exists. Returns the filename if found and exists, otherwise
returns nil."
  (let ((filename nil))
    (when (string-match "^/[a-z]+ +\\([^ ]+\\)" (string-trim command-str))
      (setq filename (match-string 1 command-str))
      (if (file-exists-p filename)
          filename
        nil))))

;;;###autoload
(defun aider-send-region-by-line ()
  "Send current region to aider line by line, ignoring empty and blank lines."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))))
        (mapc (lambda (line)
                (let ((trimmed-line (string-trim line)))
                  (unless (string-empty-p trimmed-line)
                    (aider--send-command trimmed-line t))))
              (split-string region-text "\n")))
    (message "No region selected.")))

;;;###autoload
(defun aider-send-block-by-line ()
  "Send the current paragraph to aider line by line.
Uses mark-paragraph to select the current paragraph, then sends it line by line."
  (interactive)
  (save-excursion                     ; preserve cursor position
    (mark-paragraph)                  ; mark paragraph
    (aider-send-region-by-line)       ; send region line by line
    (deactivate-mark)))               ; deactivate mark after sending

;;;###autoload
(defun aider-send-block-or-region ()
  "Send the block or selected region to aider as a single prompt."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t)))
    (save-excursion                     ; preserve cursor position
      (let ((region-text
             (progn
               (mark-paragraph)         ; mark paragraph
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
            (insert "#   C-c C-i (or SPACE in evil-normal-mode): Insert prompt in mini buffer or with helm (if you use aider-helm.el)\n")
            (insert "#   C-c C-f: Insert file path under cursor\n")
            (insert "#   C-c C-y: Cycle through /add, /read-only, /drop\n")
            (insert "# Command to interact with aider session:\n")
            (insert "#   C-c C-n: Single line prompt: Send current line or selected region line by line as multiple prompts\n")
            (insert "#   C-c C-b: Send current paragraph line by line as multiple prompts\n")
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
  "Setup YASnippet directories for `aider-prompt-mode`."
  (when (featurep 'yasnippet)
    (let ((snippet-dir (expand-file-name "snippets"
                                         (file-name-directory (file-truename (locate-library "aider"))))))
      (require 'yasnippet)
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
  "Select and insert the relative file path to git repository root."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (file (read-file-name "Select file: " git-root nil t)))
    (if (and file (file-exists-p file))
        (let ((relative-path (if git-root
                                (file-relative-name file git-root)
                              file)))
          (insert relative-path))
      (message "No valid file selected."))))

;;;###autoload
(defun aider-prompt-cycle-file-command ()
  "Cycle through file commands in the current line.
If the line doesn't contain a file command, add '/add ' to the beginning.
If it already has one of '/add', '/read-only', or '/drop', cycle to the next one."
  (interactive)
  (let* ((file-commands '("/add " "/read-only " "/drop "))
         (line-begin (line-beginning-position))
         (line-end (line-end-position))
         (line-text (buffer-substring-no-properties line-begin line-end))
         (trimmed-line (string-trim line-text))
         (current-command nil)
         (command-pos nil)
         (next-command "/add "))
    ;; Check if line contains one of the file commands
    (dolist (cmd file-commands)
      (when (string-match (regexp-quote cmd) trimmed-line)
        (setq current-command cmd)
        (setq command-pos (+ line-begin (string-match (regexp-quote cmd) line-text)))))
    ;; Determine the next command in the cycle
    (when current-command
      (let ((cmd-index (cl-position current-command file-commands :test 'string=)))
        (setq next-command (nth (mod (1+ cmd-index) (length file-commands)) file-commands))))
    ;; Apply the change
    (if current-command
        ;; Replace existing command
        (progn
          (goto-char command-pos)
          (delete-char (length current-command))
          (insert next-command))
      ;; Add new command at beginning of line
      (goto-char line-begin)
      (insert next-command))))

;; Define the Aider Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode aider-prompt-mode org-mode "Aider Prompt"
  "Major mode derived from `org-mode` for editing aider prompt files.
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
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-file-path-insertion nil t)
  ;; Bind space key to aider-core-insert-prompt when evil package is available
  (add-hook 'aider-prompt-mode-hook
            (lambda ()
              (when (bound-and-true-p evil-mode)
                (evil-define-key* 'normal aider-prompt-mode-map (kbd "SPC") #'aider-core-insert-prompt)))))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "/" (regexp-quote aider-prompt-file-name) "\\'") . aider-prompt-mode))

(provide 'aider-prompt-mode)

;;; aider-prompt-mode.el ends here
