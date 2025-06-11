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
    (define-key map (kbd "C-c C-b") #'aider-send-block-by-line) ;; considering retire this since C-u C-c C-n will do same thing
    (define-key map (kbd "C-c C-z") #'aider-switch-to-buffer)
    (define-key map (kbd "C-c C-f") #'aider-prompt-insert-add-file-path)
    (define-key map (kbd "C-c C-i") #'aider-core-insert-prompt)
    (define-key map (kbd "C-c C-y") #'aider-prompt-cycle-file-command)
    map)
  "Keymap for Aider Prompt Mode.")

(defun aider--handle-subtree-command (subdir-rel-path)
  "Switch to or start an Aider session for SUBDIR-REL-PATH relative to git root."
  (let ((git-root (magit-toplevel)))
    (if git-root
        (let* ((subdir-full-path (expand-file-name subdir-rel-path git-root))
               (expected-aider-buffer-name
                (let ((default-directory subdir-full-path)) ; Temporarily set default-directory
                  (aider-buffer-name)))
               (existing-buffer (get-buffer expected-aider-buffer-name)))
          (if existing-buffer
              (progn
                (message "Aider session already available. Switching to it.")
                (pop-to-buffer existing-buffer))
            (if (file-directory-p subdir-full-path)
                (progn
                  (message "Starting Aider in subtree: %s" subdir-rel-path)
                  (let ((default-directory subdir-full-path))
                    ;; Pass t for the subtree-only argument
                    (aider-run-aider nil t))) ; Pass nil for edit-args, t for subtree-only
              (message "Error: Subdirectory '%s' not found or not a directory relative to git root '%s'." subdir-rel-path git-root))))
      (message "Error: Not in a git repository. Cannot use subtree-only."))))

(defun aider--send-trimmed-line (line)
  "Trim LINE and send it as a command to Aider if not empty.
Handles special /subtree-only command."
  (let ((trimmed-line (string-trim line)))
    (if (string-match "^subtree-only +\\(.*\\)" trimmed-line)
        ;; Handle subtree-only command by calling the new function
        (aider--handle-subtree-command (match-string 1 trimmed-line))
      ;; Original behavior for other commands
      (unless (string-empty-p trimmed-line)
        (aider--send-command trimmed-line t)))))

;;;###autoload
(defun aider-send-line-or-region (&optional arg)
  "Send text to the Aider buffer.
With a prefix argument ARG (e.g., invoked with C-u), send the current paragraph
line by line.
If region is active, send the selected region line by line.
Otherwise, send the line under cursor.
After sending, return cursor to the original buffer."
  (interactive "P")
  (let ((orig-buffer (current-buffer)))  ; Store the original buffer
    (cond
     ;; If universal argument is provided, send paragraph by line
     (arg
      (aider-send-block-by-line))
     ;; If region is active, send region by line
     ((region-active-p)
      (aider-send-region-by-line))
     ;; Otherwise, send current line
     (t
      (aider--send-trimmed-line (thing-at-point 'line t))))
    ;; Return to the original buffer
    (when (buffer-live-p orig-buffer)
      (pop-to-buffer orig-buffer))))

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
        (mapc #'aider--send-trimmed-line (split-string region-text "\n")))
    (message "No region selected.")))

;;;###autoload
(defun aider-send-block-by-line ()
  "Send the current paragraph to aider line by line.
Uses `mark-paragraph` to select the current paragraph, then sends it by line."
  (interactive)
  (save-excursion                     ; preserve cursor position
    (mark-paragraph)                  ; mark paragraph
    (aider-send-region-by-line)       ; send region line by line
    (deactivate-mark)))               ; deactivate mark after sending

;;;###autoload
(defun aider-send-block-or-region ()
  "Send the block or selected region to aider as a single prompt.
After sending, return cursor to the original buffer."
  (interactive)
  (let ((orig-buffer (current-buffer)))  ; Store the original buffer
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
          (deactivate-mark))))  ; deactivate mark after sending
    ;; Return to the original buffer
    (when (buffer-live-p orig-buffer)
      (pop-to-buffer orig-buffer))))

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
            (insert "#   C-c C-n: Single line prompt: Send current line or selected region line by line as multiple prompts. If C-u pressed, send current paragraph line by line\n")
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
  (condition-case nil
      (when (require 'yasnippet nil t)
        (let ((snippet-dir (expand-file-name "snippets"
                                           (file-name-directory (file-truename (locate-library "aider"))))))
          (when (file-directory-p snippet-dir)
            (unless (boundp 'yas-snippet-dirs)
              (setq yas-snippet-dirs nil))
            (add-to-list 'yas-snippet-dirs snippet-dir t)
            (ignore-errors (yas-load-directory snippet-dir)))))
    (error nil))) ;; Suppress all errors

(defun aider-prompt-mode-setup-font-lock ()
  "Setup custom font lock for aider commands."
  (let* ((red-commands '("/clear" "/code" "/commit" "/exit" "/quit" "/reset" "/undo" "/lint"))
         ;; Derive green commands by removing red commands from the main list
         (green-commands (seq-filter (lambda (cmd)
                                       (not (member cmd red-commands)))
                                     aider--command-list))
         ;; Add "go ahead" and "subtree-only" separately
         (green-commands (append green-commands '("go ahead" "subtree-only"))))
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
(defun aider-prompt-cycle-file-command ()
  "Cycle through file commands in the current line.
If the line doesn't contain a file command, add '/add ' to the beginning.
If it already has one of '/add', '/read-only', or '/drop', cycle to the next.
If it has '/ask', toggle to '/architect', and vice versa."
  (interactive)
  (let* ((file-commands '("/add " "/read-only " "/drop "))
         (special-commands '("/ask " "/architect "))
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
    ;; Check if line contains one of the special commands
    (unless current-command
      (dolist (cmd special-commands)
        (when (string-match (regexp-quote cmd) trimmed-line)
          (setq current-command cmd)
          (setq command-pos (+ line-begin (string-match (regexp-quote cmd) line-text))))))
    ;; Determine the next command in the cycle
    (cond
     ;; For /ask and /architect, toggle between them
     ((string= current-command "/ask ")
      (setq next-command "/architect "))
     ((string= current-command "/architect ")
      (setq next-command "/ask "))
     ;; For file commands, cycle through the list
     (current-command
      (let ((cmd-index (cl-position current-command file-commands :test 'string=)))
        (when cmd-index
          (setq next-command (nth (mod (1+ cmd-index) (length file-commands)) file-commands)))))
     ;; Default case - no command found
     (t (setq next-command "/add ")))
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
