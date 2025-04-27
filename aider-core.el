;;; aider-core.el --- Core functionality for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides core functionality for the Aider package.

;;; Code:

(require 'comint)
(require 'magit)
(require 'savehist)
(require 'markdown-mode)

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-program "aider"
  "The name or path of the aider program."
  :type 'string
  :group 'aider)

(defcustom aider-args '()
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

(defcustom aider-auto-trigger-prompt nil
  "When non-nil, automatically trigger prompt insertion for commands like /ask, /code, etc."
  :type 'boolean
  :group 'aider)

(defvar aider--switch-to-buffer-other-frame nil
  "Boolean controlling Aider buffer display behavior.
When non-nil, open Aider buffer in a new frame.
When nil, use standard `display-buffer' behavior.")

(defvar aider-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-f") #'aider-prompt-insert-file-path)
    (define-key map (kbd "TAB") #'aider-core-insert-prompt)
    (define-key map (kbd "C-c C-y") #'aider-go-ahead)
    map)
  "Keymap for `aider-comint-mode'.")

(declare-function evil-define-key* "evil" (state map key def))

(defun aider--apply-markdown-highlighting ()
  "Set up markdown highlighting for aider buffer with optimized performance.
Ignore lines starting with '>' (command prompts/input)."
  ;; 1) Use `markdown-mode`'s syntax table:
  (set-syntax-table (make-syntax-table markdown-mode-syntax-table))
  ;; 2) For multiline constructs (like fenced code blocks), enable `markdown-syntax-propertize`:
  (setq-local syntax-propertize-function #'markdown-syntax-propertize)
  ;; 3) Reuse `markdown-mode`'s font-lock keywords for highlighting,
  ;;    but add a rule to prevent markdown highlighting on lines starting with '>'.
  (setq-local font-lock-defaults
              '(markdown-mode-font-lock-keywords
                nil nil nil nil
                (font-lock-multiline . t)
                (font-lock-extra-managed-props
                 . (composition display invisible))))
  ;; 4) Enable fenced code block highlighting, and disable special font processing:
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; https://github.com/tninja/aider.el/issues/113
  ;; TODO: temporary solution to disable bold, italic. need a better way than this, if we want to keep them in reply text
  ;; Note: The rule added above is a more targeted way to handle prompts than disabling these globally.
  ;; Consider if these are still needed or can be re-enabled depending on desired appearance for non-prompt text.
  (setq-local markdown-regex-bold nil)
  (setq-local markdown-regex-italic nil)
  (setq-local markdown-regex-strike-through nil)
  ;; 5) Jit-lock and other
  (setq-local font-lock-multiline t)  ;; Handle multiline constructs efficiently
  (setq-local jit-lock-contextually nil)  ;; Disable contextual analysis
  (setq-local font-lock-support-mode 'jit-lock-mode)  ;; Ensure JIT lock is used
  (setq-local jit-lock-defer-time 0)
  ;; 6) Register font-lock explicitly:
  (font-lock-mode 1)
  ;; 7) Force immediate fontification of visible area:
  (font-lock-flush)
  (font-lock-ensure)
  ;; 8) from https://github.com/MatthewZMD/aidermacs/pull/119/files
  ;; Enable markdown mode highlighting
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region nil t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  ;; a regex that will never match so we don't get the prompt interpreted as a block quote
  (setq-local markdown-regex-blockquote "^\\_>$")
  (if markdown-hide-markup
      (add-to-invisibility-spec 'markdown-markup)
    (remove-from-invisibility-spec 'markdown-markup)))

(define-derived-mode aider-comint-mode comint-mode "Aider Session"
  "Major mode for interacting with Aider.
Inherits from `comint-mode' with some Aider-specific customizations.
\\{aider-comint-mode-map}"
  ;; Set up font-lock
  ;; (setq font-lock-defaults '(nil t))
  ;; (font-lock-add-keywords nil aider-font-lock-keywords t)
  ;; Set up input sender for multi-line handling
  (setq-local comint-input-sender 'aider-input-sender)
  ;; Enable comint's built-in input highlighting
  (setq-local comint-highlight-input t)
  ;; Add command completion hooks
  (add-hook 'completion-at-point-functions #'aider-core--command-completion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-command-completion nil t)
  ;; Automatically trigger file path insertion for file-related commands
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-file-path-insertion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-insert-prompt nil t)
  ;; Bind space key to aider-core-insert-prompt when evil package is available
  (aider--apply-markdown-highlighting)
  (when (featurep 'evil)
    (evil-define-key* 'normal aider-comint-mode-map (kbd "SPC") #'aider-core-insert-prompt)))

;;;###autoload
(defun aider-plain-read-string (prompt &optional initial-input candidate-list)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
CANDIDATE-LIST provides additional completion options if provided.
This function combines candidate-list with history for better completion."
  ;; Combine candidate-list with history, removing duplicates
  (let ((completion-candidates
         (delete-dups (append candidate-list
                              (when (boundp 'aider-read-string-history)
                                aider-read-string-history)))))
    ;; Use completing-read with the combined candidates
    (completing-read prompt
                     completion-candidates
                     nil nil initial-input
                     'aider-read-string-history)))

;;;###autoload
(defalias 'aider-read-string #'aider-plain-read-string)

(eval-and-compile
  ;; Ensure the alias is always available in both compiled and interpreted modes.
  (defalias 'aider-read-string #'aider-plain-read-string))

(defun aider-buffer-name ()
  "Generate the Aider buffer name based on git repo or current buffer file path.
If not in a git repository and no buffer file exists, an error is raised."
  (let ((git-repo-path (magit-toplevel))
        (current-file (buffer-file-name)))
    (cond
     ;; Case 1: Valid git repo path (not nil and not containing "fatal")
     ((and git-repo-path
           (stringp git-repo-path)
           (not (string-match-p "fatal" git-repo-path)))
      (format "*aider:%s*" (file-truename git-repo-path)))
     ;; Case 2: Has buffer file (handles both nil and "fatal" git-repo-path cases)
     (current-file
      (format "*aider:%s*"
              (file-truename (file-name-directory current-file))))
     ;; Case 3: No git repo and no buffer file
     (t
      (error "Not in a git repository and current buffer is not associated with a file")))))

(defun aider--process-message-if-multi-line (str)
  "Entering multi-line chat messages.
https://aider.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines, wrap it in {aider\\nstr\\naider}.
Otherwise return STR unchanged."
  (if (string-match-p "\n" str)
      (format "{aider\n%s\naider}" str)
    str))

(defun aider--comint-send-string-syntax-highlight (buffer text)
  "Send TEXT to the comint BUFFER using comint's standard input mechanism.
Uses comint's built-in highlighting for input text."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Move to the end of the buffer
      (goto-char (point-max))
      ;; Insert the text - comint will handle the highlighting
      (insert text)
      ;; Use comint's standard input handling which will apply comint-highlight-input
      (comint-send-input))))

;; Shared helper function to send commands to corresponding aider buffer
(defun aider--send-command (command &optional switch-to-buffer log)
  "Send COMMAND to the corresponding aider comint buffer.
after performing necessary checks.
COMMAND should be a string representing the command to send.
Optional SWITCH-TO-BUFFER, when non-nil, switches to the aider buffer.
Optional LOG, when non-nil, logs the command to the message area."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (get-buffer (aider-buffer-name))))
      (let* ((command (replace-regexp-in-string "[\n\r]+\\'" "" command)) ;; Remove trailing newlines
             (command (aider--process-message-if-multi-line command))
             (aider-process (get-buffer-process aider-buffer)))
        ;; Check if the corresponding aider buffer has an active process
        (if (and aider-process (comint-check-proc aider-buffer))
            (progn
              ;; Send the command to the aider process
              (aider--comint-send-string-syntax-highlight aider-buffer (concat command "\n"))
              ;; Provide feedback to the user
              (when log
                (message "Sent command to aider buffer: %s" (string-trim command)))
              (when switch-to-buffer
                (aider-switch-to-buffer))
              (sleep-for 0.2))
          (message "No active process found in buffer %s." (aider-buffer-name))))
    (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))))

;;;###autoload
(defun aider-switch-to-buffer ()
  "Switch to the Aider buffer.
When `aider--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the current buffer is already the Aider buffer, do nothing."
  (interactive)
  (if (string= (buffer-name) (aider-buffer-name))
      (message "Already in Aider buffer")
    (if-let ((buffer (get-buffer (aider-buffer-name))))
        (progn
          (if aider--switch-to-buffer-other-frame
              (switch-to-buffer-other-frame buffer)
            (pop-to-buffer buffer))
          ;; Scroll to the end of the buffer after switching
          (with-current-buffer buffer
            (goto-char (point-max))))
      (message "Aider buffer '%s' does not exist." (aider-buffer-name)))))

;;;###autoload
(defun aider-run-aider (&optional edit-args)
  "Create a comint-based buffer and run \"aider\" for interactive conversation.
With the universal argument EDIT-ARGS, prompt to edit aider-args before running.
If current buffer is a dired, eshell, or shell buffer, ask if user wants to use --subtree-only mode."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
                                         (mapconcat #'identity aider-args " ")))
                         aider-args)))
    ;; Check if current buffer is in dired-mode, eshell-mode, or shell-mode and prompt for --subtree-only
    (when (and (memq major-mode '(dired-mode eshell-mode shell-mode))
               (y-or-n-p (format "Current buffer is %s. Use --subtree-only mode?"
                                 (cond ((eq major-mode 'dired-mode) "a directory")
                                       ((eq major-mode 'eshell-mode) "eshell")
                                       ((eq major-mode 'shell-mode) "shell")
                                       (t "")))))
      ;; Check if --subtree-only is already in the arguments
      (unless (member "--subtree-only" current-args)
        (setq current-args (append current-args '("--subtree-only")))))
    (unless (comint-check-proc buffer-name)
      (apply #'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
      (with-current-buffer buffer-name
        (aider-comint-mode))
      (message "%s" (if current-args
                       (format "Running aider from %s, with args: %s" default-directory (mapconcat #'identity current-args " "))
                     "Running aider with no args provided.")))
    (aider-switch-to-buffer)))

(defun aider-input-sender (proc string)
  "Handle multi-line inputs being sent to Aider.
PROC is the process to send the input to.
STRING is the input text to send.
Optional LOG, when non-nil, logs the command to the message area."
  (comint-simple-send proc (aider--process-message-if-multi-line string)))

(defun aider-core--command-completion ()
  "Provide auto completion for common commands in aider buffer.
When the current line starts with '/', this function returns a candidate list
of common commands such as \"/add\", \"/ask\", \"/drop\", etc."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line-str (buffer-substring-no-properties line-start line-end)))
      (when (string-match "^/\\(\\w*\\)" line-str)
        (let* ((beg (+ line-start (match-beginning 0)))
               (end (+ line-start (match-end 0)))
               (commands '("/add" "/architect" "/ask" "/code" "/reset" "/undo" "/lint" "/read-only"
                           "/drop" "/copy" "/copy-context" "/clear" "/commit" "/exit" "/quit"
                           "/paste" "/help" "/chat-mode" "/diff" "/editor" "/git"
                           "/load" "/ls" "/map" "/map-refresh" "/model" "/editor-model" "/weak-model" "/models"
                           "/multiline-mode" "/report" "/run" "/save" "/settings" "/test"
                           "/tokens" "/voice" "/web"))
               (prefix (match-string 0 line-str))
               (candidates (seq-filter (lambda (cmd)
                                         (string-prefix-p prefix cmd))
                                       commands)))
          (when candidates
            (list beg end candidates :exclusive 'no)))))))

(defun aider-core--auto-trigger-command-completion ()
  "Automatically trigger command completion in aider buffer.
If the last character in the current line is '/', invoke `completion-at-point`."
  (when (and (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?/))
    (completion-at-point)))

(defun aider-core--auto-trigger-file-path-insertion ()
  "Automatically trigger file path insertion in aider buffer.
If the current line matches one of the file-related commands
followed by a space, and the cursor is at the end of the line,
invoke `aider-prompt-insert-file-path`."
  (when (and (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?\s)  ; Check if last char is space
             (eolp))                 ; Check if cursor is at end of line
    (let ((line-content (buffer-substring-no-properties (line-beginning-position) (point))))
      ;; Match commands like /add, /read-only, /drop followed by a space at the end of the line
      (when (string-match-p "^[ \t]*\\(/add\\|/read-only\\|/drop\\) $" line-content)
        (aider-prompt-insert-file-path)))))

;;;###autoload
(defun aider-core-insert-prompt ()
  "Get user input via `aider-read-string` and insert it at point."
  (interactive)
  (let ((input (aider-read-string "Enter prompt: ")))
    (when input
      (insert input))))

(defun aider-core--auto-trigger-insert-prompt ()
  "Automatically trigger prompt insertion in aider buffer.
If the current line matches one of the commands (/ask, /code, /architect),
ends with exactly one space, and the cursor is at the end of the line,
invoke `aider-core-insert-prompt`."
  (when (and aider-auto-trigger-prompt
             (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?\s)  ; Check if last char is space
             (eolp))                 ; Check if cursor is at end of line
    (let ((line-content (buffer-substring-no-properties (line-beginning-position) (point))))
      (when (string-match-p "^[ \t]*\\(/ask\\|/code\\|/architect\\) $" line-content)
        (aider-core-insert-prompt)))))

(provide 'aider-core)

;;; aider-core.el ends here
