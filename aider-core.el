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

(declare-function evil-define-key* "evil" (state map key def))

;; Workaround: make markdown functions safe only in aider-comint-mode buffers
;; Addressing: 1. https://github.com/tninja/aider.el/issues/159; 2. https://github.com/MatthewZMD/aidermacs/issues/141
(defun aider--safe-maybe-funcall-regexp (origfn object &optional arg)
  "Call ORIGFN (`markdown-maybe-funcall-regexp') safely in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (cond ((functionp object)
                 (condition-case nil
                     (if arg (funcall object arg) (funcall object))
                   (error "")))  ; Return empty string if function call fails
                ((stringp object) object)
                ((null object) "")  ; Handle nil objects
                (t ""))  ; Return empty string for any other type
        (error ""))
    ;; In non-aider buffers, call original function normally
    (funcall origfn object arg)))

(defun aider--safe-get-start-fence-regexp (origfn &rest args)
  "Safely call `markdown-get-start-fence-regexp' in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (let ((result (apply origfn args)))
            (if (and result (stringp result) (not (string-empty-p result)))
                result
              "\\`never-match\\`"))  ; Return non-matching regex if result is invalid
        (error "\\`never-match\\`"))
    ;; In non-aider buffers, call original function normally
    (apply origfn args)))

(defun aider--safe-syntax-propertize-fenced-block-constructs (origfn start end)
  "Safely call `markdown-syntax-propertize-fenced-block-constructs' in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (funcall origfn start end)
        (error nil))  ; Silently ignore errors in aider buffers
    ;; In non-aider buffers, call original function normally
    (funcall origfn start end)))

;; Apply advice globally but they only activate in aider-comint-mode
(advice-add 'markdown-maybe-funcall-regexp
            :around #'aider--safe-maybe-funcall-regexp)
(advice-add 'markdown-get-start-fence-regexp
            :around #'aider--safe-get-start-fence-regexp)
(advice-add 'markdown-syntax-propertize-fenced-block-constructs
            :around #'aider--safe-syntax-propertize-fenced-block-constructs)

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
  "Auto-trigger prompt insertion for /ask, /code, etc. if non-nil."
  :type 'boolean
  :group 'aider)

(defcustom aider-use-branch-specific-buffers nil
  "When non-nil, Aider buffer names will include the current git branch.
The format will be *aider:<git-repo-path>:<branch-name>*."
  :type 'boolean
  :group 'aider)

(defvar aider--switch-to-buffer-other-frame nil
  "Boolean controlling Aider buffer display behavior.
When non-nil, open Aider buffer in a new frame.
When nil, use standard `display-buffer' behavior.")

(defvar aider-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-f") #'aider-prompt-insert-add-file-path)
    (define-key map (kbd "TAB") #'aider-core-insert-prompt)
    (define-key map (kbd "C-c C-y") #'aider-go-ahead)
    map)
  "Keymap for `aider-comint-mode'.")

(eval-after-load 'evil
  '(evil-define-key* 'normal aider-comint-mode-map (kbd "SPC") #'aider-core-insert-prompt))

;;;###autoload
(defun aider-prompt-insert-add-file-path ()
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
(defun aider-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aider comint buffer."
  (interactive)
  (aider--send-command "go ahead" t))

(defconst aider--command-list
  '("/add" "/architect" "/ask" "/code" "/reset" "/undo" "/lint" "/read-only"
    "/drop" "/copy" "/copy-context" "/clear" "/commit" "/exit" "/quit"
    "/paste" "/help" "/chat-mode" "/diff" "/editor" "/git"
    "/load" "/ls" "/map" "/map-refresh" "/think-tokens" "/tokens"
    "/model" "/editor-model" "/weak-model" "/models" "/reasoning-effort"
    "/multiline-mode" "/report" "/run" "/save" "/settings" "/test"
    "/voice" "/web")
  "A list of common Aider commands for completion.")

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
  ;; Use regex patterns that will never match to effectively disable italic/bold formatting
  (setq-local markdown-regex-italic "\\`never-match-this-pattern\\'")
  (setq-local markdown-regex-bold "\\`never-match-this-pattern\\'")
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
  ;; Load history from .aider.input.history if available
  (let ((history-file-path (aider--generate-history-file-name))) ; Bind history-file-path here
    (condition-case err ; catch any error during history loading
        (when (and history-file-path (file-readable-p history-file-path)) ; Check history-file-path too
          ;; Initialize comint ring for this buffer
          (setq comint-input-ring (make-ring comint-input-ring-size))
          (let ((parsed-history (aider--parse-aider-cli-history history-file-path)))
            (when parsed-history
              (dolist (item parsed-history)
                (when (and item (stringp item))
                  (comint-add-to-input-history item))))))
      (error (message "Error loading Aider input history from %s: %s. Continuing without loading history."
                      (or history-file-path "its determined location") ; provide file path if available
                      (error-message-string err)))))) ; display the error message

;; Apply markdown highlighting via the mode hook for robust initialization.
(add-hook 'aider-comint-mode-hook #'aider--apply-markdown-highlighting)

;; --- History Functions ---

(defun aider--get-git-repo-root ()
  "Return the top-level directory of the current git repository, or nil."
  (let ((git-root (magit-toplevel)))
    (when (and git-root (stringp git-root) (not (string-match-p "fatal" git-root)))
      (file-truename git-root))))

(defun aider--get-relevant-directory-for-history ()
  "Return the top-level directory of the current git repository.
If not in a git repo, return the directory of the current buffer's file.
Returns nil if neither can be determined."
  (or (aider--get-git-repo-root)
      (when-let ((bfn (buffer-file-name)))
        (file-name-directory (file-truename bfn)))))
(defun aider--generate-history-file-name ()
  "Generate path for .aider.input.history in git repo root or current buffer's dir."
  (when-let ((relevant-dir (aider--get-relevant-directory-for-history)))
    (expand-file-name ".aider.input.history" relevant-dir)))

(defun aider--parse-aider-cli-history (file-path)
  "Parse .aider.input.history file at FILE-PATH.
Return a list of commands, oldest to newest."
  (when (and file-path (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((history-items '())       ; Store final history items here
            (current-multi-line-command-parts nil)) ; Store parts of a {aider...aider} command
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (if (string-match "^\\+[ \t]*\\(.*\\)" line) ; Line starts with '+'
                (let ((content (match-string 1 line)))
                  (cond
                   ((string= content "{aider")
                    (setq current-multi-line-command-parts (list content)))
                   ((string= content "aider}")
                    (if current-multi-line-command-parts
                        (progn
                          (setq current-multi-line-command-parts (nconc current-multi-line-command-parts (list content)))
                          (push (string-join current-multi-line-command-parts "\n") history-items)
                          (setq current-multi-line-command-parts nil))
                      ;; else, it's a standalone "aider}" not part of a block, treat as single line
                      (push content history-items)))
                   (current-multi-line-command-parts
                    (setq current-multi-line-command-parts (nconc current-multi-line-command-parts (list content))))
                   (t                   ; Single line command
                    (push content history-items))))
              nil)) ; Ignore non-'+' lines (timestamps or other non-command lines)
          (forward-line 1))
        ;; If a multi-line block was started but not properly terminated by "aider}"
        ;; add what was collected as a single (potentially multi-line) command.
        (when current-multi-line-command-parts
          (push (string-join current-multi-line-command-parts "\n") history-items))
        (reverse history-items))))) ; Reverse to get chronological order (oldest first)

(defun aider--get-current-git-branch (repo-root-path)
  "Return current git branch name for git repository at REPO-ROOT-PATH.
Returns nil if REPO-ROOT-PATH is not a git repository or no branch
is checked out."
  ;; Ensure repo-root-path is a valid directory string before proceeding
  (when (and repo-root-path (stringp repo-root-path) (file-directory-p repo-root-path))
    (let ((default-directory repo-root-path)) ; Scope magit call to the repo root
      (magit-get-current-branch)))) ; magit-get-current-branch returns nil if no branch or not a repo

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

(defun aider--buffer-name-for-git-repo (git-repo-path-true)
  "Generate the Aider buffer name for a GIT-REPO-PATH-TRUE.
If `aider-use-branch-specific-buffers' is non-nil, includes the branch name.
Format: *aider:<git-repo-path>[:<branch-name>]*."
  (if aider-use-branch-specific-buffers
      (let ((branch-name (aider--get-current-git-branch git-repo-path-true)))
        (if (and branch-name (not (string-empty-p branch-name)))
            (format "*aider:%s:%s*" git-repo-path-true branch-name)
          ;; Fallback: branch name not found or empty
          (progn
            (message "Aider: Could not determine git branch for '%s', or branch name is empty. Using default git repo buffer name." git-repo-path-true)
            (format "*aider:%s*" git-repo-path-true))))
    ;; aider-use-branch-specific-buffers is nil
    (format "*aider:%s*" git-repo-path-true)))

(defun aider-buffer-name ()
  "Generate the Aider buffer name.
If in a git repository and `aider-use-branch-specific-buffers' is non-nil,
the buffer name will be *aider:<git-repo-path>:<branch-name>*.
Otherwise, it uses *aider:<git-repo-path>* or *aider:<current-file-directory>*."
  (let ((git-repo-path (aider--get-git-repo-root)))
    (cond
     ;; Case 1: In a Git repository
     (git-repo-path
      (aider--buffer-name-for-git-repo git-repo-path))
     ;; Case 2: Not in a Git repository, but current buffer has a file
     ((buffer-file-name)
      (format "*aider:%s*" (file-truename (file-name-directory (buffer-file-name)))))
     ;; Case 3: Not in a Git repository and no buffer file
     (t
      (error "Aider: Not in a git repository and current buffer is not associated with a file")))))

(defun aider--process-message-if-multi-line (str)
  "Entering multi-line chat messages.
https://aider.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines, wrap it in {aider\\nstr\\naider}.
Otherwise return STR unchanged."
  ;; Only wrap if contains newline and not already wrapped with \"{aider\"
  (if (and (string-match-p "\n" str)
           (not (string-match-p "{aider" str)))
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
Optional LOG, when non-nil, logs the command to the message area.
Returns t if command was sent successfully, nil otherwise."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (aider--validate-aider-buffer)))
    (let* ((command (replace-regexp-in-string "\\`[\n\r]+" "" command))   ;; Remove leading newlines
           (command (replace-regexp-in-string "[\n\r]+\\'" "" command)) ;; Remove trailing newlines
           (command (aider--process-message-if-multi-line command))
           (aider-process (get-buffer-process aider-buffer)))
      ;; Check if the corresponding aider buffer has an active process
      (if (and aider-process (comint-check-proc aider-buffer))
          (progn
            ;; Send the command to the aider process
            (aider--comint-send-string-syntax-highlight aider-buffer command)
            ;; Provide feedback to the user
            (when log
              (message "Sent command to aider buffer: %s" (string-trim command)))
            (when switch-to-buffer
              (aider-switch-to-buffer))
            (sleep-for 0.2)
            t) ; Return t for success
        (progn
          (message "No active process found in buffer %s." (aider-buffer-name))
          nil))) ; Return nil for failure
    (progn
      (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))
      nil))) ; Return nil for failure

;;;###autoload
(defun aider-switch-to-buffer ()
  "Switch to the Aider buffer.
When `aider--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the current buffer is already the Aider buffer, do nothing."
  (interactive)
  (if (string= (buffer-name) (aider-buffer-name))
      (message "Already in Aider buffer")
    (when-let ((buffer (aider--validate-aider-buffer)))
      (if aider--switch-to-buffer-other-frame
          (switch-to-buffer-other-frame buffer)
        (pop-to-buffer buffer))
      ;; Scroll to the end of the buffer after switching
      (with-current-buffer buffer
        (goto-char (point-max))))))

(defun aider--is-default-directory-git-root ()
  "Return t if `default-directory' is the root of a Git repository, nil otherwise."
  (let ((git-root-path (magit-toplevel)))
    (and git-root-path
         (stringp git-root-path)
         (not (string-match-p "fatal" git-root-path))
         ;; Compare canonical paths of default-directory and git-root-path
         (string= (file-truename (expand-file-name default-directory))
                  (file-truename git-root-path)))))

(defun aider--maybe-prompt-subtree-only-for-special-modes (current-args)
  "Prompt for --subtree-only in dired/eshell/shell if not in git root.
Return potentially modified CURRENT-ARGS."
  (if (and (memq major-mode '(dired-mode eshell-mode shell-mode))
           (not (aider--is-default-directory-git-root))
           (y-or-n-p (format "Current buffer is %s and current directory (%s) is not git root. Use --subtree-only mode?"
                             (cond ((eq major-mode 'dired-mode) "a directory")
                                   ((eq major-mode 'eshell-mode) "eshell")
                                   ((eq major-mode 'shell-mode) "shell")
                                   (t ""))
                             (abbreviate-file-name default-directory))))
      (if (member "--subtree-only" current-args)
          current-args
        (append current-args '("--subtree-only")))
    current-args))

;;;###autoload
(defun aider-run-aider (&optional edit-args subtree-only)
  "Run \"aider\" in a comint buffer for interactive conversation.
With prefix argument (e.g., \\[universal-argument]), prompt to edit `aider-args` (EDIT-ARGS).
If SUBTREE-ONLY is non-nil, add '--subtree-only'.
Prompts for --subtree-only in dired/eshell/shell if needed."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
                                         (mapconcat #'identity aider-args " ")))
                         aider-args)))
    (if (comint-check-proc buffer-name)
        (message "Aider session already running in buffer: %s" buffer-name)
      (progn
        ;; Handle --subtree-only prompting for special modes
        (setq current-args (aider--maybe-prompt-subtree-only-for-special-modes current-args))
        ;; Add --subtree-only if the parameter is set and it's not already present
        (when (and subtree-only (not (member "--subtree-only" current-args)))
          (setq current-args (append current-args '("--subtree-only")))
          (message "Adding --subtree-only argument as requested."))
        (apply #'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
        (with-current-buffer buffer-name
          (aider-comint-mode))
        (message "%s" (if current-args
                          (format "Running aider from %s, with args: %s.\nMay the AI force be with you!" default-directory (mapconcat #'identity current-args " "))
                        (format "Running aider from %s with no args provided.\nMay the AI force be with you!" default-directory)))))
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
of common commands."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line-str (buffer-substring-no-properties line-start line-end)))
      (when (string-match "^/\\(\\w*\\)" line-str)
        ;; Define the list of commands as a constant
        (let* ((beg (+ line-start (match-beginning 0)))
               (end (+ line-start (match-end 0)))
               (prefix (match-string 0 line-str))
               (candidates (seq-filter (lambda (cmd)
                                         (string-prefix-p prefix cmd))
                                       aider--command-list)))
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
invoke the appropriate file path insertion function."
  (when (and (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?\s)  ; Check if last char is space
             (eolp))                 ; Check if cursor is at end of line
    (let ((line-content (buffer-substring-no-properties (line-beginning-position) (point))))
      (cond
       ;; Match commands like /add, /read-only followed by a space at the end of the line
       ((string-match-p "^[ \t]*\\(/add\\|/read-only\\) $" line-content)
        (aider-prompt-insert-add-file-path))
       ;; Match /drop command followed by a space at the end of the line
       ((string-match-p "^[ \t]*/drop $" line-content)
        (aider-prompt-insert-drop-file-path))))))


(defun aider-prompt-insert-drop-file-path ()
  "Prompt for a file to drop from the list of added files and insert its path."
  (interactive)
  (let ((candidate-files (aider-core--parse-added-file-list)))
    (if candidate-files
        (let ((file-to-drop (completing-read "Drop file: " candidate-files nil t nil)))
          ;; Check if a file was actually selected and it's not an empty string
          (when (and file-to-drop (not (string-empty-p file-to-drop)))
            (insert file-to-drop)))
      (message "No files currently added to Aider to drop."))))

(defun aider-core--parse-added-file-list ()
  "Parse the Aider comint buffer to find the list of currently added files.
Searches upwards from the last Aider prompt (e.g., '>') until a blank line.
Removes trailing \" (read only)\" from file names."
  (interactive)
  (let ((aider-buf (get-buffer (aider-buffer-name)))
        (file-list '()))    ; Initialize file-list to nil (empty list)
    (when aider-buf
      (with-current-buffer aider-buf
        (save-excursion
          (goto-char (point-max))
          ;; Search backward for the last line starting with ">"
          (if (re-search-backward "^>" nil t)
              (progn
                ;; Move to the line above the prompt line
                (forward-line -1)
                ;; Loop upwards as long as not at buffer beginning and line is not blank
                (while (and (not (bobp))
                            (let ((current-line-text (buffer-substring-no-properties
                                                      (line-beginning-position)
                                                      (line-end-position))))
                              ;; Check if the line contains any non-whitespace characters
                              (string-match-p "\\S-" current-line-text)))
                  (let* ((line-text (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                         ;; Remove " (read only)" suffix from the line
                         (processed-line (replace-regexp-in-string " (read only)$" "" line-text)))
                    (push processed-line file-list))
                  (forward-line -1))))
          ;; If prompt not found, file-list remains empty
          )))
    ;; The list is built by pushing items, so it's naturally in top-to-bottom order
    ;; as read from bottom-up. No need to reverse.
    file-list))

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

;; validators

(defun aider--validate-buffer-file ()
  "Validate that current buffer is associated with a file.
Returns `buffer-file-name` if valid, nil otherwise with message."
  (if buffer-file-name
      buffer-file-name
    (message "Current buffer is not associated with a file")
    nil))

(defun aider--validate-git-repository ()
  "Validate that we're in a git repository and return git root.
Returns git root if valid, nil otherwise with message."
  (let ((git-root (magit-toplevel)))
    (if git-root
        git-root
      (message "Not in a git repository")
      nil)))

(defun aider--validate-aider-buffer ()
  "Validate that aider buffer exists and has an active process.
Returns the aider buffer if valid, otherwise returns nil with message."
  (let ((buffer-name (aider-buffer-name)))
    (cond
     ((not (get-buffer buffer-name))
      (message "Aider buffer does not exist. Please start 'aider' first")
      nil)
     (t
      (let* ((aider-buffer (get-buffer buffer-name))
             (aider-process (get-buffer-process aider-buffer)))
        (if (and aider-process (comint-check-proc aider-buffer))
            aider-buffer
          (message "No active process found in aider buffer: %s" buffer-name)
          nil))))))

(provide 'aider-core)

;;; aider-core.el ends here
