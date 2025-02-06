;;; aider.el --- Aider package for interactive conversation with aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (magit "2.1.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el

;;; Commentary:
;; This package provides an interactive interface to communicate with https://github.com/paul-gauthier/aider.

;;; Code:

(require 'comint)
(require 'dired)
(require 'transient)
(require 'magit)
(require 'which-func)
(require 'ansi-color)

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-program "aider"
  "The name or path of the aider program."
  :type 'string
  :group 'aider)

(defcustom aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022")
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

(defcustom aider--switch-to-buffer-other-frame nil
  "When non-nil, open Aider buffer in a new frame using `switch-to-buffer-other-frame'.
When nil, use standard `display-buffer' behavior."
  :type 'boolean
  :group 'aider)

(defcustom aider-popular-models '("gemini/gemini-exp-1206"  ;; free
                                  "anthropic/claude-3-5-sonnet-20241022"  ;; really good in practical
                                  "r1"  ;; performance match o1, price << claude sonnet. weakness: small context
                                  "deepseek/deepseek-chat"  ;; chatgpt-4o level performance, price is 1/100. weakness: small context
                                  "gpt-4o-mini"
                                  )
  "List of available AI models for selection.
Each model should be in the format expected by the aider command line interface.
Also based on aider LLM benchmark: https://aider.chat/docs/leaderboards/"
  :type '(repeat string)
  :group 'aider)

(defcustom aider-language-name-map '(("elisp" . "emacs-lisp")
                                     ("bash" . "sh")
                                     ("objective-c" . "objc")
                                     ("objectivec" . "objc")
                                     ("cpp" . "c++"))
  "Map external language names to Emacs names."
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
  :group 'aider)

(defface aider-command-separator
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for command separator in aider."
  :group 'aider)

(defface aider-command-text
  '((t :inherit bold))
  "Face for commands sent to aider buffer."
  :group 'aider)

(defface aider-search-replace-block
  '((t :inherit 'diff-refine-added :bold t))
  "Face for search/replace block content."
  :group 'aider)

(defvar aider-font-lock-keywords
  '(("^\x2500+\n?" 0 '(face aider-command-separator) t)
    ("^\x2500+" 0 '(face nil display (space :width 2)))
    ("^\\([0-9]+\\). " 0 font-lock-constant-face)
    ("^>>>>>>> REPLACE" 0 'aider-search-replace-block t)
    ("^<<<<<<< SEARCH" 0 'aider-search-replace-block t)
    ("^\\(```\\)\\([^[:space:]]*\\)" (1 'shadow t) (2 font-lock-builtin-face t))
    ("^=======$" 0 'aider-search-replace-block t))
  "Font lock keywords for aider buffer.")

;;;###autoload
(defun aider-plain-read-string (prompt &optional initial-input)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
This function can be customized or redefined by the user."
  (read-string prompt initial-input))

(defalias 'aider-read-string 'aider-plain-read-string)

(defvar aider--add-file-read-only nil
  "Set model parameters from `aider-menu' buffer-locally.
Affects the system message too.")

(defun aider--get-add-command-prefix ()
  "Return the appropriate command prefix based on aider--add-file-read-only."
  (if aider--add-file-read-only "/read-only" "/add"))

(defclass aider--add-file-type (transient-lisp-variable)
  ((variable :initform 'aider--add-file-read-only)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Class for toggling aider--add-file-read-only.")

(defclass aider--switch-to-buffer-type (transient-lisp-variable)
  ((variable :initform 'aider--switch-to-buffer-other-frame)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Class for toggling aider--switch-to-buffer-other-frame.")

(transient-define-infix aider--infix-add-file-read-only ()
  "Toggle aider--add-file-read-only between nil and t."
  :class 'aider--add-file-type
  :key "@"
  :description "Read-only mode"
  :reader (lambda (_prompt _initial-input _history)
           (not aider--add-file-read-only)))

(transient-define-infix aider--infix-switch-to-buffer-other-frame ()
  "Toggle aider--switch-to-buffer-other-frame between nil and t."
  :class 'aider--switch-to-buffer-type
  :key "^"
  :description "Open in new frame"
  :reader (lambda (_prompt _initial-input _history)
           (not aider--switch-to-buffer-other-frame)))

;; Transient menu for Aider commands
;; The instruction in the autoload comment is needed, see
;; https://github.com/magit/transient/issues/280.
;;;###autoload (autoload 'aider-transient-menu "aider" "Transient menu for Aider commands." t)
(transient-define-prefix aider-transient-menu ()
  "Transient menu for Aider commands."
  ["Aider: AI Pair Programming"
   ["Aider Process"
    (aider--infix-switch-to-buffer-other-frame)
    ("a" "Run Aider" aider-run-aider)
    ("z" "Switch to Aider Buffer" aider-switch-to-buffer)
    ("o" "Select Model" aider-change-model)
    ("l" "Clear Aider" aider-clear)
    ("s" "Reset Aider" aider-reset)
    ("x" "Exit Aider" aider-exit)
    ]
   ["Add File to Aider"
    (aider--infix-add-file-read-only)
    ("f" "Add Current File" aider-add-current-file)
    ("R" "Add Current File Read-Only" aider-current-file-read-only)
    ("w" "Add All Files in Current Window" aider-add-files-in-current-window)
    ("d" "Add Same Type Files under dir" aider-add-same-type-files-under-dir)
    ("b" "Batch Add Dired Marked Files" aider-batch-add-dired-marked-files)
    ]
   ["Code Change"
    ("t" "Architect Discuss and Change" aider-architect-discussion)
    ("c" "Code Change" aider-code-change)
    ("r" "Refactor Function or Region" aider-function-or-region-refactor)
    ("U" "Write Unit Test" aider-write-unit-test)
    ("T" "Fix Failing Test Under Cursor" aider-fix-failing-test-under-cursor)
    ("i" "Implement TODOs" aider-implement-todo)
    ("m" "Show Last Commit with Magit" aider-magit-show-last-commit)
    ("u" "Undo Last Change" aider-undo-last-change)
    ]
   ["Discussion"
    ("q" "Ask Question given Context" aider-ask-question)
    ("y" "Go Ahead" aider-go-ahead)
    ("e" "Explain Function or Region" aider-function-or-region-explain)
    ("p" "Explain Symbol Under Point" aider-explain-symbol-under-point)
    ("D" "Debug Exception" aider-debug-exception)
    ]
   ["Other"
    ("g" "General Command" aider-general-command)
    ("Q" "Ask General Question" aider-general-question)
    ("h" "Help" aider-help)
    ]
   ])

;; Removed the default key binding
;; (global-set-key (kbd "C-c a") 'aider-transient-menu)

(defun aider-buffer-name ()
  "Generate the Aider buffer name based on the git repo of the current active buffer using a git command.
If not in a git repository, an error is raised."
  (let ((git-repo-path (magit-toplevel)))
    (if (string-match-p "fatal" git-repo-path)
        (error "Not in a git repository")
      (format "*aider:%s*" git-repo-path))))

;;;###autoload
(defun aider-run-aider (&optional edit-args)
  "Create a comint-based buffer and run \"aider\" for interactive conversation.
With the universal argument, prompt to edit aider-args before running."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
					 (mapconcat 'identity aider-args " ")))
                         aider-args))
         (source-buffer (window-buffer (selected-window))))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
      (with-current-buffer buffer-name
        (comint-mode)
        (setq-local comint-input-sender 'aider-input-sender)
        (setq aider--font-lock-buffer
              (get-buffer-create (concat " *aider-fontify" buffer-name)))
        (add-hook 'kill-buffer-hook #'aider-kill-buffer nil t)
        (add-hook 'comint-output-filter-functions #'aider-fontify-blocks 100 t)
        (font-lock-add-keywords nil aider-font-lock-keywords t)))
    (aider-switch-to-buffer)))

(defun aider-kill-buffer ()
  "Clean-up fontify buffer."
  (when (bufferp aider--font-lock-buffer)
    (kill-buffer aider--font-lock-buffer)))

(defun aider-input-sender (proc string)
  "Reset font-lock state before executing a command."
  (aider-reset-font-lock-state)
  (comint-simple-send proc string))

;; Buffer-local variables for block processing state
(defvar-local aider--block-end-marker nil
  "The end marker for the current block being processed.")

(defvar-local aider--block-start nil
  "The starting position of the current block being processed.")

(defvar-local aider--block-end nil
  "The end position of the current block being processed.")

(defvar-local aider--last-output-start nil
  "an alternative to `comint-last-output-start' used in aider.")

(defvar-local aider--block-mode nil
  "The major mode for the current block being processed.")

(defvar-local aider--font-lock-buffer nil
  "Temporary buffer for fontification.")

(defconst aider-search-marker "<<<<<<< SEARCH")
(defconst aider-diff-marker "=======")
(defconst aider-replace-marker ">>>>>>> REPLACE")
(defconst aider-fence-marker "```")
(defvar aider-block-re
  (format "^\\(?:\\(?1:%s\\|%s\\)\\|\\(?1:%s\\).+\\)$" aider-search-marker aider-diff-marker aider-fence-marker))

(defun aider-reset-font-lock-state ()
  "Reset font lock state to default for processing another a new src block."
  (unless (equal aider--block-end-marker aider-diff-marker)
    ;; if we are processing the other half of a SEARCH/REPLACE block, we need to
    ;; keep the mode
    (setq aider--block-mode nil))
  (setq aider--block-end-marker nil
        aider--last-output-start nil
        aider--block-start nil
        aider--block-end nil))

(defun aider-fontify-blocks (_output)
  "fontify search/replace blocks in comint output."
  (save-excursion
    (goto-char (or aider--last-output-start
                   comint-last-output-start))
    (beginning-of-line)

    ;; Continue processing existing block if we're in one
    (when aider--block-start
      (aider--fontify-block))

    (setq aider--last-output-start nil)
    ;; Look for new blocks if we're not in one
    (while (and (null aider--block-start)
                (null aider--last-output-start)
                (re-search-forward aider-block-re nil t))

      ;; If it is code fence marker, we need to check if there is a SEARCH marker
      ;; directly after it
      (when (equal (match-string 1) aider-fence-marker)
        (let* ((next-line (min (point-max) (1+ (line-end-position))))
               (line-text (buffer-substring
                           next-line
                           (min (point-max) (+ next-line (length aider-search-marker))))))
          (cond ((equal line-text aider-search-marker)
                 ;; Next line is a SEARCH marker. use that instead of the fence marker
                 (re-search-forward (format "^\\(%s\\)" aider-search-marker) nil t))
                ((string-prefix-p line-text aider-search-marker)
                 ;; Next line *might* be a SEARCH marker. Don't process more of
                 ;; the buffer until we know for sure
                 (setq aider--last-output-start comint-last-output-start)))))

      (unless aider--last-output-start
        ;; Set up new block state
        (setq aider--block-end-marker
              (pcase (match-string 1)
                ((pred (equal aider-search-marker)) aider-diff-marker)
                ((pred (equal aider-diff-marker)) aider-replace-marker)
                ((pred (equal aider-fence-marker)) aider-fence-marker))
              aider--block-start (line-end-position)
              aider--block-end (line-end-position)
              aider--block-mode (aider--guess-major-mode))

        ;; Set the major-mode of the font lock buffer
        (let ((mode aider--block-mode))
          (with-current-buffer aider--font-lock-buffer
            (erase-buffer)
            (unless (eq mode major-mode)
              (condition-case e
                  (let ((inhibit-message t))
                    (funcall mode))
                (error (message "aider: failed to init major-mode `%s' for font-locking: %s" mode e))))))

        ;; Process initial content
        (aider--fontify-block)))))

(defun aider--fontify-block ()
  "Fontify as much of the current source block as possible."
  (let* ((last-bol (save-excursion
                     (goto-char (point-max))
                     (line-beginning-position)))
         (last-output-start aider--block-end)
         end-of-block-p)

    (setq aider--block-end
          (cond ((re-search-forward (concat "^" aider--block-end-marker "$") nil t)
                 ;; Found the end of the block
                 (setq end-of-block-p t)
                 (line-beginning-position))
                ((string-prefix-p (buffer-substring last-bol (point-max)) aider--block-end-marker)
                 ;; The end of the text *might* be the end marker. back up to
                 ;; make sure we don't process it until we know for sure
                 last-bol)
                ;; We can process till the end of the text
                (t (point-max))))

  ;; Append new content to temp buffer and fontify
  (let ((new-content (buffer-substring-no-properties
                      last-output-start
                      aider--block-end))
        (pos aider--block-start)
        (font-pos 0)
        fontified)

    ;; Insert the new text and get the fontified result
    (with-current-buffer aider--font-lock-buffer
      (goto-char (point-max))
      (insert new-content)
      (with-demoted-errors "aider block font lock error: %s"
        (let ((inhibit-message t))
          (font-lock-ensure)))
      (setq fontified (buffer-string)))

    ;; Apply the faces to the buffer
    (remove-overlays aider--block-start aider--block-end)
    (while (< pos aider--block-end)
      (let* ((next-font-pos (or (next-property-change font-pos fontified) (length fontified)))
             (next-pos (+ aider--block-start next-font-pos))
             (face (get-text-property font-pos 'face fontified)))
        (ansi-color-apply-overlay-face pos next-pos face)
        (setq pos next-pos
              font-pos next-font-pos))))

  ;; If we found the end marker, finalize the block
  (when end-of-block-p
    (when (equal aider--block-end-marker aider-diff-marker)
      ;; we will need to process the other half of the SEARCH/REPLACE block.
      ;; Backup so it will get matched
      (beginning-of-line))
    (aider-reset-font-lock-state))))

(defun aider--guess-major-mode ()
  "Extract the major mode from fence markers or filename."
  (save-excursion
    (beginning-of-line)
    (or
     ;; check if the block has a language id
     (when (let ((re "^```\\([^[:space:]]+\\)"))
             (or (looking-at re)
                 (save-excursion
                   (forward-line -1)
                   ;; check the previous line since this might be a SEARCH block
                   (looking-at re))))
       (let* ((lang (downcase (match-string 1)))
              (mode (map-elt aider-language-name-map lang lang)))
         (intern-soft (concat mode "-mode"))))
     ;; check the file extension in auto-mode-alist
     (when (re-search-backward "^\\([^[:space:]]+\\)" (line-beginning-position -3) t)
       (let ((file (match-string 1)))
         (cdr (cl-assoc-if (lambda (re) (string-match re file)) auto-mode-alist))))
     aider--block-mode
     'fundamental-mode)))

;; Function to switch to the Aider buffer
;;;###autoload
(defun aider-switch-to-buffer ()
  "Switch to the Aider buffer.
When `aider--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the current buffer is already the Aider buffer, do nothing."
  (interactive)
  (if (string= (buffer-name) (aider-buffer-name))
      (message "Already in Aider buffer")
    (if-let ((buffer (get-buffer (aider-buffer-name))))
        (if aider--switch-to-buffer-other-frame
            (switch-to-buffer-other-frame buffer)
          (pop-to-buffer buffer))
      (message "Aider buffer '%s' does not exist." (aider-buffer-name)))))

;; Function to reset the Aider buffer
;;;###autoload
(defun aider-clear ()
  "Send the command \"/clear\" to the Aider buffer."
  (interactive)
  (aider--send-command "/clear"))

;;;###autoload
(defun aider-reset ()
  "Send the command \"/reset\" to the Aider buffer."
  (interactive)
  (aider--send-command "/reset"))

;;;###autoload
(defun aider-exit ()
  "Send the command \"/exit\" to the Aider buffer."
  (interactive)
  (aider--send-command "/exit"))

(defun aider--comint-send-string-syntax-highlight (buffer text)
  "Send TEXT to the comint BUFFER with syntax highlighting.
This function ensures proper syntax highlighting by inheriting face properties
from the source buffer and maintaining proper process markers."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer))
          (inhibit-read-only t))
      (goto-char (process-mark process))
      ;; Insert text with proper face properties
      (insert (propertize text
                         'face 'aider-command-text
                         'font-lock-face 'aider-command-text
                         'rear-nonsticky t))
      ;; Update process mark and send text
      (set-marker (process-mark process) (point))
      (comint-send-string process text))))

(defun aider--process-message-if-multi-line (str)
  "Entering multi-line chat messages
https://aider.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines, wrap it in {aider\\nstr\\naider}.
Otherwise return STR unchanged."
  (if (string-match-p "\n" str)
      (format "{aider\n%s\naider}" str)
    str))

;; Shared helper function to send commands to corresponding aider buffer
(defun aider--send-command (command &optional switch-to-buffer)
  "Send COMMAND to the corresponding aider comint buffer after performing necessary checks.
COMMAND should be a string representing the command to send."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (get-buffer (aider-buffer-name))))
      (let* ((command (aider--process-message-if-multi-line command))
             (aider-process (get-buffer-process aider-buffer)))
        ;; Check if the corresponding aider buffer has an active process
        (if (and aider-process (comint-check-proc aider-buffer))
            (progn
              (aider-reset-font-lock-state)
              ;; Send the command to the aider process
              (aider--comint-send-string-syntax-highlight aider-buffer (concat command "\n"))
              ;; Provide feedback to the user
              ;; (message "Sent command to aider buffer: %s" (string-trim command))
              (when switch-to-buffer
                (aider-switch-to-buffer))
              (sleep-for 0.2))
          (message "No active process found in buffer %s." (aider-buffer-name))))
    (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))
    ))

;;;###autoload
(defun aider-add-or-read-current-file (command-prefix)
  "Send the command \"COMMAND-PREFIX <current buffer file full path>\" to the corresponding aider comint buffer."
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let* ((local-name (file-local-name
                       (expand-file-name buffer-file-name)))
           (formatted-path (if (string-match-p " " local-name)
                             (format "\"%s\"" local-name)
                           local-name))
           (command (format "%s %s" command-prefix formatted-path)))
      ;; Use the shared helper function to send the command
      (aider--send-command command))))

;; Function to send "/add <current buffer file full path>" to corresponding aider buffer
;;;###autoload
(defun aider-add-current-file ()
  "Send the command \"/add <current buffer file full path>\" to the corresponding aider comint buffer."
  (interactive)
  (aider-add-or-read-current-file (aider--get-add-command-prefix)))

;;;###autoload
(defun aider-current-file-read-only ()
  "Send the command \"/read-only <current buffer file full path>\" to the corresponding aider comint buffer."
  (interactive)
  (aider-add-or-read-current-file "/read-only"))

;; New function to add files in all buffers in current emacs window
;;;###autoload
(defun aider-add-files-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Aider buffer."
  (interactive)
  (let ((files (mapcar (lambda (buffer)
                         (with-current-buffer buffer
                           (when buffer-file-name
                             (expand-file-name buffer-file-name))))
                       (mapcar 'window-buffer (window-list)))))
    (setq files (delq nil files))
    (if files
        (let ((command (concat (aider--get-add-command-prefix) " " (mapconcat 'identity files " "))))
          (aider--send-command command nil))
      (message "No files found in the current window."))))

;; Function to send a custom command to corresponding aider buffer
;;;###autoload
(defun aider-general-command ()
  "Prompt the user to input COMMAND and send it to the corresponding aider comint buffer."
  (interactive)
  (let ((command (aider-read-string "Enter command to send to aider: ")))
    ;; Use the shared helper function to send the command
    (aider--send-command command t)))

;; New function to get command from user and send it prefixed with "/code "
;;;###autoload
(defun aider-code-change ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/code \"."
  (interactive)
  (let ((command (aider-read-string "Enter code change requirement: ")))
    (aider-send-command-with-prefix "/code " command)))

;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/ask \".
If a region is active, append the region text to the question.
If cursor is inside a function, include the function name as context."
  (interactive)
  ;; Dispatch to general question if in aider buffer
  (when (string= (buffer-name) (aider-buffer-name))
    (call-interactively 'aider-general-question)
    (cl-return-from aider-ask-question))
  
  (let* ((function-name (which-function))
         (initial-input (when function-name 
                          (format "About function '%s': " function-name)))
         (question (aider-read-string "Enter question to ask: " initial-input))
         (region-text (and (region-active-p) 
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         (command (if region-text
                      (format "/ask %s: %s" question region-text)
                    (format "/ask %s" question))))
    (aider-add-current-file)
    (aider--send-command command t)))

;;;###autoload
(defun aider-general-question ()
  "Prompt the user for a general question and send it to the corresponding aider comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((question (aider-read-string "Enter general question to ask: ")))
    (let ((command (format "/ask %s" question)))
      (aider--send-command command t))))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aider-help ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/help \"."
  (interactive)
  (let ((command (aider-read-string "Enter help question: ")))
    (aider-send-command-with-prefix "/help " command)))

;; New function to get command from user and send it prefixed with "/architect "
;;;###autoload
(defun aider-architect-discussion ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/architect \"."
  (interactive)
  (let ((command (aider-read-string "Enter architect discussion question: ")))
    (aider-send-command-with-prefix "/architect " command)))

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aider-debug-exception ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/debug \",
replacing all newline characters except for the one at the end."
  (interactive)
  (let ((command (aider-plain-read-string "Enter exception, can be multiple lines: ")))
    (aider--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;;;###autoload
(defun aider-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aider comint buffer."
  (interactive)
  (aider--send-command "go ahead" t))

;; New function to show the last commit using magit
;;;###autoload
(defun aider-magit-show-last-commit ()
  "Show the last commit message using Magit.
If Magit is not installed, report that it is required."
  (interactive)
  (if (require 'magit nil 'noerror)
      (magit-show-commit "HEAD")
    (message "Magit is required to show the last commit.")))

;; Modified function to get command from user and send it based on selected region
;;;###autoload
(defun aider-undo-last-change ()
  "Undo the last change made by Aider."
  (interactive)
  (aider--send-command "/undo"))

(defun aider-region-refactor-generate-command (region-text function-name user-command)
  "Generate the command string based on REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text region-text))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\"\n"
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\"\n"
              user-command processed-region-text))))

;;;###autoload
(defun aider-function-refactor ()
  "Get the function name under cursor and send refactor command to aider.
The command will be formatted as \"/architect\" followed by refactoring instructions
for the specified function."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((initial-input (format "refactor %s: " function-name))
             (user-command (aider-read-string "Enter refactor instruction: " initial-input))
             (command (format "/architect %s" user-command)))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-region-refactor ()
  "Get a command from the user and send it to the corresponding aider comint buffer based on the selected region.
The command will be formatted as \"/architect \" followed by the user command and the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (user-command (aider-read-string "Enter your refactor instruction: "))
             (command (aider-region-refactor-generate-command region-text function-name user-command)))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No region selected.")))

;;;###autoload
(defun aider-function-or-region-refactor ()
  "Call aider-function-refactor when no region is selected, otherwise call aider-region-refactor."
  (interactive)
  (if (region-active-p)
      (aider-region-refactor)
    (aider-function-refactor)))

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
      (let* ((initial-input (format "explain %s: " function-name))
             (user-question (aider-read-string "Enter your question about the function: " initial-input))
             (command (format "/ask %s" user-question)))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-function-or-region-explain ()
  "Call aider-function-explain when no region is selected, otherwise call aider-region-explain."
  (interactive)
  (if (region-active-p)
      (aider-region-explain)
    (aider-function-explain)))

;; New function to explain the symbol at line
;;;###autoload
(defun aider-explain-symbol-under-point ()
  "Ask Aider to explain symbol under point, given the code line as background info."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prompt (format "/ask Please explain what '%s' means in the context of this code line: %s"
                         symbol line)))
    (aider-add-current-file)
    (aider--send-command prompt t)))

(defun aider-send-command-with-prefix (prefix command)
  "Send COMMAND to the Aider buffer prefixed with PREFIX."
  (aider-add-current-file)
  (aider--send-command (concat prefix command) t))

;;; functions for dired related

;; New function to add multiple Dired marked files to Aider buffer
;;;###autoload
(defun aider-batch-add-dired-marked-files ()
  "Add multiple Dired marked files to the Aider buffer with the \"/add\" command."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (let ((command (concat (aider--get-add-command-prefix) " " (mapconcat 'expand-file-name files " "))))
          (aider--send-command command t))
      (message "No files marked in Dired."))))

;; New function to add all files with same suffix as current file under current directory
;;;###autoload
(defun aider-add-same-type-files-under-dir ()
  "Add all files with same suffix as current file under current directory to Aider.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (let ((command (concat (aider--get-add-command-prefix) " " (mapconcat 'identity files " "))))
          (aider--send-command command t))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

;;; functions for test fixing

;;;###autoload
(defun aider-write-unit-test ()
  "Generate unit test code for current buffer.
Do nothing if current buffer is not visiting a file.
If current buffer filename contains 'test', do nothing.
If cursor is on a function, generate unit test for that function.
Otherwise, generate unit tests for the entire file."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (if (string-match-p "test" (file-name-nondirectory buffer-file-name))
        (message "Current buffer appears to be a test file.")
      (let* ((function-name (which-function))
             (common-instructions "Keep existing tests if there are. Do not use Mock if possible. Follow standard unit testing practices.")
             (initial-input
              (if function-name
                  (format "Please write unit test code for function '%s'. %s" 
                         function-name common-instructions)
                (format "Please write unit test code for file '%s'. For each function %s" 
                       (file-name-nondirectory buffer-file-name) common-instructions)))
             (user-command (aider-read-string "Unit test generation instruction: " initial-input))
             (command (format "/architect %s" user-command)))
        (aider-add-current-file)
        (aider--send-command command t)))))

;;;###autoload
(defun aider-fix-failing-test-under-cursor ()
  "Report the current test failure to aider and ask it to fix the code.
This function assumes the cursor is on or inside a test function."
  (interactive)
  (if-let ((test-function-name (which-function)))
      (let* ((initial-input (format "The test '%s' is failing. Please analyze and fix the code to make the test pass. Don't break any other test"
                                   test-function-name))
             (test-output (aider-read-string "Architect question: " initial-input))
             (command (format "/architect %s" test-output)))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No test function found at cursor position.")))

(defun aider--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace."
  (when comment-start
    (let ((comment-str (string-trim-right comment-start)))
      (string-match-p (concat "^[ \t]*"
                             (regexp-quote comment-str)
                             "+")
                     (string-trim-left line)))))

;;;###autoload
(defun aider-implement-todo ()
  "Implement TODO comments in current context.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement TODOs for that function.
Otherwise implement TODOs for the entire current file."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (aider--is-comment-line current-line))
           (function-name (which-function))
           (initial-input
            (cond
             (is-comment
              (format "Please implement this comment: '%s'. It is already inside current code. Please do in-place implementation. Keep the existing code structure and implement just this specific comment." 
                      current-line))
             (function-name
              (format "Please implement the TODO items in function '%s'. Keep the existing code structure and only implement the TODOs in comments." 
                      function-name))
             (t
              (format "Please implement all TODO items in file '%s'. Keep the existing code structure and only implement the TODOs in comments." 
                      (file-name-nondirectory buffer-file-name)))))
           (user-command (aider-read-string "TODO implementation instruction: " initial-input))
           (command (format "/architect %s" user-command)))
      (aider-add-current-file)
      (aider--send-command command t))))

;;; Model selection functions
;;;###autoload
(defun aider-change-model ()
  "Interactively select and change AI model in current aider session."
  (interactive)
  (let ((model (aider--select-model)))
    (when model
      (aider--send-command (format "/model %s" model) t))))

(defun aider--select-model ()
  "Private function for model selection with completion."
  (completing-read "Select AI model: " aider-popular-models nil t nil nil (car aider-popular-models)))

;;; functions for sending text blocks

;; New function to send "<line under cursor>" or region line by line to the Aider buffer
;;;###autoload
(defun aider-send-line-under-cursor ()
  "If region is active, send the selected region line by line to the Aider buffer.
Otherwise, send the line under cursor to the Aider buffer."
  (interactive)
  (if (region-active-p)
      (aider-send-region-by-line)
    (let ((line (thing-at-point 'line t)))
      (aider--send-command (string-trim line) nil))))

;;; New function to send the current selected region line by line to the Aider buffer
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
                  (aider--send-command line nil)))
              (split-string region-text "\n" t)))
    (message "No region selected.")))

;;;###autoload
(defun aider-send-region ()
  "Send the current active region text as a whole block to aider session."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t)))
    (message "No region selected.")))

;; Define the keymap for Aider Minor Mode
(defvar aider-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'aider-send-line-under-cursor)
    (define-key map (kbd "C-c C-c") 'aider-send-region-by-line)
    (define-key map (kbd "C-c C-r") 'aider-send-region)
    (define-key map (kbd "C-c C-z") 'aider-switch-to-buffer)
    map)
  "Keymap for Aider Minor Mode.")

;; Define the Aider Minor Mode
;;;###autoload
(define-minor-mode aider-minor-mode
  "Minor mode for Aider with keybindings."
  :lighter " Aider"
  :keymap aider-minor-mode-map)

(when (featurep 'doom)
  (require 'aider-doom))

(provide 'aider)

;;; aider.el ends here
