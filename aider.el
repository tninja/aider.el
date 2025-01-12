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

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-program "aider"
  "The name or path of the aider program."
  :type 'string
  :group 'aider)

(defcustom aider-args '("--model" "gemini/gemini-exp-1206")
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

(defcustom aider--switch-to-buffer-other-frame nil
  "When non-nil, open Aider buffer in a new frame using `switch-to-buffer-other-frame'.
When nil, use standard `display-buffer' behavior."
  :type 'boolean
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

(defvar aider-font-lock-keywords '(("^\x2500+\n?" 0 '(face aider-command-separator) t)
                                   ("^\x2500+" 0 '(face nil display (space :width 2))))
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
    ("l" "Clear Aider" aider-clear)
    ("s" "Reset Aider" aider-reset)
    ("x" "Exit Aider" aider-exit)
    ]
   ["Add File to Aider"
    (aider--infix-add-file-read-only)
    ("f" "Add Current File" aider-add-current-file)
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

(defun aider--inherit-source-highlighting (source-buffer)
  "Inherit syntax highlighting settings from SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (let ((source-keywords font-lock-keywords)
          (source-keywords-only font-lock-keywords-only)
          (source-keywords-case-fold-search font-lock-keywords-case-fold-search)
          ;; (source-syntax-table (syntax-table))
          (source-defaults font-lock-defaults))
      (with-current-buffer (aider-buffer-name)
        ;; (set-syntax-table source-syntax-table)
        (setq font-lock-defaults
              (if source-defaults
                  source-defaults
                `((,source-keywords)
                  nil
                  ,source-keywords-case-fold-search)))
        (setq font-lock-keywords source-keywords
              font-lock-keywords-only source-keywords-only
              font-lock-keywords-case-fold-search source-keywords-case-fold-search)))))

;;;###autoload
(defun aider-run-aider ()
  "Create a comint-based buffer and run \"aider\" for interactive conversation."
  (interactive)
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (source-buffer (window-buffer (selected-window))))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aider" buffer-name aider-program nil aider-args)
      (with-current-buffer buffer-name
        (comint-mode)
        (font-lock-add-keywords nil aider-font-lock-keywords t)
        ;; Only inherit syntax highlighting when source buffer is in prog-mode
        (when (with-current-buffer source-buffer
                (derived-mode-p 'prog-mode))
          (aider--inherit-source-highlighting source-buffer)
          (font-lock-mode 1)
          (font-lock-ensure)
          (message "Aider buffer syntax highlighting inherited from %s"
                   (with-current-buffer source-buffer major-mode)))
        ))
    (aider-switch-to-buffer)))

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

;; Function to send large text (> 1024 chars) to the Aider buffer
(defun aider--comint-send-large-string (buffer text)
  "Send large TEXT to the comint buffer in chunks of 1000 characters.
Ensure proper highlighting of the text in the buffer."
  (let ((chunk-size 1000)
        (pos 0)
        (process (get-buffer-process buffer)))
    (while (< pos (length text))
      (let* ((end-pos (min (+ pos chunk-size) (length text)))
             (chunk (substring text pos end-pos)))
        ;; Insert text into buffer and ensure highlighting
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (current-point (process-mark process)))
            (goto-char current-point)
            ;; Use comint-output-filter to ensure proper text property handling
            (comint-output-filter process (propertize chunk
                                                     'face 'aider-command-text
                                                     'font-lock-face 'aider-command-text
                                                     'rear-nonsticky t))))
        ;; Send raw text to process
        (process-send-string process chunk)
        (sleep-for 0.2)
        ;; (message "Sent command to aider buffer: %s" chunk)
        (setq pos end-pos)))))

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
              ;; Send the command to the aider process
              (aider--comint-send-large-string aider-buffer (concat command "\n"))
              ;; Provide feedback to the user
              ;; (message "Sent command to aider buffer: %s" (string-trim command))
              (when switch-to-buffer
                (aider-switch-to-buffer)))
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
             (initial-input
              (if function-name
                  (format "Please write unit test code for function '%s'. Include test cases for:
1. Normal input/output scenarios
2. Edge cases and boundary conditions
3. Error handling and invalid inputs
Make the test comprehensive but maintainable. Follow standard unit testing practices." 
                         function-name)
                (format "Please write unit test code for file '%s'. For each function include test cases for:
1. Normal input/output scenarios
2. Edge cases and boundary conditions
3. Error handling and invalid inputs
Make the tests comprehensive but maintainable. Follow standard unit testing practices." 
                       (file-name-nondirectory buffer-file-name))))
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

;;; functions for sending text blocks

;; New function to send "<line under cursor>" to the Aider buffer
;;;###autoload
(defun aider-send-line-under-cursor ()
  "Send the command \"ask <line under cursor>\" to the Aider buffer."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (aider--send-command (string-trim line) t)))

;;; New function to send the current paragraph to the Aider buffer
;;;###autoload
(defun aider-send-paragraph-by-line ()
  "Get the whole text of the current paragraph, split them into lines,
   strip the newline character from each line,
   for each non-empty line, send it to aider session"
  (interactive)
  (let ((paragraph (save-excursion
                     (backward-paragraph)
                     (let ((start (point)))
                       (forward-paragraph)
                       (buffer-substring-no-properties start (point))))))
    (mapc (lambda (line)
            (unless (string-empty-p line)
              (aider--send-command line t)))
          (split-string paragraph "\n" t))))

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
    (define-key map (kbd "C-c C-c") 'aider-send-paragraph-by-line)
    (define-key map (kbd "C-c C-r") 'aider-send-region)
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
