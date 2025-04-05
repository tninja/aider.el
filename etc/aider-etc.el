
;; New function to run `find-name-dired` from the Git repository root directory
;;;###autoload
(defun aider-repo-find-name-dired (pattern)
  "Run `find-name-dired` from the Git repository root directory with the given PATTERN."
  (interactive "sFind name (pattern): ")
  (let* ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (repo-path (string-trim git-repo-path)))
    (if (string-match-p "fatal" repo-path)
        (message "Not in a git repository")
      (find-name-dired repo-path pattern))))

;;;###autoload
(defun aider-git-repo-root-dired ()
  "Open a Dired buffer at the root of the current Git repository."
  (interactive)
  (let ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel")))
    (if (string-match-p "fatal" git-repo-path)
        (message "The current buffer is not in a Git repository.")
      (let ((repo-path (string-trim git-repo-path)))
        (dired-other-window repo-path)))))

;; New function to explain the symbol at line
;;;###autoload
(defun aider-explain-symbol-under-point ()
  "Ask Aider to explain symbol under point, given the code line as background info."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (question (format "Please explain what '%s' means in the context of this code line: %s"
                         symbol line)))
    (aider-current-file-command-and-switch "/ask " question)
    ))

;;;###autoload
(defun aider-fix-failing-test-under-cursor ()
  "Report the current test failure to aider and ask it to fix the code.
This function assumes the cursor is on or inside a test function."
  (interactive)
  (if-let ((test-function-name (which-function)))
      (let* ((initial-input (format "The test '%s' is failing. Please analyze and fix the code to make the test pass. Don't break any other test"
                                   test-function-name))
             (test-output (aider-read-string "Architect question: " initial-input)))
        (aider-current-file-command-and-switch "/architect " test-output))
    (message "No test function found at cursor position.")))

;;;###autoload
(defun aider-other-process-command ()
  "Send process control commands to aider.
With prefix argument MANUAL, manually enter the command
Prompts user to select from a list of available commands:
- /clear: Clear the chat history
- /copy: Copy the last chat message
- /drop: Drop all files
- /ls: List tracked files
- /lint: Run linter on tracked files
- /map: Show file map
- /map-refresh: Refresh file map
- /paste: Paste the last copied chat message
- /settings: Show current settings
- /tokens: Show token usage"
  (interactive)
  (let* ((commands '("manual input" "/clear" "/copy" "/drop" "/ls" "/lint" "/map"
                     "/map-refresh" "/paste" "/settings" "/tokens"))
         (command (completing-read "Select command: " commands nil t)))
    (if (string= command "manual input")
        (aider-general-command)
      (aider--send-command command t))))

;; Function to send a custom command to corresponding aider buffer
;;;###autoload
(defun aider-general-command ()
  "Input COMMAND and send it to the corresponding aider comint buffer."
  (interactive)
  (let ((command (aider-read-string "Enter command to send to aider: ")))
    ;; Use the shared helper function to send the command
    (aider--send-command command t)))
