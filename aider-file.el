;;; aider-file.el --- File operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'dired)

;; Added helper function to get the relative or absolute path of the file
(defun aider--get-file-path (file-path)
  "Get the appropriate path for FILE-PATH.
If the file is in a git repository, return path relative to git root.
Otherwise, return the full local path."
  (let ((git-root (ignore-errors (magit-toplevel)))
        (full-path (expand-file-name file-path)))
    (if git-root
        ;; Get path relative to git root
        (file-relative-name full-path git-root)
      ;; Use full path for non-git files
      (file-local-name full-path))))

;; Added helper function to format file paths (process spaces)
(defun aider--format-file-path (file-path)
  "Format FILE-PATH for use in commands.
Add quotes if the path contains spaces."
  (if (string-match-p " " file-path)
      (format "\"%s\"" file-path)
    file-path))

;; Function to send "/add <current buffer file full path>" to corresponding aider buffer
;;;###autoload
(defun aider-add-current-file ()
  "Add current file to aider session."
  (interactive)
  (aider-action-current-file "/add"))

;;;###autoload
(defun aider-current-file-read-only ()
  "Add current file as read only to aider session."
  (interactive)
  (aider-action-current-file "/read-only"))

;;;###autoload
(defun aider-drop-current-file ()
  "Drop current file from aider session."
  (interactive)
  (aider-action-current-file "/drop"))

;;;###autoload
(defun aider-action-current-file (command-prefix)
  "Perform the COMMAND-PREFIX to aider session.
If the file is in a git repository, use path relative to git root."
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let* ((local-name (aider--get-file-path buffer-file-name))
           (formatted-path (aider--format-file-path local-name))
           (command (format "%s %s" command-prefix formatted-path)))
      ;; Use the shared helper function to send the command
      (aider--send-command command))))

;; New function to add files in all buffers in current emacs window
;;;###autoload
(defun aider-add-files-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Aider buffer.
If files are in a git repository, use paths relative to git root."
  (interactive)
  (let* ((files (mapcar (lambda (buffer)
                          (with-current-buffer buffer
                            (when buffer-file-name
                              (aider--get-file-path buffer-file-name))))
                        (mapcar #'window-buffer (window-list))))
         (files (delq nil files))
         (formatted-files (mapcar #'aider--format-file-path files)))
    (if files
        (let ((command (concat "/add " (mapconcat #'identity formatted-files " "))))
          (aider--send-command command nil))
      (message "No files found in the current window."))))

;; New function to show the last commit using magit
;;;###autoload
(defun aider-magit-show-last-commit-or-log (&optional log)
  "Show the last commit message using Magit.
With prefix argument (LOG), show commit log instead of single commit.
If Magit is not installed, report that it is required."
  (interactive "P")
  (if log
      (magit-log-current nil)
    (magit-show-commit "HEAD")))

;; Modified function to get command from user and send it based on selected region
;;;###autoload
(defun aider-undo-last-change ()
  "Undo the last change made by Aider."
  (interactive)
  (aider--send-command "/undo"))

;;; functions for dired related

;; New function to add multiple Dired marked files to Aider buffer
(defun aider--batch-add-dired-marked-files-with-command (command-prefix)
  "Add multiple Dired marked files to the Aider buffer with COMMAND-PREFIX.
COMMAND-PREFIX should be either \"/add\" or \"/read-only\".
Uses relative paths if files are in a git repository."
  (let ((absolute-files (dired-get-marked-files)))
    (if absolute-files
        (let* (;; Convert absolute paths to relative paths if needed
               (relative-files (mapcar #'aider--get-file-path absolute-files))
               ;; Format paths (e.g., add quotes for spaces)
               (formatted-files (mapcar #'aider--format-file-path relative-files))
               ;; Construct the command string
               (command (concat command-prefix " " (mapconcat #'identity formatted-files " "))))
          (aider--send-command command t))
      (message "No files marked in Dired."))))

;;;###autoload
(defun aider-batch-add-dired-marked-files ()
  "Add multiple Dired marked files with the \"/add\" command."
  (interactive)
  (aider--batch-add-dired-marked-files-with-command "/add"))

;;;###autoload
(defun aider-batch-add-dired-marked-files-read-only ()
  "Add multiple Dired marked files with the \"/read-only\" command."
  (interactive)
  (aider--batch-add-dired-marked-files-with-command "/read-only"))

;;;###autoload
(defun aider-add-current-file-or-dired-marked-files (&optional read-only)
  "Add files to Aider based on current context.
If current buffer is a Dired buffer, add all marked files.
Otherwise, add the current file.
With prefix argument READ-ONLY, add files as read-only."
  (interactive "P")
  (if read-only
      (aider-add-current-file-or-dired-marked-files-read-only)
    (if (derived-mode-p 'dired-mode)
        (aider-batch-add-dired-marked-files)
      (aider-add-current-file))))

;;;###autoload
(defun aider-add-current-file-or-dired-marked-files-read-only ()
  "Add files to Aider as read-only based on current context.
If current buffer is a Dired buffer, add all marked files as read-only.
Otherwise, add the current file as read-only."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (aider-batch-add-dired-marked-files-read-only)
    (aider-current-file-read-only)))

(defun aider-current-file-command-and-switch (prefix command)
  "Send COMMAND to the Aider buffer prefixed with PREFIX."
  (aider-add-current-file)
  (aider--send-command (concat prefix command) t)
  ;; (when (string-prefix-p "/architect" prefix)
  ;;   (message "Note: Aider v0.77.0 automatically accept changes for /architect command. If you want to review the code change before accepting it like before for many commands in aider.el, you can disable that flag with \"--no-auto-accept-architect\" in aider-args or .aider.conf.yml."))
  )

(defun aider--get-files-in-directory (directory suffixes)
  "Retrieve list of files in DIRECTORY matching SUFFIXES. SUFFIXES is a list of strings without dot."
  (let ((regex (concat "\\.\\(" (mapconcat #'regexp-quote suffixes "\\|") "\\)$")))
    (directory-files-recursively directory regex)))

(defun aider--filter-files-by-content-regex (files content-regex)
  "Filter FILES based on CONTENT-REGEX using grep.
Return a list of files whose content matches CONTENT-REGEX.
Return original FILES if CONTENT-REGEX is nil or empty.
Return nil if grep errors or no files match."
  (if (and files content-regex (not (string-empty-p content-regex)))
      (let* ((grep-args (list* "-l" "-E" content-regex files))
             (temp-buffer (generate-new-buffer " *grep-output*"))
             (exit-status
              (apply #'call-process "grep" nil (list temp-buffer nil) nil grep-args)))
        (prog1
            (if (or (zerop exit-status) (= exit-status 1)) ; 0 for match, 1 for no match
                (with-current-buffer temp-buffer
                  (if (zerop exit-status) ; Only parse if matches were found
                      (split-string (buffer-string) "\n" t)
                    nil)) ; No files matched
              (progn ; Handle grep errors (exit status > 1)
                (message "Error running grep: %s"
                         (with-current-buffer temp-buffer (buffer-string)))
                nil)) ; Return nil on error
          (kill-buffer temp-buffer)))
    files)) ; If no regex or no initial files, use original files

;;;###autoload
(defun aider-add-module (&optional read-only directory suffix-input content-regex)
  "Add all files from DIRECTORY with SUFFIX-INPUT to Aider session.
SUFFIX-INPUT is a comma-separated list of file suffixes without dots.
If CONTENT-REGEX is provided, only files whose content matches the regex are added.
With a prefix argument (C-u), files are added read-only (/read-only)."
  (interactive
   (list current-prefix-arg
         (read-directory-name "Module directory: " nil nil t)
         (read-string "File suffixes (comma-separated): "
                      "py,java,scala,el,sql")
         (read-string "Content regex (empty for none): " nil)))
  (let* ((cmd-prefix   (if read-only "/read-only" "/add"))
         (suffixes     (split-string suffix-input "\\s-*,\\s-*" t))
         (files-by-suffix (aider--get-files-in-directory directory suffixes))
         (filtered-files (aider--filter-files-by-content-regex files-by-suffix content-regex))
         (rel-paths    (if filtered-files (mapcar #'aider--get-file-path filtered-files) nil))
         (formatted    (if rel-paths (mapcar #'aider--format-file-path rel-paths) nil)))
    (if filtered-files
        (progn
          (aider--send-command (concat cmd-prefix " " (mapconcat #'identity formatted " ")) t)
         (message (if read-only
                      "Added %d files as read-only from %s%s"
                    "Added %d files from %s%s")
                  (length filtered-files) directory
                  (if (and content-regex (not (string-empty-p content-regex)))
                      (format " (matching content regex '%s')" content-regex)
                    "")))
      (message (format "No files found in %s with suffixes %s%s"
                       directory
                       suffix-input
                       (if (and content-regex (not (string-empty-p content-regex)))
                           (format " that also match content regex '%s'" content-regex)
                         ""))))))

;;;###autoload
(defun aider-pull-or-review-diff-file ()
  "Review a diff file with Aider or generate one if not viewing a diff.
If current buffer is a .diff file, ask Aider to review it.
Otherwise, generate the diff."
  (interactive)
  (if (and buffer-file-name (string-match-p "\\.diff$" buffer-file-name))
      (let* ((file-name (file-name-nondirectory buffer-file-name))
             (init-prompt (format "Please perform a comprehensive code review of this diff file (%s). Analyze the changes for:
1. Potential bugs, edge cases, or logic errors
2. Security vulnerabilities or performance issues
3. Adherence to best practices and design patterns
4. Code readability and maintainability
5. Completeness of implementation
6. Suggestions for improvements or alternative approaches
For each issue found, please explain:
- The specific location in the code
- Why it's problematic
- A recommended solution" file-name))
             (prompt (aider-read-string "Enter diff review prompt: " init-prompt)))
        (aider-current-file-command-and-switch "/ask " prompt))
    (aider--magit-generate-feature-branch-diff-file)))

(defun aider--validate-git-repository ()
  "Ensure we're in a git repository and return the git root.
Signal an error if not in a git repository."
  (let ((git-root (magit-toplevel)))
    (unless git-root
      (user-error "Not in a git repository"))
    git-root))

(defun aider--get-full-branch-ref (branch)
  "Get full reference for BRANCH, handling remote branches properly.
Prefer remote branch (origin/BRANCH) if it exists.
Otherwise, use local branch or ref.
Git can diff remote branches directly without checking them out locally."
  (cond
   ;; Check if it exists as a remote branch first, unless 'branch' already starts with 'origin/'
   ((and (not (string-prefix-p "origin/" branch)) (magit-branch-p (concat "origin/" branch)))
    (concat "origin/" branch))
   ;; Then check if it's a valid local branch or ref (this will also handle cases like "origin/main" directly)
   ((or (magit-branch-p branch) (magit-rev-verify branch))
    branch)
   ;; Return as is (might be a commit hash or special ref if not caught above)
   (t branch)))

(defun aider--verify-branches (base-branch feature-branch)
  "Verify that BASE-BRANCH and FEATURE-BRANCH exist.
Signal an error if either branch doesn't exist."
  ;; Verify base branch exists
  (unless (or (magit-branch-p base-branch)
              (magit-branch-p (concat "origin/" base-branch))
              (magit-rev-verify base-branch))
    (user-error "Base branch '%s' not found locally or in remotes" base-branch))
  ;; Verify feature branch exists (if not HEAD)
  (when (and (not (string= feature-branch "HEAD"))
             (not (magit-branch-p feature-branch))
             (not (magit-branch-p (concat "origin/" feature-branch)))
             (not (magit-rev-verify feature-branch)))
    (user-error "Feature branch '%s' not found locally or in remotes" feature-branch)))

(defun aider--generate-staged-diff (diff-file)
  "Generate diff for staged (staged) changes and save to DIFF-FILE."
  (message "Generating diff for staged (staged) changes...")
  (magit-run-git "diff" "--cached" (concat "--output=" diff-file)))

(defun aider--resolve-diff-branches (type input-base-branch input-feature-branch &optional branch-scope)
  "Resolve base and feature branches for diff generation.
TYPE is 'commit, 'base-vs-head, or 'branch-range.
INPUT-BASE-BRANCH and INPUT-FEATURE-BRANCH are user-provided names.
BRANCH-SCOPE is 'local or 'remote, used for 'branch-range.
Returns a cons cell (RESOLVED-BASE . RESOLVED-FEATURE)."
  (let (resolved-base-branch resolved-feature-branch)
    (pcase type
      ('commit
       ;; Input is already commit^ and commit
       (setq resolved-base-branch input-base-branch)
       (setq resolved-feature-branch input-feature-branch))
      ('base-vs-head
       ;; Base branch can be local or remote, feature is HEAD
       (setq resolved-base-branch (aider--get-full-branch-ref input-base-branch))
       (setq resolved-feature-branch "HEAD")) ; HEAD is always resolved correctly by git
      ('branch-range
       (pcase branch-scope
         ('local
          ;; User asserts branches are local
          (setq resolved-base-branch input-base-branch)
          (setq resolved-feature-branch input-feature-branch))
         ('remote
          ;; User asserts branches are remote, or should be treated as such.
          ;; aider--get-full-branch-ref will try to find origin/X if X is given.
          (setq resolved-base-branch (aider--get-full-branch-ref input-base-branch))
          (setq resolved-feature-branch (aider--get-full-branch-ref input-feature-branch)))
         (_ ; Default or unknown scope, fallback to smart resolution (should not happen with prompt)
          (setq resolved-base-branch (aider--get-full-branch-ref input-base-branch))
          (setq resolved-feature-branch (aider--get-full-branch-ref input-feature-branch))))))
    (cons resolved-base-branch resolved-feature-branch)))

(defun aider--generate-branch-or-commit-diff (diff-params diff-file)
  "Generate diff based on DIFF-PARAMS and save to DIFF-FILE.
DIFF-PARAMS is a plist with :type ('commit, 'base-vs-head, 'branch-range),
:base-branch, :feature-branch, :diff-file-name-part, and optionally :branch-scope."
  (let* ((type (plist-get diff-params :type))
         (input-base-branch (plist-get diff-params :base-branch))
         (input-feature-branch (plist-get diff-params :feature-branch))
         (branch-scope (plist-get diff-params :branch-scope)) ; Might be nil for 'commit' or 'base-vs-head'
         (diff-file-name-part (plist-get diff-params :diff-file-name-part))

         (resolved-branches (aider--resolve-diff-branches type input-base-branch input-feature-branch branch-scope))
         (resolved-base-branch (car resolved-branches))
         (resolved-feature-branch (cdr resolved-branches)))

    (message "Fetching from all remotes to ensure latest branches...")
    (magit-run-git "fetch" "--all")
    ;; Verify input branches for relevant types
    (when (memq type '(base-vs-head branch-range))
      (aider--verify-branches input-base-branch input-feature-branch))
    ;; Display message about what we're doing
    (pcase type
      ('commit
       (message "Generating diff for single commit: %s" diff-file-name-part))
      ('base-vs-head
       (message "Generating diff between %s and HEAD" resolved-base-branch))
      ('branch-range
       (message "Generating diff between branches: %s..%s (%s)"
                resolved-base-branch resolved-feature-branch (or branch-scope "unknown-scope"))))
    (when (magit-anything-modified-p)
      (message "Repository has uncommitted changes. You might want to commit or stash them first.")
      (sleep-for 1))
    (message "Generating diff file: %s" diff-file)
    (magit-run-git "diff" (concat resolved-base-branch ".." resolved-feature-branch)
                   (concat "--output=" diff-file))))

(defun aider--open-diff-file (diff-file)
  "Open the generated DIFF-FILE."
  (find-file diff-file)
  (message "Generated diff file: %s" diff-file))

(defun aider--magit-generate-feature-branch-diff-file ()
  "Generate a diff file based on user-selected type (staged, branches, commit)."
  (interactive)
  (let* ((git-root (aider--validate-git-repository))
         (diff-type-alist '(("Staged changes" . staged)
                            ("Base branch vs HEAD" . base-vs-head)
                            ("Branch range (e.g., base..feature)" . branch-range)
                            ("Single commit" . commit)))
         (raw-diff-type-choice
          (completing-read "Select diff type: "
                           diff-type-alist
                           nil t nil nil "Staged changes"))
         (selected-diff-type-value
          (if (consp raw-diff-type-choice)
              (cdr raw-diff-type-choice)
            ;; If raw-diff-type-choice is a string, it should be one of the display strings.
            ;; We look up its corresponding value in the alist.
            (cdr (assoc raw-diff-type-choice diff-type-alist))))
         ;; Declare variables that will be set in pcase
         base-branch feature-branch commit-hash branch-scope ;; branch-scope: 'local or 'remote
         diff-file-name-part diff-params diff-file)

    (pcase selected-diff-type-value
      ('staged
       (setq diff-file-name-part "staged")
       (setq diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
       ;; No diff-params needed for aider--generate-staged-diff directly
       (aider--generate-staged-diff diff-file))
      ('base-vs-head
       (setq base-branch (read-string (format "Base branch name (default: %s): "
                                              (or (magit-get-default-remote-branch) "main"))
                                      nil nil (or (magit-get-default-remote-branch) "main")))
       (setq feature-branch "HEAD")
       (setq diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch) ".HEAD"))
       (setq diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
       (setq diff-params (list :type 'base-vs-head
                               :base-branch base-branch
                               :feature-branch feature-branch
                               :diff-file-name-part diff-file-name-part))
       (aider--generate-branch-or-commit-diff diff-params diff-file))
      ('branch-range
       (setq base-branch (read-string "Base branch name: "))
       (setq feature-branch (read-string "Feature branch name: "))
       (let ((scope-choice (completing-read "Are branches local or remote? "
                                            '(("Local" . local)
                                              ("Remote (will try to prefix with 'origin/' if needed)" . remote))
                                            nil t nil nil "Local")))
         (setq branch-scope (cdr scope-choice)))
       (setq diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch)
                                         "."
                                         (replace-regexp-in-string "/" "-" feature-branch)))
       (setq diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
       (setq diff-params (list :type 'branch-range
                               :base-branch base-branch
                               :feature-branch feature-branch
                               :branch-scope branch-scope
                               :diff-file-name-part diff-file-name-part))
       (aider--generate-branch-or-commit-diff diff-params diff-file))
      ('commit
       (setq commit-hash (read-string "Commit hash: "))
       (setq base-branch (concat commit-hash "^")) ; Diff against parent
       (setq feature-branch commit-hash)
       (setq diff-file-name-part commit-hash)
       (setq diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
       (setq diff-params (list :type 'commit
                               :base-branch base-branch
                               :feature-branch feature-branch
                               :diff-file-name-part diff-file-name-part))
       (aider--generate-branch-or-commit-diff diff-params diff-file)))
    ;; Open the diff file if it was generated (i.e., diff-file is non-nil)
    ;; For 'staged' case, diff-file is set directly. For others, it's set before calling generate.
    (when diff-file
      (aider--open-diff-file diff-file))))

;;;###autoload
(defun aider-open-history ()
  "Open the Aider history file (.aider.chat-history.md under repo git root).
If the history file does not exist, notify the user."
  (interactive)
  (let ((git-root (magit-toplevel)))
    (unless git-root
      (user-error "Not inside a git repository"))
    (let ((history-file (expand-file-name ".aider.chat.history.md" git-root)))
      (if (file-exists-p history-file)
          (find-file-other-window history-file)
        (message "History file does not exist: %s" history-file)))))

;;;###autoload
(defun aider-magit-blame-analyze ()
  "Analyze current file or region Git history with AI for deeper insights.
If region is active, analyze just that region. Otherwise analyze entire file.
Combines magit-blame history tracking with AI analysis to help understand
code evolution and the reasoning behind changes."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not associated with a file"))
  (let* ((file-path (buffer-file-name))
         (file-name (file-name-nondirectory file-path))
         (has-region (use-region-p))
         (line-start (if has-region
                         (line-number-at-pos (region-beginning))
                       1))
         (line-end (if has-region
                       (line-number-at-pos (region-end))
                     (line-number-at-pos (point-max))))
         (region-text (if has-region
                          (buffer-substring-no-properties 
                           (region-beginning) (region-end))
                        nil))
         (blame-args (list "blame" "-l" 
                           (format "-L%d,%d" line-start line-end)
                           file-path))
         (blame-output (with-temp-buffer
                         (apply #'process-file "git" nil t nil blame-args)
                         (buffer-string)))
         (context (format "File: %s\nLines: %d-%d\n\n" 
                          file-path line-start line-end))
         (code-sample (if has-region
                          (concat "Selected code:\n```\n" region-text "\n```\n\n")
                        ""))
         (default-analysis "Please provide the following analysis:\n1. Code evolution patterns and timeline\n2. Key changes and their purpose\n3. Potential design decisions and thought processes\n4. Possible refactoring or improvement opportunities\n5. Insights about code architecture or design")
         (analysis-instructions (aider-read-string "Analysis instructions: " default-analysis))
         (prompt (format "Analyze the Git commit history for this code:\n\n%s%sCommit history information:\n```\n%s\n```\n\n%s"
                         context code-sample blame-output analysis-instructions)))
    (aider-add-current-file)
    (aider--send-command (concat "/ask " prompt) t)
    (message "Press (S) to skip questions when it pop up")))

(provide 'aider-file)

;;; aider-file.el ends here
