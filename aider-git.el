;;; aider-git.el --- Git operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides git operation functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'magit)

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

(defun aider--handle-staged-diff-generation (git-root)
  "Handle generation of diff for staged changes."
  (let* ((diff-file-name-part "staged")
         (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root)))
    (aider--generate-staged-diff diff-file)
    diff-file))

(defun aider--handle-base-vs-head-diff-generation (git-root)
  "Handle generation of diff between a base branch and HEAD."
  (let* ((base-branch (read-string "Base branch name: " nil nil nil))
         (feature-branch "HEAD")
         (diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch) ".HEAD"))
         (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
         (diff-params (list :type 'base-vs-head
                             :base-branch base-branch
                             :feature-branch feature-branch
                             :diff-file-name-part diff-file-name-part)))
    (aider--generate-branch-or-commit-diff diff-params diff-file)
    diff-file))

(defun aider--handle-branch-range-diff-generation (git-root)
  "Handle generation of diff between a base branch and a feature branch."
  (let* ((base-branch (read-string "Base branch name: "))
         (feature-branch (read-string "Feature branch name: "))
         (branch-scope)
         (scope-alist '(("Local" . local)
                        ("Remote (will try to prefix with 'origin/' if needed)" . remote)))
         (raw-scope-choice (completing-read "Are branches local or remote? "
                                            scope-alist
                                            nil t nil nil "Local")))
    (setq branch-scope
          (if (consp raw-scope-choice)
              (cdr raw-scope-choice)
            (cdr (assoc raw-scope-choice scope-alist))))
    (let* ((diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch)
                                       "."
                                       (replace-regexp-in-string "/" "-" feature-branch)))
           (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
           (diff-params (list :type 'branch-range
                               :base-branch base-branch
                               :feature-branch feature-branch
                               :branch-scope branch-scope
                               :diff-file-name-part diff-file-name-part)))
      (aider--generate-branch-or-commit-diff diff-params diff-file)
      diff-file)))

(defun aider--handle-commit-diff-generation (git-root)
  "Handle generation of diff for a single commit."
  (let* ((commit-hash (read-string "Commit hash: "))
         (base-branch (concat commit-hash "^")) ; Diff against parent
         (feature-branch commit-hash)
         (diff-file-name-part commit-hash)
         (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
         (diff-params (list :type 'commit
                             :base-branch base-branch
                             :feature-branch feature-branch
                             :diff-file-name-part diff-file-name-part)))
    (aider--generate-branch-or-commit-diff diff-params diff-file)
    diff-file))

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
         ;; Variable to hold the path of the generated diff file
         (diff-file))
    (setq diff-file
          (pcase selected-diff-type-value
            ('staged       (aider--handle-staged-diff-generation git-root))
            ('base-vs-head (aider--handle-base-vs-head-diff-generation git-root))
            ('branch-range (aider--handle-branch-range-diff-generation git-root))
            ('commit       (aider--handle-commit-diff-generation git-root))
            (_ (user-error "Invalid diff type selected"))))

    (when diff-file
      (aider--open-diff-file diff-file))))

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

;;;###autoload
(defun aider-magit-log-analyze ()
  "Analyze Git log with AI.
If current buffer is visiting a file named 'git.log', analyze its content.
Otherwise, prompt for number of commits (default 100), generate the log,
save it to 'PROJECT_ROOT/git.log', open this file, and then analyze its content."
  (interactive)
  (let* ((git-root (aider--validate-git-repository)) ; Ensure we're in a git repo
         (repo-name (file-name-nondirectory (directory-file-name git-root)))
         (log-output)
         ;; Define the expected path for git.log at the project root
         (project-log-file-path (expand-file-name "git.log" git-root)))
    (if (not (and buffer-file-name
                  (string-equal (file-name-nondirectory buffer-file-name) "git.log")
                  ;; Optional: more strictly check if it's the project's git.log
                  ;; (string-equal (file-truename buffer-file-name) (file-truename project-log-file-path))
                  ;; For simplicity, we'll stick to checking just the filename "git.log"
                  ;; This means any file named "git.log" will be used if currently open.
                  ))
        ;; Not a git.log file, or no file associated with buffer, or not the project's git.log
        (let* ((num-commits-str (read-string (format "Number of commits to fetch for %s (default 100): " repo-name) "100"))
               (num-commits (if (string-empty-p num-commits-str) "100" num-commits-str)))
          (message "Fetching Git log for %s (%s commits with stats)... This might take a moment." repo-name num-commits)
          (setq log-output (magit-git-output "log" "--pretty=medium" "--stat" "-n" num-commits))
          (message "Saving Git log to %s" project-log-file-path)
          (with-temp-file project-log-file-path
            (insert log-output))
          (find-file project-log-file-path) ; Open the generated/updated git.log file
          (message "Git log saved to %s and opened. Proceeding with analysis." project-log-file-path)))
    ;; Common analysis part, using the determined log-output
    (let* ((context (format "Repository: %s\n\n" repo-name))
           (default-analysis
            (concat "Please analyze the following Git log for the entire repository. Provide insights on:\n"
                    "1. Overall project evolution and major development phases.\n"
                    "2. Identification of key features, refactorings, or architectural changes and their timeline.\n"
                    "3. Patterns in development activity (e.g., periods of rapid development, bug fixing, etc.).\n"
                    "4. Significant contributors or shifts in contribution patterns (if discernible from commit messages).\n"
                    "5. Potential areas of technical debt or architectural concerns suggested by the commit history.\n"
                    "6. General trends in the project's direction or focus over time."))
           (analysis-instructions (aider-read-string "Analysis instructions for repository log: " default-analysis))
           ;; Changed prompt to refer to "Git Log content" generically
           (prompt (format "Analyze the Git commit history for the entire repository '%s'.\n\n%sThe detailed Git log content is in the 'git.log' file (which has been added to the chat).\nPlease use its content for your analysis, following these instructions:\n%s"
                           repo-name context analysis-instructions)))
      (aider-add-current-file) ;; git.log
      (aider--send-command (concat "/ask " prompt) t)
      (message "AI analysis of repository log initiated. Press (S) to skip questions if prompted by Aider."))))

;; add a function aider-magit-blame-or-log-analyze, when C-u pressed,
;; trigger aider-magit-log-analyze, otherwise, trigger aider-magit-blame-analyze

(provide 'aider-git)

;;; aider-git.el ends here
