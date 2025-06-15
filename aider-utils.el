;;; aider-utils.el --- Utility functions for Aider -*- lexical-binding: t; -*-

;; Author: extracted from aider-core.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Utility functions for the Aider package.

;;; Code:

(require 'magit)
(require 'subr-x)

(defvar aider-use-branch-specific-buffers)

;;; Git & path utilities

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

;;; History parsing

(defun aider--parse-aider-cli-history (file-path)
  "Parse .aider.input.history file at FILE-PATH.
Return a list of commands, oldest to newest."
  (when (and file-path (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((history-items '())
            (current-multi-line-command-parts nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (when (string-match "^\\+[ \t]*\\(.*\\)" line)
              (let ((content (match-string 1 line)))
                (cond
                 ((string= content "{aider")
                  (setq current-multi-line-command-parts (list content)))
                 ((string= content "aider}")
                  (if current-multi-line-command-parts
                      (progn
                        (setq current-multi-line-command-parts
                              (nconc current-multi-line-command-parts (list content)))
                        (push (string-join current-multi-line-command-parts "\n")
                              history-items)
                        (setq current-multi-line-command-parts nil))
                    (push content history-items)))
                 (current-multi-line-command-parts
                  (setq current-multi-line-command-parts
                        (nconc current-multi-line-command-parts (list content))))
                 (t
                  (push content history-items))))))
          (forward-line 1))
        (when current-multi-line-command-parts
          (push (string-join current-multi-line-command-parts "\n")
                history-items))
        (reverse history-items)))))

;;; Message formatting

(defun aider--process-message-if-multi-line (str)
  "Entering multi-line chat messages.
https://aider.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines, wrap it in {aider\\nstr\\naider}.
Otherwise return STR unchanged."
  (if (and (string-match-p "\n" str)
           (not (string-match-p "{aider" str)))
      (format "{aider\n%s\naider}" str)
    str))

;;; Validators

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

(defun aider--get-current-git-branch (repo-root-path)
  "Return current git branch name for git repository at REPO-ROOT-PATH.
Returns nil if REPO-ROOT-PATH is not a git repository or no branch
is checked out."
  ;; Ensure repo-root-path is a valid directory string before proceeding
  (when (and repo-root-path (stringp repo-root-path) (file-directory-p repo-root-path))
    (let ((default-directory repo-root-path)) ; Scope magit call to the repo root
      (magit-get-current-branch)))) ; magit-get-current-branch returns nil if no branch or not a repo

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

(defun aider--is-default-directory-git-root ()
  "Return t if `default-directory' is the root of a Git repository, nil otherwise."
  (let ((git-root-path (magit-toplevel)))
    (and git-root-path
         (stringp git-root-path)
         (not (string-match-p "fatal" git-root-path))
         ;; Compare canonical paths of default-directory and git-root-path
         (string= (file-truename (expand-file-name default-directory))
                  (file-truename git-root-path)))))

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

(provide 'aider-utils)

;;; aider-utils.el ends here
