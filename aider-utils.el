;;; aider-utils.el --- Utility functions for Aider -*- lexical-binding: t; -*-

;; Author: extracted from aider-core.el
;; SPDX-License-Identifier: Apache-2.0

(require 'magit)
(require 'subr-x)

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

(provide 'aider-utils)

;;; aider-utils.el ends here
