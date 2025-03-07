;;; aider-helm.el --- Helm completion for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Package-Requires: ((emacs "26.1") (helm "3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for aider.el
;; To use this, ensure both aider.el and helm are installed.

;;; Code:

(require 'helm)
(require 'cl-lib)  ; For `cl-subseq`

(defun aider-helm-read-string-with-history (prompt history-file-name &optional initial-input)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string."
  ;; Load history from file
  (let* ((history-file (expand-file-name history-file-name user-emacs-directory))
         (history (when (file-exists-p history-file)
                    (with-temp-buffer
                      (insert-file-contents history-file)
                      (delete-dups (read (buffer-string))))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 history
                 :must-match nil
                 :name "Helm Read String"
                 :fuzzy t
                 :initial-input initial-input)))
    ;; Add to history if non-empty and save
    (unless (string-empty-p input)
      (push input history)
      (with-temp-file history-file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 10000))))  ; Keep last 10000 entries
          (insert (prin1-to-string history-entries)))))
    input))

(defun aider-helm-read-string (prompt &optional initial-input)
  "Read a string with Helm completion for aider, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string."
  (aider-helm-read-string-with-history prompt "aider-helm-read-string-history.el" initial-input))

(declare-function aider-read-string "aider")

;;;###autoload
(with-eval-after-load 'aider
  (if (featurep 'helm)
    (defalias 'aider-read-string 'aider-helm-read-string)
    (message "Helm is not available. Please install helm package to use aider-helm features")))

(provide 'aider-helm)
;;; aider-helm.el ends here
