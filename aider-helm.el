;;; aider-helm.el --- Helm completion for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for aider.el
;; To use this, ensure both aider.el and helm are installed.

;;; Code:

(require 'cl-lib)  ; For `cl-subseq`

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))

(defun aider-helm-read-string-with-history (prompt history-file-name &optional initial-input candidate-list)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  ;; Load history from file
  (let* ((history-file (expand-file-name history-file-name user-emacs-directory))
         (history (when (file-exists-p history-file)
                    (with-temp-buffer
                      (insert-file-contents history-file)
                      (delete-dups (read (buffer-string))))))
         ;; Combine candidate-list and history with a separator
         (candidates (if candidate-list
                         (append candidate-list 
                                 (when history 
                                   (cons "==================== HISTORY ========================================" history)))
                       history))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 candidates
                 :must-match nil
                 :name "Helm Read String, Use C-c C-y to edit selected command. C-b and C-f to move cursor during editing"
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

(defun aider-helm-read-string (prompt &optional initial-input candidate-list)
  "Read a string with Helm completion for aider, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  (aider-helm-read-string-with-history prompt "aider-helm-read-string-history.el" initial-input candidate-list))

(declare-function aider-read-string "aider")

;;;###autoload
(if (featurep 'helm)
    (defalias 'aider-read-string #'aider-helm-read-string)
  (message "Helm is not available. Please install helm package to use aider-helm features"))

(provide 'aider-helm)
;;; aider-helm.el ends here
