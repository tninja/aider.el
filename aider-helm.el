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
         ;; Extract the most recent item from history (if exists)
         (most-recent (when (and history (not (null history)))
                        (car history)))
         ;; Remove the first item to add it back later
         (rest-history (when (and history (not (null history)))
                         (cdr history)))
         ;; Combine completion list: most recent + candidates + separator + rest of history
         (completion-list
          (append
           ;; If most recent item exists, put it at the top
           (when most-recent
             (list most-recent))
           ;; Add candidate list
           (or candidate-list '())
           ;; Add separator and rest of history
           (when rest-history
             (cons "==================== HISTORY ========================================" rest-history))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 completion-list
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
    (defalias 'aider-read-string #'aider-helm-read-string))

(provide 'aider-helm)
;;; aider-helm.el ends here
