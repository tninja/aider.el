;;; aider-highlight-changes.el --- Highlight SEARCH/REPLACE blocks -*- lexical-binding: t; -*-

;; Author: Vagn Johansen <gonz808@hotmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; This file provides support for SEARCH/REPLACE blocks in Aider buffers.
;;
;; Features:
;;
;; 1. Visual Highlighting:
;;    - When the point is inside a SEARCH/REPLACE block, the differences between the
;;      SEARCH and REPLACE sections are highlighted using overlays styled similarly
;;      to smerge-mode. This highlighting is updated lazily via an idle timer.
;;
;; 2. Block Comparison: C-c d
;;    - The command `aider-compare-search-replace-blocks` allows users to compare
;;      the SEARCH and REPLACE sections of the block at point. It opens a new buffer
;;      showing a unified diff (requires the external `diff` program).
;;

;;; Code:
(require 'smerge-mode)


(defvar aider-search-block-limit 20000
  "Maximum number of characters to look back when searching for the start of a SEARCH/REPLACE block.")

(defvar aider--last-highlight-state nil
  "Previous highlighting state to avoid unnecessary overlay clearing.")

(defvar aider--highlight-timer nil
  "Global timer for SEARCH/REPLACE block highlighting.")

(defun aider--ensure-highlight-timer ()
  "Ensure highlight timer is running, create if needed."
  (unless aider--highlight-timer
    (setq aider--highlight-timer
          (run-with-idle-timer 0.2 t #'aider--idle-timer-change-block-highlight))))

(defun aider--clear-diff-overlays ()
  "Remove all diff highlighting overlays."
  (let ((start (max (point-min) (- (point) aider-search-block-limit)))
        (end (min (point-max) (+ (point) aider-search-block-limit))))
    (dolist (overlay (overlays-in start end))
      (let ((face (overlay-get overlay 'face)))
        (when (or (memq face '(smerge-refined-removed smerge-refined-added))
                  (and (listp face)
                       (or (memq 'smerge-refined-removed face)
                           (memq 'smerge-refined-added face))))
          (delete-overlay overlay))))))

(defun aider--find-search-replace-block-at-point (point)
  "Find SEARCH/REPLACE block boundaries at POINT.
Returns (block-start search-start search-end replace-start replace-end block-end) or nil."
  (save-excursion
    (goto-char point)
    (let ((block-start nil) (search-start nil) (search-end nil)
          (replace-start nil) (replace-end nil) (block-end nil)
          (search-limit (max (point-min) (- point aider-search-block-limit))))
      (when (re-search-backward "^<<<<<<< SEARCH" search-limit t)
        (setq block-start (match-beginning 0))
        (setq search-start (match-end 0))
        (when (re-search-forward "^=======" nil t)
          (setq search-end (match-beginning 0))
          (setq replace-start (match-end 0))
          (when (re-search-forward "^>>>>>>> REPLACE" nil t)
            (setq replace-end (match-beginning 0))
            (setq block-end (match-end 0))
            (when (and (>= point block-start) (<= point block-end))
              (list block-start search-start search-end replace-start replace-end block-end))))))))

(defun aider--find-conflict-at-point (point)
  "Find SEARCH/REPLACE conflict at POINT, return (search-start search-end replace-start replace-end)."
  (let ((block-data (aider--find-search-replace-block-at-point point)))
    (when block-data
      (list (nth 1 block-data) (nth 2 block-data) (nth 3 block-data) (nth 4 block-data)))))

(defun aider--smerge-refine-conflict (conflict-data)
  "Highlight SEARCH/REPLACE conflict regions using smerge-mode style overlays."
  (aider--clear-diff-overlays)
  (when conflict-data
    (condition-case err
        (let ((search-start (nth 0 conflict-data))
              (search-end (nth 1 conflict-data))
              (replace-start (nth 2 conflict-data))
              (replace-end (nth 3 conflict-data)))
          (smerge-refine-regions search-start search-end replace-start replace-end
                                nil  ; PROPS-C (changed chars)
                                nil  ; PREPROC
                                '((face . smerge-refined-removed) (priority . 1000))    ; PROPS-R (removed)
                                '((face . smerge-refined-added) (priority . 1000))))    ; PROPS-A (added)
      (error (message "Error highlighting differences: %s" (error-message-string err))))))


(defun aider--extract-search-replace-blocks ()
  "Extract SEARCH/REPLACE block content if point is inside a block.
Returns a (search-text replace-text) pair if point is within
a SEARCH/REPLACE block or nil."
  (let ((block-data (aider--find-search-replace-block-at-point (point))))
    (when block-data
      (let ((search-start (nth 1 block-data))
            (search-end (nth 2 block-data))
            (replace-start (nth 3 block-data))
            (replace-end (nth 4 block-data)))
        (let ((search-text (buffer-substring-no-properties search-start search-end))
              (replace-text (buffer-substring-no-properties replace-start replace-end)))
          (list search-text replace-text))))))


;; Add idle timer after comint mode is created
;; NOTE: This function is called frequently!
(defun aider--idle-timer-change-block-highlight ()
  "Highlight differences between SEARCH and REPLACE blocks using smerge-mode highlighting."
  (when (eq major-mode 'aider-comint-mode)
    (let ((current-point (point)))
      (save-excursion
        (let* ((conflict-data (aider--find-conflict-at-point current-point))
               (inside-block (and conflict-data
                                  (let ((face (get-text-property current-point 'face)))
                                    (or (eq face 'markdown-code-face)
                                        (and (listp face) (memq 'markdown-code-face face)))))))
          (if inside-block
              ;; Inside a block - use smerge-style highlighting
              (progn
                (aider--smerge-refine-conflict conflict-data)
                (setq aider--last-highlight-state t))
            ;; Not inside a block - only clear if we were previously highlighting
            (when aider--last-highlight-state
              (aider--clear-diff-overlays)
              (setq aider--last-highlight-state nil))))))))


;;;###autoload
(defun aider-compare-search-replace-blocks ()
  "Compare SEARCH and REPLACE blocks using diff program.
Creates two temporary files with the content and calls diff to show differences."
  (interactive)
  (let ((block (aider--extract-search-replace-blocks)))
    (if (null block)
        (message "Point is not inside a SEARCH/REPLACE block")
      (let* ((search-text (car block))
             (replace-text (cadr block))
             (search-file (make-temp-file "aider-search-" nil ".txt"))
             (replace-file (make-temp-file "aider-replace-" nil ".txt"))
             (diff-buffer (generate-new-buffer "*aider-diff*")))
        (if (not (executable-find "diff"))
            (message "The 'diff' executable was not found in your PATH.")
          (with-temp-file search-file
            (insert search-text))
          (with-temp-file replace-file
            (insert replace-text))

          ;; Run diff and display results
          (with-current-buffer diff-buffer
            (let ((exit-code (call-process "diff" nil t nil "-u" search-file replace-file)))
              (if (= exit-code 0)
                  (insert "No differences found between SEARCH and REPLACE blocks.")
                (progn
                  (goto-char (point-min))
                  (diff-mode)
                  (view-mode 1))))

            ;; Display the diff buffer
            (pop-to-buffer diff-buffer)

            ;; Clean up temporary files
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when (file-exists-p search-file)
                          (delete-file search-file))
                        (when (file-exists-p replace-file)
                          (delete-file replace-file)))
                      nil t)))))))


(provide 'aider-highlight-changes)
