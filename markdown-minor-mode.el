;;; markdown-minor-mode.el --- Minor mode for Markdown syntax highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.5"))
;; Keywords: markdown, languages
;; URL: https://github.com/yourusername/markdown-minor-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for Markdown syntax highlighting
;; that can be used with any major mode.  It leverages the syntax
;; highlighting capabilities of `markdown-mode' but applies them as a
;; minor mode.

;;; Code:

(require 'markdown-mode)

(defvar markdown-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `markdown-minor-mode'.")

;; Store original values
(defvar-local markdown-minor-mode--original-font-lock-keywords nil
  "Original font-lock-keywords before enabling markdown-minor-mode.")

(defvar-local markdown-minor-mode--original-font-lock-defaults nil
  "Original font-lock-defaults before enabling markdown-minor-mode.")

(defvar-local markdown-minor-mode--original-syntax-propertize-function nil
  "Original syntax-propertize-function before enabling markdown-minor-mode.")

;;;###autoload
(define-minor-mode markdown-minor-mode
  "Minor mode for editing Markdown files.
This minor mode provides Markdown syntax highlighting for any major mode."
  :lighter " md"
  :keymap markdown-minor-mode-map
  (if markdown-minor-mode
      (markdown-minor-mode-enable)
    (markdown-minor-mode-disable)))

(defun markdown-minor-mode--initialize-markdown-variables ()
  "Initialize necessary variables from markdown-mode."
  ;; Set essential markdown variables
  (setq-local markdown-mode-font-lock-keywords
              (markdown-get-mode-font-lock-keywords))
  
  ;; Initialize markdown syntax table if needed
  (unless (boundp 'markdown-mode-syntax-table)
    (with-temp-buffer
      (markdown-mode)
      (setq-local markdown-mode-syntax-table (syntax-table))))
  
  ;; Initialize other important markdown variables
  (setq-local markdown-list-indent-width 4)
  (setq-local markdown-code-lang-modes
              (if (boundp 'markdown-code-lang-modes)
                  markdown-code-lang-modes
                '(("cpp" . c++-mode)
                  ("c" . c-mode)
                  ("python" . python-mode)
                  ("js" . js-mode)
                  ("javascript" . js-mode)
                  ("java" . java-mode)
                  ("ruby" . ruby-mode)
                  ("html" . html-mode)
                  ("xml" . xml-mode)
                  ("css" . css-mode)
                  ("shell" . sh-mode)
                  ("bash" . sh-mode))))
  
  ;; Set up markdown regex patterns if not already defined
  (unless (boundp 'markdown-regex-italic)
    (markdown-setup-font-lock)))

(defun markdown-minor-mode-enable ()
  "Enable Markdown minor mode."
  ;; Save original font-lock settings
  (setq markdown-minor-mode--original-font-lock-keywords font-lock-keywords
        markdown-minor-mode--original-font-lock-defaults font-lock-defaults
        markdown-minor-mode--original-syntax-propertize-function syntax-propertize-function)
  
  ;; Initialize necessary markdown-mode variables
  (markdown-minor-mode--initialize-markdown-variables)
  
  ;; Apply markdown font-lock settings
  (setq font-lock-defaults
        '(markdown-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . markdown-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  
  ;; Set up syntax properties
  (setq-local syntax-propertize-function #'markdown-syntax-propertize)
  
  ;; Add hooks for syntax highlighting
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region nil t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  
  ;; Apply syntax properties to the buffer
  (syntax-propertize (point-max))
  
  ;; Force font-lock refresh
  (font-lock-flush))

(defun markdown-minor-mode-disable ()
  "Disable Markdown minor mode."
  ;; Restore original font-lock settings
  (setq font-lock-keywords markdown-minor-mode--original-font-lock-keywords
        font-lock-defaults markdown-minor-mode--original-font-lock-defaults
        syntax-propertize-function markdown-minor-mode--original-syntax-propertize-function)
  
  ;; Remove hooks
  (remove-hook 'syntax-propertize-extend-region-functions
               #'markdown-syntax-propertize-extend-region t)
  (remove-hook 'jit-lock-after-change-extend-region-functions
               #'markdown-font-lock-extend-region-function t)
  
  ;; Force font-lock refresh
  (font-lock-flush))

(provide 'markdown-minor-mode)
;;; markdown-minor-mode.el ends here
