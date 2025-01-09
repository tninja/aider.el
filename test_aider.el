(require 'ert)

(ert-deftest aider-buffer-name-test ()
  "Test the aider-buffer-name function."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "/path/to/git/repo")))
    (should (equal (aider-buffer-name) "*aider:/path/to/git/repo*")))
  
  ;; Test error case when not in a git repo
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "fatal: not a git repository")))
    (should-error (aider-buffer-name) :type 'error)))

(ert-deftest aider--process-message-if-multi-line-test ()
  "Test the aider--process-message-if-multi-line function."
  ;; 测试单行文本
  (should (equal (aider--process-message-if-multi-line "single line")
                 "single line"))
  ;; 测试多行文本
  (should (equal (aider--process-message-if-multi-line "line 1\nline 2")
                 "{aider\nline 1\nline 2\naider}"))
  ;; 测试空字符串
  (should (equal (aider--process-message-if-multi-line "")
                 ""))
  ;; 测试只有换行符的情况
  (should (equal (aider--process-message-if-multi-line "\n")
                 "{aider\n\n\naider}")))

(ert-deftest aider--get-add-command-prefix-test ()
  "Test the aider--get-add-command-prefix function."
  ;; 测试 read-only 模式关闭时
  (let ((aider--add-file-read-only nil))
    (should (equal (aider--get-add-command-prefix) "/add")))
  ;; 测试 read-only 模式开启时
  (let ((aider--add-file-read-only t))
    (should (equal (aider--get-add-command-prefix) "/read-only"))))

(ert-deftest aider-region-refactor-generate-command-test ()
  "Test the aider-region-refactor-generate-command function."
  ;; 测试有函数名的情况
  (should (equal (aider-region-refactor-generate-command 
                 "code block" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: code block\"\n"))
  
  ;; 测试没有函数名的情况
  (should (equal (aider-region-refactor-generate-command 
                 "code block" 
                 nil 
                 "make it better")
                 "/architect \"for the following code block, make it better: code block\"\n"))
  
  ;; 测试多行代码块
  (should (equal (aider-region-refactor-generate-command 
                 "line 1\nline 2" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: line 1\nline 2\"\n"))
  
  ;; 测试空代码块
  (should (equal (aider-region-refactor-generate-command 
                 "" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: \"\n")))

(ert-deftest aider-font-lock-keywords-test ()
  "Test the aider-font-lock-keywords variable."
  ;; 确保 font-lock-keywords 包含正确的模式和face
  (should (equal (car (car aider-font-lock-keywords))
                 "^\x2500+\n?"))
  (should (equal (nth 2 (car aider-font-lock-keywords))
                 '(face aider-command-separator)))
  (should (equal (car (cadr aider-font-lock-keywords))
                 "^\x2500+"))
  (should (equal (nth 2 (cadr aider-font-lock-keywords))
                 '(face nil display (space :width 2)))))

(ert-deftest aider-faces-test ()
  "Test that aider faces are properly defined."
  ;; 测试 aider-command-separator face
  (should (face-differs-from-default-p 'aider-command-separator))
  ;; 测试 aider-command-text face
  (should (face-differs-from-default-p 'aider-command-text)))

;; 辅助函数，用于测试 face 是否与默认 face 不同
(defun face-differs-from-default-p (face)
  "Check if FACE has different properties from default face."
  (let ((face-props (face-all-attributes face nil))
        (default-props (face-all-attributes 'default nil)))
    (not (equal face-props default-props))))
