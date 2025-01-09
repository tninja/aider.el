
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

(ert-deftest test-aider-region-refactor-generate-command ()
  "Test the aider-region-refactor-generate-command function."
  (should (equal (aider-region-refactor-generate-command "some code"
                                                         "my-function" "refactor this")
                 "/architect \"in function my-function, for the following code block, refactor this: some code\"\n"))
  (should (equal (aider-region-refactor-generate-command "some code" nil
                                                         "make it more functional")
                 "/architect \"for the following code block, make it more functional: some code\"\n"))
  )
