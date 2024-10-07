
(require 'ert)

(ert-deftest aider-buffer-name-from-git-repo-path-test ()
  "Test the aider-buffer-name-from-git-repo-path function."
  (should (equal (aider-buffer-name-from-git-repo-path "/Users/username/git/repo" "/Users/username")
                 "*aider:~/git/repo*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/home/username/git/repo" "/home/username")
                 "*aider:~/git/repo*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/Users/username/git/repo/subdir" "/Users/username")
                 "*aider:~/git/repo/subdir*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/home/username/git/repo/subdir" "/home/username")
                 "*aider:~/git/repo/subdir*")))

(ert-deftest test-aider-region-refactor-generate-command ()
  "Test the aider-region-refactor-generate-command function."
  (should (equal (aider-region-refactor-generate-command "some code"
                                                         "my-function" "refactor this")
                 "/architect \"in function my-function, for the following code block, refactor this: some code\"\n"))
  (should (equal (aider-region-refactor-generate-command "some code" nil
                                                         "make it more functional")
                 "/architect \"for the following code block, make it more functional: some code\"\n"))
  )
