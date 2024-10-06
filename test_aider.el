
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
