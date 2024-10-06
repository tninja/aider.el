
(require 'ert)

(ert-deftest aider-buffer-name-from-git-repo-path-test ()
  "Test the aider-buffer-name-from-git-repo-path function."
  (should (equal (aider-buffer-name-from-git-repo-path "/Users/username/git/repo")
                 "*aider:~git/repo*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/home/username/git/repo")
                 "*aider:~git/repo*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/Users/username/git/repo/subdir")
                 "*aider:~git/repo/subdir*"))
  (should (equal (aider-buffer-name-from-git-repo-path "/home/username/git/repo/subdir")
                 "*aider:~git/repo/subdir*")))
