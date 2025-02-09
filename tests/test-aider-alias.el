;;; tests/test-aider-alias.el --- Tests for aider alias
;;
;; Author: Your Name
;; Version: 0.1
;; License: MIT
;;
;;; Commentary:
;; This file contains automated tests using ERT for the aider alias.
;; It verifies that `aider-read-string` is correctly aliased to
;; `aider-plain-read-string` in both interpreted and compiled environments.
;;
;;; Code:

(require 'ert)
(require 'aider)

(ert-deftest aider-read-string-alias-test ()
  "Test that `aider-read-string` is aliased to `aider-plain-read-string`."
  (should (eq (symbol-function 'aider-read-string)
              (symbol-function 'aider-plain-read-string))))

(provide 'test-aider-alias)

;;; tests/test-aider-alias.el ends here
