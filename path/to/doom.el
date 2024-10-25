;; doom.el --- Keybindings for Aider using Doom Emacs conventions -*- lexical-binding: t; -*-

(map! :leader
      :prefix "a"
      :desc "Run Aider" "a" #'aider-run-aider
      :desc "Switch to Aider Buffer" "z" #'aider-switch-to-buffer
      :desc "Clear Aider" "l" #'aider-clear
      :desc "Reset Aider" "s" #'aider-reset
      :desc "Add Current File" "f" #'aider-add-current-file
      :desc "Add All Files in Current Window" "w" #'aider-add-files-in-current-window
      :desc "Batch Add Dired Marked Files" "b" #'aider-batch-add-dired-marked-files
      :desc "Find Files in the Git Repo" "F" #'aider-repo-find-name-dired
      :desc "Open Git Repo Root Dired" "R" #'aider-git-repo-root-dired
      :desc "Code Change" "c" #'aider-code-change
      :desc "Architect Discuss and Change" "t" #'aider-architect-discussion
      :desc "Refactor Code in Selected Region" "r" #'aider-region-refactor
      :desc "Show last commit with magit" "m" #'aider-magit-show-last-commit
      :desc "Undo Last Change" "u" #'aider-undo-last-change
      :desc "Ask Question" "q" #'aider-ask-question
      :desc "Explain Code in Selected Region" "e" #'aider-region-explain
      :desc "Debug Exception" "d" #'aider-debug-exception
      :desc "General Command" "g" #'aider-general-command
      :desc "Help" "h" #'aider-help)
