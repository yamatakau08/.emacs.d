(use-package magit
  :ensure t

  :custom
  (magit-refresh-verbose t) ; put the process time in Message buffer.
  (magit-clone-set-remote.pushDefault t)

  :config
  ;; to disable auto fill mode in commit buffer, delete git-commit-turn-on-auto-fill
  (delq 'git-commit-turn-on-auto-fill git-commit-setup-hook)

  (defun magit-clone (repository directory)
    (interactive
     (let ((url (magit-read-string-ns "Clone repository" "git@github.com:Username/Repository.git")))
       (list url (read-directory-name
                  "Clone to: " nil nil nil
                  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
                       (match-string 1 url)))))
     (funcall 'magit-clone repository directory)))
  )
