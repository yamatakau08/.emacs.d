(use-package magit
  :ensure t

  :custom
  (magit-refresh-verbose t) ; put out the process time in Message buffer.
  (magit-clone-set-remote.pushDefault t)

  :config
  ;; to disable auto fill mode in commit buffer, delete git-commit-turn-on-auto-fill
  (delq 'git-commit-turn-on-auto-fill git-commit-setup-hook)

  ;; On Windows, it's too slow to open the file.
  ;; because opening the file, run-hooks(change-major-mode-after-body-hook after-change-major-mode-hook) is executed.
  ;; on my environment
  ;; change-major-mode-after-body-hook nil.
  ;; after-change-major-mode-hook have magit-auto-revert-mode-enable-in-buffers and global-magit-file-mode-enable-in-buffers
  ;; Since I suppose these two functions are root causes, delete that from after-change-major-mode-hook
  ;; modify after-change-major-mode-hook without magit-auto-revert-mode-enable-in-buffers and global-magit-file-mode-enable-in-buffers
  ;; this workarround doesn't work well
  ;; (setq after-change-major-mode-hook (delq 'magit-auto-revert-mode-enable-in-buffers after-change-major-mode-hook))
  ;; (setq after-change-major-mode-hook (delq 'global-magit-file-mode-enable-in-buffers after-change-major-mode-hook))

  (defun magit-clone (repository directory)
    (interactive
     (let ((url (magit-read-string-ns "Clone repository" "git@github.com:Username/Repository.git")))
       (list url (read-directory-name
                  "Clone to: " nil nil nil
                  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
                       (match-string 1 url)))))
     (funcall 'magit-clone repository directory)))
  )
