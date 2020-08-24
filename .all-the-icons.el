(use-package all-the-icons
  :ensure t

  :config
  ;; https://github.com/domtronn/all-the-icons.el#slow-rendering
  ;; when set this, (insert (all-the-icons-icon-for-file "foo.js")) returns nil
  (setq inhibit-compacting-font-caches t))

