(use-package all-the-icons
  :ensure t)

;; need to check
;(require 'font-lock)
;(require 'font-lock+)

;; https://github.com/domtronn/all-the-icons.el#slow-rendering
;; when set this, (insert (all-the-icons-icon-for-file "foo.js")) returns nil
(setq inhibit-compacting-font-caches t)
