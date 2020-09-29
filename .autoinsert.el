(use-package autoinsert
  :init
  (add-hook 'find-file-hooks 'auto-insert)

  :custom
  (auto-insert-directory "~/.emacs.d/insert/")

  :config
  (add-to-list 'auto-insert-alist '(org-mode    . "template.org"))
  (add-to-list 'auto-insert-alist '(ruby-mode   . "template.rb"))
  (add-to-list 'auto-insert-alist '("\\.puml\\" . "template.puml")))
