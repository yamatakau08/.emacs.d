;; http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/autoinsert.html
(use-package autoinsert
  :init
  (add-hook 'find-file-hooks 'auto-insert)

  :config
  (custom-set-variables '(auto-insert-directory "~/.emacs.d/insert/"))

  (add-to-list 'auto-insert-alist '(org-mode  . "template.org"))
  (add-to-list 'auto-insert-alist '(ruby-mode . "template.rb"))
  (add-to-list 'auto-insert-alist '("\\.puml" . "template.puml")))
