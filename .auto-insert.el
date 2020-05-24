;; http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/autoinsert.html
(add-hook 'find-file-hooks 'auto-insert)

(custom-set-variables '(auto-insert-directory "~/.emacs.d/insert/"))

(setq auto-insert-alist
      (append '(
                (org-mode  . "template.org")
                (ruby-mode . "template.rb")
                ) auto-insert-alist))
