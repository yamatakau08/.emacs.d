(use-package affe
  :ensure t

  :after orderless

  :custom
  (affe-grep-command
   "rg --ignore-case --null --color=never --max-columns=1000 --no-heading --line-number --hidden -v ^$ .")

  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  ;;(consult-customize affe-grep :preview-key (kbd "M-.")))

(provide '.affe)

