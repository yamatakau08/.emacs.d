(use-package selectrum
  :ensure t

  :custom
  (selectrum-num-candidates-displayed 20)

  :config ; https://libraries.io/emacs/selectrum#getting-started
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  )
