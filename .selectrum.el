(use-package selectrum
  :ensure t

  :custom
  (selectrum-num-candidates-displayed 20)

  :config ; https://libraries.io/emacs/selectrum#getting-started
  (selectrum-mode +1)

  ;; with selectrum-prescient completion is better.
  ;; see .selectrum-prescient.el
  )
