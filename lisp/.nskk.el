(use-package nskk
  :vc (:url "https://github.com/takeokunn/nskk.el" :rev :newest)

  :bind (("C-x C-j" . nskk-mode))

  :custom
  (nskk-dict-user-dictionary-file "~/.emacs.d/skk-jisyo/.nskk-jisyo")

  )

(provide '.nskk)
