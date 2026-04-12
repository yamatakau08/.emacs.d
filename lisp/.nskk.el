(use-package nskk
  :unless (eq window-system 'w32)
  :vc (:url "https://github.com/takeokunn/nskk.el" :rev :newest)

  :init
  (setopt nskk-state-default-mode 'hiragana)
  (setopt nskk-use-kana-in-registration t)
  (setopt nskk-dict-system-dictionary-files
   '("~/.emacs.d/skk-jisyo/SKK-JISYO.L"))
  (setopt nskk-dict-user-dictionary-file "~/.emacs.d/skk-jisyo/.nskk-jisyo")

  :bind (("C-x C-j" . nskk-mode))

  )

(provide '.nskk)
