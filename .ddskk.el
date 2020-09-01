(use-package skk
  :ensure ddskk
  :bind  (("C-x C-j" . skk-mode)
	  ;;("C-x j"   . skk-auto-fill-mode)
	  ;;("C-x t"   . skk-tutorial)
	  )
  :custom
  ;;https://www.arat.xyz/wordpress/?p=129
  (skk-jisyo-code 'utf-8) ; refer skk-vars.el
  (skk-jisyo "~/.emacs.d/skk-jisyo/.skk-jisyo")
  (skk-henkan-strict-okuri-precedence t))
