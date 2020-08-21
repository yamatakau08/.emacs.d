(use-package skk
  :ensure ddskk
  :bind  (("C-x C-j" . skk-mode)
	  ;;("C-x j"   . skk-auto-fill-mode)
	  ;;("C-x t"   . skk-tutorial)
	  )
  :config
  ;;https://www.arat.xyz/wordpress/?p=129
  (custom-set-variables '(skk-jisyo-code 'utf-8)) ; refer skk-vars.el

  (custom-set-variables '(skk-jisyo "~/.emacs.d/skk-jisyo/.skk-jisyo"))
  (custom-set-variables '(skk-henkan-strict-okuri-precedence t))
  )
