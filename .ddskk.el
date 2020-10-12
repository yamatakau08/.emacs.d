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
  (skk-henkan-strict-okuri-precedence t)

  ;; emacs-jp
  ;; covert candidate will be showin in tooltip, not mini-buffer
  ;; ddskk use the low level x-show-tip instead of tooltip-show
  ;; (skk-show-tooltip t)
  ;; (skk-tooltip-parameters
  ;;  '((foreground-color . "navy blue")))

  ;; this is more advantage than the above settings
  (skk-show-inline 'vertical) ;;  available in terminal, no need to configuration for color
  )
