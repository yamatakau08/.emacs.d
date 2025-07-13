(when (eq system-type 'darwin)
  ;; On Mac with JIS keyboard setting
  ;; To change type ¥ -> '\'
  ;; Create the file ~/Library/KeyBindings/DefaultKeyBinding.dict
  ;; put the following the four lines
  ;; {
  ;;  "¥"   = ("insertText:", "\\");
  ;;  "~\\" = ("insertText:", "¥");
  ;; }
  ;;
  ;; option + ¥ -> '¥'
  ;; Since type option + ¥ on Emacs has the error "M-¥ is undefined"
  (global-set-key (kbd "M-¥")
		  (lambda () (interactive) (insert ?¥)))

  ;; to enable C-M-¥ (indent region)
  (global-set-key (kbd "C-M-¥") 'indent-region)
  )

(provide 'my-global-set-key)
