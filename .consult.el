(use-package consult
  :ensure t

  :custom
  (consult-ripgrep-command
   "rg -i --null --line-buffered --color=ansi --max-columns=1000\
   --no-heading --line-number . -e ARG OPTS")

  :bind (("M-g g" . consult-goto-line)
	 ("C-x b" . consult-buffer)
	 ("C-s"   . consult-line)
	 ("C-r"   . consult-line))

  :config
  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

  (consult-customize
   ;;:preview-key '(:debounce 3 any) ; after 3s
   :preview-key nil
   )
  )
