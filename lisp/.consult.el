(use-package consult
  :ensure t

  :custom
  (consult-ripgrep-command
   "rg --ignore-case --null --line-buffered --color=ansi --max-columns=1000\
   --no-heading --line-number --hidden . -e ARG OPTS")

  :bind
  (("M-g g" . consult-goto-line)
   ("C-x b" . consult-buffer)
   ("C-s"   . consult-line)
   ("C-r"   . consult-line)
   )

  :config
  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

  (consult-customize
   ;;:preview-key '(:debounce 3 any) ; after 3s
   :preview-key nil)

  (defun consult-ripgrep1 (&optional dir initial)
    "Search for regexp with rg in the files in DIR with INITIAL input.
See `consult-grep' for more details."
    (interactive "P")
    ;;(consult--grep "Ripgrep" consult-ripgrep-command dir initial)
    (consult--grep "Ripgrep" consult-ripgrep-command dir "search_word -- --max-depth 1"))
  )

(provide '.consult)
