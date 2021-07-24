(use-package ansi-color
  :config
  (defun my-ansi-color-apply ()
    "to color ansi escape sequence in buffer supported by emacs-jp slack"
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  )

(provide '.ansi-color)
