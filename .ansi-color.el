(defun my-ansi-color-apply ()
  "to clor ansi escape sequence in buffer
supported by emacs-jp slack"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
