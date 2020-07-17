(defun my-nxml-mode-hook ()
  (hs-minor-mode 1)
  ;;(my-sgml-pretty-print) ; when open xml files, this function call sgml-pretty-print and add new line "</...>"
  )

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
