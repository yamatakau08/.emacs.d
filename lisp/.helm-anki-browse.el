(use-package helm-anki-browse
  :load-path "my-anki-browse"
  :after my-anki-browse

  :config
  (defun helm-anki-browse-english ()
    (interactive)
    (helm-anki-browse "英語"))
  )

(provide '.helm-anki-browse)

