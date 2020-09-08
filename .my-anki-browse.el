(use-package my-anki-browse
  :load-path "~/.emacs.d/my-anki-browse"
  :after google-translate
  :custom (my-anki-browse-anki-main-deck-name "英語")
  :config
  (defun my-google-translate-register-item-in-anki ()
    "push note which have from and to word Basic note-type in Anki deck \
   specified gt-anki-push-deck variable through AnkiConnect"
    (interactive)
    (if (string= "*Google Translate*" (buffer-name))
	(let ((deck "英語") ; fixed
	      (front (read-string "Front: " (my-google-translate-register-item-read-front)))
	      (back  (read-string "Back : " (my-google-translate-register-item-read-back ))))
	  ;;(my-anki-connect-push-notex deck front back)
	  (my-anki-browse-addNote deck front back))))

  (defun google-translate-buffer-insert-translation-advice (&rest args)
    "advice to enable \"r\" key to register items in \"*Google Translate*\" buffer"
    (local-set-key "r" 'my-google-translate-register-item-in-anki))

  (advice-add 'google-translate-buffer-insert-translation :before
	      #'google-translate-buffer-insert-translation-advice)
  )

(use-package helm-anki-browse
  :load-path "~/.emacs.d/my-anki-browse"
  :after my-anki-browse)
