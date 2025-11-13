(use-package google-translate
  :ensure t

  :demand t

  :custom
  (google-translate-default-source-language "auto") ; "auto" is available when target-language is specified
  (google-translate-default-target-language "ja")

  ;; window (buffer with translation) gets focus in google-translate-core-ui.el
  (google-translate-pop-up-buffer-set-focus t)

  :bind
  (;;("C-c t" . google-translate-at-point)
   ;;("C-c T" . google-translate-query-translate)
   ("C-c t" . my-google-translate)
   ("C-c T" . my-google-query-translate)
   ("C-c r" . google-translate-at-point-reverse)
   ("C-c R" . google-translate-query-translate-reverse)
   :map google-translate-mode-map
   ("r" . my-google-translate-register-item-in-anki))

  :config
  ;; defualt 'emacs, since error occurs ad-Advice-search-forward: Search failed: ",tkk:'"
  ;; https://github.com/atykhonov/google-translate/issues/137#issuecomment-723938431
  ;; workaround
  ;; I'm not sure if this workaround is permanently and finally merged the original source.
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (defun my-google-translate ()
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (thing-at-point 'word t))))
      (cond
       ((and text (string-match "\\cj" text))
	(google-translate-translate "auto" "en" text))
       (text
	(google-translate-translate "auto" "ja" text))
       (t
	(message "No text selected or found")))))

  (defun my-google-query-translate (from)
    (interactive "sFrom: ")
    (if (string-match "\\cj" from)
	(google-translate-translate "ja" "en" from)
      (google-translate-translate "auto" "ja" from)))

  (defun my-google-translate-get-source ()
    "Get source sentence from *Google Translate* buffer for registering anki card's front"
    (interactive)
    (if (string-equal (buffer-name) "*Google Translate*")
	(progn
	  (goto-line 3)
	  ;; use 'sentence to get line content without \n
	  (thing-at-point 'sentence t))))

  (defun my-google-translate-get-target ()
    "Get target sentence from *Google Translate* buffer for registering anki card's back"
    (interactive)
    (if (string-equal (buffer-name) "*Google Translate*")
	(progn
	  (goto-line 5)
	  ;; use 'sentence to get line content without \n
	  (thing-at-point 'sentence t))))
  )

(provide '.google-translate)
