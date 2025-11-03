(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-listening-debug t) ; ?

  (google-translate-default-source-language "auto") ; "auto" is available when target-language is specified
  (google-translate-default-target-language "ja")

  ;; window (buffer with translation) gets focus in google-translate-core-ui.el
  (google-translate-pop-up-buffer-set-focus t)

  :bind
  (("C-c t" . google-translate-at-point)
   ("C-c T" . google-translate-query-translate)
   ;;("C-c t" . my-google-translate)
   ;;("C-c T" . my-google-query-translate)
   ("C-c r" . google-translate-at-point-reverse)
   ("C-c R" . google-translate-query-translate-reverse))

  :config
  ;; defualt 'emacs, since error occurs ad-Advice-search-forward: Search failed: ",tkk:'"
  ;; https://github.com/atykhonov/google-translate/issues/137#issuecomment-723938431
  ;; workaround
  ;; I'm not sure if this workaround is permanentaly and finally marged the original source.
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (defun my-google-translate ()
    (interactive)
    (let ((region (use-region-p))
	  (word (thing-at-point 'word)))
      (if region
	  (if (string-match "\\cj" (buffer-substring (region-beginning) (region-end)))
	      (google-translate-at-point-reverse)
	    (google-translate-at-point))
	(if word
	    (if (string-match "\\cj" word) ; utilize '\\cj' is used in "sdic"
		(%google-translate-at-point nil t)
	      (%google-translate-at-point nil nil))))))

  (defun my-google-query-translate (from)
    (interactive "sFrom: ")
    (if (string-match "\\cj" from)
	(google-translate-translate "ja" "en" from nil)
      (google-translate-translate "en" "ja" from nil)))

  (defun my-google-translate-register-item-read-front ()
    "Get sentence from *Google Translate* buffer for registering anki card's front"
    (interactive)
    (if (string-equal (buffer-name) "*Google Translate*")
	(progn
	  (goto-line 3)
	  ;; use 'sentence to get line content without \n
	  (thing-at-point 'sentence))))

  (defun my-google-translate-register-item-read-back ()
    "Get sentence from *Google Translate* buffer for registering anki card's back"
    (interactive)
    (if (string-equal (buffer-name) "*Google Translate*")
	(progn
	  (goto-line 5)
	  ;; use 'sentence to get line content without \n
	  (thing-at-point 'sentence t))))
  )

(provide '.google-translate)
