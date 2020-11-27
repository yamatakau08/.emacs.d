(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-listening-debug t) ; ?

  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")

  ;; window (buffer with translation) gets focus in google-translate-core-ui.el
  (google-translate-pop-up-buffer-set-focus t)

  :bind
  (;;("C-c t" . google-translate-at-point)
   ;;("C-c T" . google-translate-query-translate)
   ("C-c t" . my-google-translate-at-point)
   ("C-c T" . my-google-query-translate)
   ("C-c r" . google-translate-at-point-reverse)
   ("C-c R" . google-translate-query-translate-reverse))

  :config
  ;; defualt 'emacs, since error occurs ad-Advice-search-forward: Search failed: ",tkk:'"
  ;; https://github.com/atykhonov/google-translate/issues/137#issuecomment-723938431
  ;; workaround
  (defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))

  (defun my-google-translate-at-point ()
    (interactive)
    (if (string-match "\\cj" (thing-at-point 'word)) ; utilize '\\cj' is used in "sdic"
	(%google-translate-at-point nil t)
      (%google-translate-at-point nil nil)))

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
	  (thing-at-point 'sentence))))
  )

;; for debugging
;; google-translate somehow url-retrieve never returns on mingw64 emacs SELF Compiled
;; before that I used google translate on mingw64 emacs which gotten packman probabliy.
;; this is for arround advice url-retrieve-synchronously to debug, but url-retrieve never returns
;;(defun advice:around (origi-func &rest args)
;;  (message "    args:%s" args)
;;  (setcdr args '(nil))
;;  (message "new args:%s" args)
;;  (apply origi-func args))
;;(advice-add 'url-retrieve-synchronously :around 'advice:around)
