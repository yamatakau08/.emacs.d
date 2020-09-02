;; Workaround on emacs 28.0.50
;; Error (use-package): google-translate/:catch: Cannot open load file: No such file or directory, popup
;; File is missing: Cannot open load file, No such file or directory, popup
(use-package popup
  :ensure t)

(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-listening-debug t)

  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")

  ;; window (buffer with translation) gets focus in google-translate-core-ui.el
  (google-translate-pop-up-buffer-set-focus t)

  :bind
  (;("C-c t" . google-translate-at-point)
   ;("C-c T" . google-translate-query-translate)
   ("C-c t" . my-google-translate-at-point)
   ("C-c T" . my-google-query-translate)
   ("C-c r" . google-translate-at-point-reverse)
   ("C-c R" .'google-translate-query-translate-reverse))
  )

;;; for registering word in Anki
;;(add-to-list 'load-path "~/.emacs.d/my-anki-connect")
;;(require 'my-anki-connect)

(add-to-list 'load-path "~/.emacs.d/my-anki-browse")
(require 'my-anki-browse)

(require 'google-translate-default-ui) ; need for my-google-translate-at-point

;;;
;;; my original function is enable not to care if the word is English or Japanese
;;;
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

;;; advice to enable "r" key to register items in "*Google Translate*" buffer
(defun google-translate-buffer-insert-translation-advice (&rest args)
  (local-set-key "r" 'my-google-translate-register-item-in-anki))

(advice-add 'google-translate-buffer-insert-translation :before
	    #'google-translate-buffer-insert-translation-advice)

;;; move point for front in *Google Translate* buffer
(defun my-google-translate-register-item-read-front ()
  (interactive)
  (goto-line 3)
  ;; use 'sentence to get line content without \n
  (thing-at-point 'sentence))

;;; move point for back in *Google Translate* buffer
(defun my-google-translate-register-item-read-back ()
  (interactive)
  (goto-line 5)
  ;; use 'sentence to get line content without \n
  (thing-at-point 'sentence))

;;; for debugging
;;; from that moment google-translate somehow url-retrieve never returns on mingw64 emacs self compiled that
;;; before that can use google translate.
;;; this is for arround advice url-retrieve-synchronously to debug, but url-retrieve never returns
;;(defun advice:around (origi-func &rest args)
;;  (message "    args:%s" args)
;;  (setcdr args '(nil))
;;  (message "new args:%s" args)
;;  (apply origi-func args))
;;(advice-add 'url-retrieve-synchronously :around 'advice:around)

;;; to put out URL-DEBUG
;;(setq url-debug t)
