(use-package tabbar
  :ensure t
  :init (tabbar-mode 1)

  :custom
  (tabbar-use-images nil)
  (tabbar-buffer-groups-function nil) ; the variable defined defvar var custom-set-variable is available

  ;; :bind* (;;("C-." . tabbar-forward-tab) ; comment because C-. is assigned embark-act
  ;; 	  ;; When buffer is in org-mode, "C-," is assinged org-cycle-agenda-files
  ;; 	  ;; prior the key as in org-mode, not tabbar
  ;; 	  ;; should use :bind* not :bind
  ;;         ("C-," . tabbar-backward-tab))

  ;; changed to :bind from :bind* to test
  :bind (:map tabbar-mode-map
         ;; ("C-." . tabbar-forward-tab) ; comment because C-. is assigned embark-act
         ;; When buffer is in org-mode, "C-," is assinged org-cycle-agenda-files
	 ;; prior the key as in org-mode, not tabbar
	 ;; should use :bind* not :bind
	 ("C-," . tabbar-backward-tab))

  :config
  ;; http://hico-horiuchi.hateblo.jp/entry/20121208/1354975316
  (set-face-attribute
   'tabbar-default nil
   ;;:background "white"
   ;;:family "Inconsolata"
   :height 1.0)

  (set-face-attribute
   'tabbar-selected nil
   :background "white")

  (set-face-attribute
   'tabbar-unselected nil
   ;;:background "white"
   :foreground "black"
   :box nil)

  ;; redefine
  ;; Not to show *helm-mini* in tab bar after selecting the buffer in helm-mini
  ;; because that buffer is JUST buffer and helm-mini function is not available.
  (defun tabbar-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
    (let ((hide-buffers '("*helm mini*" "*Completions*" "*helm find files*" "*howm-keys:*")))
      (delq nil
            (mapcar #'(lambda (b)
			(cond
			 ;; Always include the current buffer.
			 ((eq (current-buffer) b) b)
			 ((buffer-file-name b) b)
			 ((char-equal ?\  (aref (buffer-name b) 0)) nil)
			 ((buffer-live-p b)
			  (if (member (buffer-name b) hide-buffers)
			      nil
			    b))))
                    (buffer-list)))))
  )
