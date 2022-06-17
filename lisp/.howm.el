(use-package howm
  :ensure t

  :custom
  (howm-directory
   (if (file-directory-p "~/OneDrive/howm")
       "~/OneDrive/howm/"
     (if (file-directory-p "~/GoogleDrive/howm")
	 "~/GoogleDrive/howm"
       "~/howm/")))

)

(provide '.howm)
