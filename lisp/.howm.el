(use-package howm
  :ensure t

  :custom
  (howm-directory
   (if (file-directory-p "~/gdrive/howm")
       "~/gdrive/howm"
       "~/howm/"))

)

(provide '.howm)
