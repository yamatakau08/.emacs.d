(use-package howm
  :ensure t

  :custom
  (howm-directory
   (if (file-directory-p "~/OneDrive/howm")
       "~/OneDrive/howm/"
     "~/howm/"))

)

(provide '.howm)
