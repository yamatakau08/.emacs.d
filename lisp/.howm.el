(use-package howm
  :ensure t

  :custom
  (howm-directory
   (seq-some (lambda (dir) (if (file-directory-p dir) dir))
          '("~/gdrive/howm" "~/gdrive/Obsidian" "~/howm")))

)

(provide '.howm)
