(use-package dired-subtree
  ;; https://github.com/Fuco1/dired-hacks/blob/master/README.md#dired-subtree
  :ensure t
  ;; :custom
  ;; (dired-subtree-use-background nil) ; not effect

  :bind
  (:map dired-mode-map
	("i" . dired-subtree-insert)
	("I" . dired-subtree-remove))

  :custom-face
  (dired-subtree-depth-1-face
   ((t (:background "snow"))))
  (dired-subtree-depth-2-face
   ((t (:background "snow"))))
  (dired-subtree-depth-3-face
   ((t (:background "snow"))))
  (dired-subtree-depth-4-face
   ((t (:background "snow"))))
  (dired-subtree-depth-5-face
   ((t (:background "snow"))))
  (dired-subtree-depth-6-face
   ((t (:background "snow"))))
  )
