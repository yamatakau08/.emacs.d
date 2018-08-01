;; following code doesn't work
(defadvice magit-clone (around magit-clone-repository-template-windows)
  "set the \"git@github:Username/Repository_name.git\" in prompt"
  (let ((ad-set-arg 0 "git@github:Username/Repository_name.git"))
    ad-do-it))
(ad-activate 'magit-clone)
