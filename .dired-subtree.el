(use-package dired-subtree
  ;; https://github.com/Fuco1/dired-hacks/blob/master/README.md#dired-subtree
  :ensure t
  ;; :custom
  ;; (dired-subtree-use-background nil) ; not effect

  :bind
  (:map dired-mode-map ("i" . dired-subtree-toggle))

  :custom-face
  (dired-subtree-depth-1-face
   ((t (:background "snow"))))

  ;; (setq dired-subtree-depth-2-face
  ;; 	'((t (:background "#232a2b")))
  ;; 	"Background for depth 2 subtrees"
  ;; 	:group 'dired-subtree-faces)

  ;; (setq dired-subtree-depth-3-face
  ;; 	'((t (:background "#212627")))
  ;; 	"Background for depth 3 subtrees"
  ;; 	:group 'dired-subtree-faces)

  ;; (setq dired-subtree-depth-4-face
  ;; 	'((t (:background "#1e2223")))
  ;; 	"Background for depth 4 subtrees"
  ;; 	:group 'dired-subtree-faces)

  ;; (setq dired-subtree-depth-5-face
  ;; 	'((t (:background "#1c1d1e")))
  ;; 	"Background for depth 5 subtrees"
  ;; 	:group 'dired-subtree-faces)

  ;; (setq dired-subtree-depth-6-face
  ;; 	'((t (:background "#1a191a")))
  ;; 	"Background for depth 6 subtrees"
  ;; 	:group 'dired-subtree-faces)

  )
