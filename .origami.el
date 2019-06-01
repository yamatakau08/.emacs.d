;; refer https://github.com/gregsexton/origami.el

;; for git clone
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)

;; check if ~/.emacs.d/origami/origami.el
;; if it does not exist, git clone from
;; https://github.com/gregsexton/origami.el.git
(let ((file (expand-file-name "~/.emacs.d/origami/origami.el")))
  (if (not (file-exists-p file))
      (my-git-source-get "https://github.com/gregsexton/origami.el.git" "~/.emacs.d/origami")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/origami"))
(require 'origami)
