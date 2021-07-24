;; for git clone
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)

;; check if ~/.emacs.d/instant-maximized-window/instant-maximized-window.el"
;; if it does not exist, git clone
(let ((file (expand-file-name "~/.emacs.d/instant-maximized-window/instant-maximized-window.el")))
  (if (not (file-exists-p file))
      (my-git-source-get "https://github.com/blue0513/instant-maximized-window.git")))

;; set for instant-maximized-window
(add-to-list 'load-path "~/.emacs.d/instant-maximized-window")

;;
(require 'instant-maximized-window)

;; M-x describe-bindings shows the global key mapping
;; (define-key global-map (kbd "C-i") 'window-temp-maximize) ; Since C-i is assigned TAB, comment this line.

(provide '.instant-maximized-window)



