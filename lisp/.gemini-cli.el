;; for eat terminal backend:
;; (use-package eat :ensure t)
(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat"))

;; for vterm terminal backend:
;; (use-package vterm :ensure t)

;; for slash commands popup
(use-package popup :ensure t)
;; install gemini-cli.el
(use-package gemini-cli :ensure t
  :vc (:url "https://github.com/linchen2chris/gemini-cli.el" :rev :newest)
  :config (gemini-cli-mode)
  :bind-keymap ("C-c c" . gemini-cli-command-map)) ;; or your preferred key

(provide '.gemini-cli)
