; Since emacs 24.5.1 on Mac Japanese character is bloken
; need the following 
(set-language-environment "Japanese")

; font
;(add-to-list 'default-frame-alist '(font . "ricty-12"))
(add-to-list 'default-frame-alist '(font . "Menlo-16"))

;; refer https://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
;; fish doesn't work
;  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;; fish work
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (getenv "PATH"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; package
(load "~/.emacs.d/.package.el")

;; ddskk
(load "~/.emacs.d/.ddskk.el")

;; howm
(load "~/.emacs.d/.howm.el")

;; google-translate
(load "~/.emacs.d/.google-translate.el")

;; migemo
(load "~/.emacs.d/.migemo.el")

