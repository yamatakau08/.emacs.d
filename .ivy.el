(use-package ivy
  :ensure t)

(ivy-mode 1)

;;; counsel-switch-buffer
;; assigne C-x b
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; list Bufers and Recentf
(setq ivy-use-virtual-buffers t)

;;; quit ivy buffer with Esc
;(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

;;; enable emacs edit in ivy buffer
;; C-x C-f 時に、ivy buffer で、ディレクトリ候補が1つに絞られたら、そのディレクトリ名を編集できる様になる
(setq enable-recursive-minibuffers t)


;;; for counsel-ag
;; C-M-n (key sequence Esc then Ctrl-n) displays the search pattern with the source in another buffer
;; but key stroke is not coveninent, change key assign to C-n
;; key stroke is defined in variable "ivy-minibuffer-map"
;(define-key ivy-minibuffer-map (kbd "C-M-n") 'ivy-next-line-and-call)
;(define-key ivy-minibuffer-map (kbd "C-M-p") 'ivy-previous-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line-and-call)

