(defun my-c-mode-common-hook ()
  ;; use Ellemtel style for all C like languages
;  (c-set-style "ellemtel")
  (c-set-style "k&r")
  ;; other customizations can go here
  (setq c-basic-offset 4)
; case �� label����Ƭ��switch�ˤ�����
; c-set-style �� "ellemtel" �λ��ˤ�ͭ���ˤ��롣 
;  (setq c-offsets-alist (append (list '(case-label . 0))
;				c-offsets-alist))
  (gtags-mode 1)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
