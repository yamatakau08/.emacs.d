;;; 画面関連
;; color
; OK
;(setq initial-frame-alist ; 初期フレームだけに適用される
;      '((foreground-color . "black")
;	(background-color . "LemonChiffon")))
;

;(if (not (eq window-system 'w32))
;    (setq default-frame-alise '(background-color . "gray"))
;  (message "tako"))

(if (not (eq window-system 'nil))
    (setq default-frame-alist ; add-to-listで書き直す
	  '((foreground-color . "black")
	    (background-color . "LemonChiffon")
	    (border-color . "black")
	    (mouse-color  . "white")
	    (cursor-color . "black")
;; for ime-font
;;	(ime-font . "private-fontset-japanese-jisx208")
;;	(ime-font . "fixedsys-jisx0208")
;;	(ime-font . "ms-mincho-12-jisx0208")
;; for font after launch
;;	(font . "private-fontset"); TrueType
;;	(font . "fixedsys-fontset"); TrueType
;;	(font . "ms-mincho-12-fontset"); TrueType
;;	(font . "bdf-fontset")    ; BDF
;;	(font . "tt-fontset")    ; for Meadow2
	    (width . 80)
	    (height . 43)
	    (top  . 0)
	    (left . 100)
	    (alpha . 80) ; 透明度
	    )))

;; alpha
;; http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#alpha
;; カレントウィンドウの透明度を変更する (85%)
;; (set-frame-parameter nil 'alpha 0.85)
;; (set-frame-parameter nil 'alpha 50)
;; http://d.hatena.ne.jp/khiker/20090809/emacs_opacity
;; 関数で、alpha度合いを確認
(defun my-set-frame-alpha (alpha)
;  (let ((prompt (format "nalpha:%s" (frame-parameter (selected-frame) 'alpha)))
  (interactive "nalpha:")
  (let ((x (if (> alpha 100)
	       100
	     alpha)))
    (set-frame-parameter (selected-frame) 'alpha x)))
