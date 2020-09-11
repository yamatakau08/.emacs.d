(use-package frame
  :if window-system
  :custom
  (default-frame-alist
    '((foreground-color . "black")
      (background-color . "LemonChiffon")
      (border-color . "black")
      (mouse-color  . "white")
      (cursor-color . "black")
      (width . 80)
      (height . 45)
      (top . 0)
      (left . 100)
      (alpha . 80)
      ;; font
      ;;(font . "Myrica M-10") ; set by :config section
      ;; ime font
      ;; (ime-font . "private-fontset-japanese-jisx208")
      ;; (ime-font . "fixedsys-jisx0208")
      ;; (ime-font . "ms-mincho-12-jisx0208")
      ))

  :config
  ;; font setting
  (dolist (font '("Myrica M" "MyricaM M"))
    (catch 'aaa
      (if (member font (font-family-list))
	  (cond ((eq system-type 'darwin)
		 (add-to-list 'default-frame-alist `(font . ,(concat font "-18"))))
		((equal (system-name) "JPC20165182")
		 (add-to-list 'default-frame-alist `(font . ,(concat font "-10"))))
		(t
		 (add-to-list 'default-frame-alist `(font . ,font))))
	(throw 'aaa font))))


  ;; alpha
  ;; http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#alpha
  ;; カレントウィンドウの透明度を変更する (85%)
  ;; (set-frame-parameter nil 'alpha 0.85)
  ;; (set-frame-parameter nil 'alpha 50)
  ;; http://d.hatena.ne.jp/khiker/20090809/emacs_opacity
  ;; 関数で、alpha度合いを確認
  (defun my-set-frame-alpha (alpha)
    ;; (let ((prompt (format "nalpha:%s" (frame-parameter (selected-frame) 'alpha)))
    (interactive "nalpha:")
    (let ((x (if (> alpha 100)
		 100
	       alpha)))
      (set-frame-parameter (selected-frame) 'alpha x)))

  )

;; font setting memo

;; On windows, you can check the font you are using
;; (w32-select-font)

;; font設定を行なわない場合、"Courie New 標準 サイズ10 文字セット 欧米" が選択される様だが、
;; SKK変換モードに入れると、非常に重くなってしまうので、使用を止める

;; "Meiryo UI" 等幅フォントでないので使用をやめる

;; ricty (diminished)
;; refer the folloing page install
;; on mac
;; https://hajipro.com/local-development-environment-mac/ricty-diminished
;; on windows
;; http://vdeep.net/ricty-font#WindowsRicty

;; http://www.techscore.com/blog/2016/12/25/emacs-%E3%81%93%E3%81%A8%E3%81%AF%E3%81%98%E3%82%81-%E7%A7%81%E3%81%AE%E4%BA%8B%E4%BE%8B/
;; "フォントを統一する" の項参照
;; フォントサイズを変更するには、
;; (add-to-list 'default-frame-alist '(font . "Ricty Diminished-15")))
;; の -15 の様に、サイズを指定する

;; Windows環境で、Rictiy dimishedフォントを用いていると、
;; Wanderlust summary bufferでのgb232 メールのsubjectの一部が文字化けしてしまう。

;; Myrica https://myrica.estable.jp/
;; download Myrica.ttc or MyricaM.ttc zip,
;; unzip it, then dobule click Myrica.TTC to install
;; restart Emacs, evaluate (w32-select-font) in *scratch* buffer
;; check if "Myrica M" or "MyricaM" is selected
