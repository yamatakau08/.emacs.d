(use-package frame
  :if window-system
  :custom
  (default-frame-alist
   '(
     ;; Since using modus-themes, comment about color setting
     ;;(foreground-color . "black")
     ;;(background-color . "LemonChiffon")
     ;;(border-color . "black")
     ;;(mouse-color  . "white")
     ;;(cursor-color . "black")
     (width . 120) ; column, unit is NOT pixel, get pixel (frame-pixel-width)
     (height . 47) ; row, unit is NOT pixel,get pixel (frame-pixel-height)
     (top . 8)
     (left . 339) ; Fix on for Windows (workarea width - frame-pixel-width) / 2
     (alpha . 100)
     ;; font
     ;;(font . "Myrica M-10") ; set by :config section
     ;; ime font
     ;; (ime-font . "private-fontset-japanese-jisx208")
     ;; (ime-font . "fixedsys-jisx0208")
     ;; (ime-font . "ms-mincho-12-jisx0208")
     ))

  :config
  ;; once change font size to see easily on MacBook Air 13inch
  (set-face-attribute 'default nil :height 150)

  ;; font setting
  ;; the followings are the values from frame-parameters when using Myrica M font
  ;; (font . "-outline-Myrica M-normal-normal-normal-mono-20-*-*-*-c-*-iso8859-1") ; on Windows
  ;; When set the above value on Mac, returns the following
  ;; (font .       "-*-Myrica M-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1")
  ;; almost the same but, height will be shorter than Windows'one

  ;; font setting, Myrica font https://myrica.estable.jp/
  (catch 'font-added
    (dolist (font '("Myrica M" "MyricaM M"))
      (if (member font (font-family-list))
	  (progn
	    (cond ((eq system-type 'darwin)
		   ;; -16 is the almost same as on Windows Myrica M font -10
		   (add-to-list 'default-frame-alist `(font . ,(concat font "-16"))))
		  ((eq system-type 'windows-nt)
		   (if (equal (system-name) "JPC20545731")
		       (add-to-list 'default-frame-alist `(font . ,(concat font "-14")))
		     (add-to-list 'default-frame-alist `(font . ,(concat font "-10")))))
		  (t
		   (add-to-list 'default-frame-alist `(font . ,(concat font "-10")))))
	    (throw 'font-added font)))))

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

  (defun my-put-frame-parameters ()
    "put frame parameters"
    (dolist (param (frame-parameters))
      (pp param)))

  (defun workarea-width-height ()
    "get width and height of workarea size of display monitio which is the most small"
    (let ((width (nth  2 (frame-monitor-workarea)))
	  (height (nth 3 (frame-monitor-workarea))))
      (list width height)))

  ;; for debug, when launching Emacs, after loading this block
  ;; On Windows
  ;; work-area-width-height (1920 1042)
  ;; frame-pixel-width,height 1002 828
  (message "[debug][.frame.el] workarea: %S" (workarea-width-height))
  (message "[debug][.frame.el] frame   : %S %S" (frame-pixel-width) (frame-pixel-height))
  ;; frame-pixel-width,height is not final one
  )

(provide '.frame)

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
;; on both Winodws and Mac
;; restart Emacs,
;; in scratch
;; (member "Myrica M" (font-family-list))
;; (member "MyricaM M" (font-family-list))
;;
;; on Windows (w32-select-font) in *scratch* buffer
;; check if "Myrica M" or "MyricaM" is selected

;; Mac Book Air 13.3 inch
;; Final frame pixel size, final means font setting is applied.
;; (frame-pixel-width)
;; 995
;; (frame-pixel-height)
;; 756
