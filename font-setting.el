;;; font setting

;;; you can check the font you are using
;;; (w32-select-font)

;;; ricty (diminished)
;;; refer the folloing page install
;;; on mac
;;; https://hajipro.com/local-development-environment-mac/ricty-diminished
;;; on windows
;;; http://vdeep.net/ricty-font#WindowsRicty

;;; http://www.techscore.com/blog/2016/12/25/emacs-%E3%81%93%E3%81%A8%E3%81%AF%E3%81%98%E3%82%81-%E7%A7%81%E3%81%AE%E4%BA%8B%E4%BE%8B/
;;; "フォントを統一する" の項参照
;;; フォントサイズを変更するには、
;;; (add-to-list 'default-frame-alist '(font . "Ricty Diminished-15")))
;;; の -15 の様に、サイズを記述する

;;; font設定を行なわない場合、"Courie New 標準 サイズ10 文字セット 欧米" が選択される様だが、SKKの変換モードに入れると非常に重くなってしまうので、使用を止める

;;; Windows環境で、Rictiy dimishedフォントを用いていると、Wanderlust summary bufferでのgb232 メールのsubjectの一部が文字化けしてしまう。
;(if (member "Ricty Diminished" (font-family-list))
;    (add-to-list 'default-frame-alist '(font . "Ricty Diminished-15")))

;(if (member "Meiryo UI" (font-family-list))
;    (add-to-list 'default-frame-alist '(font . "Meiryo UI-12"))) ; 等幅フォントでないので使用をやめる

;;; Myrica https://myrica.estable.jp/
;;; download Myrica.ttc or MyricaM.ttc zip,
;;; unzip it, then dobule click Myrica.TTC to install
;;; restart Emacs, evaluate (w32-select-font) in *scratch* buffer
;;; check if "Myrica M" or "MyricaM" is selected
(defun my-add-font-in-default-frame-alist (font)
  (add-to-list 'default-frame-alist `(font . ,font)))

(dolist (font '("Myrica M" "MyricaM M"))
  (catch 'aaa
    (if (member font (font-family-list))
	(cond ((eq system-type 'darwin)
	       (my-add-font-in-default-frame-alist (concat font "-18")))
	      ((equal (system-name) "JPC20165182")a
	       (my-add-font-in-default-frame-alist (concat font "-10")))
	      (t
	       (my-add-font-in-default-frame-alist font)))
      (throw 'aaa font))))
