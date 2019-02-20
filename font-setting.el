;;; font setting

;; ricty (diminished)
;; refer the folloing page install
;; on mac
;; https://hajipro.com/local-development-environment-mac/ricty-diminished
;; on windows
;; http://vdeep.net/ricty-font#WindowsRicty

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
(if (member "Myrica M" (font-family-list))
    (cond
     ((equal (system-name) "JPC20165182") ; to big font size on laptop environment
      (add-to-list 'default-frame-alist '(font . "Myrica M-14")))
     (t
      (add-to-list 'default-frame-alist '(font . "Myrica M")))
     ))

