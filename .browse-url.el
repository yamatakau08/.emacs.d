;(require 'browse-url) ; requireしなくても起動可能

; 設定しなくても M-x browse-url-at-point で起動するのでコメント
;(setq browse-url-browser-function 'browse-url-default-windows-browser)

; Shift + mouse 真中ボタンで、browserが起動するが、
; browserが2つ起動するのでコメント
(global-set-key [S-mouse-2] 'browse-url-at-mouse)
(global-set-key "\C-c\C-z"  'browse-url-at-point)
; ファイルを開くアプリケーションの選択ダイアログが出るのでコメント
;(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
;(global-set-key "\C-c\C-zr" 'browse-url-of-region)
;(global-set-key "\C-c\C-zu" 'browse-url)
;(global-set-key "\C-c\C-zv" 'browse-url-of-file)
(add-hook 'dired-mode-hook
	  (lambda ()
             (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))
 
(cond
 ((eq window-system 'x)
  (setq browse-url-browser-function 'browse-url-generic)
  (if (executable-find "chromium-browser")
      (setq browse-url-generic-program "chromium-browser")
    (setq browse-url-generic-program "firefox"))))
 
(defalias 'browse-url-at-point 'mybrowse-urlunc-at-point)
;(defun mybrowse-url-at-point (&optional arg)
;  "URL表記の後ろ全角スペースを削除してbrowse-urlを起動する。"
;  (interactive "P")
;  (let ((url (substring (browse-url-url-at-point) 0
;		      (string-match "　" (browse-url-url-at-point)))))
;    (if url
;	(browse-url url (if arg
;			    (not browse-url-new-window-flag)
;			  browse-url-new-window-flag))
;      (error "No URL found"))))

(setq thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,:\\") ; add "\\" in thing-at-point-file-name-chars

(defun mybrowse-urlunc-at-point ()
  (interactive)
  (let ((url (thing-at-point 'filename))) ; It's important to set filename as a argument, not url!	
    (setq url (replace-regexp-in-string "aa.bb.119.24" "aa.cc.126.1" url))
    (cond
     ((string-match "\\(http\\|ftp\\)" url)
      (browse-url url))
     ((string-match "\\\\" url)
      (setq ufile (replace-regexp-in-string "\\\\" "/" url))
      (if (eq window-system 'w32) ; on windows
	  (if (file-exists-p ufile)
	      (w32-shell-execute "open" url)
	    (message "No such file or directory: %s" url))
	(message "Use on Windows! %s" url)))
     (t
      (message "Can't execute! %s" url)))
    ))
