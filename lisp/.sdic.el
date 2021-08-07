(use-package sdic
  :load-path "sdic/lisp"

  :init
  ;; work arround for sdic doesn't work
  ;; "eval-buffer: Symbol’s value as variable is void: default-fill-column"
  ;; refer http://suzuki.tdiary.net/20161226.html
  (setq default-fill-column (default-value 'fill-column))

  :bind
  (("C-c W" . sdic-describe-word)
   ("C-c w" . sdic-describe-word-at-point)
   :map sdic-mode-map
   ;;("r" . my-sdic-register-item) ; 'r' is not invalid after sdic-descibe-word, assing C-r
   ("C-r" . my-sdic-register-item)
   )

  :config
  ;; for Windows Emacs
  (setq sdic-default-coding-system  'utf-8)
  (setq sdicf-default-coding-system 'utf-8)

  ;; for dic setting
  ;; http://pogin.hatenablog.com/entry/20110418/1303062923
  (setq sdic-eiwa-dictionary-list
	'(; (sdicf-client "~/.emacs.d/dict/gene-euc.sdic") ; Mac/Linux?
	  (sdicf-client "~/.emacs.d/dict/gene-utf8.sdic")  ; Win(Cygwin/Msys2),Mac
	  )
	sdic-waei-dictionary-list
	'(; (sdicf-client "~/.emacs.d/dict/jgene-euc.sdic")  ; Mac/Linux?
	  (sdicf-client "~/.emacs.d/dict/jgene-utf8.sdic" (strategy direct)) ; Win(Cygwin/Msys2),Mac
	  ))

  (defun my-sdic-register-item ()
    "push note into Anki through AnkiConnect"
    (interactive)
    (if (string-equal (buffer-name) "*sdic*")
	(let ((deck "英語") ; deck is fixed
	      (front (read-string "Front: "
				  (if (region-active-p)
				      (buffer-substring (region-beginning) (region-end))
				    (thing-at-point 'word t))))
	      (back  (read-string "Back : " )))
	  (my-anki-browse-addNote "英語" front back))))
  )

(provide '.sdic)
