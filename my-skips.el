;;; for "post skips" is managing man-hour in company
;;;
;;; 社員工数分類
;;; 評価プロジェクト:     プロマネ工程/エンジニアリング工程
;;; 評価プロジェクト以外: SQA業務改善、学習・チャレンジ、その他
;;;
;;; in *Org Select* buffer, template
;;; [p]   プロジェクトマネージメント
;;; [a]   BAR_SUS_SK
;;;
;;; in ~/.notes set org-default-notes-file variable
;;; * プロジェクトマネージメント
;;; ** test
;;;    :LOGBOOK:
;;;    CLOCK: [2019-04-11 木 18:54]
;;;    :END:
;;;
;;; following sample is supported by emacs-jp slack
;;;
;;; (setq org-capture-templates
;;;      '(("p" "プロジェクトマネージメント")
;;;        ("p1" "プロジェクトマネージメント" entry
;;;         (file+headline "~/.notes.org" "プロジェクトマネージメント")
;;;         "** %?" :clock-in 1 :clock-keep 1)
;;;        ("p2" "BAR_SUS_SK" entry
;;;         (file+olp "~/.notes.org" "プロジェクトマネージメント" "BAR_SUS_SK")
;;;         "** %?" :clock-in 1 :clock-keep 1)))

;;; org file for skips
;;; to open the org file for
;;; http://www.mhatta.org/wp/category/org-mode/#%E3%83%A1%E3%83%A2%E3%82%92%E5%8F%96%E3%82%8B
(if (company-network-p)
    (setq my-skips-org-file (concat "/plink:yama@" elmo-imap4-default-server ":~/org/skips.org"))
  ;; (setq org-default-notes-file (concat org-directory "/notes.org")) ; need to consider the org file
  (setq my-skips-org-file (concat org-directory "/notes.org")))

(global-set-key (kbd "C-c o") 'my-skips-org-file-open)

;;;
(defun my-skips-org-file-open ()
  "open skips file"
  (interactive)
  (let ((skips-org-file my-skips-org-file))
    (find-file skips-org-file)))

;;; org-capture-templates
;;;
;;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:org:capture:start#%E3%83%86%E3%83%B3%E3%83%97%E3%83%AC%E3%83%BC%E3%83%88%E3%81%AE%E8%A8%AD%E5%AE%9A
;;; https://orgmode.org/manual/Template-elements.html
;;; https://orgmode.org/manual/Template-expansion.html#Template-expansion

;;; skips items list
(setq my-org-capture-templates-skips-items-list
      '((("m"    "プロジェクトマネージメント") ; important not associated list
	 ("m1" . "プロジェクトマネージメント")
	 ("m2" . "BAR_SUS_SK")
	 ("m3" . "BAR_SB23")
	 ("m4" . "OTH_UMA")
	 ("m5" . "BAR_EB1")
	 ("m6" . "その他"))
	(("e"    "プロジェクトエンジニアリング")
	 ("e1" . "プロジェクトエンジニアリング")
	 ("e2" . "その他"))
	(("o"    "評価プロジェクト以外")
         ("o1" . "評価プロジェクト以外")
	 ("o2" . "SQA業務改善")
	 ("o3" . "学習・チャレンジ")
	 ("o4" . "一般会議")
	 ("o5" . "その他"))))

;;;
(defun my-org-capture-templates-set-skips-parent-item (parent-item)
  (add-to-list 'org-capture-templates parent-item t))

;;;
(defun my-org-capture-templates-set-skips-child-first-item (child-item)
;;        ("p1"
;;         "プロジェクトマネージメント"
;;          entry
;;         (file+headline "~/Documents/org/notes.org" "プロジェクトマネージメント")
;;         "** %?"
;;         :clock-in 1 :clock-keep 1)
  (let* ((keys (car child-item))
	 (description (cdr child-item))
	 (type 'entry)
	 (node-headline description)
	 (target `(file+headline ,my-skips-org-file ,node-headline))
	 (template "** %?")
	 (props '(:clock-in 1 :clock-keep 1))) ;; properties should be set directly?
    ;; (add-to-list 'org-capture-templates `(,keys ,description ,type ,target ,template :clock-in 1 :clock-keep 1) t)
    (add-to-list 'org-capture-templates `(,keys ,description ,type ,target ,template ,@props) t)
    ))

;;; Note; if level 2 heading is none, error happens
;;; org-find-olp: Heading not found on level 2
(defun my-org-capture-templates-set-skips-child-after-first-item (parent-item child-item)
;;        ("p2"
;;         "BAR_SUS_SK"
;;         entry
;;         (file+olp "~/Documents/org/notes.org" "プロジェクトマネージメント" "BAR_SUS_SK")
;;         "** %?"
;;         :clock-in 1 :clock-keep 1)))
  (let* ((keys (car child-item))
	 (description (cdr child-item))
	 (type 'entry)
	 (node-headline description)
	 (lvl1heading (car (cdr parent-item)))
	 (target `(file+olp ,my-skips-org-file ,lvl1heading ,node-headline))
	 (template "** %?")
	 (props '(:clock-in 1 :clock-keep 1)))
    ;; (add-to-list 'org-capture-templates `(,keys ,description ,type ,target ,template :clock-in 1 :clock-keep 1) 
    (add-to-list 'org-capture-templates `(,keys ,description ,type ,target ,template ,@props) t)
    ))

;;;
(defun my-org-capture-templates-set-skips-child-after-first-items (parent-item child-items)
  (cond ((null child-items) nil)
	(t
	 (my-org-capture-templates-set-skips-child-after-first-item parent-item (car child-items))
	 (my-org-capture-templates-set-skips-child-after-first-items parent-item (cdr child-items)))))

;;;
(defun my-org-capture-templates-set-skips-template (parent-item child-items)
  (my-org-capture-templates-set-skips-parent-item parent-item)
  (my-org-capture-templates-set-skips-child-first-item (car child-items))
  (my-org-capture-templates-set-skips-child-after-first-items parent-item (cdr child-items)))

;;;
(defun my-org-capture-templates-set-skips-templates (silist)
  " argument description
    silist: skips items list, format is my-org-capture-templates-skips-items-list"
  (cond ((null silist) nil)
	(t
	 (let* ((item (car silist)))
	   (my-org-capture-templates-set-skips-template	(car item) (cdr item))
	   (my-org-capture-templates-set-skips-templates (cdr silist))))))

;;;; org-capture-mode-hook doesn't effect, (with-eval-after-load 'org-capture) effects
;;;; https://ox-hugo.scripter.co/doc/org-capture-setup/
(with-eval-after-load 'org-capture
  (my-org-capture-templates-set-skips-templates my-org-capture-templates-skips-items-list))
