;;; You need to install fonts used by all-the-icons package
;;; git clone git@github.com:domtronn/all-the-icons.el.git
;;; go to the fonts directory under the git clone directory
;;; do dobule click the ttf font files to install that into fonts system
;;; the following lisp expression can judge if the fonts is installed.

;; (member "all-the-icons" (font-family-list))
;; (member "FontAwesome" (font-family-list))
;; (member "file-icons" (font-family-list))
;; (member "Material Icons" (font-family-list))
;; (member "github-octicons" (font-family-list))
;; (member "Weather Icons" (font-family-list))

(use-package all-the-icons
  :ensure t

  :if (and (member "all-the-icons" (font-family-list))
	   (member "FontAwesome" (font-family-list))
	   (member "file-icons" (font-family-list))
	   (member "Material Icons" (font-family-list))
	   (member "github-octicons" (font-family-list))
	   (member "Weather Icons" (font-family-list)))

  :config
  ;; https://github.com/domtronn/all-the-icons.el#slow-rendering
  ;; when set this, (insert (all-the-icons-icon-for-file "foo.js")) returns nil
  (setq inhibit-compacting-font-caches t))

(provide '.all-the-icons)
