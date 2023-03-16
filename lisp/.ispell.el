(use-package ispell
  :if (executable-find "hunspell")

  :init
  ;; hunspell refers environment variable DICTIONARY to use the dictionary
  ;; Since if it's not set, ispell back end is hunspell doesn't work, execute setenv for safety.
  (setenv "DICTIONARY" "en_US")

  :custom
  (ispell-program-name "hunspell")

  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

  (defun ispell-find-hunspell-dictionaries:around (orig-fun &rest args)
    "On Windows Emacs with cygwin environment,
ispell-find-hunspell-dictionaries function passes null-device as `NUL` to hunspell.
Though null-device is \"/dev/null\" as default value in files.el, bind its to \"NULL\" in somewhere Emacs process.
Since hunspell called from ispell-call-process function outpus \"Can't open NUL.\", ispell can't work correctly.
this around advice function is for work around for that."
    (let ((null-device "/dev/null"))
      (apply orig-fun args)))

  (when (and (eq system-type 'windows-nt)
	     (file-exists-p "c:/cygwin64"))
    ;; on windows need to set ispell-hunspell-dict-paths-alist
    (setq ispell-hunspell-dict-paths-alist
	  `(("en_US-large" ,(concat user-emacs-directory "myspell/en_US-large.aff"))
	    ("en_US" ,(concat user-emacs-directory "myspell/en_US.aff")))
	  )
    (advice-add 'ispell-find-hunspell-dictionaries :around #'ispell-find-hunspell-dictionaries:around))

  )

(provide '.ispell)
