(use-package flyspell
  ;;:ensure-system-package ; remove this section,work but finally didn't work well on msys2
  ;;((hunspell . "pacman -S mingw-w64-i686-hunspell")
  ;; (hunspell . "pacman -S mingw-w64-i686-hunspell-en")))

  :if (executable-find "hunspell")

  :init
  (setenv "DICTIONARY" "en_US")

  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell nil)

  :config
  ;; http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  )
