(use-package flyspell
  ;; should install hunspell,hunspell-en
  
  ;; :ensure-system-package ; Give up, don't work well on msys2
  ;; ((hunspell    . "pacman -S --noconfirm mingw-w64-i686-hunspell")
  ;;  (hunspell-en . "pacman -S --noconfirm mingw-w64-i686-hunspell-en"))

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
