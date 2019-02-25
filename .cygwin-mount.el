(if (eq system-type 'windows-nt)
    (progn 
      (load "~/.emacs.d/cygwin-mount.el")
      (cygwin-mount-activate)))
