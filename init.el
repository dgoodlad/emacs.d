(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

(setq custom-file "~/.emacs.d/dgoodlad/custom.el")
(load custom-file 'noerror)

(load "dgoodlad/global")
(load "dgoodlad/defuns")
(load "dgoodlad/fonts")
(load "dgoodlad/utf-8")
(load "dgoodlad/scratch")

(vendor 'color-theme)
