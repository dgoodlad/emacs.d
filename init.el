(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

(setq custom-file "~/.emacs.d/dgoodlad/custom.el")
(load custom-file 'noerror)

(load "dgoodlad/env")
(load "dgoodlad/global")
(load "dgoodlad/defuns")
(load "dgoodlad/fonts")
(load "dgoodlad/utf-8")
(load "dgoodlad/scratch")
;(load "dgoodlad/ido")

(vendor 'color-theme)
(vendor 'vimpulse)
(vendor 'magit)
;; Load icicles after everything else to avoid keybinding clashes
(vendor 'icicles)
