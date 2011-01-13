(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

(setq custom-file "~/.emacs.d/dgoodlad/custom.el")
(load custom-file 'noerror)
(setq viper-custom-file-name "~/.emacs.d/dgoodlad/dot-viper.el")

(load "dgoodlad/env")
(load "dgoodlad/global")
(load "dgoodlad/defuns")
(load "dgoodlad/fonts")
(load "dgoodlad/utf-8")
(load "dgoodlad/scratch")

(vendor 'color-theme)
(vendor 'vimpulse)
(vendor 'magit)
(vendor 'magithub)
(vendor 'ruby-mode)
;; Load icicles after everything else to avoid keybinding clashes
(vendor 'icicles)
