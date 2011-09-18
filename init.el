;; Emacs Configuration

; Emacs home dir on load path
(add-to-list 'load-path "~/.emacs.d")

; Start the server
(server-start)

; Globals - General global config
(load "conf/global")

; Key bindings
(load "conf/bindings")

; Packaging - elpa, el-get, etc.
(load "conf/packaging")

; Actual package list - hand over, it will load the configs on a per package
; basis
(load "packages")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/gtd.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
