(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(require 'auto-complete-config)
(ac-config-default)

;; This stops "end" followed by "RET" getting completed to something
;; like "endomorph" - have to use an explicit "TAB" to complete.
(define-key ac-complete-mode-map (kbd "\r") nil)

;; Make auto-complete co-exist with viper-mode nicely
(when *viper-enabled*
  (define-key ac-complete-mode-map (kbd "C-n") 'dabbrev-expand)
  (define-key ac-complete-mode-map (kbd "C-p") 'dabbrev-expand)
  (define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key))
