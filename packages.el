;; Contains the list of packages we are using

(setq el-get-sources
      '((:name vimpulse
              :after (lambda()
                       (load "conf/dot-viper")
                       (load "conf/vimpulse")))
        (:name multi-term
               :after (lambda()
                        (setq multi-term-program "/bin/zsh")
                        (global-set-key (kbd "C-c t") 'multi-term-next)
                        (global-set-key (kbd "C-c T") 'multi-term)))
        (:name ruby-mode
               :after (lambda() (load "conf/ruby")))
        (:name inf-ruby
               :type elpa
               :load "inf-ruby.el")
        (:name auto-complete
               :after (lambda () (load "conf/auto-complete")))
        (:name magit
               :after (lambda()
                        (global-set-key (kbd "C-c g") 'magit-status)))
        (:name html5-el
               :type git
               :url "https://github.com/hober/html5-el.git"
               :build ("make relaxng")
               :features whattf-dt
               :before (lambda()
                         (eval-after-load "rng-loc"
                           '(add-to-list 'rng-schema-locating-files "~/.emacs.d/el-get/html5-el/schemas.xml"))))
        (:name markdown-mode
               :after (lambda ()
                        (setq markdown-mode-hook 'turn-on-auto-fill)))
        (:name org-mode
               :after (lambda ()
                        (global-set-key "\C-cl" 'org-store-link)
                        (global-set-key "\C-cc" 'org-capture)
                        (global-set-key "\C-ca" 'org-agenda)
                        (global-set-key "\C-cb" 'org-iswitchb)
                        (add-to-list 'load-path "~/.emacs.d/el-get/org-mode")))
        ))

(setq my-packages
      (append
        '(color-theme color-theme-solarized color-theme-zenburn vimpulse
                      switch-window
                      multi-term ruby-mode ruby-electric inf-ruby
                      ruby-compilation rhtml-mode haml-mode
                      auto-complete paredit coffee-mode magit magithub
                      js2-mode markdown-mode org-mode)
        (mapcar 'el-get-source-name el-get-sources)))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync my-packages)
