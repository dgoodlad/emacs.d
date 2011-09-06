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
        (:name color-theme
               :type emacsmirror)
        (:name color-theme-solarized
               :compile ("solarized-definitions.el" "solarized-light-theme.el" "solarized-dark-theme.el")
               :load-path (".")
               :load nil
               :post-init (lambda ()
                            (require 'solarized-definitions)
                            (require 'solarized-light-theme)
                            (require 'solarized-dark-theme)))
        (:name js2-mode
               :type :git
               :url "https://github.com/mooz/js2-mode.git")
        (:name scss-mode)
        (:name yaml-mode)
        ))

(setq my-packages
      (append
        '(color-theme-solarized
           vimpulse
           switch-window
           multi-term
           ruby-mode
           ruby-electric
           ruby-compilation
           rhtml-mode
           haml-mode
           auto-complete
           paredit
           coffee-mode
           magit
           magithub
           js2-mode
           markdown-mode
           org-mode
           )
        ;()))
        (mapcar 'el-get-source-name el-get-sources)))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync my-packages)
