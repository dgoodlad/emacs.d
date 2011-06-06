;; Contains the list of packages we are using

(setq el-get-sources
      '((:name package
               :after (lambda()
                        (setq package-archives '("tromey" . "http://tromey.com/elpa/"))))
        (:name color-theme
               :type git
               :url "https://github.com/emacsmirror/color-theme.git")
        (:name color-theme-solarized
               :after (lambda()
                        (color-theme-solarized-dark)))
        (:name color-theme-zenburn)
        (:name vimpulse
              :after (lambda()
                       (load "conf/dot-viper")
                       (load "conf/vimpulse")))
        (:name ruby-mode
               :after (lambda() (load "conf/ruby")))
        (:name ruby-electric)
        (:name inf-ruby
               :type elpa
               :load "inf-ruby.el")
        (:name ruby-compilation)
        (:name rhtml-mode)
        (:name css-mode :type elpa)
        (:name yaml-mode)
        (:name auto-complete
               :after (lambda () (load "conf/auto-complete")))
        (:name paredit)
        (:name coffee-mode)
        (:name magit)
        (:name magithub)
        (:name nxhtml)
        (:name js2-mode
               :type git
               :url "https://github.com/mooz/js2-mode")
        (:name markdown-mode
               :after (lambda ()
                        (setq markdown-mode-hook 'turn-on-auto-fill)))
        (:name org-mode
               :url "git://orgmode.org/org-mode.git"
               :after (lambda ()
                        (global-set-key "\C-cl" 'org-store-link)
                        (global-set-key "\C-cc" 'org-capture)
                        (global-set-key "\C-ca" 'org-agenda)
                        (global-set-key "\C-cb" 'org-iswitchb)))
        ))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync)
