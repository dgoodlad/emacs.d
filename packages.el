;; Contains the list of packages we are using

(setq el-get-sources
      '((:name package
               :after (lambda()
                        (setq package-archives '("tromey" . "http://tromey.com/elpa/"))))
        (:name color-theme-solarized
               :load "solarized-definitions.el"
               :after (lambda()
                        (require 'color-theme-solarized)
                        (color-theme-solarized-dark)))
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
        (:name css-mode :type elpa)
        ;(:name textmate
        ;       :type git
        ;       :url "https://github.com/defunkt/textmate.el.git"
        ;       :load "textmate.el"
        ;       :after (lambda() (textmate-mode)))
        ;(:name rvm
        ;       :type git
        ;       :url "http://github.com/djwhitt/rvm.el.git"
        ;       :load "rvm.el"
        ;       :compile ("rvm.el")
        ;       :after (lambda() (rvm-use-default)))
        (:name rhtml-mode)
        (:name yaml-mode)
        (:name auto-complete
               :after (lambda () (load "conf/auto-complete")))
        (:name paredit)
        (:name coffee-mode)
        (:name markdown-mode
               :after (lambda ()
                        (setq markdown-mode-hook 'turn-on-auto-fill)))
        (:name magit)
        (:name magithub)
        (:name nxhtml)
        ))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync)
