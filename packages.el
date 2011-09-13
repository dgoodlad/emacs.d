;; Contains the list of packages we are using

(setq el-get-sources
      '((:name evil
               :after (lambda()
                        (evil-mode 1)
                        (setq evil-shift-width 2)))
        (:name multi-term
               :after (lambda()
                        (setq multi-term-program "/bin/zsh")
                        (setq term-default-bg-color nil)
                        (setq term-default-fg-color nil)
                        ; After ansi-term has fired up the shell process, set
                        ; it to UTF-8. ansi-term ignores the default coding
                        ; for some reason.
                        (add-hook 'term-exec-hook
                                  (function
                                   (lambda ()
                                     (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))
                        (global-set-key (kbd "C-c t") 'multi-term-next)
                        (global-set-key (kbd "C-c T") 'multi-term)))
        (:name ruby-mode
               :after (lambda() (load "conf/ruby")))
        (:name inf-ruby
               :type elpa
               :load "inf-ruby.el")
        (:name js2-mode
               :type git
               :url "https://github.com/mooz/js2-mode.git"
               :compile nil
               :load "js2-mode.elc"
               :build ("emacs --batch -f batch-byte-compile js2-mode.el"))
        (:name slime
               :description "Superior Lisp Interaction Mode for Emacs"
               :type git
               :module "slime"
               ;:info "doc"
               :url "https://github.com/nablaone/slime.git"
               :load-path ("." "contrib")
               :compile (".")
               )
        (:name swank-js
               :type git
               :url "https://github.com/ivan4th/swank-js.git"
               :depends slime
               :require nil
               :after (lambda ()
                        (require 'slime)
                        (slime-setup '(slime-repl slime-js))
                        (global-set-key [f5] 'slime-js-reload)
                        (add-hook 'js2-mode-hook
                                  (lambda () (slime-js-minor-mode 1)))
                        (add-hook 'css-mode-hook
                                  (lambda ()
                                    (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))))
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
        (:name zenburn-theme
               :type http
               :url "https://github.com/bbatsov/zenburn-emacs/raw/master/zenburn-theme.el"
               :load "zenburn-theme.el"
               :after (lambda ()
                        (enable-theme 'zenburn)))
        (:name scss-mode)
        (:name yaml-mode)
        ))

(setq my-packages
      (append
        '(evil
          switch-window
          multi-term
          ruby-mode
          ruby-compilation
          rhtml-mode
          haml-mode
          auto-complete
          autopair
          yasnippet
          paredit
          coffee-mode
          magit
          magithub
          markdown-mode
          mustache-mode
          org-mode
          puppet-mode
          )
        ;()))
        (mapcar 'el-get-source-name el-get-sources)))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync my-packages)
