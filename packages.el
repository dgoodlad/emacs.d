;; Contains the list of packages we are using

(setq el-get-user-package-directory "~/.emacs.d/conf")

(setq el-get-sources
      '((:name evil-surround
               :depends evil
               :type git
               :url "https://github.com/timcharper/evil-surround.git")
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
               :require nil)
        (:name zenburn-theme
               :type http
               :url "https://github.com/bbatsov/zenburn-emacs/raw/master/zenburn-theme.el"
               :load "zenburn-theme.el"
               :after (lambda ()
                        (enable-theme 'zenburn)))
        (:name color-theme-solarized
               :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme"
               :type git
               :url "https://github.com/sellout/emacs-color-theme-solarized.git"
               :load ("solarized-dark-theme.el" "solarized-light-theme.el")
               :compile ()
               :require nil
               :depends ())
        (:name ack-and-a-half
               :type git
               :url "https://github.com/jhelwig/ack-and-a-half.git")
        (:name idomenu
               :load "idomenu.el"
               :require "idomenu")
        ))


(setq my-packages
      '(evil
        evil-surround

        ;zenburn-theme
        color-theme-solarized

        auto-complete
        yasnippet
        ack-and-a-half
        idomenu
        switch-window

        multi-term

        magit
        ;magithub

        ruby-mode
        ruby-compilation
        inf-ruby

        js2-mode
        slime
        swank-js

        coffee-mode
        haml-mode
        markdown-mode
        mustache-mode
        puppet-mode
        rhtml-mode
        scss-mode
        yaml-mode
        ))

; Do this sync, so required packages are installed and loaded before running
(el-get 'sync my-packages)
