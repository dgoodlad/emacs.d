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
        (:name yasnippet
               :type git
               :url "https://github.com/capitaomorte/yasnippet.git"
               :features "yasnippet"
               :prepare (lambda ()
                          ;; Set up the default snippets directory
                          ;;
                          ;; Principle: don't override any user settings
                          ;; for yas/snippet-dirs, whether those were made
                          ;; with setq or customize.  If the user doesn't
                          ;; want the default snippets, she shouldn't get
                          ;; them!
                          (unless (or (boundp 'yas/snippet-dirs) (get 'yas/snippet-dirs 'customized-value))
                            (setq yas/snippet-dirs
                                  (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")))))

               :post-init (lambda ()
                            ;; Trick customize into believing the standard
                            ;; value includes the default snippets.
                            ;; yasnippet would probably do this itself,
                            ;; except that it doesn't include an
                            ;; installation procedure that sets up the
                            ;; snippets directory, and thus doesn't know
                            ;; where those snippets will be installed.  See
                            ;; http://code.google.com/p/yasnippet/issues/detail?id=179
                            (put 'yas/snippet-dirs 'standard-value
                                 ;; as cus-edit.el specifies, "a cons-cell
                                 ;; whose car evaluates to the standard
                                 ;; value"
                                 (list (list 'quote
                                             (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets"))))))
               ;; byte-compile load vc-svn and that fails
               ;; see https://github.com/dimitri/el-get/issues/200
               :compile nil)
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
        smex

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
