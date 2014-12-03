;; @dgoodlad emacs.d/init.el

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; -----------------------------------------------------------------------------
;; Directories
;; -----------------------------------------------------------------------------

(defvar my-config-dir (file-name-directory load-file-name)
  "The root dir of my emacs configuration")
(defvar my-savefile-dir (expand-file-name "savefile" my-config-dir)
  "The folder to store automatically-generated save/history files")

(setq custom-file (expand-file-name "custom.el" my-config-dir))

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------

(defvar my-packages '(ag
                      better-defaults
                      cider
                      clojure-mode
                      enh-ruby-mode
                      eval-sexp-fu
		      evil
		      evil-surround
		      evil-leader
                      evil-lisp-state
                      flx-ido
                      flycheck
                      git-link
                      gitconfig-mode
                      github-browse-file
                      gitignore-mode
		      idle-highlight-mode
		      ido-ubiquitous
		      magit
                      powerline
                      powerline-evil
                      projectile
                      puppet-mode
                      puppetfile-mode
                      rainbow-delimiters
                      rspec-mode
		      smartparens
                      smex
                      switch-window
                      solarized-theme
                      yaml-mode
                      zenburn-theme
                      zoom-frm
))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------------------------------------
;; Look & feel
;; -----------------------------------------------------------------------------

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Meslo LG M DZ")
  (set-face-attribute 'default nil :height 120) ; size in 1/10 points
  )

(load-theme 'zenburn t)

(require 'powerline)
(require 'powerline-evil)
(powerline-center-evil-theme)

(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; -----------------------------------------------------------------------------
;; Environment
;; -----------------------------------------------------------------------------

;; read in PATH from .zshrc
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))

;; ensure that ansi-term can render utf-8
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; paste in ansi-term
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; -----------------------------------------------------------------------------
;; Editing
;; -----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 8)          ;; but render hard tabs 8 characters

;; Newline at EOF
(setq require-final-newline t)

;; Revert buffers automatically when the underlying file changes
(global-auto-revert-mode t)

;; TAB indents the current line, or if already indents, runs auto-complete
(setq tab-always-indent 'complete)

;; -----------------------------------------------------------------------------
;; ido Mode
;; -----------------------------------------------------------------------------

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'smex) ;; ido-based M-x
(global-set-key (kbd "M-x") 'smex)

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

(require 'zoom-frm)
(global-set-key (kbd "C-+") 'zoom-in/out)
(global-set-key (kbd "C--") 'zoom-in/out)
(global-set-key (kbd "C-0") 'zoom-in/out)

(global-set-key (kbd "C-x o") 'switch-window)

(global-set-key (kbd "C-x g") 'magit-status)

;; -----------------------------------------------------------------------------
;; Evil
;; -----------------------------------------------------------------------------

(require 'evil)
(require 'evil-leader)
(require 'evil-lisp-state)

(global-evil-leader-mode)
(evil-mode 1)

(setq evil-shift-width 2)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "g" 'magit-status
  "p" 'projectile-switch-project
  "," 'projectile-find-file)

(define-key evil-normal-state-map "L" 'evil-lisp-state)

; Magit/evil
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

;; -----------------------------------------------------------------------------
;; Smartparens
;; -----------------------------------------------------------------------------

(require 'smartparens-config) ; Use the default smartparens config
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" my-savefile-dir))
(setq projectile-switch-project-action 'projectile-find-file)
(projectile-global-mode t)

(defun subfolder-projects (dir)
  (--map (file-relative-name it dir)
        (-filter (lambda (subdir)
                   (--reduce-from (or acc (funcall it subdir)) nil
                                 projectile-project-root-files-functions))
                 (-filter #'file-directory-p (directory-files dir t)))))

(defun -add-known-subfolder-projects (dir)
  (-map #'projectile-add-known-project
        (--map (concat (file-name-as-directory dir) it)
               (subfolder-projects dir))))

(defun add-known-subfolder-projects ()
  "Prompts for a directory, and adds all projects found there to projectile"
  (interactive)
  (-add-known-subfolder-projects (ido-read-directory-name "Add projects under: ")))

;; -----------------------------------------------------------------------------
;; Programming modes
;; -----------------------------------------------------------------------------

(add-hook 'prog-mode-hook (lambda ()
                            (idle-highlight-mode 1)
                            (if window-system (hl-line-mode t))
                            (smartparens-mode 1)))

;; -----------------------------------------------------------------------------
;; Flycheck
;; -----------------------------------------------------------------------------

(require 'flycheck)
(global-flycheck-mode)

;; -----------------------------------------------------------------------------
;; Clojure
;; -----------------------------------------------------------------------------

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; -----------------------------------------------------------------------------
;; Ruby
;; -----------------------------------------------------------------------------

(require 'enh-ruby-mode)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$$" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile$" . enh-ruby-mode))

(provide 'init)
;;; init.el ends here
