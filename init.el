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
(load custom-file)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------

(defvar my-packages '(ag
                      better-defaults
                      cider
                      clojure-mode
                      company-mode
                      diminish
                      enh-ruby-mode
                      eval-sexp-fu
                      evil
                      evil-surround
                      evil-leader
                      evil-lisp-state
                      flx-ido
                      flycheck
                      git-gutter-fringe
                      git-link
                      gitconfig-mode
                      github-browse-file
                      gitignore-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      key-chord
                      magit
                      markdown-mode
                      projectile
                      puppet-mode
                      puppetfile-mode
                      rainbow-delimiters
                      rspec-mode
                      smart-mode-line
                      smartparens
                      smex
                      switch-window
                      solarized-theme
                      yaml-mode
                      zenburn-theme
                      zoom-frm
))

(defun packages-installed-p (packages)
  "Check if all packages in `packages` are installed"
  (every #'package-installed-p packages))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (packages-installed-p my-packages)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (message "%s" "Installing packages...")
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  (message "%s" " done."))

;; -----------------------------------------------------------------------------
;; Look & feel
;; -----------------------------------------------------------------------------

(setq inhibit-splash-screen t)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Meslo LG M DZ")
  (set-face-attribute 'default nil :height 120) ; size in 1/10 points
  )

(load-theme 'zenburn t)

(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "rainbow-delimiters" '(diminish 'rainbow-delimiters-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))

(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

(global-hl-line-mode 1)

(require 'whitespace)
(defun enable-whitespace ()
  "Enable whitespace-mode"
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (whitespace-mode +1))
(add-hook 'text-mode-hook 'enable-whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

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

(defalias 'yes-or-no-p 'y-or-n-p)

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

(require 'key-chord)
(key-chord-mode 1)

(require 'zoom-frm)
(global-set-key (kbd "C-+") 'zoom-in/out)
(global-set-key (kbd "C--") 'zoom-in/out)
(global-set-key (kbd "C-0") 'zoom-in/out)
(global-set-key (kbd "s-f") 'toggle-frame-fullscreen)

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

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "g" 'magit-status
  "p" 'projectile-switch-project
  "," 'projectile-find-file)

(define-key evil-normal-state-map "L" 'evil-lisp-state)

; Magit/evil
(require 'magit)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

(key-chord-define magit-status-mode-map ",," 'projectile-find-file)
(key-chord-define magit-status-mode-map ",p" 'projectile-switch-project)

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
                            (company-mode 1)
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

;; -----------------------------------------------------------------------------
;; Markdown
;; -----------------------------------------------------------------------------

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
