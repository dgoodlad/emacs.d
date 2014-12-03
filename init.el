;; @dgoodlad emacs.d/init.el

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------

(defvar my-packages '(better-defaults
                      switch-window
		      evil
		      evil-surround
		      evil-leader
                      evil-lisp-state
		      smartparens
		      idle-highlight-mode
		      ido-ubiquitous
		      magit
                      ag
                      zenburn-theme
                      solarized-theme
                      projectile))

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

(load-theme 'solarized-dark t)

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
;; Keybindings
;; -----------------------------------------------------------------------------

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
  "g" 'magit-status)

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
(smartparens-global-mode 1)
