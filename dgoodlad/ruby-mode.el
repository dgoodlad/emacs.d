(vendor 'ruby-electric 'ruby-electric-mode)

(add-hook 'ruby-mode-hook
          (lambda ()
	    (ruby-electric-mode t)
	    (flymake-ruby-load)
            ))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("autotest$" . ruby-mode))
(add-to-list 'auto-mode-alist '("irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rake$" . ruby-mode))
