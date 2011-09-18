(require 'slime)
(slime-setup '(slime-repl slime-js))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda () (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))

