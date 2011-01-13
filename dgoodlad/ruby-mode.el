(vendor 'ruby-electric 'ruby-electric-mode)

(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-electric-mode)
            ))
