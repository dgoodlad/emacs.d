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
(global-set-key (kbd "C-c T") 'multi-term)
