;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            ;; strip newlines etc so the regexp below will match a multiline comment
            (let ((btext (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t)))
              (setq js2-additional-externs
                    (split-string
                     (if (string-match "/\\* *global \\(.*?\\)\\*/" btext) (match-string-no-properties 1 btext) "")
                     "[ ,]+" t))
              )))

(add-hook 'js2-mode-hook
          (lambda () (setq js2-basic-offset 2)))
