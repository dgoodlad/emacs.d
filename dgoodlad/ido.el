;;; Interactive do, find-file and iswitchb replacement with fuzzy/flex
;;; matching.

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching ftw
(setq ido-enable-last-directory-history nil) ; forget latest selected dir names

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
