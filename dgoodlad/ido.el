;;; Interactive do, find-file and iswitchb replacement with fuzzy/flex
;;; matching.

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching ftw
(setq ido-enable-last-directory-history nil) ; forget latest selected dir names
