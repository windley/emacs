;;; some stuff specific to aquaemacs

;;; for aquaemacs
(cua-mode 0)
(transient-mark-mode 1)
(one-buffer-one-frame-mode 0)
(setq mac-option-modifier 'meta)

(define-key osx-key-mode-map (kbd "A-q") 'fill-paragraph)

;;; something that the apple version of emacs writes
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cua-mode nil nil (cua-base))
 '(ecb-options-version "2.32")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-wget-setup (quote cons))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar)))


;; ok, this is to fix carbon emacs cursor bug.
;; (setq initial-frame-alist (cons '(cursor-type . bar) (copy-alist initial-frame-alist)))
;; (setq default-frame-alist (cons '(cursor-type . bar) (copy-alist default-frame-alist)))
;; (set-cursor-color "lightgray")

