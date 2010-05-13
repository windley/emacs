;; -*- mode: emacs-lisp; -*-


;; I keep all my emacs-related stuff under ~/emacs
(defvar emacs-root "~/" 
 "My home directory --- the root of my personal emacs load-path.")

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (concat emacs-root "emacs"))

;; add all the elisp directories under ~/emacs to my load path
(labels ((add-path (p)
	 (add-to-list 'load-path
			(concat emacs-root p))))
  (add-path "emacs/speedbar-0.14beta4")
  (add-path "emacs/eieio-0.17")
  (add-path "emacs/semantic-1.4.4")
  (add-path "emacs/ecb-2.32")
  (add-path "emacs/magit-0.7")
  (add-path "emacs/predictive")
  (add-path "emacs/predictive/html")
  (add-path "emacs/slime-1.2.1")
  (add-path "emacs/color-theme-6.6.0")
  (add-path "emacs/org-6.32b")
)

(defvar aquamacs-p (string-match "Aquamacs" (version)))

(load-library "modes") ;; blogging stuff
(load-library "efuncs") ;; my functions and keybindings
(load-library "blogging") ;; blogging stuff
(if aquamacs-p (load-library "aquamacs")) ;; specific to Aquaemacs
(load-library "my-config") ;; blogging stuff

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (load-file "~/emacs/themes/color-theme-dark-vee.el")
     (color-theme-dark-vee)))


;;better face colors for my eyes
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:stipple nil :background "black" :foreground "sky blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "adobe-courier"))))
;;  '(font-lock-comment-face ((((class color) (background dark)) (:foreground "slate blue"))))
;;  '(paren-face-match ((((class color)) (:foreground "turquoise"))))
;;  '(paren-face-match-light ((((class color)) (:foreground "pale turquoise"))))
;;  '(region ((t (:background "#ffff99" :foreground "black"))))
;;  '(show-paren-match ((t (:bold nil :foreground "white" :background "steel blue"))))
;;  '(show-paren-mismatch ((t (:bold nil :foreground "white" :background "Red")))))
  

