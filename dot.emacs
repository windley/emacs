;; -*- mode: emacs-lisp; -*-

(require 'cl-lib)

;; I keep all my emacs-related stuff under ~/emacs
(defvar emacs-root "~/" 
  "My home directory --- the root of my personal emacs load-path.")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "~/bin")


(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (concat emacs-root "emacs"))

;; add all the elisp directories under ~/emacs to my load path
(cl-labels ((add-path (p)
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
  (add-path "emacs/org-7.4/lisp")
  (add-path "emacs/git")
  (add-path "emacs/weblogger")
;  (add-path "emacs/google-weather")
)

(defvar aquamacs-p (string-match "Aquamacs" (version)))

(load-library "modes")                    ;; set up and start various modes
(load-library "efuncs")                   ;; my functions and keybindings
(load-library "blogging")                 ;; blogging stuff
(if aquamacs-p (load-library "aquamacs")) ;; specific to Aquaemacs
(load-library "my-config")                ;; blogging stuff

;; use only one desktop
(setq desktop-path '("~/tmp/"))
(setq desktop-dirname "~/tmp/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-save-mode 1)

;;; tool bar
(if window-system
    (tool-bar-mode 0))


(load-theme 'deeper-blue t)

(put 'ido-exit-minibuffer 'disabled nil)
