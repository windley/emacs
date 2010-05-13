;; -*- mode: emacs-lisp; -*-

;;; Emacs Load Path
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/Users/pjw/emacs")
(add-to-list 'load-path "/Users/pjw/emacs/speedbar-0.14beta4")
(add-to-list 'load-path "~/emacs/eieio-0.17")
(add-to-list 'load-path "~/emacs/semantic-1.4.4")
(add-to-list 'load-path "~/emacs/ecb-2.32")
(add-to-list 'load-path "~/emacs/magit-0.7")
(add-to-list 'load-path "~/emacs/predictive")
(add-to-list 'load-path "~/emacs/predictive/html")


;; Insert date at this point
(defun insert-date () 
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;; edit the file Journal.txt and put a datastamp at the end
(fset 'journal
   [?\C-x ?\C-f ?\C-f ?\C-a ?\C-k ?~ ?/ ?D ?o ?c tab ?p ?e ?r tab ?J ?o ?u ?r tab ?J ?o ?u ?r tab ?. tab return escape ?> return escape ?x ?i ?n ?s ?e ?  ?d ?  return return return ?\C-x ?\C-s])


(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\M-4" 'ispell-word)
(global-set-key "\C-cj" 'flyspell-check-previous-highlighted-word)

(defun mark-buffer-and-copy ()
  "Mark the entire buffer and put it in the kill ring"
  (interactive)
    (kill-ring-save (point-min) (point-max)))

(global-set-key "\C-xh" 'mark-buffer-and-copy)

(setq bookmark-save-flag 1)

;; count words in region (from elisp intro)
(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

;; Count the words in the entire document
(defun count-words-buffer ()
  "Count all the words in the buffer"
  (interactive)
  (count-words-region (point-min) (point-max) )
)

;; Key mapping for counting words in buffer
(global-set-key "\C-c\C-cw" 'count-words-buffer)


;;; general set up
(transient-mark-mode t)      ; higlight to mark
(tool-bar-mode nil)          ; Hate this toolbar. remove it
(server-start)               ; start the server for emacsclient

(setq display-time-24hr-format t)  ;24h time
(display-time)     
(setq visible-bell t)                         ;no more beeps, just screen flash

;; unbind exit key sequence to prevernt accidental exits
(global-unset-key "\C-x\C-c")

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;;; abbrev mode
(setq abbrev-file-name "~/.abbrev_defs")
;(read-abbrev-file)     ; Load my abbreviation definitions
(setq dabbrev-case-replace nil)  ; Preserve case when expanding
(setq abbrev-mode t)


;;set up major mode
(setq-default fill-column 69)
(setq default-major-mode 'text-mode)
(setq text-mode-hook
   '(lambda () 
      ;;(auto-fill-mode 1)
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode)))


;;; tex
(setq tex-mode-hook
   '(lambda () 
;      (auto-fill-mode 1)
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode)))

(setq latex-mode-hook
   '(lambda () 
;      (auto-fill-mode 1)
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode)))

;;; SVN
(autoload 'svn-status "psvn.el" nil t)

;;; org-mode
(add-to-list 'load-path "~/emacs/org-6.32b")
;;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 
	  (lambda ()
	  'turn-on-font-lock
	  (flyspell-mode 1)))


;;; HTML
(autoload 'html-mode "sgml-mode" "Yay SGML" t)
(add-to-list 'auto-mode-alist '("\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\.shtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\.inc$" . html-mode))
(add-to-list 'auto-mode-alist '("\.body$" . html-mode))
(setq html-mode-hook
   '(lambda () 
      (setq word-wrap 1)
;      (auto-fill-mode 1)
      (abbrev-mode t)
      (flyspell-prog-mode)))

;;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; Javascript
;(autoload 'javascript-mode "javascript" "Javascript mode" t)
;(add-to-list 'auto-mode-alist '("\.js$" . javascript-mode))
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))

;;; KRL
(add-to-list 'auto-mode-alist '("\.krl$" . krl-mode))

;;; Puppet
(autoload 'puppet-mode "puppet-mode" "Puppet mode" t)
(add-to-list 'auto-mode-alist '("\.pp$" . puppet-mode))

;;; mediawiki
(autoload 'wikipedia-mode "wikipedia-mode.el"
"Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist
'("\\.wiki\\'" . wikipedia-mode))
(add-to-list 'auto-mode-alist '("\\.wikipedia\\.org.*\\.txt\\'" . wikipedia-mode))


;;; MaGIT
(autoload 'wikipedia-mode "magit.elc")


(setq ispell-program-name "/usr/local/bin/ispell") 
(setq ispell-personal-dictionary "~/lib/ispell/personal-dictionary") 
(setq ispell-silently-savep t)   
(setq ispell-dictionary "english")

; Function to run Tidy HTML parser on region 
; NOTE: this requires external Tidy program

(global-set-key "\C-xt" 'tidy-region)

;;;; (define-key ctl-x-map "it" 'tidy-region)
(setq shell-command-default-error-buffer "tidy-errors") ; define an error buffer
(defun tidy-region ()
  "Run Tidy HTML parser on current region."
  (interactive)
  (let ((start (mark))
        (end (point))
        (command "/usr/local/bin/tidy -config /Users/pjw/config/tidyconfig.txt -asxhtml"))
        (shell-command-on-region start end command t t  shell-command-default-error-buffer)))


;; (setq lpr-switches '("-PLaser"))
(setq lpr-switches '("-Php_LaserJet_1320_series__89ACCF_"))
(global-set-key "\M-p" 'lpr-buffer)


(defun unfill(beg end) 
   (interactive "r") 
   (shell-command-on-region beg end "fmt -w2000" nil t))

;;; bibtex mode
(setq bibtex-maintain-sorted-entries t)
(setq bibtex-user-optional-fields
  '(("url" "URL link (for bib2xhtml)")
    ("postscript" "PostScript file (for bib2xhtml)")
    ("pdf" "PDF file (for bib2xhtml)")
    ("dvi" "DVI file (for bib2xhtml)")))
(setq bibtex-mode-hook
   '(lambda () 
      (abbrev-mode t)
      (flyspell-mode)))



;; ruby stuff
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; uncomment the next line if you want syntax highlighting
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     (flyspell-prog-mode)
	     ))    

(defun try-complete-abbrev (old)
   (if (expand-abbr`ev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-complete-file-name
	try-expand-dabbrev))

(require 'rails)
(require 'ruby-electric)




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


;;; slime
(add-to-list 'auto-mode-alist '("\.cl$" . lisp-mode))
(setq inferior-lisp-program "/usr/local/ccl/scripts/openmcl")
(add-to-list 'load-path "/Users/pjw/emacs/slime-1.2.1")
(require 'slime)
(slime-setup)


;;; speedbar
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
;; Texinfo fancy chapter tags
(add-hook 'texinfo-mode-hook (lambda () (require 'sb-texinfo)))
;; HTML fancy chapter tags
(add-hook 'html-mode-hook (lambda () (require 'sb-html)))

;;; semantic
(setq semantic-load-turn-everything-on t)
(require 'semantic-load)

;;; ECB
(require 'ecb)

;;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have


;; use normal find-file function for ftp files
(setq ido-slow-ftp-host-regexps '(".*"))
;; don't search files in other directories
(setq ido-work-directory-list-ignore-regexps '(".*"))

;;; flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;(add-to-list 'flyspell-prog-text-faces "nxml-text-face")

;;; predictive
(require 'predictive)
(require 'predictive-html)

;;; blogging stuff

(define-skeleton blogging-skeleton
  "inserts a blog skeleton into the current buffer"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 "newsletter")
  "<!-- title: \n"   (or v1 (setq v1 (skeleton-read "Title: "))) "\n-->\n"
  "<!-- category: newsletter -->\n"
  "<!-- keywords: \n"   (or v2 (setq v2 (skeleton-read "keywords: "))) "\n-->\n"
  "<p>" \n _  \n "</p>")

(defun make-blog-name (name)
    (random t)
    (concat name
	    (format-time-string "-%Y%m%d-") 
	    (number-to-string (random 100))
            ".html"))

(setq *blog-dir* "~/Documents/blogging/")

(defun blog (name)
   "start blogging!"
   (interactive "sname: " "tech")
   (let ((name (make-blog-name name))
	 )
     (find-file (concat *blog-dir* name))
     (blogging-skeleton)))

(global-set-key "\C-x\M-b" 'blog)


(defun unutf ()
  "remove UTF-8 chars from text"
  (interactive)

  (point-to-register 'x)

  (mapcar 
   (lambda (x)
     (beginning-of-buffer)
     (while (search-forward (car x) nil t)
       (replace-match (cdr x) nil t)))
   `((,(string (make-char 'mule-unicode-0100-24ff  114 124)) . "\"")
     (,(string (make-char 'mule-unicode-0100-24ff  114 125)) . "\"")
     (,(string (make-char 'mule-unicode-0100-24ff  114 120)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  114 121)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  114 115)) . "--")
     (,(string (make-char 'mule-unicode-0100-24ff  114 116)) . "---")
     (,(string (make-char 'mule-unicode-e000-ffff  117 61)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  115 38)) . "...")
     ("'" . "'")
     ("&" . "&")
     ("\"" . "\"")))

  (jump-to-register 'x)
  )


(defun remove-brackets (beginning end)
  "remove UTF-8 chars from text"
  (interactive "r")
  (mapcar 
   (lambda (x)
     (goto-char beginning)
     (while (search-forward (car x) nil t)
       (replace-match (cdr x) nil t)))
   `(("<" . "&lt;")
     (">" . "&gt;")
     )
   )
  )

;;(set-face-attribute 'default nil :family "lucida" :height 100)
;;(set-face-attribute 'default nil :family "lucida" :height 120)
;;(set-face-attribute 'default nil :family "lucida" :height 140)


;; ;ok, this is to fix carbon emacs cursor bug.
(setq initial-frame-alist (cons '(cursor-type . bar) (copy-alist initial-frame-alist)))
(setq default-frame-alist (cons '(cursor-type . bar) (copy-alist default-frame-alist)))
(set-cursor-color "lightgray")


(add-to-list 'load-path "~/emacs/color-theme-6.6.0")
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
  

(global-set-key "\C-x\C-i" 'indent-region)

;;; php

(require 'php-mode)
(setq auto-mode-alist
  (append '(("\\.php$" . php-mode)
            ("\\.php3$" . php-mode))
              auto-mode-alist))

;;; for aquaemacs
(cua-mode 0)
(transient-mark-mode 1)
(one-buffer-one-frame-mode 0)
(setq mac-option-modifier 'meta)



(define-key osx-key-mode-map (kbd "A-q") 'fill-paragraph)
