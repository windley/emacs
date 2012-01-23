;; my major mode configs


;;set up major mode
(setq-default fill-column 69)
(setq default-major-mode 'org-mode)

;; smart tabbing
(require 'smart-tab) ;;; http://www.emacswiki.org/emacs/TabCompletion#SmartTab
(global-smart-tab-mode 1)

(setq text-mode-hook
   '(lambda () 
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode t)
      )
   )


(setq ispell-program-name "/usr/local/bin/ispell") ;; has to be set before load.
(if (file-exists-p "/usr/local/bin/ispell") 
;    (require 'ispell)
    ;;; flyspell
    (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;   (add-to-list 'flyspell-prog-text-faces "nxml-text-face")
)



;;; tex
(setq tex-mode-hook
   '(lambda () 
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode t)))

(setq latex-mode-hook
   '(lambda () 
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode t)))

;;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 
	  (lambda ()
	    'turn-on-font-lock
	    (setq word-wrap 1)
	    (setq truncate-lines nil)
	    (flyspell-mode 1)))

;(require 'remember)
;(org-remember-insinuate)

(require 'google-weather)
(require 'org-google-weather)


(setq org-directory "~/Dropbox/Documents/orgfiles/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(add-hook 'org-capture-mode-hook '(lambda () (setq truncate-lines nil)))
(define-key global-map "\C-cr" 'org-capture)


(setq org-capture-templates 
  `(
	  ("a" "Appointment" entry (file+headline 
				    ,(concat org-directory "taskdiary.org") "Calendar") 
	   "* APPT %^{Description} %^g
%?
Added: %U") 

	  ("n" "Notes" entry (file+datetree 
			      ,(concat org-directory "notes.org"))
	   "* %^{Description} %^g %? 
Added: %U") 

	  ("b" "Book" entry (file+datetree 
				   ,(concat  "~/Dropbox/Documents/KRL Book/krl.txt"))
	   "* Topic: %^{Description}  %^g
%?
Added: %U") 

	  ("j" "Journal" entry (file ,(concat org-directory "journal.org"))
	   "** %^{Title} %U  %(journal-google-weather \"Lindon, UT\")
%?
")
	  ))



 (defadvice org-capture-finalize (after delete-capture-frame activate)  
   "Advise capture-finalize to close the frame if it is the capture frame"  
   (if (equal "capture" (frame-parameter nil 'name))  
       (delete-frame)))  
   
 (defadvice org-capture-destroy (after delete-capture-frame activate)  
   "Advise capture-destroy to close the frame if it is the rememeber frame"  
   (if (equal "capture" (frame-parameter nil 'name))  
       (delete-frame)))  
   
 ;; make the frame contain a single window. by default org-capture  
 ;; splits the window.  
 (add-hook 'org-capture-mode-hook  
           'delete-other-windows)  
   
 (defun make-capture-frame ()  
   "Create a new frame and run org-capture."  
   (interactive)  
   (make-frame '((name . "capture") (width . 120) (height . 15)))  
   (select-frame-by-name "capture") 
   (setq word-wrap 1)
   (setq truncate-lines nil)
   (org-capture))  

;;; weather
(defun journal-google-weather (location)
  (let* ((data (ignore-errors
		 (google-weather-get-data location)))
	 (forecast (when data (caddr (google-weather-data->weather data)))))
    (when forecast
      (let ((condition (cdaadr (assoc 'condition forecast)))
            (low (cdaadr (assoc 'low forecast)))
            (high (cdaadr (assoc 'high forecast)))
            (city (google-weather-data->city data))
	    )
	(format-spec "%L | %c | High: %hF Low: %lF"
		     `((?c . ,condition)
		       (?L . ,location)
		       (?l . ,low)
		       (?h . ,high)))))))




(setq org-remember-templates
      `(("Journal" ?j "\n* %^{day's theme} %T \n%[~/emacs/templates/dailyreview.txt]%i\n" ,(concat org-directory "journal.org"))
	("Book" ?b "\n* %^{Book Title} %T :BOOK: \n%[~/emacs/templates/booktemp.txt]%?\n" ,(concat org-directory "book.org"))
	("Notes" ?n "\n* %^{topic} %T \n%i%?\n" ,(concat org-directory "notes.org"))
	("Fax" ?f "\n* From: Phil Windley\n* Date: %T\n* To: %^{addresses} \n%i%?\n" ,(concat org-directory "faxes.org"))
	))


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
      (flyspell-mode t)
      ))

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
(require 'krl-mode)
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
      (flyspell-mode t)))



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

(require 'rails)
(require 'ruby-electric)




;;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-electric-keywords t)
(setq cperl-invalid-face (quote off)) ;; don't highlight trailing whitespace

(add-hook 'cperl-mode-hook
          '(lambda ()
	     (flyspell-prog-mode t)
	     ))    



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

;;; predictive
(require 'predictive)
(require 'predictive-html)

;;; php

(require 'php-mode)
(setq auto-mode-alist
  (append '(("\\.php$" . php-mode)
            ("\\.php3$" . php-mode))
              auto-mode-alist))

(require 'git)


