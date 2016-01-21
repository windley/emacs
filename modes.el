;; my major mode configs


;;set up major mode
(setq-default fill-column 69)
(setq default-major-mode 'org-mode)

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq text-mode-hook
   '(lambda ()
      (setq word-wrap 1)
      (abbrev-mode t)
      (flyspell-mode t)
      )
   )

;; for GNOME
(if (string= system-type "gnu/linux")
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-selection-value)
  )


;;; ispell
(setq ispell-program-name "/usr/local/bin/aspell") ;; has to be set before 
(setq ispell-personal-dictionary "~/emacs/personal-dictionary") 
(setq ispell-silently-savep t)   
(setq ispell-dictionary "english")

;; (setq ispell-program-name "/opt/local/bin/ispell") ;; has to be set before load.
;; (setq ispell-library-directory "/opt/local/lib")
;; (if (file-exists-p "/opt/local/bin/ispell")
;; ;    (require 'ispell)
;;     ;;; flyspell
;;     (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; ;   (add-to-list 'flyspell-prog-text-faces "nxml-text-face")
;; )


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq ispell-list-command "--list") ;;; to support running with aspell instead of ispell
(add-hook 'flyspell-mode-hook
   (lambda nil
     (interactive)
     (setq flyspell-sort-corrections nil)
     ))

(require 'ws-trim)

;;; elisp

(add-hook  'elisp-mode-hook
	   '(lambda ()
             (add-to-list ‘write-file-functions ‘delete-trailing-whitespace)
            ))


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

 (setq org-mobile-directory "~/Dropbox/MobileOrg")

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
;          "** %^{Title} %U  %(journal-google-weather \"Lindon, UT\")
           "
** %^{Title} %U  \"Lindon, UT\"
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
(autoload 'magic-mode "magit.elc")
(global-set-key (kbd "C-x g") 'magit-status)


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
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(setq cperl-electric-keywords t)
(setq cperl-electric-parens t)
(setq cperl-indent-level 4)
(setq cperl-invalid-face (quote off)) ;; don't highlight trailing whitespace

(add-hook 'cperl-mode-hook
          '(lambda ()
;             (flyspell-prog-mode t)
;             (add-to-list ‘write-file-functions ‘delete-trailing-whitespace)
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
;; From xsteve. Thants, xsteve.
(defun ido-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))
 (global-set-key "\C-x\C-r" 'ido-recentf)

;;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)
(add-hook 'markdown-mode-hook
   (lambda nil
     (interactive)
     (define-key markdown-mode-map (kbd "\C-c_") 'markdown-insert-hr)
     (define-key markdown-mode-map (kbd "\C-c-") (lambda nil 
						   (interactive)
						   (insert "&mdash;")
						   ))
     (define-key markdown-mode-map (kbd "\C-cm") 'markdown-preview-file)
     ))

 


(electric-pair-mode 1) ;;; auto-insert matching parens
(show-paren-mode 1) ;;; turn on paren match highlighting

(require 'git)

(require 'json-pretty-print)

(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
