;; my specific flag settings

(setq bookmark-save-flag 1)

;;; general set up
(transient-mark-mode t)      ; higlight to mark
(tool-bar-mode nil)          ; Hate, hate, hate this toolbar. remove it
(server-start)               ; start the server for emacsclient

(setq display-time-24hr-format t)  ;24h time
(display-time)     
(setq visible-bell t)                         ;no more beeps, just screen flash


;;; abbrev mode
(setq abbrev-file-name "~/.abbrev_defs")
(setq dabbrev-case-replace nil)  ; Preserve case when expanding
(setq abbrev-mode t)

;;; ispell
(setq ispell-program-name "/opt/local/bin/ispell") 
(setq ispell-personal-dictionary "~/lib/ispell/personal-dictionary") 
(setq ispell-silently-savep t)   
(setq ispell-dictionary "english")

;; (setq lpr-switches '("-PLaser"))
(setq lpr-switches '("-Php_LaserJet_1320_series__89ACCF_"))
(global-set-key "\M-p" 'lpr-buffer)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

(require 'json-pretty-print)

