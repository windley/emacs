

;; my specific flag settings



(setq bookmark-save-flag 1)

;;; general set up
(transient-mark-mode t)      ; higlight to mark
(tool-bar-mode nil)          ; Hate, hate, hate this toolbar. remove it
(server-start)               ; start the server for emacsclient
(require 'edit-server)       ; for edit in emacs Chrome extension
(edit-server-start)

(setq display-time-24hr-format t)  ;24h time
(display-time)     
(setq visible-bell nil) ;; broken in 24?                         ;no more beeps, just screen flash


;;; abbrev mode
(setq abbrev-file-name "~/.abbrev_defs")
(setq dabbrev-case-replace nil)  ; Preserve case when expanding
(setq abbrev-mode t)

;; (setq lpr-switches '("-PLaser"))
(setq lpr-switches '("-Php_LaserJet_1320_series__89ACCF_"))
(global-set-key "\M-p" 'lpr-buffer)

;; Real men end follow periods with one space only.
(setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; remap kill region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; make undo easier to do
(global-set-key (kbd "C-z") 'undo)

;;; keep up with Dropbox syncs from other machines
(global-auto-revert-mode 1)

(require 'mouse-copy)
(global-set-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [C-S-down-mouse-1] 'mouse-drag-secondary-moving)
