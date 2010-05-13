
;;; predictive-latex.el --- predictive mode LaTeX setup function
;;;                         (assumes AMSmath)


;; Copyright (C) 2004-2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.7.2
;; Keywords: predictive, setup function, latex
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion package.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.7.3
;; * updated `predictive-latex-setup' to conform to new way major-mode setup
;;   functions are called by `predictive-mode', and deleted now redundant
;;   `predictive-latex-disable'
;; * avoid clobbering `completion-dynamic-syntax-override-alist' settings
;;
;; Version 0.7.2
;; * added `predictive-latex-reparse-buffer' command to allow easy reparsing
;;   of auto-overlays if they get in a mess due to bugs in the auto-overlays
;;   code :-(
;; * restore buffer's modified flag after enabling predictive mode, since
;;   "electric" synchronization of LaTeX envionments can modify buffer without
;;   actually changing buffer text
;; * renamed "nest" regexps to "nested" regexps
;; * prevent scheduling of "electric" environment syncronization when blocked
;;   by `predictive-latex-disable-env-synchronize'
;; * don't bother synchronizing if environments are already synchronized
;;
;; Version 0.7.1
;; * fixed regexps for {, }, \[ and \] so that they correctly deal with having
;;   an even number of \'s in front of them
;; 
;; Version 0.7
;; * added automatic synchronization of environment names
;; * added interactive commands for navigating by LaTeX environments
;; * added automatic switching of main dictionary based on document class
;;
;; Version 0.6.8
;; * fixed small bug in `predictive-latex-unload-package'
;;
;; Version 0.6.7
;; * updated to work with new versions of the completion-UI and auto-overlays
;;   packages
;; * predictive mode can once again be enabled and disabled independently in
;;   different LaTeX buffers that share the same TeX-master, but disabling it
;;   in the TeX-master buffer will now disable it in all those related LaTeX
;;   buffers
;;
;; Version 0.6.6
;; * added the appropriate checks for changed regexp definitions when starting
;;   the auto-overlay set
;;
;; Version 0.6.5
;; * removed `auto-overlay-functions' and changed to use new interface
;; * renamed "stack" regexps to "nest" regexps
;;
;; Version 0.6.4
;; * overlays are no longer deleted unnecessarily when a buffer is killed
;;
;; Version 0.6.3
;; * generalised label-specific function for adding part of a completion
;;   candidate and re-completing to `predictive-completion-add-to-regexp'
;;
;; Version 0.6.2
;; * rewrote `predictive-latex-forward-word'
;; * prevented label dictionary being clobbered if it's already loaded
;;
;; Version 0.6.1
;; * added missing overlay-local `completion-override-syntax-alist' binding
;;
;; Version 0.6
;; * \usepackage commands now cause dictionaries and regexps to be
;;   automatically loaded and unloaded as necessary
;; * updated to reflect changes in `auto-overlays.el'
;; * \label commands now add (and delete) entries to a special label
;;   dictionary, used to complete cross-reference commands, e.g. \ref.
;; * made the latex completion browser come up as the first menu for LaTeX
;;   commands and labels, and dropped link back to basic completion menu
;; * added eval-when-compile to prevent bogus compilation errors
;;
;; Version 0.5.6
;; * fixed some variables that were set globally instead of locally by the
;;   setup function, and added kill-local-variable lines to the hook function
;;   to restore the default values when predictive mode is disabled
;;
;; Version 0.5.5
;; * minor enhancements and bug fixes
;;
;; Version 0.5.4
;; * updated to reflect changes in completion-ui.el
;;
;; Version 0.5.3
;; * updated to reflect naming changes in dict-tree.el
;;
;; Version 0.5.2
;; * bug fixes
;; * set `predictive-completion-browser-menu' so that LaTeX browser is called
;;   when completing a LaTeX command (can't use overlay-local binding anymore
;;   since LaTeX commands no longer use auto-overlays)
;;
;; Version 0.5.1
;; * renamed to `predictive-latex' and released as only latex setup package
;;
;; Version 0.5
;; * added support for completion browser
;; * modified latex enviroments to use new stack-sync class
;; * stopped using auto-overlays for LaTeX commands, and just set `dict-latex'
;;   as second main dictionary instead
;;
;; Version 0.4
;; * modified to work with new auto-overlays package
;;
;; Version 0.3
;; * changed LaTeX commands back to (new) 'word regexps
;; * added comments as 'line regexps
;; * modified priorities and ordering so things work with new switch-dict code
;;
;; Version 0.2
;; * changed 'word regexps to 'start and 'end regexps so that
;;   predictive-learn-from- functions can learn LaTeX commands
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)

(provide 'predictive-latex)
(add-to-list 'predictive-major-mode-alist
	     '(LaTeX-mode . predictive-setup-latex))
(add-to-list 'predictive-major-mode-alist
	     '(latex-mode . predictive-setup-latex))



;;;============================================================
;;;                  Customization Options

(defgroup predictive-latex nil
  "Predictive completion mode LaTeX support."
  :group 'predictive)


(defcustom predictive-latex-electric-environments t
  "*When enabled, environment names are automatically synchronized
between \\begin{...} and \\end{...} commands."
  :group 'predictive-latex
  :type 'boolean)


(defcustom predictive-latex-docclass-alist nil
  "*Alist associating LaTeX document classes with dictionaries.
When a document class is in the list, "
  :group 'predictive-latex
  :type '(repeat (cons string symbol)))




;;;============================================================
;;;                       Variables 

;; variables holding dictionaries for different LaTeX contexts
(defvar predictive-latex-dict 'dict-latex)
(make-variable-buffer-local 'predictive-latex-dict)
(defvar predictive-latex-math-dict 'dict-latex-math)
(make-variable-buffer-local 'predictive-latex-math-dict)
(defvar predictive-latex-preamble-dict 'dict-latex-preamble)
(make-variable-buffer-local 'predictive-latex-preamble-dict)
(defvar predictive-latex-env-dict 'dict-latex-env)
(make-variable-buffer-local 'predictive-latex-env-dict)
(defvar predictive-latex-label-dict nil)
(make-variable-buffer-local 'predictive-latex-label-dict)


;; alist holding functions called when loading and unloading latex packages
(defvar predictive-latex-usepackage-functions nil)


;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-latex-word 'forward-op 'predictive-latex-forward-word)
;; set up 'predictive-latex-label-word to be a `thing-at-point' symbol
(put 'predictive-latex-label-word 'forward-op
     'predictive-latex-label-forward-word)


;; convenience variable holding alist associating latex dictionary variables
;; and corresponding dictionary name prefices
(defvar predictive-latex-dict-classes
  '((predictive-latex-dict . "dict-latex-")
    (predictive-latex-math-dict . "dict-latex-math-")
    (predictive-latex-preamble-dict . "dict-latex-preamble-")
    (predictive-latex-env-dict . "dict-latex-env-")))


;; variables used to restore local settings of variables when predictive mode
;; is disabled in a LaTeX buffer
(defvar predictive-restore-main-dict nil)
(make-variable-buffer-local 'predictive-restore-main-dict)
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)


;; prevent bogus compiler warnings
(eval-when-compile
  (defvar dict-latex-docclass)
  (defvar dict-latex-bibstyle)
  (defvar TeX-master))


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-color nil)




;;;=========================================================
;;;                  Setup function

(defun predictive-setup-latex (arg)
  "With a positive ARG, set up predictive mode for use with LaTeX major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."
  
  (cond
   ;; ----- enabling LaTeX setup -----
   ((> arg 0)
    
    ;; save overlays and unload regexp definitions before killing buffer
    (add-hook 'kill-buffer-hook
	      (lambda ()
		(auto-overlay-stop 'predictive nil 'save 'leave-overlays)
	      (auto-overlay-unload-regexp 'predictive))
	    nil t)
    
    ;; use latex browser menu if first character of prefix is "\"
    (make-local-variable 'completion-menu)
    (setq completion-menu
	  (lambda (prefix completions)
	    (if (string= (substring prefix 0 1) "\\")
		(predictive-latex-construct-browser-menu prefix completions)
	      (completion-construct-menu prefix completions))
	    ))
    ;; save predictive-main-dict; restored when predictive mode is disabled
    (setq predictive-restore-main-dict predictive-main-dict)

    
    (cond
     ;; if we're not the TeX master, visit the TeX master buffer, enable
     ;; predictive mode in it, and share buffer-local settings with it
     ((and (boundp 'TeX-master) (stringp TeX-master))
      (let (filename buff used-dicts main-dict latex-dict math-dict
		     preamble-dict env-dict label-dict)
	(setq filename (expand-file-name TeX-master))
	(unless (string= (substring filename -4) ".tex")
	  (setq filename (concat filename ".tex")))
	(save-window-excursion
	  (find-file filename)
	  (turn-on-predictive-mode)
	  (setq buff (current-buffer))
	  (setq used-dicts predictive-used-dict-list)
	  (setq main-dict predictive-main-dict)
	  (setq latex-dict predictive-latex-dict)
	  (setq math-dict predictive-latex-math-dict)
	  (setq preamble-dict predictive-latex-preamble-dict)
	  (setq env-dict predictive-latex-env-dict)
	  (setq label-dict predictive-latex-label-dict))
	(auto-overlay-share-regexp-set 'predictive buff)
	(setq predictive-used-dict-list used-dicts)
	(setq predictive-main-dict main-dict)
	(setq predictive-latex-dict latex-dict)
	(setq predictive-latex-math-dict math-dict)
	(setq predictive-latex-preamble-dict preamble-dict)
	(setq predictive-latex-env-dict env-dict)
	(setq predictive-latex-label-dict label-dict)
	;; start the auto-overlays, restoring buffer's modified flag afterwards,
	;; since automatic synchronization of LaTeX envionments can modify
	;; buffer without actually changing buffer text
	(let ((restore-modified (buffer-modified-p)))
	  (auto-overlay-start 'predictive)
	  (set-buffer-modified-p restore-modified))
	))

     
     ;; if we're the TeX master file, set up LaTeX auto-overlay regexps
     ;; FIXME: probably need to handle null TeX-master case differently
     (t
      ;; load the latex dictionaries
      (when predictive-latex-dict
	(when (atom predictive-latex-dict)
	  (setq predictive-latex-dict (list predictive-latex-dict)))
	(mapc 'predictive-load-dict predictive-latex-dict))
      (when predictive-latex-math-dict
	(when (atom predictive-latex-math-dict)
	  (setq predictive-latex-math-dict (list predictive-latex-math-dict)))
	(mapc 'predictive-load-dict predictive-latex-math-dict))
      (when predictive-latex-preamble-dict
	(when (atom predictive-latex-preamble-dict)
	  (setq predictive-latex-preamble-dict
		(list predictive-latex-preamble-dict)))
	(mapc 'predictive-load-dict predictive-latex-preamble-dict))
      (when predictive-latex-env-dict
	(when (atom predictive-latex-env-dict)
	  (setq predictive-latex-env-dict (list predictive-latex-env-dict)))
	(mapc 'predictive-load-dict predictive-latex-env-dict))
      (predictive-load-dict 'dict-latex-docclass)
      (predictive-load-dict 'dict-latex-bibstyle)
      ;; load the label dictionary
      (predictive-latex-load-label-dict)
      
      ;; add main latex dictionary list to main dictionary list
      (make-local-variable 'predictive-main-dict)
      (when (atom predictive-main-dict)
	(setq predictive-main-dict (list predictive-main-dict)))
      (setq predictive-main-dict
	    (append predictive-main-dict predictive-latex-dict))
      
      ;; delete any existing predictive auto-overlay regexps and load latex
      ;; auto-overlay regexps
      (auto-overlay-unload-regexp 'predictive)
      (predictive-latex-load-regexps)
      
      ;; start the auto-overlays, skipping the check that regexp definitions
      ;; haven't changed if there's a file of saved overlay data to use, and
      ;; restoring buffer's modified flag afterwards since automatic
      ;; synchronization of LaTeX envionments can modify buffer without
      ;; actually changing buffer text
      (let ((restore-modified (buffer-modified-p)))
	(auto-overlay-start 'predictive nil nil 'no-regexp-check)
	(set-buffer-modified-p restore-modified))
      ))

    
    ;; load the keybindings and related settings
    (predictive-latex-load-keybindings)
    ;; consider \ as start of a word
    (setq completion-word-thing 'predictive-latex-word)
    (set (make-local-variable 'words-include-escapes) nil)

    t)  ; indicate successful setup
   
   
   
   ;; ----- Disabling LaTeX setup -----
   ((< arg 0)
    ;; if we're the TeX-master, first disable predictive mode in all related
    ;; LaTeX buffers, which we find by looking for buffers that share the
    ;; auto-overlays 'predictive regexp set
    (when (eq TeX-master t)
      (dolist (buff (auto-o-get-buffer-list 'predictive))
	;; TeX-master itself will be in list of buffers sharing regexp set, so
	;; need to filter it out
	(unless (eq buff (current-buffer))
	  (save-excursion
	    (set-buffer buff)
	    (predictive-mode -1)))))
    
    ;; stop predictive auto overlays
    (auto-overlay-stop 'predictive nil 'save)
    (auto-overlay-unload-regexp 'predictive)
    ;; restore predictive-main-dict to saved setting
    (kill-local-variable 'predictive-main-dict)
    (setq predictive-main-dict predictive-restore-main-dict)
    (kill-local-variable 'predictive-restore-main-dict)
    ;; restore completion-dynamic-override-syntax-alist to saved setting
    (kill-local-variable 'completion-dynamic-override-syntax-alist)
    (setq completion-dynamic-override-syntax-alist
	  predictive-restore-override-syntax-alist)
    (kill-local-variable 'predictive-restore-override-syntax-alist)
    ;; remove other local variable settings
    (kill-local-variable 'completion-menu)
    (kill-local-variable 'words-include-escapes)
    (kill-local-variable 'predictive-latex-dict)
    (kill-local-variable 'predictive-latex-math-dict)
    (kill-local-variable 'predictive-latex-env-dict)
    (kill-local-variable 'predictive-latex-label-dict)
    ;; remove hook function that saves overlays
    (remove-hook 'kill-buffer-hook
		 (lambda ()
		   (auto-overlay-stop 'predictive nil 'save 'leave-overlays)
		   (auto-overlay-unload-regexp 'predictive))
		 t)
    
    t))  ; indicate successful reversion of changes
)




(defun predictive-latex-load-regexps ()
  "Load the predictive mode LaTeX auto-overlay regexp definitions."

  ;; %'s start comments that last till end of line
  (auto-overlay-load-regexp
   `(line "%" (dict . predictive-main-dict) (priority . 50) (exclusive . t)
	  (completion-menu . predictive-latex-construct-browser-menu)
	  (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive)
    
  ;; $'s delimit the start and end of maths regions...
  (auto-overlay-load-regexp
   `(self "\\$" (dict . predictive-latex-math-dict) (priority . 40)
	  (completion-menu . predictive-latex-construct-browser-menu)
	  (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive nil 'inline-math)
  
  ;; ...as do \[ and \], but not \\[ and \\] etc.
  ;; Note: regexps contain a lot of \'s because it has to check whether number
  ;; of \'s in front of { is even or odd
  (auto-overlay-load-regexp '(nested) 'predictive nil 'display-math)
  (auto-overlay-load-compound-regexp
   `(start ("[^\\]\\(\\\\\\\\\\)*\\(\\\\\\[\\)" . 2)
	   (dict . predictive-latex-math-dict) (priority . 40)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'display-math)
  (auto-overlay-load-compound-regexp
   `(start ("^\\(\\\\\\[\\)" . 1)
	   (dict . predictive-latex-math-dict) (priority . 40)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'display-math)
  (auto-overlay-load-compound-regexp
   `(end ("[^\\]\\(\\\\\\\\\\)*\\(\\\\\\]\\)" . 2)
	 (dict . predictive-latex-math-dict) (priority . 40)
	 (completion-menu . predictive-latex-construct-browser-menu)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'display-math)
  (auto-overlay-load-compound-regexp
   `(end ("^\\(\\\\\\]\\)" . 1)
	 (dict . predictive-latex-math-dict) (priority . 40)
	 (completion-menu . predictive-latex-construct-browser-menu)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'display-math)
  
  ;; \begin{ and \end{ start and end LaTeX environments. Other \<command>{'s
  ;; do various other things. All are ended by } but not by \}. The { is
  ;; included to ensure all { and } match, but \{ is excluded
  (auto-overlay-load-regexp '(nested) 'predictive nil 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\usepackage{" (dict . t) (priority . 30)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\label{" (dict . t) (priority . 30)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\ref{" (dict . predictive-latex-label-dict) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (completion-word-thing . predictive-latex-label-word)
	   (completion-dynamic-syntax-alist . ((?w . (add t word))
					       (?_ . (add t word))
					       (?  . (accept t none))
					       (?. . (add t word))
					       (t  . (reject t none))))
	   (completion-dynamic-override-syntax-alist
	    . ((?: . ((lambda ()
			(predictive-latex-completion-add-to-regexp ":"))
		      t word))
	       (?_ . ((lambda ()
			(predictive-latex-completion-add-to-regexp "\\W"))
		      t word))
	       (?} . (accept t none))))			       
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\eqref{" (dict . predictive-latex-label-dict) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (completion-word-thing . predictive-latex-label-word)
	   (completion-dynamic-syntax-alist . ((?w . (add t word))
				       (?_ . (add t word))
				       (?  . (accept t none))
				       (?. . (add t word))
				       (t  . (reject t none))))
	   (completion-dynamic-override-syntax-alist
	    . ((?: . ((lambda ()
			(predictive-latex-completion-add-to-regexp
			 ":"))
		      t word))
	       (?} . (accept t none))))
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\cite{" (dict . t) (priority . 30)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\begin{" (dict . predictive-latex-env-dict) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\end{" (dict . predictive-latex-env-dict) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\text{"
	   (dict . predictive-main-dict) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\documentclass\\(\\[.*\\]\\)?{"
	   (dict . dict-latex-docclass) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start "\\\\bibliographystyle\\(\\[.*\\]\\)?{"
	   (dict . dict-latex-bibstyle) (priority . 30)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  ;; Note: regexps contain a lot of \'s because it has to check whether number
  ;; of \'s in front of { is even or odd. Also, since auto-overlay regexps
  ;; aren't allowed to match across lines, we have to deal with the case of {
  ;; or } at the start of a line separately.
  (auto-overlay-load-compound-regexp
   `(start ("^\\({\\)" . 1) (priority . 30)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(start ("[^\\]\\(\\\\\\\\\\)*\\({\\)" . 2) (priority . 30)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(end ("^\\(}\\)" . 1) (priority . 30)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  (auto-overlay-load-compound-regexp
   `(end ("[^\\]\\(\\\\\\\\\\)*\\(}\\)" . 2) (priority . 30)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'brace)
  
  
  ;; preamble lives between \documentclass{...} and \begin{document}
  (auto-overlay-load-regexp '(nested) 'predictive nil 'preamble)
  (auto-overlay-load-compound-regexp
   '(start "\\\\documentclass\\(\\[.*?\\]\\)?{.*?}"
	   (dict . predictive-latex-preamble-dict) (priority . 20)
	   (completion-menu . predictive-latex-construct-browser-menu))
   'predictive 'preamble)
  (auto-overlay-load-compound-regexp
   '(end "\\\\begin{document}"
	   (dict . predictive-latex-preamble-dict) (priority . 20)
	   (completion-menu . predictive-latex-construct-browser-menu))
   'predictive 'preamble)
	   
  
  ;; \begin{...} and \end{...} start and end LaTeX environments, which we make
  ;; "electric" by using a special env overlay class (defined below) if the
  ;; corresponding customization option is enabled
  (if predictive-latex-electric-environments
      (auto-overlay-load-regexp '(predictive-latex-env)
				'predictive nil 'environment)
    (auto-overlay-load-regexp '(nested)
			      'predictive nil 'environment))
  (auto-overlay-load-compound-regexp
   `(start ("\\\\begin{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
	    0 1)
	   (dict . predictive-latex-math-dict) (priority . 10)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'environment)
  (auto-overlay-load-compound-regexp
   `(end ("\\\\end{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
	  0 1)
	 (dict . predictive-latex-math-dict) (priority . 10)
	 (completion-menu . predictive-latex-construct-browser-menu)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'environment)
  (auto-overlay-load-compound-regexp
   `(start ("\\\\begin{\\(.*?\\)}" 0 1) (priority . 10) (dict . nil)
	   (face . nil))
   'predictive 'environment)
  (auto-overlay-load-compound-regexp
   `(end ("\\\\end{\\(.*?\\)}" 0 1) (priority . 10) (dict . nil)
	 (face . nil))
   'predictive 'environment)

  
  ;; \documentclass defines the document type. Through the use of a special
  ;; "docclass" regexp class defined below, this automagically changes the
  ;; main dictionary if one is defined for the docclass in
  ;; `predictive-latex-docclass-alist'
  (auto-overlay-load-regexp
   '(predictive-latex-docclass
     ("\\\\documentclass\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 2))
   'predictive)
  
  
  ;; \usepackage loads a latex package. Through the use of a special
  ;; "usepackage" regexp class defined below, this automagically loads new
  ;; dictionaries and auto-overlay regexps.
  (auto-overlay-load-regexp
   '(predictive-latex-usepackage
     ("\\\\usepackage\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 2))
   'predictive)

  
  ;; \label creates a cross-reference label. Through the use of a special
  ;; "label" regexp class defined below, this automagically adds the label to
  ;; the label dictionary.
  (auto-overlay-load-regexp
   '(predictive-latex-label ("\\\\label{\\(.*?\\)}" . 1))
   'predictive nil 'label)
)



(defun predictive-latex-load-keybindings ()
  "Load the predictive mode LaTeX key bindings."

  ;; remove AUCTeX bindings so completion ones work
  (local-unset-key [?$])
  (local-unset-key [?\"])
  (local-unset-key [?_])
  (local-unset-key [?^])
  (local-unset-key [?\\])
  (local-unset-key [?-])
  
  ;; make "\", "$", "{" and "}" do the right thing
  (setq predictive-restore-override-syntax-alist
	completion-dynamic-override-syntax-alist)
  (make-local-variable 'completion-dynamic-override-syntax-alist)
  (setq completion-dynamic-override-syntax-alist
	(append
	 completion-dynamic-override-syntax-alist
	 '((?\\ . ((lambda ()
		     (if (and (char-before) (= (char-before) ?\\)
			      (or (not (char-before (1- (point))))
				  (not (= (char-before (1- (point)))
					  ?\\))))
			 'add 'accept))
		   t word))
	   (?$  . (accept t none))
	   (?_ . (accept t none))
	   (?^ . (accept t none))
	   (?{ . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (cond
		     ((auto-overlays-at-point
		       nil '(eq dict predictive-latex-env-dict))
		      (complete-in-buffer ""))
		     ((and (char-before) (= (char-before) ?\\))
		      'word)
		     (t 'none)))))
	   (?} . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'word 'none))))
	   (?\" . ((lambda ()
		     (if (and (char-before) (= (char-before) ?\\))
			 'add 'accept))
		   (lambda ()
		     (if (or (and (char-before) (= (char-before) ?\\))
			     (not (fboundp 'TeX-insert-quote)))
			 t
		       (TeX-insert-quote nil)
		       nil))
		   (lambda ()
		     (if (and (char-before (1- (point)))
			      (= (char-before (1- (point))) ?\\))
			 'word 'none))))
	   (?' . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?( . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	     (?) . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add 'accept))
		    t
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  'word 'none))))
	   (?+ . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?, . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?- . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?\; . ((lambda ()
		     (if (and (char-before) (= (char-before) ?\\))
			 'add 'accept))
		   t
		   (lambda ()
		     (if (and (char-before (1- (point)))
			      (= (char-before (1- (point))) ?\\))
			 'word 'none))))
	   (?< . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?= . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?> . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   (?[ . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	     (?] . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add 'accept))
		    t
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  'word 'none))))
	   (?` . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  t
		  (lambda ()
		    (if (and (char-before (1- (point)))
			     (= (char-before (1- (point))) ?\\))
			'word 'none))))
	   )))
)



;;;=======================================================================
;;;                 Miscelaneous interactive commands

(defun predictive-latex-goto-matching-delim ()
  "If the point is currently on some kind of LaTeX delimeter
\(\\begin{...}, \\end{...}, \\[, \\] or $\), move to its matching
delimeter."
  (interactive)
  
  ;; get innermost LaTeX environment match overlay
  (let ((o-match
	 (car (sort (auto-overlays-at-point
		     nil `((eq auto-overlay-match t)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-parent o-other)

    ;; if we haven't found a match overlay, display a message
    (if (not o-match)
	(message "Not at a LaTeX delimeter")
      ;; otherwise, if other edge of its parent is not matched, display
      ;; message
      (if (not (and (setq o-parent (overlay-get o-match 'parent))
		    (setq o-other
			  (overlay-get o-parent
				       (if (eq o-match
					       (overlay-get o-parent 'start))
					   'end 'start)))))
	  (message "Unmatched LaTeX delimeter")
	;; otherwise, move point to the other edge
	(goto-char (overlay-get o-other
				(if (eq o-other (overlay-get o-parent 'start))
				    'delim-end 'delim-start)))))
    )
)



(defun predictive-latex-goto-delim-start ()
  "If the point is currently within a LaTeX environment or
math-mode, move to the start of it."
  (interactive)
  
  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)
    
    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match
    (if (and overlay (setq o-match (overlay-get overlay 'start)))
	(goto-char (overlay-get o-match 'delim-end))
      (message "Not within a LaTeX environment")))
)



(defun predictive-latex-goto-delim-end ()
  "If the point is currently within a LaTeX environment or
math-mode, move to the end of it."
  (interactive)
  
  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)
    
    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match, otherwise display message
    (if (and overlay (setq o-match (overlay-get overlay 'end)))
	(goto-char (overlay-get o-match 'delim-start))
      (message "Not within a LaTeX environment")))
)



;;; FIXME: generalise label searching to allow cycling through all label
;;;        definitions and references

(defun predictive-latex-goto-label-def ()
  "If the point is on a reference command, move to the label it
refers to."
  (interactive)

  (let ((current-dict (predictive-current-dict))
	label overlay-list filtered-list)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))
    
    ;; when we're not within a referencing command (which we check by checking
    ;; if current dictionary is the label dictionary), display a message,
    ;; otherwise...
    (if (null (member (eval (predictive-latex-label-dict-name)) current-dict))
	(message "Not on LaTeX cross-reference")
      
      ;; get label name and list of label overlays
      (setq label (thing-at-point 'predictive-latex-label-word))
      (setq overlay-list (auto-overlays-in (point-min) (point-max)
					   '((identity auto-overlay)
					     (eq set-id predictive)
					     (eq entry-id label))))
      ;; filter overlay list for matching label name (filtered list should
      ;; only contain one element, unless label is multiply defined)
      (mapc (lambda (o)
	      (when (string= (buffer-substring-no-properties
			      (overlay-start o) (overlay-end o))
			     label)
		(push o filtered-list)))
	    overlay-list)
      ;; goto matching label definition, displaying warning if the label is
      ;; multiply defined
      (when filtered-list
	(goto-char (overlay-start (car filtered-list)))
	(when (> (length filtered-list) 1)
	  (message "LaTeX label \"%s\" is multiply defined" label)))
      ))
)




;;;=======================================================================
;;;    Automatic main dictionary switching based on document class

(put 'predictive-latex-docclass 'auto-overlay-parse-function
     'predictive-latex-parse-docclass-match)
(put 'predictive-latex-docclass 'auto-overlay-suicide-function
     'predictive-latex-docclass-suicide)


(defun predictive-latex-parse-docclass-match (o-match)
  ;; Create a new word overlay for a docclass command, and load and set the
  ;; appropriate dictionaries

  ;; create new word overlay and extract docclass name
  (let ((o-new (auto-o-parse-word-match o-match))
	(docclass (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))
	dict)
    ;; save the docclass in an overlay property
    (overlay-put o-match 'docclass-name docclass)
    ;; update the main dict
    (predictive-latex-docclass-change-main-dict docclass)
    ;; return the new overlay
    o-new)
)



(defun predictive-latex-docclass-suicide (o-match)
  ;; Delete the word overlay for a docclass command, and unload the
  ;; appropriate dictionaries
  
  (let ((docclass (overlay-get o-match 'docclass-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload the dictionary and restore the default main dictionary
    (predictive-latex-docclass-restore-main-dict docclass))
)



(defun predictive-latex-schedule-docclass-update
  (o-self modified &rest unused)
  ;; All docclass overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-docclass-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-docclass-update o-self)))
)



(defun predictive-latex-docclass-update (o-self)
  ;; Update the main dictionary dictionary after a modification, in case
  ;; docclass has changed
  
  (let* ((o-match (overlay-get o-self 'start))
	 (docclass (buffer-substring-no-properties
		    (overlay-get o-match 'delim-start)
		    (overlay-get o-match 'delim-end))))
    ;; if we haven't been deleted by a suicide function, and docclass has
    ;; changed, restore main dict and then change to new one
    (when (and (overlay-buffer o-self)
	       (not (string= docclass (overlay-get o-match 'docclass))))
      (predictive-latex-docclass-restore-main-dict
       (overlay-get o-match 'docclass))
      (predictive-latex-docclass-change-main-dict docclass)))
)



(defun predictive-latex-docclass-change-main-dict (docclass)
  ;; If there is a dictionary associated with the docclass matched by OVERLAY,
  ;; load it and change the main dict

  ;; look for a dictionary associated with the docclass
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))
      ;; if loading of any of the dictionaries in the list fails, unload those
      ;; which succeeded and don't change dictionary
      (if (not (catch 'failed
		 (mapc (lambda (dic)
			 (unless (predictive-load-dict dic)
			   (throw 'failed nil)))
		       dict-list)))
	  (mapc 'predictive-unload-dict dict-list)
	;; otherwise, unload the old main dictionary and change to the new one
	(let ((old-dict predictive-restore-main-dict))
	  (when (atom old-dict) (setq old-dict (list old-dict)))
	  (mapc 'predictive-unload-dict old-dict))
	(setq predictive-main-dict (append dict-list predictive-latex-dict))
	)))
)



(defun predictive-latex-docclass-restore-main-dict (docclass)
  ;; Unload any dictionary that has been loaded for DOCCLASS, and restore the
  ;; default main dict
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))
      (mapc 'predictive-unload-dict dict-list)
      (setq dict-list predictive-restore-main-dict)
      (when (atom dict-list) (setq dict-list (list dict-list)))
      (setq predictive-main-dict (append dict-list predictive-latex-dict))))
)




;;;=======================================================================
;;;  Automatic synchronization of LaTeX \begin{...} \end{...} environments

;;; FIXME: The features provided by this new auto-overlay class should be
;;;        integrated into the standard nested class once they work reliably

(put 'predictive-latex-env 'auto-overlay-parse-function
     'predictive-latex-parse-env-match)
(put 'predictive-latex-env 'auto-overlay-suicide-function
     'predictive-latex-env-suicide)



(defun predictive-latex-parse-env-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a
  ;; nested regexp.

  ;; add synchronization function to match overlay's modification hooks
  (overlay-put o-match 'modification-hooks
	       (append (overlay-get o-match 'modification-hooks)
		       '(predictive-latex-schedule-env-synchronize)))
  
  
  ;; update auto overlays as necessary
  (let* ((overlay-stack (auto-o-nested-stack o-match))
	 (o (car overlay-stack)))
    (cond
     ;; if the stack is empty, just create and return a new unmatched overlay
     ((null overlay-stack)
      (auto-o-make-nested o-match 'unmatched))
     
     ;; if appropriate edge of innermost overlay is unmatched, just match it
     ((or (and (eq (auto-o-edge o-match) 'start)
	       (not (auto-o-start-matched-p o)))
	  (and (eq (auto-o-edge o-match) 'end)
	       (not (auto-o-end-matched-p o))))
      (predictive-latex-match-env-overlay o o-match)
      ;; return nil since haven't created any new overlays
      nil)
     
     ;; otherwise...
     (t
      ;; create new innermost overlay and add it to the overlay stack
      (push (auto-o-make-nested o-match) overlay-stack)
      ;; sort out the overlay stack
      (predictive-latex-env-stack-cascade overlay-stack)
      ;; return newly created overlay
      (car overlay-stack)))
    )
)



(defun predictive-latex-env-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading the stack.
  
  (let* ((overlay-stack (auto-o-nested-stack o-self))
	(o-parent (car overlay-stack)))
    
    (cond
     ;; if other end of parent is unmatched, just delete parent
     ((not (auto-o-edge-matched-p
	    o-parent
	    (if (eq (auto-o-edge o-self) 'start) 'end 'start)))
      (auto-o-delete-overlay o-parent))

     ;; if parent is the only overlay in the stack...
     ((= (length overlay-stack) 1)
      ;; if we're a start match, make parent start-unmatched
      (if (eq (auto-o-edge o-self) 'start)
	  (predictive-latex-match-env-overlay o-parent 'unmatched nil)
	    ;; if we're an end match, make parent end-unmatched
	(predictive-latex-match-env-overlay o-parent nil 'unmatched)))
     
      ;; otherwise, unmatch ourselves from parent and cascade the stack
     (t
      (overlay-put o-parent (auto-o-edge o-self) nil)
      (overlay-put o-self 'parent nil)
      (predictive-latex-env-stack-cascade overlay-stack))
     ))
)



;; Variable used to temporarily disable \begin{...} \end{...} synchronization
;; when the text within a match overlay is being modified
(defvar predictive-latex-disable-env-synchronize nil)


(defun predictive-latex-schedule-env-synchronize
  (o-self &optional modified &rest unused)
  ;; Schedule synchronization of \begin{...} and \end{...} environment names
  (unless (or modified predictive-latex-disable-env-synchronize)
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-env-synchronize o-self))))


(defun predictive-latex-env-synchronize (o-self)
  ;; Synchronize the corresponding start/end match after any modification

  ;; if synchronization has not been disabled, and we haven't been deleted by
  ;; the suicide function...
  (when (and (not predictive-latex-disable-env-synchronize)
	     (overlay-buffer o-self))
    (let ((o-other (overlay-get (overlay-get o-self 'parent)
				(if (eq (auto-o-edge o-self) 'start)
				    'end 'start)))
	  env)
      
      ;; if other end of parent overlay is matched...
      (when o-other
	(save-excursion
	  ;; get environment name from self
	  (goto-char (overlay-start o-self))
	  (when (search-forward-regexp "{\\(.*?\\)}" (overlay-end o-self) t)
	    (setq env (match-string-no-properties 1))
	    ;; replace environment name in other edge
	    (goto-char (overlay-start o-other))
	    (when (and (search-forward-regexp "{\\(.*?\\)}"
					      (overlay-end o-other) t)
		       (not (string= env (match-string-no-properties 1))))
	      (let ((predictive-latex-disable-env-synchronize t))
		;; Have to force `auto-o-run-after-update-functions' to
		;; (recursively) call itself a second time, since doing the
		;; replace-match will schedule some suicides and updates.
		(auto-o-schedule-update (line-number-at-pos (point)))
		;; Note: the replace-match will *not* in fact cause a
		;; synchronization to be scheduled for the other match
		;; overlay, since it is impossible for a function called by
		;; `auto-o-run-after-change-functions' to schedule something
		;; else in the same pending list as itself. Therefore, the
		;; `predictive-latex-disable-env-synchronization' mechanism to
		;; protect against recursion is probably redundant.
		(replace-match env t t nil 1)))
	    )))
      ))
)



(defun predictive-latex-env-stack-cascade (overlay-stack)
  ;; Cascade the ends of the overlays in OVERLAY-STACK up or down the stack,
  ;; so as to re-establish a valid stack. It assumes that only the innermost
  ;; is incorrect.
  
  (let ((o (car overlay-stack)) o1)
    (cond
     
     ;; if innermost overlay is start-matched (and presumably
     ;; end-unmatched)...
     ((auto-o-start-matched-p o)
      ;; cascade overlay end matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(predictive-latex-match-env-overlay o nil
			      (if (overlay-get o1 'end)
				    (overlay-get o1 'end)
				'unmatched)
			      nil nil 'protect-match))
      ;; if final overlay is start-matched, make it end-unmatched, otherwise
      ;; delete it
      (if (auto-o-start-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (predictive-latex-match-env-overlay
	   o1 nil 'unmatch nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match)))
     
     
     ;; if innermost overlay is end-matched (and presumably
     ;; start-unmatched)...
     ((auto-o-end-matched-p o)
      ;; cascade overlay start matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(predictive-latex-match-env-overlay o (if (overlay-get o1 'start)
						   (overlay-get o1 'start)
						 'unmatched)
					     nil nil nil 'protect-match))
      ;; if final overlay is end-matched, make it start-unmatched, otherwise
      ;; delete it
      (if (auto-o-end-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (predictive-latex-match-env-overlay
	   o1 'unmatch nil nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match))))
    )
)



(defun predictive-latex-match-env-overlay
  (overlay start &optional end no-props no-parse protect-match)
  
  ;; match the overlay
  (auto-o-match-overlay overlay start end no-props no-parse protect-match)

  ;; if overlay is now both start and end matched, synchronize end with start
  (setq start (overlay-get overlay 'start))
  (setq end (overlay-get overlay 'end))
  (when (and start end)
    (save-excursion
      (let (env)
	;; get environment name from start
	(goto-char (overlay-get start 'delim-start))
	(when (search-forward-regexp "{\\(.*?\\)}" (overlay-end start) t)
	  (setq env (match-string-no-properties 1))
	  ;; replace environment name in end
	  (goto-char (overlay-start end))
	  (when (and (search-forward-regexp "{\\(.*?\\)}" (overlay-end end) t)
		     (not (string= env (match-string-no-properties 1))))
	    (let ((predictive-latex-disable-env-synchronize t))
	      (replace-match env t t nil 1)))
	  ))))
)




;;;=======================================================================
;;;  Automatic loading and unloading of LaTeX package dictionaries etc.

(put 'predictive-latex-usepackage 'auto-overlay-parse-function
     'predictive-latex-parse-usepackage-match)
(put 'predictive-latex-usepackage 'auto-overlay-suicide-function
     'predictive-latex-usepackage-suicide)


(defun predictive-latex-parse-usepackage-match (o-match)
  ;; Create a new word overlay for a usepackage command, and load the
  ;; appropriate dictionaries

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	package)
    ;; extract package name
    (setq package (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))
    
    ;; save package name in overlay property
    (overlay-put o-match 'package-name package)
    ;; load package dictionaries and run the load function
    (predictive-latex-load-package package)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; return the new overlay
    o-new)
)



(defun predictive-latex-usepackage-suicide (o-match)
  ;; Delete the word overlay for a usepackage command, and unload the
  ;; appropriate dictionaries
  
  (let ((package (overlay-get o-match 'package-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload package dictionaries and run the unload function
    (predictive-latex-unload-package package))
)



(defun predictive-latex-schedule-usepackage-update
  (o-self modified &rest unused)
  ;; All usepackage overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-usepackage-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-usepackage-update o-self)))
)



(defun predictive-latex-usepackage-update (o-self)
  ;; Update the package dictionaries and re-run the load function after a
  ;; modification, in case package name has changed

  (let (package)
    ;; if we haven't been deleted by a suicide function...
    (when (overlay-buffer o-self)
      ;; unload old package dictionaries and run unload function
      (predictive-latex-unload-package
       (overlay-get (overlay-get o-self 'start) 'package-name))
      ;; extract package name
      (setq package (buffer-substring-no-properties
		     (overlay-start o-self)
		     (overlay-end o-self)))
      ;; load new package dictionaries and run load function
      (overlay-put (overlay-get o-self 'start) 'package-name package)
      (predictive-latex-load-package package)))
)



(defun predictive-latex-load-package (package)
  "Try to load the package dictionary and run the load functions
for LaTeX PACKAGE."

  (let (dict)
    ;; try to load package dictionaries and add them to the appropriate lists
    ;; if they exists
    (dolist (dic predictive-latex-dict-classes)
      (setq dict (concat (cdr dic) package))
      (when (predictive-load-dict dict)
	(setq dict (intern-soft dict))
	(if (and (listp (eval (car dic)))
		 (not (dictree-p (eval (car dic)))))
	    (nconc (eval (car dic)) (list dict))
	  (set (car dic) (list (eval (car dic)) dict))))))
  
  ;; try to load lisp library for the package
  (require (intern (concat "predictive-latex-" package)) nil t)
  ;; run load function for package, if one is defined
  (let ((func (nth 1 (assoc package predictive-latex-usepackage-functions))))
    (when func (funcall func)))
  ;; display message
  (message (format "LaTeX package \"%s\" loaded" package))
)



(defun predictive-latex-unload-package (package)
  "Unload the package dictionary and run the unload functions
for LaTeX package PACKAGE."

  (let (dict)
    ;; unload any package dictionaries
    (dolist (dic predictive-latex-dict-classes)
      (when (setq dict (intern-soft (concat (cdr dic) package)))
	(predictive-unload-dict (eval dict))
	(when (and (listp (eval (car dic)))
		   (not (dictree-p (eval (car dic)))))
	  ;; we don't use "(set ... (delq ..." here because other variables
	  ;; may share structure with the dictionary list variables, and the
	  ;; element we want to delete can not be the first one, as that is
	  ;; always the standard dictionary
	  (delq dict (eval (car dic)))))))
  
  ;; try to load lisp library for the package
  (require (intern (concat "predictive-latex-" package)) nil t)
  ;; run unload function for package, if one is defined
  (let ((func (nth 2 (assoc package predictive-latex-usepackage-functions))))
    (when func (funcall func)))
  ;; display informative message
  (message (format "LaTeX package \"%s\" unloaded" package))
)




;;;============================================================
;;;        Automatically generated dictionary of labels

(put 'predictive-latex-label 'auto-overlay-parse-function
     'predictive-latex-parse-label-match)
(put 'predictive-latex-label 'auto-overlay-suicide-function
     'predictive-latex-label-suicide)


(defun predictive-latex-parse-label-match (o-match)
  ;; Create a new word overlay for a label command, and add the label to the
  ;; label dictionary

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	label)
    ;; extract label
    (setq label (buffer-substring-no-properties
		 (overlay-get o-match 'delim-start)
		 (overlay-get o-match 'delim-end)))
    ;; save label in overlay property
    (overlay-put o-match 'label label)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-latex-schedule-label-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-latex-schedule-label-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-latex-schedule-label-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; add label to dictionary
    (predictive-add-to-dict (if (dictree-p predictive-latex-label-dict)
				predictive-latex-label-dict
			      (eval predictive-latex-label-dict))
			    label 0)
    ;; return the new overlay
    o-new)
)
  


(defun predictive-latex-label-suicide (o-match)
  ;; Delete the word overlay for a label command, and delete the label from
  ;; the label dictionary
  
  (let ((label (overlay-get o-match 'label)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; delete the label from the dictionary
    (dictree-delete (if (dictree-p predictive-latex-label-dict)
			predictive-latex-label-dict
		      (eval predictive-latex-label-dict))
		    label))
)



(defun predictive-latex-schedule-label-update
  (o-self modified &rest unused)
  ;; All label overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-label-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-label-update o-self)))
)



(defun predictive-latex-label-update (o-self)
  ;; Update the label dictionary with new label. Run after modification.

  (let (label)
    ;; delete old label from label dictionary
    (dictree-delete (if (dictree-p predictive-latex-label-dict)
			predictive-latex-label-dict
		      (eval predictive-latex-label-dict))
		    (overlay-get (overlay-get o-self 'start) 'label))
    
    ;; if overlay has not been deleted...
    (when (overlay-buffer o-self)
      ;; extract label
      (setq label (buffer-substring-no-properties
		   (overlay-start o-self)
		   (overlay-end o-self)))
      ;; save label in overlay property
      (overlay-put (overlay-get o-self 'start) 'label label)
      ;; add new label to dictionary
      (predictive-add-to-dict (if (dictree-p predictive-latex-label-dict)
				  predictive-latex-label-dict
				(eval predictive-latex-label-dict))
			      label 0)))
)



(defmacro predictive-latex-label-dict-name ()
  ;; Return the label dictionary name
  '(intern
    (concat "dict-latex-label-"
	     (file-name-sans-extension
	      (file-name-nondirectory (buffer-file-name))))))



(defun predictive-latex-load-label-dict ()
  "Load/create the label LaTeX dictionary for the current buffer."
  (let (dictname filename)
    (cond
     ;; if buffer is associated with a file...
     ((buffer-file-name)
      (setq dictname (predictive-latex-label-dict-name))
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    (symbol-name dictname) ".elc"))
      ;; if a label dictionary isn't loaded, load it if it exists, otherwise
      ;; create it
      (unless (featurep dictname)
	(if (not (file-exists-p filename))
	    (predictive-create-dict dictname filename)
	  (load filename)
	  (predictive-load-dict dictname)
	  ;; FIXME: probably shouldn't be using an internal dict-tree.el
	  ;;        function
	  (dictree--set-filename (eval (predictive-latex-label-dict-name))
				 filename)))
      ;; set the label dictionary to the loaded/new dictionary
      (setq predictive-latex-label-dict dictname))
     
     ;; if buffer is not associated with a file, 
     (t
      (setq predictive-latex-label-dict (predictive-create-dict))
      ;; FIXME: shouldn't be using internal dict-tree.el functions. Probably
      ;;        need to make `predictive-create-dict' interface more flexible.
      (dictree--set-name predictive-latex-label-dict "latex-label")
      (dictree--set-autosave predictive-latex-label-dict nil))
     ))
)



(defun predictive-latex-unload-label-dict ()
  "Unload and possibly save the current buffer's LaTeX label dictionary."
  (dictree-unload (if (dictree-p predictive-latex-label-dict)
		      predictive-latex-label-dict
		    (eval predictive-latex-label-dict)))
)



(defun predictive-latex-completion-add-to-regexp (regexp)
  "Add characters up to REGEXP from a completion candidate,
then cause `completion-self-insert' to add the last typed
character and re-complete.

Intended to be used as the \"resolve\" entry in
`completion-dynamic-syntax-alist' or
`completion-dynamic-override-syntax-alist'."
  
  (let (overlay completion)
    ;; if completion characters contain REGEXP, insert characters up to first
    ;; regexp match, and add them to the completion overlay prefix
    (when (and (setq overlay (completion-overlay-at-point))
	       (setq completion (buffer-substring-no-properties
				 (overlay-start overlay)
				 (overlay-end overlay)))
	       (string-match regexp completion))
      
      (insert (setq completion (substring completion 0 (match-beginning 0))))
      (move-overlay overlay (point) (overlay-end overlay))
      (overlay-put overlay 'prefix
		   (concat (overlay-get overlay 'prefix) completion)))

    ;; return 'add, causing `completion-self-insert' to add last typed
    ;; character to the prefix
    'add)
)




;;;=============================================================
;;;                Completion-browser functions

(defun predictive-latex-construct-browser-menu (prefix completions)
  "Construct the LaTeX browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       prefix completions 'predictive-latex-browser-menu-item)))
    (setq menu (butlast menu 2)))
)



(defun predictive-latex-browser-menu-item (prefix completion &rest ignore)
  "Construct predictive LaTeX completion browser menu item."
  
  (cond
   ;; if entry is \begin or \end, create sub-menu containing environment
   ;; completions
   ((or (string= (concat prefix completion) "\\begin")
	(string= (concat prefix completion) "\\end"))
    ;; find all latex environments
    (let ((envs (dictree-complete
		 (mapcar (lambda (dic) (if (dictree-p dic) dic (eval dic)))
			 predictive-latex-env-dict)
		 ""))
	  (menu (make-sparse-keymap)))
      (setq envs (mapcar (lambda (e) (concat completion "{" (car e) "}"))
			 envs))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix envs 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (\begin or \end) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; if entry is \documentclass, create sub-menu containing environment
   ;; completions
   ((string= (concat prefix completion) "\\documentclass")
    ;; find all latex docclasses
    (let ((classes
	   (dictree-mapcar (lambda (word entry) word) dict-latex-docclass))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix classes 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (i.e. \documentclass) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; if entry is \bibliographystyle, create sub-menu containing bib styles
   ((string= (concat prefix completion) "\\bibliographystyle")
    ;; find all bib styles
    (let ((styles
	   (dictree-mapcar (lambda (word entry) word) dict-latex-bibstyle))
	  (menu (make-sparse-keymap)))
      (setq styles
	    (mapcar (lambda (e) (concat completion "{" e "}")) styles))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix styles 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (i.e. \bibliographystyle) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; otherwise, create a selectable completion item
   (t `(lambda () (insert ,completion))))
)




;;;=============================================================
;;;               Miscelaneous utility functions

(defun predictive-latex-forward-word (&optional n)
  (let (m)
    ;; going backwards...
    (if (and n (< n 0))
	(unless (bobp)
	  (setq m (- n))
	  (dotimes (i m)
	    ;; make sure we're at the end of a word
	    (re-search-backward "\\\\\\|\\w")
	    (forward-char)
	    ;; if point is within or just after a sequence of \'s, go
	    ;; backwards for the correct number of \'s
	    (if (= (char-before) ?\\)
		(let ((pos (point)))
		  (save-excursion
		    (while (= (char-before) ?\\) (backward-char))
		    (setq pos (- pos (point))))
		  (if (= (mod pos 2) 1) (backward-char) (backward-char 2)))
	      ;; otherwise, go back one word, plus one \ if there is one
	      (backward-word 1)  ; argument not optional in Emacs 21
	      (when (and (not (bobp)) (= ?\\ (char-before)))
		(backward-char)))))
      
      ;; going forwards...
      (unless (eobp)
	(setq m (or n 1))
	;; deal with point within sequence of \'s
	(when (= (char-after) ?\\)
	  (let ((pos (point)))
	    (save-excursion
	      (while (= (char-before) ?\\) (backward-char))
	      (setq pos (- pos (point))))
	    (when (= (mod pos 2) 1) (backward-char))))
	;; go forward, counting \ as part of word, \\ as entire word
	(dotimes (i m)
	  (re-search-forward "\\\\\\|\\w" nil t)
	  (backward-char)
	  (re-search-forward "\\\\\\W\\|\\\\\\w+\\|\\w+" nil t)
	  (when (= (char-before) ?\n) (backward-char))))
      ))
)



(defun predictive-latex-label-forward-word (&optional n)
  (let (m)
    ;; going backwards...
    (if (and n (< n 0))
	(unless (bobp)
	  (setq m (- n))
	  (when (= ?\\ (char-before))
	    (while (= ?\\ (char-before)) (backward-char))
	    (setq m (1- m)))
	  (dotimes (i m)
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (while (and (char-before)
			(or (= (char-syntax (char-before)) ?w)
			    (= (char-syntax (char-before)) ?_)
			    (= (char-syntax (char-before)) ?.)))
	      (backward-char))))
      ;; going forwards...
      (unless (eobp)
	(setq m (if n n 1))
	(dotimes (i m)
	  (unless (re-search-forward "\\(\\w\\|\\s_\\|\\s.\\)+" nil t)
	    (goto-char (point-max)))))
      ))
)



(defun predictive-latex-reparse-buffer ()
  "Clear all auto-overlays, then reparse buffer from scratch."
  (interactive)
  (auto-overlay-stop 'predictive)
  (auto-overlay-start 'predictive nil 'ignore-save-file 'no-regexp-check)
)

;;; predictive-latex.el ends here
