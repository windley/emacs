
;;; predictive-setup-html.el --- predictive mode HTML setup function


;; Copyright (C) 2005 2007 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.3
;; Keywords: predictive, setup function, html

;; This file is part of the Emacs Predictive Completion package.
;;
;; The Emacs Predicive Completion package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Predicive Completion package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Predicive Completion package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Change Log:
;;
;; Version 0.3
;; * major overhaul to bring it up to date with current auto-overlays,
;;   compltion-ui and predictive code
;;
;; Version 0.2
;; * modified to use the new auto-overlays package
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(require 'auto-overlay-nested)
(require 'auto-overlay-flat)

(provide 'predictive-html)
(add-to-list 'predictive-major-mode-alist
	     '(html-mode . predictive-setup-html))



;; variables used to restore local settings of variables when predictive mode
;; is disabled in a Html buffer
(defvar predictive-restore-main-dict nil)
(make-variable-buffer-local 'predictive-restore-main-dict)
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-color nil)



(defun predictive-setup-html (arg)
  "With a positive ARG, set up predictive mode for use with html major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."

  (cond
   ;; ----- enabling html setup -----
   ((> arg 0)
    
    ;; save overlays and unload regexp definitions before killing buffer
    (add-hook 'kill-buffer-hook
	      (lambda ()
		(auto-overlay-stop 'predictive nil 'save 'leave-overlays)
		(auto-overlay-unload-regexp 'predictive))
	      nil t)
    
    ;; use html browser menu if first character of prefix is "\"
    (make-local-variable 'completion-menu)
    (setq completion-menu
	  (lambda (prefix completions)
	    (if (or (string= (substring prefix 0 1) "<")
		    (string= (substring prefix 0 1) "&"))
		(predictive-html-construct-browser-menu prefix completions)
	      (completion-construct-menu prefix completions))
	    ))
    
    ;; save predictive-main-dict; restored when predictive mode is disabled
    (setq predictive-restore-main-dict predictive-main-dict)
    
    ;; load the dictionaries
    (predictive-load-dict 'dict-html)
    (predictive-load-dict 'dict-html-char-entity)
    ;; general attributes
    (predictive-load-dict 'dict-html-common)
    (predictive-load-dict 'dict-html-core)
    (predictive-load-dict 'dict-html-events)
    (predictive-load-dict 'dict-html-international)
    ;; tag-specific attributes
    (predictive-load-dict 'dict-html-a)
    (predictive-load-dict 'dict-html-area)
    (predictive-load-dict 'dict-html-base)
    (predictive-load-dict 'dict-html-quote)
    (predictive-load-dict 'dict-html-body)
    (predictive-load-dict 'dict-html-button)
    (predictive-load-dict 'dict-html-col)
    (predictive-load-dict 'dict-html-del)
    (predictive-load-dict 'dict-html-form)
    (predictive-load-dict 'dict-html-head)
    (predictive-load-dict 'dict-html-img)
    (predictive-load-dict 'dict-html-input)
    (predictive-load-dict 'dict-html-ins)
    (predictive-load-dict 'dict-html-label)
    (predictive-load-dict 'dict-html-legend)
    (predictive-load-dict 'dict-html-link)
    (predictive-load-dict 'dict-html-map)
    (predictive-load-dict 'dict-html-meta)
    (predictive-load-dict 'dict-html-object)
    (predictive-load-dict 'dict-html-optgroup)
    (predictive-load-dict 'dict-html-option)
    (predictive-load-dict 'dict-html-param)
    (predictive-load-dict 'dict-html-script)
    (predictive-load-dict 'dict-html-select)
    (predictive-load-dict 'dict-html-style)
    (predictive-load-dict 'dict-html-table)
    (predictive-load-dict 'dict-html-td)
    (predictive-load-dict 'dict-html-textarea)
    (predictive-load-dict 'dict-html-tr)
    
    ;; add html dictionaries to main dictionary list
    (make-local-variable 'predictive-main-dict)
    (when (atom predictive-main-dict)
      (setq predictive-main-dict (list predictive-main-dict)))
    (setq predictive-main-dict
	  (append predictive-main-dict '(dict-html dict-html-char-entity)))
    
    ;; delete any existing predictive auto-overlay regexps and load html
    ;; auto-overlay regexps
    (auto-overlay-unload-regexp 'predictive)
    (predictive-html-load-regexps)
    (auto-overlay-start 'predictive)
    
    ;; load the keybindings and related settings
    (predictive-html-load-keybindings)
    ;; consider \ as start of a word
    (setq completion-word-thing 'predictive-html-word)
    
    t)  ; indicate successful setup


   ;; ----- disabling html setup -----
   ((< arg 0)
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
    ;; remove hook function that saves overlays
    (remove-hook 'kill-buffer-hook
		 (lambda ()
		   (auto-overlay-stop 'predictive nil 'save 'leave-overlays)
		   (auto-overlay-unload-regexp 'predictive))
		 t)
    
    t))  ; indicate successful reversion of changes
)





(defun predictive-html-load-regexps ()
  "Load the predictive mode html auto-overlay regexp decinitions."

  ;; "<!--" and "-->" delimit comments
  (auto-overlay-load-regexp '(flat) 'predictive nil 'comment)
  (auto-overlay-load-compound-regexp
   `(start "<!--" (dict . predictive-main-dict) (priority . 20)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'comment)
  (auto-overlay-load-compound-regexp
   `(end "-->" (dict . predictive-main-dict) (priority . 20)
	 (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'comment)
  
  ;; "<..." starts various tags, ended by ">". "<" makes sure all other ">"s
  ;; are matched
  (auto-overlay-load-regexp '(nested) 'predictive nil 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<a\\( \\|$\\)"
	   (dict . (dict-html-a dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<area\\( \\|$\\)"
	   (dict . (dict-html-area dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<base\\( \\|$\\)"
	   (dict . dict-html-base)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<bdo\\( \\|$\\)"
	   (dict . (dict-html-international dict-html-core))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<\\(blockquote\\|q\\)\\( \\|$\\)"
	   (dict . (dict-html-quote dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<body\\( \\|$\\)"
	   (dict . (dict-html-body dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<br\\( \\|$\\)"
	   (dict . dict-html-core)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<button\\( \\|$\\)"
	   (dict . (dict-html-button dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<col\\( \\|$\\)"
	   (dict . (dict-html-col dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<colgroup\\( \\|$\\)"
	   (dict . (dict-html-col dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<del\\( \\|$\\)"
	   (dict . (dict-html-del dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<form\\( \\|$\\)"
	   (dict . (dict-html-form dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<head\\( \\|$\\)"
	   (dict . (dict-html-head dict-html-international))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<hr\\( \\|$\\)"
	   (dict . (dict-html-core dict-html-events))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<html\\( \\|$\\)"
	   (dict . dict-html-international)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<img\\( \\|$\\)"
	   (dict . (dict-html-img dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<input\\( \\|$\\)"
	   (dict . (dict-html-input dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<ins\\( \\|$\\)"
	   (dict . (dict-html-ins dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<label\\( \\|$\\)"
	   (dict . (dict-html-label dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<legend\\( \\|$\\)"
	   (dict . (dict-html-legend dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<link\\( \\|$\\)"
	   (dict . (dict-html-link dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<map\\( \\|$\\)"
	   (dict . (dict-html-map dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<meta\\( \\|$\\)"
	   (dict . (dict-html-meta dict-html-international))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<object\\( \\|$\\)"
	   (dict . (dict-html-object dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<optgroup\\( \\|$\\)"
	   (dict . (dict-html-optgroup dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<option\\( \\|$\\)"
	   (dict . (dict-html-option dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<param\\( \\|$\\)"
	   (dict . dict-html-param)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<script\\( \\|$\\)"
	   (dict . dict-html-script)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<select\\( \\|$\\)"
	   (dict . (dict-html-select dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<style\\( \\|$\\)"
	   (dict . (dict-html-style dict-html-international))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<table\\( \\|$\\)"
	   (dict . (dict-html-table dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<t\\(r\\|body\\|head\\|foot\\)\\( \\|$\\)"
	   (dict . (dict-html-tr dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<t[dh]\\( \\|$\\)"
	   (dict . (dict-html-td dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<textarea\\( \\|$\\)"
	   (dict . (dict-html-textarea dict-html-common))
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<title\\( \\|$\\)"
	   (dict . dict-html-international)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   `(start "<[[:alnum:]]+?\\( \\|$\\)"
	   (dict . dict-html-common)
	   (priority . 10)
	   (face . (background-color . ,predictive-overlay-debug-color)))
   'predictive 'tag)
  (auto-overlay-load-compound-regexp
   '(end ">" (priority . 10))
   'predictive 'tag)
)



(defun predictive-html-load-keybindings ()
  "Load the predictive mode html key bindings."
  
  ;; make "<", ">", and "&" do the right thing
  (setq predictive-restore-override-syntax-alist
	completion-dynamic-override-syntax-alist)
  (make-local-variable 'completion-dynamic-override-syntax-alist)
  (setq completion-dynamic-override-syntax-alist
	(append completion-dynamic-override-syntax-alist
		'((?< . (accept t word))
		  (?> . (accept t none))
		  (?& . (accept t word)))
		))
)



(defun predictive-html-construct-browser-menu (prefix completions)
  "Construct the html browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       prefix completions 'completion-browser-menu-item)))
    (setq menu (butlast menu 2)))
)

;;; predictive-setup-html.el ends here
