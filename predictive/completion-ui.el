
;;; completion-ui.el --- in-buffer completion user interface


;; Copyright (C) 2006-2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.6.4
;; Keywords: completion, ui, user interface
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
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


;;; Commentary:
;;
;; This package provides a user-interface for in-buffer text
;; completion. It doesn't find completions itself. Instead, a completion
;; package can simply set the `completion-function' variable to a
;; function that takes two arguments, a string PREFIX and an integer
;; MAXNUM, and returns a list of at most MAXNUM completion candidates
;; for PREFIX. Completion-UI does the rest.
;;
;; That's it! Completion-UI, the auto-completion minor mode, and user
;; customizations take care of the rest. (Avoid the temptation to set
;; completion-UI customization variables from Elisp code to alter its
;; behaviour. The user knows what they want better than you do!)
;;
;; Examples are available on the Emacs wiki, at:
;; www.emacswiki.org/cgi-bin/wiki/CompletionUI
;; 
;; Why use completion-UI? Typically, a lot of code in packages providing
;; some kind of text completion deals with the user interface. The
;; ultimate goal is that all packages providing in-buffer (and possibly
;; one day also mini-buffer) completion should use this package to
;; provide a common user interface, freeing them to concentrate on
;; finding the completion candidates in the first place. The Elisp
;; programmer benfits by not having to reinvent the wheel, and the Emacs
;; user benefits by having a standard yet highly customizable
;; user-interface that they can customize once and for all to to suit
;; their preferences, for all completion mechanisms they use.
;;
;; Various completion user-interfaces are provided, all of which can be
;; individually enabled, disabled and extensively tweaked via
;; customization variables:
;;
;; * Dynamic completion: provisionally insert the first available
;;   completion candidate into the buffer, and accept, reject or update
;;   it based on character syntax.
;;
;; * Completion hotkeys: single-key selection of a completion
;;   candidate.
;;
;; * Cycling: cycle through completion candidates.
;;
;; * Tab-completion: "traditional" expansion to longest common
;;   substring.
;;
;; * Help-echo: display a list of completion candidates in the
;;   echo-area.
;;
;; * Tooltip: display a list of completion candidates in a tool-tip
;;   located below the point.
;;
;; * Completion menu: allow completion candidates to be selected from
;;   a drop-down menu located below the point.
;;
;; * Completion browser: browse through all possible completion
;;   candidates in a hierarchical menu located below the point.
;;
;; Completion-UI also provides a new minor mode, called
;; auto-completion-mode. When enabled, Emacs will automatically complete
;; words as they are typed, using the `completion-function' to find
;; completion candidates. The same customization variables determine how
;; those candidates are displayed and can be selected. This works
;; particularly well with dynamic completion (see above).
;;
;; This package will work alongside the auto-overlays package if it's
;; available, but does not require it.



;;; Change Log:
;;
;; Version 0.6.4
;; * defined properties to make delete-selection-mode work correctly (thanks
;;   to Sivaram for drawing my attention to this)
;; * minor improvement to text displayed in completion browser bucket menu
;;   entries
;;
;; Version 0.6.3
;; * fixed M-<space> bindings so that prefix argument is passed to
;;   `completion-reject', and fixed C-<space> bindings
;;
;; Version 0.6.2
;; * modified the default `completion-dynamic-syntax-alist' to make
;;   parentheses behave like punctuation
;; * minor bug-fix to `completion-show-menu-if-within-overlay'
;; * fixed `completion-self-insert' again so that it works if called with an
;;   explicit char (auto-fill will not work in that case)
;; * fixed `complete-dynamic' so that the completion overlay ends up in the
;;   right place even when modification hooks cause text to be inserted in the
;;   buffer during its execution
;;
;; Version 0.6.1
;; * modified define-minor-mode usage for auto-completion-mode to work in
;;   older Emacs versions
;; * fixed `completion-self-insert' so that auto-fill works again
;; * if command remapping isn't supported, attempt to simulate it more
;;   effectively for deletion commands
;;
;; Version 0.6
;; * added `completion-prefix' and `completion-tooltip' variables to
;;   allow overriding of default methods for determining prefix at point
;;   and constructing tooltip text
;; * fixed bugs related to backwards-deletion (thanks to Maciej
;;   Katafiasz for pointing some of these out)
;; * added optional arguements to `completion-self-insert' to allow
;;   automatically determined character and syntax to be overridden, and
;;   created key bindings to insert characters as word constituents
;; * modified `completion-backward-delete', created corresponding
;;   `completion-delete' function, and defined a whole host of deletion
;;   and kill commands that are substituted for the standard ones
;; * added convenience function
;;   `completion-define-word-constituent-binding' for defining bindings
;;   to insert characters as word-constituents
;;   
;;
;; Version 0.5.2
;; * fixed tooltip face issues, which included defining a new
;;   `completion-tooltip-face'
;; * implemented better method of positioning tooltip, avoiding moving
;;   the mouse (thanks to Nikolaj Schumacher for this!)
;;
;; Version 0.5.1
;; * fixed small bug in `completion-self-insert' (thanks to Nikolaj
;;   Schumacher for pointing it out)
;;
;; Version 0.5
;; Modifications arising from discussions with rms:
;; * removed `completion-define-minor-mode' macro; to use completion-UI,
;;   `completion-function' should just be set appropriately
;; * auto-completion is now a separate minor mode
;; * renamed various variables and functions
;;
;; Version 0.4.1
;; * small but important bug-fix to `completion-accept'
;;
;; Version 0.4
;; * accept and reject hooks now called with two or three arguments
;;   instead of one: the prefix, the full word (this is what was passed
;;   previously) and possibly the interactive prefix argument.
;; * moved some anonymous commands into named functions to sanitize
;;   key-bindings
;;
;; Version 0.3.13
;; * Tried to work around annoying `completion-select' bug
;;
;; Version 0.3.12
;; * added `completion-backward-delete-delay' customization option
;;
;; Version 0.3.11
;; * finally figured out how to prevent list of completions displayed in
;;   echo area from being logged
;;
;; Version 0.3.10
;; * fixed start-of-word behaviour in `completion-self-insert'
;;
;; Version 0.3.9
;; * `completion-select' now uses the `completion-trap-recursion'
;;   variable, instead of testing if 'trap-recursion is bound
;;
;; Version 0.3.8
;; * fixed `completion-run-if-within-overlay' so it doesn't error if
;;   there's no "normal" binding for the key sequence used to invoke it
;; * defined a new `completion-trap-recursion' variable in case the
;;   symbol trap-recursion is bound outside
;;   `completion-run-if-within-overlay'
;;
;; Version 0.3.7
;; * fixed M-<space> binding so it's only active within an overlay
;;
;; Version 0.3.6
;; * fixed bug in `completion-define-minor-mode'
;;
;; Version 0.3.5
;; * added eval-when-compile to prevent bogus compilation errors
;;
;; Version 0.3.4
;; * added function to `after-change-functions' to hide tooltip
;; * made self-insert behaviour alists more flexible
;; * minor fix to `completion-cycle' to leave point at end of word if
;;   dynamic completion is disabled
;; * `completion-hotkey-list' no longer a customization option, since it
;;   must be set *before* completion-ui.el is loaded
;;
;; Version 0.3.3
;; * minor bug-fix to `completion-self-insert'
;; * removed cl dependency
;;
;; Version 0.3.2
;; * bug fixes
;; * incorporated compatability code
;;
;; Version 0.3.1
;; * bug fixes
;;
;; Version 0.3
;; * incorporated a lot of code from predictive.el
;; * rewrote things so that all a package needs to do is set
;;   the `completion-function' variable
;; * `completon-overlay-at-point' is kludgy no more
;;
;; Version 0.2.1
;; * added commentary
;; * prevented any attempt to display tooltips and menus when not
;;   running X
;;
;; Version 0.2
;; * bug fixes (thanks to Mark Zonzon for patch)
;; * added `completion-min-chars' and `completion-delay' options
;;   (thanks to Jin Tong for suggestions)
;; * renamed to `completion-ui.el'
;; 
;; Version 0.1
;; * initial release


;;; Code:

(provide 'completion-ui)
(require 'auto-overlay-common nil t)




;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui nil
  "Completion user interface."
  :group 'convenience)


(defcustom completion-max-candidates 10
  "*Maximum number of completion candidates to offer."
  :group 'completion-ui
  :type 'integer)


(defcustom completion-resolve-old-method 'leave
  "*Determines what to do if there's already a completion in
progress elsewhere in the buffer:

  'leave:   leave the old completion pending
  'accept:  automatically accept the old completion
  'reject:  automatically reject the old completion
  'ask:     ask what to do with the old completion"
  :group 'completion-ui
  :type '(choice (const :tag "leave" leave)
		 (const :tag "accept" accept)
		 (const :tag "reject" reject)
		 (const :tag "ask" ask)))



;;; ===== Auto-completion customizations =====

(defcustom auto-completion-min-chars nil
  "*Minimum number of characters before completions are offered."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))


(defcustom auto-completion-delay nil
  "*Number of seconds to wait before activating completion mechanisms
in auto-completion mode."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (float :tag "On")))


(defcustom auto-completion-backward-delete-delay 0.1
  "*Number of seconds to wait before activating completion mechanisms
after deleting backwards in auto-completion mode."
  :group 'completion-ui
  :type 'float)



;;; ===== Dynamic completion customizations =====

(defcustom completion-use-dynamic t
  "*Enable dynamic completion."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-dynamic-syntax-alist
  '(
    ;; word constituents add to current completion
    (?w . (add t word))
    (?_ . (accept t none))
    ;; whitespace and punctuation chars accept current completion
    (?  . (accept t none))
    (?. . (accept t none))
    (?\( . (accept t none))
    (?\) . (accept t none))
    ;; anything else rejects the current completion
    (t  . (reject t none)))
  "*Alist associating character syntax with completion behaviour.
Used by the `completion-self-insert' function to decide what to
do based on a typed character's syntax. The car should be a
syntax descriptor, the cdr a list, BEHAVIOUR, whose first entry
defines the behaviour before a character is inserted, and whose
last entry defines the completion behaviour after inserting. (The
second entry determines whether the typed character is inserted
or not, but this can only be modified by lisp packages.)

If the FIRST ENTRY of BEHAVIOUR is 'accept or 'reject, characters
with the associated syntax accept or reject any provisional
completion candidate at the point. 'add causes the character to
be added to the any completion prefix at the point.

If the last entry of BEHAVIOUR is 'basic, the prefix at the point
will be completed. If it is 'word, it will produce more advanced
completion behaviour that tries to do the right thing with the
word at the point. If it is 'none, no completion will take
place."
  :group 'completion-ui
  :type '(alist :key-type (choice character (const :tag "default" t))
		:value-type (list (choice (const :tag "accept" accept)
					  (const :tag "reject" reject)
					  (const :tag "add" add))
				  (const t)
				  (choice (const :tag "basic" basic)
					  (const :tag "word" word)
					  (const :tag "none" none)))))


(defcustom completion-dynamic-override-syntax-alist
  '((?0 . (reject t none))
    (?1 . (reject t none))
    (?2 . (reject t none))
    (?3 . (reject t none))
    (?4 . (reject t none))
    (?5 . (reject t none))
    (?6 . (reject t none))
    (?7 . (reject t none))
    (?8 . (reject t none))
    (?9 . (reject t none)))
  "*Alist associating characters with completion behaviour.
Overrides the default behaviour defined by the character's syntax
in `completion-dynamic-syntax-alist'. The format is the same as for
`completion-dynamic-synax-alist', except that the alist keys are
characters rather than syntax descriptors."
  :group 'completion-ui
  :type '(alist :key-type character
		:value-type (cons (choice (const :tag "accept" accept)
					  (const :tag "reject" reject)
					  (const :tag "add" add))
				  (choice (const :tag "basic" basic)
					  (const :tag "word" word)
					  (const :tag "none" nil)))))

(defface completion-dynamic-face
  '((((class color) (background dark))
     (:background "blue" :foreground "white"))
    (((class color) (background light))
     (:background "orange1" :foreground "black")))
  "*Face used for provisional completions during dynamic completion.
Also used to highlight selected completion in tooltip."
  :group 'completion-ui)



;; ===== Hotkey customizations =====

(defcustom completion-use-hotkeys t
  "*Enable completion hotkeys (single-key selection of completions)."
  :group 'completion-ui
  :type 'boolean)


;; not a defcustom, since setting it after loading completion-ui.el (as
;; defcustom typically will ) does not work
(defvar completion-hotkey-list
  '([?0] [?1] [?2] [?3] [?4] [?5] [?6] [?7] [?8] [?9])
  "*List of keys (vectors) to use for selecting completions
when `completion-use-hotkeys' is enabled. This variable must be
set *before* completion-ui.el is laoded to take effect.")



;;; ===== Tooltip customizations =====

(defcustom completion-use-tooltip t
  "*Display completions in a tooltip."
  :group 'completion-ui
  :type 'boolean)

(defcustom completion-tooltip-delay 3
  "*Number of seconds to wait after activating completion
mechanisms before displaying completions in a tooltip."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (float :tag "On")))

(defcustom completion-tooltip-timeout 15
  "*Number of seconds to display completions in a tooltip
\(not relevant if help-echo text is displayed in echo area\)."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))

(defcustom completion-tooltip-offset '(0 . 0)
   "Pixel offset for tooltip.
This sometimes needs to be tweaked manually to get tooltip in
correct position on different operating systems."
  :group 'completion-ui
  :type '(cons integer integer))

(defface completion-tooltip-face
  `((t (:background ,(or (face-attribute 'menu :background) "white")
        :foreground ,(or (face-attribute 'menu :foreground) "black"))))
  "*Face used in tooltip. Only foreground and background attributes are\
 used."
  :group 'completion-ui)



;;; ===== Completion menu customizations =====

(defcustom completion-auto-show-menu nil
  "*Display completion menu automatically."
  :group 'completion-ui
  :type 'boolean)

(defcustom completion-browser-max-items 25
  "*Maximum number of completions to display
in a completion browser submenu."
  :group 'predictive
  :type 'integer)


(defcustom completion-browser-buckets 'balance
  "*Policy for choosing number of buckets in completion browser:

balance:  balance number of buckets and size of content
maximize: maximize number of buckets, minimize size of contents
mininize: minimize number of buckets, maximize size of contents"
  :group 'predictive
  :type '(choice (const :tag "balance" balance)
		 (const :tag "maximize" max)
		 (const :tag "minimize" min)))



;;; ===== Echo-text customizations =====

(defcustom completion-use-echo t
  "*Display completions in echo area."
  :group 'completion-ui
  :type 'boolean)





;;; ============================================================
;;;                 Other configuration variables

(defvar completion-function nil
  "Function that accepts two arguments, PREFIX and MAXNUM,
and returns a list of at most MAXNUM completion candidates for
the PREFIX string. If MAXNUM is nil, it should return all
completion candidates for PREFIX.")

(make-variable-buffer-local 'completion-function)


(defvar completion-prefix nil
  "Function that finds a prefix to complete at the point.
It should return the prefix to complete as a string.

Used by `complete-word-at-point' and
`completion-backward-delete'.")


(defvar completion-word-thing 'word
  "Symbol used to determine what is considered a word.

Used by `complete-word-at-point' and `completion-backward-delete'
in calls to `thing-at-point'.  See `thing-at-point' for more
details. It is ignored if `completion-prefix' is set, since that
is used instead.")
(make-variable-buffer-local 'completion-word-thing)


(defvar completion-menu nil
  "Menu keymap for completion menu, or a function to call
to get a menu keymap. The function is called with two arguments,
prefix and completions, and should return a menu keymap.")


(defvar completion-browser-menu nil
  "Menu keymap for the completion browser, or a function to call
to get a menu keymap.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-tooltip nil
  "Function to call to construct the tooltip text.

The function is called with three arguments, the prefix,
completions, and index of the currently active completion. It
should return a string containing the text to be displayed in the
tooltip.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-accept-functions nil
  "Hook run after a completion is accepted.

Completions are accepted by calling `completion-accept',
selecting one with a hotkey, or selecting one from a
menu. Functions are passed three arguments: the prefix, the
complete string that was accepted \(the concatenation of the
prefix and the accepted completion string\), and any prefix
argument supplied to and interactive accept command.")


(defvar completion-reject-functions nil
  "Hook run after a completion is rejected.

Completions are rejected by calling
`completion-reject'. Functions are passed three arguments: the
prefix, the complete string that was rejected \(the concatenation
of the prefix and the rejected completion string\), and any
prefix argument supplied to an interactive rejection command.")


;; (defvar completion-tab-complete-functions nil
;;   "Hook run after tab-completion.
;; Functions are passed two arguments: the complete string that has
;; been inserted so far \(prefix and tab-completion combined\).")


(defvar completion-map nil
  "Keymap active when a completion-function is defined.")


(defvar auto-completion-map nil
  "Keymap active when auto-completion-mode is enabled.")


(defvar completion-hotkey-map nil
  "Keymap used for hotkey completion (single-key selection of
  completions).

  Do NOT bind keys in this keymap directly. The keymap is
  constructed automatically from `completion-hotkey-list'. You
  should modify that instead, before `completion-ui.el' is
  loaded.")


(defvar completion-dynamic-map nil
  "Keymap active in a dynamic completion overlay.")




;;; ============================================================
;;;                     Internal variables

(defvar completion-overlay-list nil
  "List of overlays used during completion")
(make-variable-buffer-local 'completion-overlay-list)


(defvar completion-tooltip-timer (timer-create)
  "Timer used to postpone tooltip until there's a pause in typing.")


(defvar completion-backward-delete-timer nil
  "Timer used to postpone completion until finished deleting.")


(defvar completion-trap-recursion nil
  "Used to trap recursive calls to certain completion functions")




;;; =================================================================
;;;                 Set modification hook functions

(add-hook 'after-change-functions
	  (lambda (&rest unused) (completion-cancel-tooltip)))




;;; =================================================================
;;;            Set properties for delete-selection-mode

(put 'completion-self-insert 'delete-selection t)
(put 'completion-accept-and-newline 'delete-selection t)
(put 'completion-backward-delete-char 'delete-selection 'supersede)
(put 'completion-backward-delete-char-untabify 'delete-selection 'supersede)
(put 'completion-delete-char 'delete-selection 'supersede)




;;; =================================================================
;;;                     Setup default keymaps

;; Set the default keymap if it hasn't been defined already (most likely
;; in an init file)
(unless completion-map
  (let ((map (make-sparse-keymap)))
    
    ;; M-<tab> cycles or completes word at point
    (define-key map [?\M-\t]
      (lambda ()
	"Cycle through available completions if there are any,\
 otherwise complete the word at point."
	(interactive)
	(if (completion-overlay-at-point)
	    (completion-cycle)
	  (complete-word-at-point))))
    ;; M-<shift>-<tab> cycles backwards
    (define-key map '[(meta shift iso-lefttab)]
      (lambda ()
	"Cycle backwards through completions if there are any,\
 otherwise complete the word at point."
	(interactive)
	(if (completion-overlay-at-point)
	    (completion-cycle -1)
	  (complete-word-at-point))))
    ;; if command remapping is supported, remap delete commands
    (if (fboundp 'command-remapping)
	(progn
	  (define-key map [remap delete-char]
	    'completion-delete-char)
	  (define-key map [remap backward-delete-char]
	    'completion-backward-delete-char)
	  (define-key map [remap delete-backward-char]
	    'completion-backward-delete-char)
	  (define-key map [remap backward-delete-char-untabify]
	    'completion-backward-delete-char-untabify)
	  (define-key map [remap kill-word]
	    'completion-kill-word)
	  (define-key map [remap backward-kill-word]
	    'completion-backward-kill-word)
	  (define-key map [remap kill-sentenve]
	    'completion-kill-sentenve)
	  (define-key map [remap backward-kill-sentenve]
	    'completion-backward-kill-sentenve)
	  (define-key map [remap kill-sexp]
	    'completion-kill-sexp)
	  (define-key map [remap backward-kill-sexp]
	    'completion-backward-kill-sexp)
	  (define-key map [remap kill-paragraphs]
	    'completion-kill-paragraph)
	  (define-key map [remap backward-kill-paragraph]
	    'completion-backward-kill-paragraph))
      ;; otherwise, can't do better than define bindings for the keys that are
      ;; currently bound to them
      (dolist (key '([delete] [deletechar] [backspace] "\d"
		     [(control delete)] [(control deletechar)]
		     [(meta delete)] [(meta deletechar)]
		     [(control backspace)] [(meta backspace)] "\M-\d"))
	(catch 'rebound
	  (dolist (binding '((delete-char . completion-delete-char)
			     (kill-word . completion-kill-word)
			     (kill-sentence . completion-kill-sentence)
			     (kill-sexp . completion-kill-sexp)
			     (kill-paragraph . completion-kill-paragraph)
			     (backward-delete-char
			      . completion-backward-delete-char)
			     (delete-backward-char
			      . completion-backward-delete-char)
			     (backward-delete-char-untabify
			      . completion-backward-delete-char-untabify)
			     (backward-kill-word
			      . completion-backward-kill-word)
			     (backward-kill-sentence
			      . completion-backward-kill-sentence)
			     (backward-kill-sexp
			      . completion-backward-kill-sexp)
			     (backward-kill-paragraph
			      . completion-backward-kill-paragraph)))
	    (when (eq (key-binding key) (car binding))
	      (define-key map key (cdr binding))
	      (throw 'rebound t)))))
      )
    
    
    ;; If the current Emacs version doesn't support overlay keybindings
    ;; half decently, have to simulate them using
    ;; `completion-run-if-within-overlay' hack in the main
    ;; `completion-map'.
    (when (<= emacs-major-version 22)
      ;; <tab> does traditional tab-completion
      (define-key map "\t" 'completion-tab-complete-if-within-overlay)
      ;; RET accepts any pending completion candidate, then runs
      ;; whatever is usually bound to RET
      (define-key map "\r" 'completion-accept-if-within-overlay)
      ;; M-<down> displays the completion menu
      (define-key map [M-down] 'completion-show-menu-if-within-overlay)
      
      ;; C-<space> abandons
      (define-key map [?\C- ] 'completion-reject-if-within-overlay)
      ;; M-<space> abandons and inserts a space
      (define-key map "\M- "
	(lambda ()
	  "Reject current provisional completion and insert a space."
	  (interactive)
	  (completion-run-if-within-overlay
	   (lambda (&optional arg) (interactive "P")
	     (completion-reject arg) (insert " "))
	   'completion-function)))
      ;; M-. inserts "." as a word-constituent
      (define-key map "\M-."
	(lambda () "Insert \".\" as though it were a word-constituent."
	  (interactive)
	  (completion-run-if-within-overlay
	   (lambda () (interactive)
	     (completion-self-insert ?. ?w))
	   'completion-function)))
      ;; M-- inserts "-" as a word-constituent
      (define-key map "\M--"
	(lambda () "Insert \"-\" as though it were a word-constituent."
	  (interactive)
	  (completion-run-if-within-overlay
	   (lambda () (interactive)
	     (completion-self-insert ?- ?w))
	   'completion-function)))
      ;; M-/ inserts "/" as a word-constituent
      (define-key map "\M-/"
	(lambda () "Insert \"/\" as though it were a word-constituent."
	  (interactive)
	  (completion-run-if-within-overlay
	   (lambda () (interactive)
	     (completion-self-insert ?/ ?w))
	   'completion-function)))
      )
    
    (setq completion-map map)))


;; make sure completion-map is associated with `completion-function' in
;; the minor-mode-keymap-alist, so that the M-tab bindings are enabled
;; whenever a completion function is defined
(let ((existing (assq 'completion-function minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-map)
    (push (cons 'completion-function completion-map)
	  minor-mode-map-alist)))

    


;; Set the default auto-completion-mode keymap if it hasn't been defined
;; already (most likely in an init file or setup function)
(unless auto-completion-map
  (let (map)
    ;; if we can remap keys, do that
    (if (fboundp 'command-remapping)
	(progn
	  (setq map (make-sparse-keymap))
	  ;; remap printable characters to run completion-self-insert
	  (define-key map [remap self-insert-command]
	    'completion-self-insert))

      ;; otherwise, create a great big keymap where all printable
      ;; characters run completion-self-insert, which decides what to
      ;; do based on the character's syntax
      (setq map (make-keymap))
      (define-key map "A" 'completion-self-insert)
      (define-key map "a" 'completion-self-insert)
      (define-key map "B" 'completion-self-insert)
      (define-key map "b" 'completion-self-insert)
      (define-key map "C" 'completion-self-insert)
      (define-key map "c" 'completion-self-insert)
      (define-key map "D" 'completion-self-insert)
      (define-key map "d" 'completion-self-insert)
      (define-key map "E" 'completion-self-insert)
      (define-key map "e" 'completion-self-insert)
      (define-key map "F" 'completion-self-insert)
      (define-key map "f" 'completion-self-insert)
      (define-key map "G" 'completion-self-insert)
      (define-key map "g" 'completion-self-insert)
      (define-key map "H" 'completion-self-insert)
      (define-key map "h" 'completion-self-insert)
      (define-key map "I" 'completion-self-insert)
      (define-key map "i" 'completion-self-insert)
      (define-key map "J" 'completion-self-insert)
      (define-key map "j" 'completion-self-insert)
      (define-key map "K" 'completion-self-insert)
      (define-key map "k" 'completion-self-insert)
      (define-key map "L" 'completion-self-insert)
      (define-key map "l" 'completion-self-insert)
      (define-key map "M" 'completion-self-insert)
      (define-key map "m" 'completion-self-insert)
      (define-key map "N" 'completion-self-insert)
      (define-key map "n" 'completion-self-insert)
      (define-key map "O" 'completion-self-insert)
      (define-key map "o" 'completion-self-insert)
      (define-key map "P" 'completion-self-insert)
      (define-key map "p" 'completion-self-insert)
      (define-key map "Q" 'completion-self-insert)
      (define-key map "q" 'completion-self-insert)
      (define-key map "R" 'completion-self-insert)
      (define-key map "r" 'completion-self-insert)
      (define-key map "S" 'completion-self-insert)
      (define-key map "s" 'completion-self-insert)
      (define-key map "T" 'completion-self-insert)
      (define-key map "t" 'completion-self-insert)
      (define-key map "U" 'completion-self-insert)
      (define-key map "u" 'completion-self-insert)
      (define-key map "V" 'completion-self-insert)
      (define-key map "v" 'completion-self-insert)
      (define-key map "W" 'completion-self-insert)
      (define-key map "w" 'completion-self-insert)
      (define-key map "X" 'completion-self-insert)
      (define-key map "x" 'completion-self-insert)
      (define-key map "Y" 'completion-self-insert)
      (define-key map "y" 'completion-self-insert)
      (define-key map "Z" 'completion-self-insert)
      (define-key map "z" 'completion-self-insert)
      (define-key map "'" 'completion-self-insert)
      (define-key map "-" 'completion-self-insert)
      (define-key map "<" 'completion-self-insert)
      (define-key map ">" 'completion-self-insert)
      (define-key map " " 'completion-self-insert)
      (define-key map "." 'completion-self-insert)
      (define-key map "," 'completion-self-insert)
      (define-key map ":" 'completion-self-insert)
      (define-key map ";" 'completion-self-insert)
      (define-key map "?" 'completion-self-insert)
      (define-key map "!" 'completion-self-insert)
      (define-key map "\"" 'completion-self-insert)
      (define-key map "0" 'completion-self-insert)
      (define-key map "1" 'completion-self-insert)
      (define-key map "2" 'completion-self-insert)
      (define-key map "3" 'completion-self-insert)
      (define-key map "4" 'completion-self-insert)
      (define-key map "5" 'completion-self-insert)
      (define-key map "6" 'completion-self-insert)
      (define-key map "7" 'completion-self-insert)
      (define-key map "8" 'completion-self-insert)
      (define-key map "9" 'completion-self-insert)
      (define-key map "~" 'completion-self-insert)
      (define-key map "`" 'completion-self-insert)
      (define-key map "@" 'completion-self-insert)
      (define-key map "#" 'completion-self-insert)
      (define-key map "$" 'completion-self-insert)
      (define-key map "%" 'completion-self-insert)
      (define-key map "^" 'completion-self-insert)
      (define-key map "&" 'completion-self-insert)
      (define-key map "*" 'completion-self-insert)
      (define-key map "_" 'completion-self-insert)
      (define-key map "+" 'completion-self-insert)
      (define-key map "=" 'completion-self-insert)
      (define-key map "(" 'completion-self-insert)
      (define-key map ")" 'completion-self-insert)
      (define-key map "{" 'completion-self-insert)
      (define-key map "}" 'completion-self-insert)
      (define-key map "[" 'completion-self-insert)
      (define-key map "]" 'completion-self-insert)
      (define-key map "|" 'completion-self-insert)
      (define-key map "\\" 'completion-self-insert)
      (define-key map "/" 'completion-self-insert))
    (setq auto-completion-map map)))



;; set default bindings for the keymap used when dynamic completion is
;; enabled
(unless completion-dynamic-map
  (let (map)
    ;; Note: rebinding printable characters here is redundant if
    ;; `auto-completion-mode' is enabled, since they are also bound in
    ;; `auto-completion-map', but we still need to ensure that the
    ;; provisional completion is correctly dealt with even if
    ;; `auto-completion-mode' is disabled.
    
    ;; if we can remap keys, do that
    (if (fboundp 'command-remapping)
	(progn
	  (setq map (make-sparse-keymap))
	  ;; remap printable characters to run completion-self-insert
	  (define-key map [remap self-insert-command]
	    'completion-self-insert))
       
      ;; otherwise, create a great big keymap where all printable
      ;; characters run completion-self-insert, which decides what to
      ;; do based on the character's syntax
      (setq map (make-keymap))
      (define-key map "A" 'completion-self-insert)
      (define-key map "a" 'completion-self-insert)
      (define-key map "B" 'completion-self-insert)
      (define-key map "b" 'completion-self-insert)
      (define-key map "C" 'completion-self-insert)
      (define-key map "c" 'completion-self-insert)
      (define-key map "D" 'completion-self-insert)
      (define-key map "d" 'completion-self-insert)
      (define-key map "E" 'completion-self-insert)
      (define-key map "e" 'completion-self-insert)
      (define-key map "F" 'completion-self-insert)
      (define-key map "f" 'completion-self-insert)
      (define-key map "G" 'completion-self-insert)
      (define-key map "g" 'completion-self-insert)
      (define-key map "H" 'completion-self-insert)
      (define-key map "h" 'completion-self-insert)
      (define-key map "I" 'completion-self-insert)
      (define-key map "i" 'completion-self-insert)
      (define-key map "J" 'completion-self-insert)
      (define-key map "j" 'completion-self-insert)
      (define-key map "K" 'completion-self-insert)
      (define-key map "k" 'completion-self-insert)
      (define-key map "L" 'completion-self-insert)
      (define-key map "l" 'completion-self-insert)
      (define-key map "M" 'completion-self-insert)
      (define-key map "m" 'completion-self-insert)
      (define-key map "N" 'completion-self-insert)
      (define-key map "n" 'completion-self-insert)
      (define-key map "O" 'completion-self-insert)
      (define-key map "o" 'completion-self-insert)
      (define-key map "P" 'completion-self-insert)
      (define-key map "p" 'completion-self-insert)
      (define-key map "Q" 'completion-self-insert)
      (define-key map "q" 'completion-self-insert)
      (define-key map "R" 'completion-self-insert)
      (define-key map "r" 'completion-self-insert)
      (define-key map "S" 'completion-self-insert)
      (define-key map "s" 'completion-self-insert)
      (define-key map "T" 'completion-self-insert)
      (define-key map "t" 'completion-self-insert)
      (define-key map "U" 'completion-self-insert)
      (define-key map "u" 'completion-self-insert)
      (define-key map "V" 'completion-self-insert)
      (define-key map "v" 'completion-self-insert)
      (define-key map "W" 'completion-self-insert)
      (define-key map "w" 'completion-self-insert)
      (define-key map "X" 'completion-self-insert)
      (define-key map "x" 'completion-self-insert)
      (define-key map "Y" 'completion-self-insert)
      (define-key map "y" 'completion-self-insert)
      (define-key map "Z" 'completion-self-insert)
      (define-key map "z" 'completion-self-insert)
      (define-key map "'" 'completion-self-insert)
      (define-key map "-" 'completion-self-insert)
      (define-key map "<" 'completion-self-insert)
      (define-key map ">" 'completion-self-insert)
      (define-key map " " 'completion-self-insert)
      (define-key map "." 'completion-self-insert)
      (define-key map "," 'completion-self-insert)
      (define-key map ":" 'completion-self-insert)
      (define-key map ";" 'completion-self-insert)
      (define-key map "?" 'completion-self-insert)
      (define-key map "!" 'completion-self-insert)
      (define-key map "\"" 'completion-self-insert)
      (define-key map "0" 'completion-self-insert)
      (define-key map "1" 'completion-self-insert)
      (define-key map "2" 'completion-self-insert)
      (define-key map "3" 'completion-self-insert)
      (define-key map "4" 'completion-self-insert)
      (define-key map "5" 'completion-self-insert)
      (define-key map "6" 'completion-self-insert)
      (define-key map "7" 'completion-self-insert)
      (define-key map "8" 'completion-self-insert)
      (define-key map "9" 'completion-self-insert)
      (define-key map "~" 'completion-self-insert)
      (define-key map "`" 'completion-self-insert)
      (define-key map "@" 'completion-self-insert)
      (define-key map "#" 'completion-self-insert)
      (define-key map "$" 'completion-self-insert)
      (define-key map "%" 'completion-self-insert)
      (define-key map "^" 'completion-self-insert)
      (define-key map "&" 'completion-self-insert)
      (define-key map "*" 'completion-self-insert)
      (define-key map "_" 'completion-self-insert)
      (define-key map "+" 'completion-self-insert)
      (define-key map "=" 'completion-self-insert)
      (define-key map "(" 'completion-self-insert)
      (define-key map ")" 'completion-self-insert)
      (define-key map "{" 'completion-self-insert)
      (define-key map "}" 'completion-self-insert)
      (define-key map "[" 'completion-self-insert)
      (define-key map "]" 'completion-self-insert)
      (define-key map "|" 'completion-self-insert)
      (define-key map "\\" 'completion-self-insert)
      (define-key map "/" 'completion-self-insert)
      )
    
    ;; <tab> does traditional tab-completion
    (define-key map "\t" 'completion-tab-complete)
    ;; M-<tab> cycles
    (define-key map [?\M-\t] 'completion-cycle)
    ;; M-<shift>-<tab> cycles backwards (note: [\M-\S-iso-lefttab] would
    ;; also work)
    (define-key map '[(meta shift iso-lefttab)]
      (lambda () "Cycle backwards through completions."
	(interactive) (completion-cycle -1)))
    ;; C-<space> abandons
    (define-key map [?\C- ] 'completion-reject)
    ;; M-<space> abandons and inserts a space
    (define-key map "\M- "
      (lambda (&optional arg)
	"Reject current provisional completion and insert a space."
	(interactive "P")
	(completion-reject arg)
	(insert " ")))
    ;; M-. inserts "." as a word-constituent
    (define-key map "\M-."
      (lambda () "Insert \".\" as though it were a word-constituent."
	(interactive)
	(completion-self-insert ?. ?w)))
    ;; M-- inserts "-" as a word-constituent
    (define-key map "\M--"
      (lambda () "Insert \"-\" as though it were a word-constituent."
	(interactive)
	(completion-self-insert ?- ?w)))
    ;; M-/ inserts "/" as a word-constituent
    (define-key map "\M-/"
      (lambda () "Insert \"/\" as though it were a word-constituent."
	(interactive)
	(completion-self-insert ?/ ?w)))
    ;; RET accepts any pending completion candidate, then runs whatever
    ;; is usually bound to RET
    (define-key map "\r" 'completion-accept-and-newline)
    ;; M-<down> displays the completion menu
    (define-key map [M-down] 'completion-show-menu)
    ;; clicking on completion opens completion menu
    (define-key map [mouse-2] 'completion-show-menu)
    
    (setq completion-dynamic-map map))
)



;; construct the keymap used for hotkey selection from
;; completion-hotkey-list
(let ((map (make-sparse-keymap)) key)
  (dolist (key completion-hotkey-list)
    (define-key map key 'completion-select-if-within-overlay))
  (setq completion-hotkey-map map))

;; make sure completion-hotkey-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-use-hotkeys minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-hotkey-map)
    (push (cons 'completion-use-hotkeys completion-hotkey-map)
	  minor-mode-map-alist)))



(defun completion-define-word-constituent-binding (key char
						       &optional syntax)
  "Setup key bindings for KEY so that it inserts character CHAR
as though it's syntax were SYNTAX (defaults to word-constituent, ?w)."

  (when (null syntax) (setq syntax ?w))
  (let ((doc (concat "Insert \"" (string char) "\" as though it were a\
 word-constituent.")))
    
    ;; create `completion-dynamic-map' binding
    (define-key completion-dynamic-map key
      `(lambda () ,doc
	 (interactive)
	 (completion-self-insert ,char ,syntax)))
    
    ;; if emacs version doesn't support overlay keymaps properly, create
    ;; binding in `completion-map' to simulate them via
    ;; `completion-run-if-within-overlay' hack
    (when (<= emacs-major-version 22)
      (define-key completion-map key
	`(lambda () ,doc
	   (interactive)
	   (completion-run-if-within-overlay
	    (lambda () (interactive)
	      (completion-self-insert ,char ,syntax))
	    'completion-function)))))
)




;;; ================================================================
;;;                Replacements for CL functions

(defun completion--sublist (list start &optional end)
  "Return the sub-list of LIST from START to END.
If END is omitted, it defaults to the length of the list
If START or END is negative, it counts from the end."
  (let (len)
    ;; sort out arguments
    (if end
	(when (< end 0) (setq end (+ end (setq len (length list)))))
      (setq end (or len (setq len (length list)))))
    (when (< start 0)
      (setq start (+ start (or len (length list)))))
    
    ;; construct sub-list
    (let (res)
      (while (< start end)
	(push (nth start list) res)
	(setq start (1+ start)))
      (nreverse res)))
)



(defun completion--position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'equal."
  (let (el (i 0))
    (catch 'found
      (while (setq el (nth i list))
	(when (equal item el) (throw 'found i))
	(setq i (1+ i))
	nil)))
)




;;; =======================================================
;;;         Auto-completion minor-mode definition

(define-minor-mode auto-completion-mode
  "Toggle auto-completion mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

In auto-completion-mode, Emacs will try to complete words as you
type, using whatever completion method has been set up (either by the
major mode, or by another minor mode)."
  nil                  ; init-value
  " complete"          ; lighter
  auto-completion-map  ; keymap
)



(defun turn-on-auto-completion-mode ()
  "Turn on auto-completion mode. Useful for adding to hooks."
  (unless auto-completion-mode (auto-completion-mode))
)




;;; =======================================================
;;;              User-interface functions

(defun complete-in-buffer (prefix &optional overlay auto pos)
  "Complete PREFIX at point.

If OVERLAY is supplied, use that instead of looking for a
completion overlay at the point.

If AUTO is non-nil, assume we're auto-completing and respect
settings of `auto-completion-min-chars' and
`auto-completion-delay'.

If POS is non-nil, only complete if point is at POS (only used
internally)."

  ;; cancel any timer so that we don't have two running at once
  (cancel-timer completion-tooltip-timer)
  ;; resolve any provisional completions
  (completion-resolve-old overlay)

  
  ;; if auto-completing, only complete prefix if it has requisite number
  ;; of characters and point is at POS (latter is only used when called
  ;; from timer)
  (unless (and auto
	       (or (and pos (/= (point) pos))
		   (and auto-completion-min-chars
			(< (length prefix) auto-completion-min-chars))))
    
    ;; if we're auto-completing and `auto-completion-delay' is set,
    ;; delay completing by setting a timer to call ourselves later
    (if (and auto auto-completion-delay)
	(setq completion-tooltip-timer
	      (run-with-idle-timer auto-completion-delay nil
				   'complete-in-buffer
				   prefix overlay nil (point)))
      
      
      ;; otherwise, call completion function to find completions
      (let ((completions
	     (funcall completion-function
		      prefix completion-max-candidates)))
	;; setup completion overlay
	(setq overlay
	      (completion-setup-overlay prefix completions nil overlay))
	
	;; activate dynamic completion
	(when completion-use-dynamic (complete-dynamic overlay))
	
	;; display completion echo text
	(when completion-use-echo (complete-echo overlay))
	
	;; display completion tooltip
	(when completion-use-tooltip (complete-tooltip overlay))
	
	;; activate completion menu
	(when completion-auto-show-menu (completion-show-menu overlay))
	
	;; no need to do anything for hotkeys, it's all done when
	;; completion-select is called
	)))
)



(defun complete-dynamic (&optional overlay)
  "Insert dynamic completion and update completion overlay
accordingly. If OVERLAY is supplied, use that instead of finding
or creating one."

  ;; for some reason, the delete-region or insert (below) can sometimes
  ;; delete or move the completion overlay, so we store its start
  ;; position before doing anything else, so we can move the completion
  ;; overlay into the correct new position later
  (let ((pos (make-marker)))
    (move-marker pos (overlay-start overlay))
    
    ;; delete old completion
    (delete-region (overlay-start overlay) (overlay-end overlay))
    ;; for some reason, the delete-region sometimes deletes the overlay,
    ;; and even moving it back fails, so we have to re-create it
    (unless (overlay-buffer overlay)
      (setq completion-overlay-list
	    (delq overlay completion-overlay-list))
      (setq overlay
	    (completion-setup-overlay
	   (overlay-get overlay 'prefix)
	   (overlay-get overlay 'completions))))
  
    ;; insert new one, if any
    (let ((completions (overlay-get overlay 'completions)))
      (when completions
	(insert (car completions))
	(move-overlay overlay pos (+ pos (length (car completions))))
	(overlay-put overlay 'completion-num 0))
      (goto-char (overlay-start overlay)))
    )
)



(defun complete-tooltip (&optional overlay nodelay)
  "Display list of completions in a tooltip.
If OVERLAY is supplied, use that instead of finding or creating
one. If NODELAY is non-nil, display tooltip immediately,
irrespective of the setting of `completion-tooltip-delay'."

  ;; cancel any running timer so we don't have two running at the same
  ;; time
  (cancel-timer completion-tooltip-timer)
  
  ;; if `completion-tooltip-delay' is unset or NODELAY supplied,
  ;; display tooltip immediately
  (if (or (null completion-tooltip-delay) nodelay)
      (completion-show-tooltip overlay)
    ;; otherwise, postpone displaying tooltip until there's a pause in
    ;; typing
    (setq completion-tooltip-timer
	  (run-with-idle-timer completion-tooltip-delay nil
			       'completion-show-tooltip
			       overlay (point))))
)



(defun complete-echo (overlay)
  "Display completion candidates in the echo-area."
  (let ((message-log-max nil))
    (message (completion-construct-echo-text overlay)))
)
  


(defun completion-show-menu (&optional overlay menu)
  "Show completion menu for completion at point.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll have a sneezing fit.

If MENU is supplied, use that to construct the menu, unless an
overlay overrides it. Defaults to the \"overlay local\" binding
of 'completion-menu, or `completion-menu' if there is none."
  (interactive)
  (completion-cancel-tooltip)
  
  (setq menu (or menu
		 (and (fboundp 'auto-overlay-local-binding)
		      (auto-overlay-local-binding 'completion-menu))
		 completion-menu
		 'completion-construct-menu))
  ;; if we haven't been passed one, get completion overlay at point
  ;; or create new one if none exists
  (unless overlay (setq overlay (completion-overlay-at-point)))
    
  (let (keymap result)
    (cond
     ;; if `menu' is a function, evaluate it to get menu
     ((functionp menu)
      (setq keymap (funcall menu (overlay-get overlay 'prefix)
			    (overlay-get overlay 'completions)))
      ;; throw error if return value has wrong type
      (unless (or (null keymap) (keymapp keymap))
	(error "`completion-menu' returned wrong type:null or\
 keymapp, %s"
	       (prin1-to-string keymap))))
       
     ;; if `menu' is a keymap, use that
     ((keymapp menu) (setq keymap menu))
       
     ;; otherwise, throw an error
     (t (error "Wrong type in `completion-menu': functionp or\
 keymapp, %s"
	       (prin1-to-string menu))))
      
      
    ;; if we've constructed a menu, display it
    (when keymap
      (setq result
	    (x-popup-menu (completion-posn-at-point-as-event
			   nil nil nil (+ (frame-char-height) 3))
			  keymap))
	
	
      ;; if they ain't selected nuffin', don't do nuffin'!
      (when result
	;; convert result to a vector for key lookup
	(setq result (apply 'vector result))
	  
	(cond
	 ;; if they selected a completion from the menu...
	 ((string-match "^completion-insert"
			(symbol-name (aref result
					   (1- (length result)))))
	  ;; run accept hooks
	  (run-hook-with-args
	   'completion-accept-functions
	   (overlay-get overlay 'prefix)
	   (concat (overlay-get overlay 'prefix)
		   (buffer-substring-no-properties
		    (overlay-start overlay)
		    (overlay-end overlay))))
	  ;; insert selected completion
	  (setq completion-overlay-list
		(delq overlay completion-overlay-list))
	  (delete-region (overlay-start overlay)
			 (overlay-end overlay))
	  (delete-overlay overlay)
	  (funcall (lookup-key keymap result)))
	   
	 ;; otherwise, run whatever they did select
	 (t (funcall (lookup-key keymap result))))
	)))
)


(defun completion-show-menu-if-within-overlay ()
  "Display completion menu for current completion
if there is one, otherwise run whatever command would normally be
bound to the key sequence."
  (interactive)
  (completion-run-if-within-overlay 'completion-show-menu
				    'completion-function)
)



(defun completion-show-tooltip (&optional overlay position)
  "Show completion tooltip.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll have bad luck for
thirteen years.

If POSITION is supplied, a tooltip will only be displayed if
point is at position."
  (interactive)

  (when (and window-system (fboundp 'x-show-tip)
	     (or (null position) (= (point) position)))
    (unless overlay (setq overlay (completion-overlay-at-point)))
    
    ;; if point is in a completion overlay...
    (when overlay
      (let ((mouse-pos (mouse-pixel-position))
	    (pos (completion-frame-posn-at-point))
	    (fg (face-attribute 'completion-tooltip-face :foreground))
	    (bg (face-attribute 'completion-tooltip-face :background))
	    params text text-func)

	;; construct the tooltip text using the "overlay-local" binding
	;; of 'tooltip-function, or `completion-tooltip-function' if
	;; there is none, or failing that
	;; `completion-construct-tooltip-text'
	(setq text-func
	      (or (and (fboundp 'auto-overlay-local-binding)
		       (auto-overlay-local-binding 'completion-tooltip))
		  completion-tooltip
		  'completion-construct-tooltip-text))
	(setq text (funcall text-func
			    (overlay-get overlay 'prefix)
			    (overlay-get overlay 'completions)
			    (overlay-get overlay 'completion-num)))
	
	;; mouse position can be nil if mouse is outside Emacs frame in
	;; certain window systems (e.g. windows); in this case, we move
	;; mouse into frame (there's no way to restore its position
	;; afterwards, since we can't find out its position)
	(set-mouse-position (selected-frame) 1 0)
	(setq mouse-pos (mouse-pixel-position))
	
	;; set face and frame parameters
	(when (stringp fg)
	  (setq params
		(tooltip-set-param params 'foreground-color fg))
	  (setq params (tooltip-set-param params 'border-color fg)))
	(when (stringp bg)
	  (setq params
		(tooltip-set-param params 'background-color bg)))
	(setq params
	      (tooltip-set-param params 'internal-border-width 0))
	(setq params
	      (tooltip-set-param params 'border-width 0))
;; 	(setq params
;; 	      (tooltip-set-param
;; 	       params 'left
;; 	       (+ (car pos) completion-tooltip-x-offset)))
;; 	(setq params
;; 	      (tooltip-set-param
;; 	       params 'top
;; 	       (+ (cdr pos) completion-tooltip-y-offset)))
	
	;; show tooltip
	;; note: there's no reliable way to display a tooltip at the
	;; *screen* position (which is what x-show-tip requires) of
	;; point, so we use the kludge of calculating an offset from the
	;; mouse position and displaying the tooltip relative to the
	;; mouse
	(x-show-tip text nil params completion-tooltip-timeout
		    (+ (- (car pos) (cadr mouse-pos))
		       (car completion-tooltip-offset))
		    (+ (- (cdr pos) (cddr mouse-pos)) (frame-char-height)
		       (cdr completion-tooltip-offset)))
	)))
)




;;; ===============================================================
;;;                Commands for binding to keys


(defun completion-self-insert (&optional char syntax)
  "Execute a completion function based on syntax.

Decide what completion function to execute by looking up the
syntax of the character corresponding to the last input event in
`completion-dynamic-syntax-alist'. The syntax-derived function
can be overridden for individual characters by
`completion-dynamic-override-syntax-alist'.

If CHAR is supplied, it is used instead of the last input event
to determine the character typed. If SYNTAX is supplied, it
overrides the character's syntax, and is used instead to lookup
the behaviour in the alists.

The default functions in `completion-dymamic-syntax-alist' all
insert the last input event, in addition to taking any completion
related action \(hence the name,
`completion-self-insert'\). Therefore, unless you know what you
are doing, only bind `completion-self-insert' to printable
characters."
  (interactive)
  (completion-cancel-tooltip)

  ;; if CHAR or SYNTAX were supplied, use them, otherwise get character
  ;; and syntax from last input event (relies on sensible key bindings
  ;; being used for this command)
  (when (null char) (setq char last-input-event))
  (when (null syntax) (setq syntax (char-syntax last-input-event)))
;;  (message "Syntax of %c (%d): %c (%d)" char char syntax syntax)

  
  ;; if we're not automatically completing or doing dynamic
  ;; completion, just resolve provisional completions and insert last
  ;; input event
  (if (and (not auto-completion-mode)
	   (not completion-use-dynamic))
      (progn
	(completion-resolve-old)
	(setq last-input-event char)
	(self-insert-command 1))
    
    
    ;; otherwise, lookup behaviour in syntax alists
    (let* ((syntax-alist
	    (if (fboundp 'auto-overlay-local-binding)
		(auto-overlay-local-binding
		 'completion-dynamic-syntax-alist)
	      completion-dynamic-syntax-alist))
	   (override-alist
	    (if (fboundp 'auto-overlay-local-binding)
		(auto-overlay-local-binding
		 'completion-dynamic-override-syntax-alist)
	      completion-dynamic-override-syntax-alist))
	   (behaviour
	    (or (cdr (assq char override-alist))
		(cdr (assq syntax syntax-alist))
		(cdr (assq t syntax-alist))))
	   (resolve-behaviour (nth 0 behaviour))
	   (insert-behaviour (nth 1 behaviour))
	   (complete-behaviour (nth 2 behaviour))
	   (overlay (completion-overlay-at-point))
	   wordstart prefix)
      
      
      ;; if behaviour alist entry is a function, call it
      (when (functionp resolve-behaviour)
	(setq resolve-behaviour (funcall resolve-behaviour)))

      ;; do whatever is specified in alists
      (cond
       ;; no-op
       ((null resolve-behaviour))
       ;; accept
       ((eq resolve-behaviour 'accept)
	(completion-accept nil overlay)
	(setq prefix (string char))
	(setq wordstart t))
       ;; reject
       ((eq resolve-behaviour 'reject)
	(completion-reject nil overlay)
	(setq prefix (string char))
	(setq wordstart t))
       ;; add to prefix
       ((eq resolve-behaviour 'add)
	(if (null overlay)
	    (setq prefix (string char))
	  (delete-region (overlay-start overlay)
			 (overlay-end overlay))
	  (setq prefix (concat (overlay-get overlay 'prefix)
			       (string char))))
	(setq wordstart (or (completion-beginning-of-word-p)
			    (and (not (completion-within-word-p))
				 (not (completion-end-of-word-p))))))
       ;; error
       (t (error "Invalid entry in `completion-dynamic-syntax-alist'\
  or `completion-dynamic-override-syntax-alist', %s"
		 (prin1-to-string resolve-behaviour))))
      
      
      ;; if behaviour alist entry is a function, call it
      (when (functionp insert-behaviour)
	(setq insert-behaviour (funcall insert-behaviour)))
      ;; insert typed character and move overlay, unless told not to
      ;; by return value of complete-after function
      (when insert-behaviour
	;; use `self-insert-command' if possible, auto-fill depends on it
	(if (eq char last-input-event)
	    (self-insert-command 1)
	  (insert char))
	(when overlay (move-overlay overlay (point) (point))))
      
      
      ;; if behaviour alist entry is a function, call it
      (when (functionp complete-behaviour)
	(setq complete-behaviour (funcall complete-behaviour)))
      
      (cond
       ;; no-op
       ((null complete-behaviour))
       
       ;; if not using automatic completion or not completing after
       ;; inserting, resolve any overlay
       ((or (not auto-completion-mode)
	    (eq complete-behaviour 'none))
	(when overlay
	  (delete-overlay overlay)
	  (setq completion-overlay-list
		(delq overlay completion-overlay-list))
	  (completion-resolve-old)))

       ;; if doing basic completion, or we're in a completion overlay
       ;; or at the beginning of a word, do normal completion
       ((or (eq complete-behaviour 'basic) overlay wordstart)
	(complete-in-buffer prefix overlay t))
       
       ;; if completing word at point, do so
       ((eq complete-behaviour 'word)
	(complete-word-at-point overlay 'auto))

       ;; error
       (t (error "Invalid entry in `completion-dynamic-syntax-alist'\
 or `completion-dynamic-override-syntax-alist', %s"
		 (prin1-to-string complete-behaviour))))
      ))
)



(defun complete-word-at-point (&optional overlay auto)
  "Complete the word at or next to the point.

When called from Lisp programs, OVERLAY is used if supplied
instead of looking for one at the point. The point had better be
within OVERLAY or carrots will start growing out your ears.

If AUTO is non-nil, assume we're auto-completing rather than
being invoked manually"
  (interactive)

  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  (let ((word-thing (if (fboundp 'auto-overlay-local-binding)
			(auto-overlay-local-binding
			 'completion-word-thing)
		      completion-word-thing))
	prefix pos prefix-fun)
    (cond
     ;; if within an existing overlay, complete its prefix
     (overlay (complete-in-buffer (overlay-get overlay 'prefix)
				  overlay auto))

     ;; if `completion-prefix' is set, use that to determine the prefix
     ;; and complete it
     ((setq prefix-fun
	    (or (and (fboundp 'auto-overlay-local-binding)
		     (auto-overlay-local-binding 'completion-prefix))
		completion-prefix))
      (setq prefix (funcall completion-prefix))
      (complete-in-buffer prefix overlay auto))
     
     ;; if point is at end of a word, complete it
     ((completion-end-of-word-p)
      (setq pos (point))
      (save-excursion
	(forward-thing word-thing -1)
	(setq prefix (buffer-substring-no-properties (point) pos)))
      (complete-in-buffer prefix overlay auto))
     
     ;; if point is within a word, delete part of word after point (up
     ;; to overlay, if there is one) and complete remaining prefix
     ((completion-within-word-p)
      (setq pos (point))
      ;; find first completion overlay within word
      (unless overlay
	(save-excursion
	  (forward-thing word-thing)
	  (setq overlay
		(car (sort (completion-overlays-in pos (point))
			   (lambda (a b) (< (overlay-start a)
					    (overlay-start b))))))))
      ;; delete old completion and complete new prefix
      (save-excursion
	(forward-thing word-thing)
	(delete-region pos (if overlay (overlay-start overlay)
			     (point)))
	(forward-thing word-thing -1)
	(setq prefix (buffer-substring-no-properties (point) pos)))
      (complete-in-buffer prefix overlay auto))
     ))
)



(defun completion-select (&optional n overlay)
  "Select completion corresponding to the last input event
when hotkey completion is active.

If integer N is supplied, insert completion corresponding to that
instead. If OVERLAY is supplied, use that instead of finding one
at point. The point had better be within OVERLAY or a meteorite
will smash through your ceiling.

Intended to be bound to keys in `completion-hotkey-map'."
  (interactive)
  (completion-cancel-tooltip)
  
  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; find completion index corresponding to last input event
  (unless n
    (let ((key (this-command-keys-vector)))
      ;; work around apparent bug where keys are doubled in vector
      (when (> (length key) 1) (setq key (vector (aref key 0))))
      (setq n (completion--position key completion-hotkey-list))))
  
  ;; if within a completion overlay...
  (when overlay
    (let ((completions (overlay-get overlay 'completions)))
      (cond
       ;; if there are no completions, run whatever would otherwise be
       ;; bound to the key
       ((null completions)
	(when completion-trap-recursion
	  (error "Recursive call to `completion-select'"))
	(setq completion-use-hotkeys nil)
	(let ((completion-trap-recursion t))
	  (unwind-protect
	      (command-execute
	       (key-binding (this-command-keys) 'accept-default))
	    (setq completion-use-hotkeys t))))
       
       ;; if there are too few completions, display message
       ((>= n (length completions))
	(beep)
	(message "Only %d completions available"
		 (length (overlay-get overlay 'completions))))
       
       ;; otherwise, replace dynamic completion with selected one
       (t
	(setq completion-overlay-list
	      (delq overlay completion-overlay-list))
	(delete-region (overlay-start overlay) (overlay-end overlay))
	(insert (nth n completions))
	;; run accept hooks
	(run-hook-with-args 'completion-accept-functions
			    (overlay-get overlay 'prefix)
			    (concat (overlay-get overlay 'prefix)
				    (nth n completions)))
	(delete-overlay overlay))
       )))
)


(defun completion-select-if-within-overlay ()
  "Select a completion to insert if there is one, otherwise run
whatever command would normally be bound to the key sequence used
to invoke this function."
  (interactive)
  (completion-run-if-within-overlay 'completion-select
				    'completion-use-hotkeys)
)



(defun completion-accept (&optional arg overlay)
  "Accept current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-accept-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was accepted, returns a cons cell containing the
prefix and the entire accepted completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (let ((o (or overlay (completion-overlay-at-point)))
	prefix completion)
    
    ;; resolve any other old provisional completions
    (completion-resolve-old o)
    (completion-cancel-tooltip)
    
    ;; if point is in a completion overlay...
    (when o
      (setq prefix (overlay-get o 'prefix))
      (setq completion
	    (concat prefix
		    (buffer-substring-no-properties (overlay-start o)
						    (overlay-end o))))
      ;; accept current completion
      (goto-char (overlay-end o))
      ;; run accept hooks
      (run-hook-with-args 'completion-accept-functions
			  prefix completion arg)
      ;; delete overlay if we've found it ourselves
      (unless overlay
	(setq completion-overlay-list (delq o completion-overlay-list))
	(delete-overlay o))
      (cons prefix completion)
      ))
)


(defun completion-accept-if-within-overlay (&optional arg)
  "Accept current completion if there is one,
then run whatever command would normally be bound to the key
sequence used to invoke this function.

ARG is the prefix argument, which is passed as the third argument
to any functions called from the `completion-accept-functions'
hook."
  (interactive "P")
  (completion-run-if-within-overlay
   (lambda () (interactive) (completion-accept arg))
   'completion-function 'before)
)



(defun completion-reject (&optional arg overlay)
  "Reject current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-reject-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was rejected, returns a cons cell containing the
prefix and the entire rejected completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (let ((o (or overlay (completion-overlay-at-point)))
	prefix completion)
    
    ;; resolve any other old provisional completions
    (completion-resolve-old o)
    (completion-cancel-tooltip)
    
    ;; if point is in a completion overlay...
    (when o
      (setq prefix (overlay-get o 'prefix))
      (setq completion
	    (concat prefix (buffer-substring-no-properties
			    (overlay-start o)
			    (overlay-end o))))
      ;; reject current completion
      (delete-region (overlay-start o) (overlay-end o))
      ;; run reject hooks
      (run-hook-with-args 'completion-reject-functions
			  prefix completion arg)
      ;; delete overlay if we've found it ourselves
      (unless overlay
	(setq completion-overlay-list
	      (delq o completion-overlay-list))
	(delete-overlay o))
      ;; return cons cell containing prefix and rejected completion
      (cons prefix completion)
      ))
)


(defun completion-reject-if-within-overlay (&optional arg)
  "Reject the current completion if there is one, otherwise run
whatever would normally be bound to the key sequence used to
invoke this function.

ARG is the prefix argument, which is passed as the third argument
to any function called from the `completion-reject-functions'
hook."
  (interactive "P")
  (completion-run-if-within-overlay
   (lambda () (interactive) (completion-reject arg))
   'completion-function)
)



(defun completion-scoot-ahead (&optional overlay)
  "Accept the characters from the current completion, and recomplete
the resulting string.

When called from Lisp programs, use OVERLAY instead of finding
one. The point had better be within OVERLAY or the oceans will
boil away."
  (interactive)
  (completion-cancel-tooltip)

  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if within a completion overlay, accept characters it contains
  (when (and overlay (/= (point) (overlay-end overlay)))
    (goto-char (overlay-end overlay))
    (move-overlay overlay (point) (point))
    (completion-setup-overlay
     (concat (overlay-get overlay 'prefix)
	     (nth (overlay-get overlay 'completion-num)
		  (overlay-get overlay 'completions)))
     nil nil overlay))
  
  ;; if auto-completing, do so
  (when auto-completion-mode
    (complete-in-buffer (overlay-get overlay 'prefix) overlay t))
)



(defun completion-cycle (&optional n overlay)
  "Cycle through available completions.

Optional argument N specifies the number of completions to cycle
forwards \(backwards if negative\). Default is 1. Interactively,
N is the prefix argument.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be stuck by
lightening."
  (interactive "P")
  (when (null n) (setq n 1))
  
  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if within a completion overlay, cycle to next completion
  (when overlay
    (let* (i string)
      (when (null (setq i (overlay-get overlay 'completion-num)))
	(setq i -1))
      (setq i (mod (+ i n)
		   (length (overlay-get overlay 'completions))))
      (setq string (nth i (overlay-get overlay 'completions)))
      ;; delete old completion and insert new one
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert string)
      (move-overlay overlay (overlay-start overlay)
		    (+ (overlay-start overlay) (length string)))
      (overlay-put overlay 'completion-num i)
      (when completion-use-dynamic
	(goto-char (overlay-start overlay)))
      ;; display echo text and tooltip if using them
      (when completion-use-echo	(complete-echo overlay))
      (when completion-use-tooltip
	(completion-cancel-tooltip)
	(complete-tooltip overlay 'no-delay))))
)



(defun completion-tab-complete (&optional overlay)
  "Tab-complete completion at point
\(i.e. insert longest common prefix of all the completions\).

If OVERLAY is supplied, use that instead of finding one."
  (interactive)
  (completion-cancel-tooltip)
  
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if within a completion overlay
  (when overlay
    (let ((str (try-completion ""
			       (overlay-get overlay 'completions))))
      (unless (or (null str) (string= str ""))
	;; do tab-completion
	(delete-region (overlay-start overlay) (overlay-end overlay))
	(insert str)
	(move-overlay overlay (point) (point))
	(overlay-put overlay 'prefix
		     (concat (overlay-get overlay 'prefix) str))
	(overlay-put overlay 'completions nil))
      ;; when auto-completing, do so
      (when auto-completion-mode
	(complete-in-buffer (overlay-get overlay 'prefix) overlay t))))
)


(defun completion-tab-complete-if-within-overlay ()
  "Tab-complete current completion if there is one, otherwise run
whatever command would normally be bound to the key sequence used
to invoke this function."
  (interactive)
  (completion-run-if-within-overlay 'completion-tab-complete
				    'completion-function)
)



(defun completion-accept-and-newline (&optional n overlay)
  "Insert a newline. Accepts the current completion
if there is one. If N is specified, insert that many
newlines. Interactively, N is the prefix argument."
  (interactive "p")
  (completion-cancel-tooltip)
  (unless n (setq n 1))
  (completion-accept nil overlay)
  (newline n)
)



(defun completion-reject-and-newline (&optional n overlay)
  "Insert a newline. Rejects the current completion
if there is one. If N is specified, insert that many
newlines. Interactively, N is the prefix argument."
  (interactive "p")
  (completion-cancel-tooltip)
  (unless n (setq n 1))
  (completion-reject nil overlay)
  (newline n)
)



(defun completion-backward-delete (command &rest args)
  "Run backward-delete COMMAND, passing it ARGS.
Any provisional completion at the point is first rejected. If
COMMAND deletes into a word and auto-completion is enabled,
complete what remains of that word."

  ;; start by cancelling any tooltip that's stil hanging around
  (completion-cancel-tooltip)
  
  (let ((overlay (completion-overlay-at-point))
	(wordstart (completion-beginning-of-word-p))
	(word-thing (if (fboundp 'auto-overlay-local-binding)
			(auto-overlay-local-binding
			 'completion-word-thing)
		      completion-word-thing)))
    
    (combine-after-change-calls
      ;; if not auto-completing, just resolve old pending completiong
      ;; and delete backwards
      (if (not auto-completion-mode)
	  (progn
	    (when overlay
	      (delete-region (overlay-start overlay)
			     (overlay-end overlay))
	      (delete-overlay overlay)
	      (setq completion-overlay-list
		    (delq overlay completion-overlay-list)))
	    (completion-resolve-old)
	    (apply command args))
	
	
	;; if auto-completing...
	;; resolve any old provisional completions
	(completion-resolve-old overlay)
	;; delete current provisional completion
	(when overlay
	  (delete-region (overlay-start overlay)
			 (overlay-end overlay)))
	;; delete backwards
	(apply command args)

	(cond
	 ;; if we're not in or at the end of a word, reject any
	 ;; completion and cancel any timer that's been set up
	 ((and (not (completion-within-word-p))
	       (not (completion-end-of-word-p)))
	  (completion-reject)
	  (when (timerp completion-backward-delete-timer)
	    (cancel-timer completion-backward-delete-timer))
	  (setq completion-backward-delete-timer nil))
	 
	 ;; otherwise, we're in or at the end of a word, so if
	 ;; auto-completion is enabled we're going to complete the word
	 ;; at point...
	 (auto-completion-mode
	  ;; if point was at start of word before deleting, setup
	  ;; overlay to prevent word after point being deleted
	  (when (or overlay
		    (and wordstart
			 (null (completion-overlay-at-point))
			 (or (completion-within-word-p)
			     (completion-end-of-word-p))))
	    (let ((pos (point)) prefix prefix-func)
	      (if (setq prefix-func
			(or (and (fboundp 'auto-overlay-local-binding)
				 (auto-overlay-local-binding
				  'completion-prefix))
			    completion-prefix))
		  (setq prefix (funcall completion-prefix))
		(save-excursion
		  (forward-thing word-thing -1)
		  (setq prefix
			(buffer-substring-no-properties (point) pos))))
	      (completion-setup-overlay prefix nil nil overlay)))
	  
	  ;; if there's no existing timer, set one up to complete
	  ;; remainder of word after some idle time
	  (when (timerp completion-backward-delete-timer)
	    (cancel-timer completion-backward-delete-timer))
	  (setq completion-backward-delete-timer
		(run-with-idle-timer
		 auto-completion-backward-delete-delay nil
		 ;; FIXME: tooltip doesn't seem to be displayed - why?
		 (lambda ()
		   (complete-word-at-point)
		   (setq completion-backward-delete-timer nil)))))
	 
	 ;; if auto-completion isn't enabled, delete the overlay and do
	 ;; nothing
	 (t (delete-overlay overlay)))
	)))
)



(defun completion-delete (command &rest args)
  "Call forward-delete COMMAND, passing it ARGS.
If there is a provisional completion at point after deleting, reject
it."
  
  ;; start by cancelling any tooltip that's stil hanging around
  (completion-cancel-tooltip)
  ;; call the deletion command
  (apply command args)
  ;; if there's a completion overlay at point after deleting, reject it
  (let ((overlay (completion-overlay-at-point)))
    (when overlay
      (delete-region (overlay-start overlay)
		     (overlay-end overlay))
      (delete-overlay overlay)
      (setq completion-overlay-list
	    (delq overlay completion-overlay-list))))
  (completion-resolve-old)
)



(defun completion-delete-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
If there is a provisional completion at point after deleting,
reject it.  \(If N is negative, behaviour is instead as for
`completion-backward-delete-char'.\)

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if n was explicitly specified."
  (interactive "p")
  (when (and (interactive-p) n) (setq killflag t))

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-delete-char n killflag)
    (completion-delete 'delete-char n killflag))
)



(defun completion-backward-delete-char (n &optional killflag)
  "Delete the previous N characters (following if N is negative).
Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word. \(If N is negative,
behaviour is instead as for `completion-delete-char'.\)

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified."
  (interactive "p")
  (when (and (interactive-p) n) (setq killflag t))

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char n killflag)
    (completion-backward-delete 'backward-delete-char n killflag))
)



(defun completion-backward-delete-char-untabify (n &optional killflag)
  "Delete N characters backward, changing tabs into spaces.
Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word. \(If N is negative,
behaviour is instead as for `completion-delete-char'.\)

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified.

The exact behavior depends on `backward-delete-char-untabify-method'."
  (interactive "p")
  (when (and (interactive-p) n) (setq killflag t))

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char n killflag)
    (completion-backward-delete 'backward-delete-char-untabify
				n killflag))
)



(defun completion-kill-word (&optional n)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-word'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-word n)
    (completion-delete 'kill-word n))
)




(defun completion-backward-kill-word (&optional n)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-word'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-word n)
    (completion-backward-delete 'backward-kill-word n))
)



(defun completion-kill-sentence (&optional n)
  "Kill from point to end of sentence.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-sentence'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sentence n)
    (completion-delete 'kill-sentence n))
)




(defun completion-backward-kill-sentence (&optional n)
  "Kill back from point to start of sentence.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-sentence'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sentence n)
    (completion-backward-delete 'backward-kill-sentence n))
)



(defun completion-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) following point.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-sexp'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sexp n)
    (completion-delete 'kill-sexp n))
)




(defun completion-backward-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) before point.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-sexp'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sexp n)
    (completion-backward-delete 'backward-kill-sexp n))
)



(defun completion-kill-paragraph (&optional n)
  "Kill forward to end of paragraph.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-paragraph'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-paragraph n)
    (completion-delete 'kill-paragraph n))
)




(defun completion-backward-kill-paragraph (&optional n)
  "Kill backward to start of paragraph.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-paragraph'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-paragraph n)
    (completion-backward-delete 'backward-kill-paragraph n))
)



;; (defun completion-backward-delete-if-within-overlay (&optional n)
;;   "Delete backwards N characters.
;; If there is a provisional completion at the point, delete it first."
;;   (interactive "p")
;;   (completion-run-if-within-overlay
;;    (lambda () (interactive) (completion-backward-delete n))
;;    'completion-function 'instead)
;; )






;;; ==============================================================
;;;                    Internal functions

(defun completion-beginning-of-word-p (&optional point)
  "Return non-nil if POINT is at beginning of a word
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (< point (point-max))
	   (setq bounds
		 (bounds-of-thing-at-point
		  (if (fboundp 'auto-overlay-local-binding)
		      (auto-overlay-local-binding
		       'completion-word-thing)
		    completion-word-thing)))
	   (= point (car bounds)))))
)
  


(defun completion-within-word-p (&optional point)
  "Return non-nil if POINT is within or at end of a word
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (setq bounds
		 (bounds-of-thing-at-point
		  (if (fboundp 'auto-overlay-local-binding)
		      (auto-overlay-local-binding
		       'completion-word-thing)
		    completion-word-thing)))
	   (> point (car bounds))
	   (< point (cdr bounds)))))
)



(defun completion-end-of-word-p (&optional point)
  "Return non-nil if POINT is at end of a word
\(POINT defaults to the point\)"
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (> point (point-min))
	   (setq bounds
		 (bounds-of-thing-at-point
		  (if (fboundp 'auto-overlay-local-binding)
		      (auto-overlay-local-binding
		       'completion-word-thing)
		    completion-word-thing)))
	   (= point (cdr bounds)))))
)



(defun completion-setup-overlay
  (prefix &optional completions num overlay)
  "Get completion overlay at point, or create a new one
if none exists, and set its properties according to PREFIX,
COMPLETIONS and NUM. If NUM is t, the overlay's completion-num
property is left unchanged."

  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; if overlay does not already exists, create one
  (unless overlay
    (setq overlay (make-overlay (point) (point)))
    ;; set permanent overlay properties
    (overlay-put overlay 'completion-overlay t)
    (overlay-put overlay 'face 'completion-dynamic-face)
    (overlay-put overlay 'keymap completion-dynamic-map)
    (overlay-put overlay 'help-echo
		 'completion-construct-help-echo-text)
    (overlay-put overlay 'priority 100)
    ;; add overlay to list
    (push overlay completion-overlay-list))
  
  ;; update modifiable overlay properties
  (overlay-put overlay 'prefix prefix)
  (overlay-put overlay 'completions completions)
  (unless (eq num t) (overlay-put overlay 'completion-num num))
  
  ;; return the new overlay
  overlay
)



(defun completion-overlay-at-point (&optional point)
  "Return dynamic completion overlay overlapping point.
\(There should only be one; if not, one is returned at random\)"
  (setq point (or point (point)))

  ;; and overlays starting at POINT
  (let (overlay-list)
    (catch 'found
      ;; check overlays overlapping POINT (including zero-length)
      (setq overlay-list (overlays-in point point))
      (dolist (o overlay-list)
	(when (overlay-get o 'completion-overlay)
	  (throw 'found o)))

      ;; check overlays ending at POINT
      (setq overlay-list (overlays-in (1- point) point))
      (dolist (o overlay-list)
	(when (and (overlay-get o 'completion-overlay)
		   (= (overlay-end o) point))
	  (throw 'found o)))
      
      ;; check overlays starting at POINT
      (setq overlay-list (overlays-in point (1+ point)))
      (dolist (o overlay-list)
	(when (and (overlay-get o 'completion-overlay)
		   (= (overlay-start o) point))
	  (throw 'found o)))
      ))
)



(defun completion-overlays-in (start end)
  "Return list of completion overlays between START and END."

  ;; get overlays between START and END
  (let ((o-list (overlays-in start end))
	overlay-list)
    ;; filter overlay list
    (dolist (o o-list)
      (when (overlay-get o 'completion-overlay)
	(push o overlay-list)))
    ;; return the overlay list
    overlay-list)
)



(defun completion-resolve-old (&optional overlay)
  "Resolve old dynamic completions according to the setting of
`completion-reslove-method'. Any completion overlay specified by
OVERLAY will be left alone."

  ;; temporarily remove ignored overlay from list
  (setq completion-overlay-list
	(delq overlay completion-overlay-list))
  
  (cond
   ;; leave old completions (but accept zero-length ones)
   ((eq completion-resolve-old-method 'leave)
    (mapc (lambda (o)
	    (overlay-put o 'evaporate t)
	    (unless (overlay-buffer o)
	      (setq completion-overlay-list
		    (delq o completion-overlay-list))
	      	    (run-hook-with-args 'completion-accept-functions
					(overlay-get o 'prefix)
					(overlay-get o 'prefix))))
	  completion-overlay-list))
   
   ;; accept old completions
   ((eq completion-resolve-old-method 'accept)
    (mapc (lambda (o)
	    (run-hook-with-args 'completion-accept-functions
				(overlay-get o 'prefix)
				(concat (overlay-get o 'prefix)
					(buffer-substring-no-properties
					 (overlay-start o)
					 (overlay-end o))))
	    (delete-overlay o))
	  completion-overlay-list)
    (setq completion-overlay-list nil))
   
   ;; reject old completions
   ((eq completion-resolve-old-method 'reject)
    (mapc (lambda (o)
	    (run-hook-with-args 'completion-reject-functions
				(overlay-get o 'prefix)
				(concat (overlay-get o 'prefix)
					(buffer-substring-no-properties
					 (overlay-start o)
					 (overlay-end o))))
	    (delete-region (overlay-start o) (overlay-end o))
	    (delete-overlay o))
	  completion-overlay-list)
    (setq completion-overlay-list nil))
   
   ;; ask 'em
   ((eq completion-resolve-old-method 'ask)
    (save-excursion
      (mapc (lambda (o)
	      (goto-char (overlay-end o))
	      ;; FIXME: remove hard-coded face
	      (overlay-put o 'face '(background-color . "red"))
	      (if (y-or-n-p "Accept completion? ")
		  ;; accept
		  (run-hook-with-args
		   'completion-accept-functions
		   (overlay-get o 'prefix)
		   (concat (overlay-get o 'prefix)
			   (buffer-substring-no-properties
			    (overlay-start o)
			    (overlay-end o)))
		;; reject
		(run-hook-with-args
		 'completion-reject-functions
		 (overlay-get o 'prefix)
		 (concat (overlay-get o 'prefix)
			 (buffer-substring-no-properties
			  (overlay-start o)
			  (overlay-end o))))
		(delete-region (overlay-start o) (overlay-end o)))
	      (delete-overlay o)))
	    completion-overlay-list)
      (setq completion-overlay-list nil))))

   
  ;; add ignored overlay back into the list
  (when (overlayp overlay) (push overlay completion-overlay-list))
)



(defun completion-run-if-within-overlay
  (command variable &optional when)
  "Run COMMAND if within a completion overlay.

If WHEN is null or 'instead, run whatever would normally be bound
to the key sequence used to invoke this function if not within a
completion overlay. If WHEN is 'before or 'after, run the normal
binding before or after COMMAND.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset at the
end of this function.

Intended to be bound to a key sequence in a keymap."
  (interactive)

  ;; throw and error if executing recursively
  (when completion-trap-recursion
    (error "Recursive call to `completion-run-if-within-overlay';\
 supplied variable probably doesn't disable keymap"))

  (let ((overlay (completion-overlay-at-point)))
    ;; run command if running before, or if running instead and we're
    ;; within an overlay
    (when (or (eq when 'before)
	      (and (or (null when) (eq when 'instead)) overlay))
      (command-execute command))
  
    ;; run whatever would normally be bound to the key sequence,
    ;; unless running instead and we're within an overlay
    (unless (and (or (null when) (eq when 'instead)) overlay)
      (let ((completion-trap-recursion t)
	    (restore (eval variable))
	    command)
	(set variable nil)
	(setq command
	      (key-binding (this-command-keys) 'accept-default))
	(unwind-protect
	    (when (commandp command) (command-execute command))
	  (set variable restore))))
    
    ;; run command if running after
    (when (eq when 'after) (command-execute command)))
)



(defun completion-construct-tooltip-text
  (prefix completions &optional num)
  "Function to return completion text for a tooltip.
Optional argument NUM specifies the number of the currently
inserted dynamic completion."
  
  (let* ((text "") str
	 (maxlen (apply 'max (mapcar 'length completions))))
    
    (dotimes (i (length completions))
      ;; pad all strings to same length
      (setq str (concat prefix (nth i completions)
			(make-string
			 (- maxlen (length (nth i completions))) ? )))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (when (and completion-use-hotkeys
		 (< i (length completion-hotkey-list)))
	(setq str
	      (concat str " "
		      (format "(%s)"
			      (key-description
			       (nth i completion-hotkey-list))))))
      ;; if current completion is the inserted dynamic completion, use
      ;; `completion-dynamic-face' to highlight it
      (when (and num (= i num))
	;; setting 'face attribute to 'completion-dynamic-face
	;; doesn't seem to work with defface using display classes
	(put-text-property
	 0 (length str) 'face
	 `((foreground-color . ,(face-attribute 'completion-dynamic-face
						:foreground))
	   (background-color . ,(face-attribute 'completion-dynamic-face
						:background)))
	 str))
      (setq text (concat text str "\n")))
      
    ;; return constructed text
    text)
)



(defun completion-cancel-tooltip ()
  "Hide any displayed tooltip and cancel any tooltip timer."
  (interactive)
  (and completion-function
       window-system
       (fboundp 'x-show-tip)
       (tooltip-hide)
       (timerp completion-tooltip-timer)
       (cancel-timer completion-tooltip-timer))
)



(defun completion-construct-echo-text (overlay)
  "Function to return completion text for echo area."
  
  (let* ((prefix (overlay-get overlay 'prefix))
	 (completions (overlay-get overlay 'completions))
	 (text "") str)
    
    (dotimes (i (length completions))
      (setq str (concat prefix (nth i completions)))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (cond
       ((and completion-use-hotkeys
	     (< i (length completion-hotkey-list)))
	(setq str
	      (concat
	       (format "(%s) "
		       (key-description
			(nth i completion-hotkey-list))) str)))
       (completion-use-hotkeys
	(setq str (concat "() " str))))
      (setq text (concat text str "  ")))
    
    ;; return constructed text
    text)
)



(defun completion-construct-help-echo-text (dummy1 overlay dummy2)
  "Function to return text for help-echo property
of completion overlay."
  
  (let* ((text "") str
	 (prefix (overlay-get overlay 'prefix))
	 (completions (overlay-get overlay 'completions))
	 (num (overlay-get overlay 'completion-num)))

    ;; if `tooltip-mode' is enabled, construct text for tooltip
    (if tooltip-mode
	(dotimes (i (length completions))
	  ;; if using hotkeys and one is assigned to current
	  ;; completion, show it next to completion text
	  (if (and completion-use-hotkeys
		   (< i (length completion-hotkey-list)))
	      (setq str
		    (format "(%c)"
			    (key-description
			     (nth i completion-hotkey-list))))
	    (setq str "    "))
	  ;; add completion to text
	  (setq str (concat str " " prefix (nth i completions)))
	  (setq text (concat text str "\n")))

      ;; otherwise, construct text for echo area
      (setq text (completion-construct-echo-text overlay)))
    
    ;; return constructed text
    text)
)




(defun completion-construct-menu (prefix completions)
  "Construct and return menu keymap defining the completion menu."

  (let ((menu (make-sparse-keymap))
	(num (length completions))
	n)
    
    ;; construct menu keymap from available completions
    (dotimes (i num)
      (setq n (- num i 1))
      (define-key menu
	(vector (intern (concat "completion-insert-"
				(number-to-string n))))
	(list 'menu-item
	      (concat prefix (nth n completions))
	      `(lambda () (insert ,(nth n completions)))
	      ;; if a hotkeys is associated with completion, show it
	      ;; in menu
	      :keys (when (and completion-use-hotkeys
			       (< n (length completion-hotkey-list)))
		      (key-description
		       (nth n completion-hotkey-list))))))
    
    ;; add entry to switch to completion browser
    (define-key-after menu [separator-browser] '(menu-item "--"))
    (define-key-after menu [completion-browser]
      (list 'menu-item "Browser..."
	    (lambda ()
	      (completion-show-menu
	       nil (or (and (fboundp 'auto-overlay-local-binding)
			    (auto-overlay-local-binding
			     'completion-browser-menu))
		       'completion-construct-browser-menu))
	      )))
    
    ;; return the menu keymap
    menu)
)



(defun completion-construct-browser-menu
  (prefix completions &optional menu-item-func sub-menu-func)
  "Construct the completion browser menu keymap
from the supplied PREFIX (COMPLETIONS is ignored and replaced by
all completions of PREFIX in the current dictionary).

MENU-ITEM-FUNC and SUB-MENU-FUNC override the default functions
for creating the sub-menus and menu items. Both functions are
passed a 4-item list containing PREFIX, a list of completions of
PREFIX, MENU-ITEM-FUNC and SUB-MENU-FUNC."

  ;; inform user it's in progress (note: can't display "done" message
  ;; since this function returns as soon as main menu is constructed,
  ;; before all submenus have been constructed by :filter functions)
  (message "Creating predictive completion browser\
 (C-g to cancel if taking too long)...")
  
  ;; default menu creation functions
  (unless menu-item-func
    (setq menu-item-func 'completion-browser-menu-item))
  (unless sub-menu-func
    (setq sub-menu-func 'completion-browser-sub-menu))
  
  ;; find all completions of prefix
  (setq completions (funcall completion-function prefix))
  
  ;; main browser menu is just a browser submenu...
  (let ((menu (funcall sub-menu-func
		       prefix completions
		       menu-item-func sub-menu-func)))
    ;; ... with an item added for switching to the basic completion
    ;; menu
    (define-key-after menu [separator-basic] '(menu-item "--"))
    (define-key-after menu [completion-menu]
      (list 'menu-item "Basic..." 'completion-show-menu))
    
    ;; return keymap
    menu)
)



;; Note:
;;
;; We should probably use some `imenu' function to create the menu,
;; since `imenu' already deals with "bucketising" menus (an ugly
;; necessity which should anyway be replaced with menu scrollbars,
;; preferably with just-in-time calculation of menu entries --
;; heads-up Emacs devs!).
;;
;; My excuses are that `imenu--mouse-menu' etc. are undocumented,
;; rolling my own was easier, and anyway I think my buckets are better
;; (they're optimal in the information-theoretic sense that you need
;; to make the least number of choices to get to the entry you want).
;;
;; One day I might patch the `imenu' "bucketising" code, and use
;; `imenu' here instead. Don't hold your breath.

(defun completion-browser-sub-menu
  (prefix completions menu-item-func sub-menu-func)
  "Construct a predictive completion browser sub-menu keymap."
  
  (let* ((menu (make-sparse-keymap))
	 (num-completions (length completions)))
    
    ;; if menu does not need to be divided into buckets, just add the
    ;; completions themselves to the keymap
    (if (< num-completions completion-browser-max-items)
	(dotimes (i num-completions)
	  (define-key-after menu
	    (vector (intern (concat "completion-insert-"
				    (number-to-string i))))
	    (list 'menu-item (concat prefix (nth i completions))
		  ;; call function to generate menu item
		  (funcall menu-item-func
			   prefix (nth i completions)
			   menu-item-func sub-menu-func))))
      
      
      ;; if menu needs to be divided into buckets, construct a menu
      ;; keymap containing the bucket menus
      (let* ((num-buckets
	      (cond
	       ;; maximize number of buckets, minimize size of
	       ;; contents
	       ((eq completion-browser-buckets 'max)
		completion-browser-max-items)
	       ;; minimuze number of buckets, maximize size of
	       ;; contents
	       ((eq completion-browser-buckets 'min)
		(1+ (/ (1- num-completions)
		       completion-browser-max-items)))
	       ;; balance number of buckets and size of contents
	       (t
		(min completion-browser-max-items
		     (round (sqrt num-completions))))))
	     (num-per-bucket (/ num-completions num-buckets))
	     (num-large-buckets (% num-completions num-buckets))
	     (num-small-buckets (- num-buckets num-large-buckets))
	    i j)
	(dotimes (b num-buckets)
	  
	  ;; if bucket has only 1 entry, don't bother with bucket
	  ;; menu, just add completion itself to keymap
	  (if (and (= 1 num-per-bucket) (< b num-small-buckets))
	      (define-key-after menu
		(vector (intern (concat "completion-insert-"
					(number-to-string b))))
		(list 'menu-item (concat prefix (nth b completions))
		      ;; call function to generate menu item
		      (funcall menu-item-func
			       prefix (nth b completions)
			       menu-item-func sub-menu-func)))
	    
	    ;; if bucket has more than 1 entry...
	    ;; index of first completion in bucket
	    (setq i (+ (* (min b num-small-buckets) num-per-bucket)
		       (* (max 0 (- b num-small-buckets))
			  (1+ num-per-bucket))))
	    ;; index of last completion in bucket
	    (setq j (+ i num-per-bucket
		       (if (< b num-small-buckets) 0 1)))
	    ;; add bucket menu to keymap
	    (define-key-after menu
	      (vector (intern (concat "bucket-" (number-to-string b))))
	      (list 'menu-item (concat "From \""
				       prefix (nth i completions)
				       "\" to \""
				       prefix (nth j completions) "\"")
		    ;; call function to generate sub-menu
		    (funcall sub-menu-func
			     prefix (completion--sublist completions i j)
			     menu-item-func sub-menu-func))))
	)))
    
    ;; return constructed menu
    menu)
)




(defun completion-browser-menu-item
  (prefix cmpl menu-item-func sub-menu-func)
  "Construct predictive completion browser menu item."
  
  (let (completions)
    ;; get completions for entry, dropping the empty string which
    ;; corresponds to the same entry again (which would lead to
    ;; infinite recursion)
    (setq completions
	  (funcall completion-function (concat prefix cmpl)))
    (setq completions
	  (mapcar (lambda (c) (concat cmpl c)) completions))
    (setq completions (cdr completions))
    
    ;; if there are no completions (other than the entry itself),
    ;; create a selectable completion item
    (if (null completions)
	`(lambda () (insert ,cmpl))
      (let ((menu (funcall sub-menu-func prefix completions
			   menu-item-func sub-menu-func)))
	;; otherwise, create a sub-menu containing them
	(define-key menu [separator-item-sub-menu] '(menu-item "--"))
	(define-key menu [completion-insert-root]
	  (list 'menu-item (concat prefix cmpl)
		`(lambda () (insert ,cmpl))))
	;; return the menu keymap
	menu)))
)





(defun completion-posn-at-point-as-event
  (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))
  
  (let* ((pos (posn-at-point position window))
	 (x-y (posn-x-y pos))
	 (edges (window-inside-pixel-edges window))
	 (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
	    (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
		  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos))
)



(defun completion-window-posn-at-point (&optional position window)
  "Return pixel position of top left of corner glyph at POSITION,
relative to top left corner of WINDOW. Defaults to the position
of point in the selected window.

See also `completion-window-inside-posn-at-point' and
`completion-frame-posn-at-point'."
  
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  
  (let ((x-y (posn-x-y (posn-at-point position window)))
	(edges (window-inside-pixel-edges window))
	(win-x-y (window-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (- (car win-x-y)))
	  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)))))
)



(defun completion-window-inside-posn-at-point
  (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of the text area in WINDOW. Defaults
to the position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-frame-posn-at-point'.."
  
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (posn-x-y (posn-at-point position window))
)



(defun completion-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-window-inside-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  
  (let ((x-y (posn-x-y (posn-at-point position window)))
	(edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
	  (+ (cdr x-y) (cadr edges))))
)





;;; ===============================================================
;;;                     Compatibility Stuff

;; prevent bogus compiler warnings
(eval-when-compile
  (defun completion-compat-window-offsets (dummy)))



(unless (fboundp 'posn-at-point)
;;  (require 'completion-ui-compat)
  
  
  (defun completion-compat-frame-posn-at-point
    (&optional position window)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
    
    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))
    
    ;; get window-relative position in units of characters
    (let* ((x-y (compute-motion (window-start) '(0 . 0)
				position
				(cons (window-width) (window-height))
				(window-width)
				; prob. shouldn't be 0
				(cons (window-hscroll) 0)
				window))
	   (x (nth 1 x-y))
	   (y (nth 2 x-y))
	   (offset (completion-compat-window-offsets window))
	   (restore (mouse-pixel-position))
	   pixel-pos)
      
      ;; move and restore mouse position using position in units of
      ;; characters to get position in pixels
      (set-mouse-position (window-frame window)
			  (+ x (car offset)) (+ y (cdr offset)))
      (setq pixel-pos (cdr (mouse-pixel-position)))
      (set-mouse-pixel-position (car restore) (cadr restore)
				(cddr restore))
      
      ;; return pixel position
      (setcdr pixel-pos
	      (- (cdr pixel-pos)
		 (/ (frame-char-height (window-frame window)) 2)))
      pixel-pos))
  
  
  
  (defun completion-compat-posn-at-point-as-event
    (&optional position window dx dy)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the
glyph."
  
    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))
    (unless dx (setq dx 0))
    (unless dy (setq dy 0))
    
    ;; get window-relative position in units of characters
  (let* ((x-y (compute-motion (window-start) '(0 . 0)
			      position
			      (cons (window-width) (window-height))
			      (window-width)
			      ; prob. shouldn't be 0
			      (cons (window-hscroll) 0)
			      window))
	 (x (nth 1 x-y))
	 (y (nth 2 x-y))
	 (offset (completion-compat-window-offsets window))
	 (restore (mouse-pixel-position))
	 (frame (window-frame window))
	 (edges (window-edges window))
	 pixel-pos)
    
    ;; move and restore mouse position using position in units of
    ;; characters to get position in pixels
    (set-mouse-position (window-frame window)
			(+ x (car offset)) (+ y (cdr offset)))
    (setq pixel-pos (cdr (mouse-pixel-position)))
    (set-mouse-pixel-position (car restore) (cadr restore)
			      (cddr restore))
    
    ;; convert pixel position from frame-relative to window-relative
    ;; (this is crude and will fail e.g. if using different sized
    ;; fonts)
    (setcar pixel-pos (- (car pixel-pos) 1
			 (* (frame-char-width frame) (car edges))))
    (setcdr pixel-pos (- (cdr pixel-pos) 1
			 (* (frame-char-height frame) (nth 1 edges))
			 (/ (frame-char-height frame) 2)))
    
    ;; return a fake event containing the position
    (setcar pixel-pos (+ (car pixel-pos) dx))
    (setcdr pixel-pos (+ (cdr pixel-pos) dy))
    (list 'mouse-1 (list window position pixel-pos))))
  
  
    
;;; Borrowed from senator.el (I promise I'll give it back when I'm
;;; finished...)
  
  (defun completion-compat-window-offsets (&optional window)
    "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET))
in WINDOW'S frame."
    (let* ((window  (or window (selected-window)))
	   (e       (window-edges window))
	   (left    (nth 0 e))
	   (top     (nth 1 e))
	   (right   (nth 2 e))
	   (bottom  (nth 3 e))
	   (x       (+ left (/ (- right left) 2)))
	   (y       (+ top  (/ (- bottom top) 2)))
	   (wpos    (coordinates-in-window-p (cons x y) window))
	   (xoffset 0)
	   (yoffset 0))
      (if (consp wpos)
	  (let* ((f  (window-frame window))
		 (cy (/ 1.0 (float (frame-char-height f)))))
	    (setq xoffset (- x (car wpos))
		  yoffset (float (- y (cdr wpos))))
	    ;; If Emacs 21 add to:
	    ;; - XOFFSET the WINDOW left margin width.
	    ;; - YOFFSET the height of header lines above WINDOW.
	    (if (> emacs-major-version 20)
		(progn
		  (setq wpos    (cons (+ left xoffset) 0.0)
			bottom  (float bottom))
		  (while (< (cdr wpos) bottom)
		    (if (eq (coordinates-in-window-p wpos window)
			    'header-line)
			(setq yoffset (+ yoffset cy)))
		    (setcdr wpos (+ (cdr wpos) cy)))
		  (setq xoffset
			(floor (+ xoffset
				  (or (car (window-margins window))
				      0))))))
	    (setq yoffset (floor yoffset))))
      (cons xoffset yoffset)))
  
  
  
  (defun completion-compat-line-number-at-pos (pos)
    "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
    (1+ (count-lines (point-min) pos)))
  
  
  
  (defalias 'completion-posn-at-point-as-event
    'completion-compat-posn-at-point-as-event)
  (defalias 'completion-frame-posn-at-point
    'completion-compat-frame-posn-at-point)
)


;;; completion-ui.el ends here
