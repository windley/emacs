;; HOL mode, and its idiosyncratic commands.
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.
;; Adapted from xscheme.el and sml.el by Phil Windley

;; Caveat --- much of this was used vebatim without much thought.
;; It seems to work mostly, but odd things will need to be corrected.

(provide 'hol)

(defvar *hol-mode-syntax-table* nil "")
(defvar *hol-mode-abbrev-table* nil "")

(if (not *hol-mode-syntax-table*)
    (let ((i 0))
      (setq *hol-mode-syntax-table* (make-syntax-table))
      (set-syntax-table *hol-mode-syntax-table*)

      ;; Default is atom-constituent.
      (while (< i 256)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))

      ;; Word components.
      (setq i ?0)
      (while (<= i ?9)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))

      ;; Whitespace
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n "    ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?  "    ")

      ;; These characters are delimiters but otherwise undefined.
      ;; Brackets and braces balance for editing convenience.
      (modify-syntax-entry ?[ "(]  ")
      (modify-syntax-entry ?] ")[  ")
      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")

      ;; Other atom delimiters
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")
      (modify-syntax-entry ?\% "<%  ")
      (modify-syntax-entry ?\% ">%  ")
      (modify-syntax-entry ?' "''  ")
      (modify-syntax-entry ?` "'`  ")
      (modify-syntax-entry ?\" "\"\"  ")

      ;; Special characters
      (modify-syntax-entry ?\| ".   ")
      (modify-syntax-entry ?* ". 23")
      (modify-syntax-entry ?, ".   ")
      (modify-syntax-entry ?; ".   ")
      (modify-syntax-entry ?@ ".   ")
      (modify-syntax-entry ?# ".   ")))

(define-abbrev-table '*hol-mode-abbrev-table* ())


(defun hol-mode-variables ()
  (set-syntax-table *hol-mode-syntax-table*)
  (setq local-abbrev-table *hol-mode-abbrev-table*)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'hol-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'hol-comment-indent))

(defun hol-mode-commands (map)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\t" 'hol-indent-line)
  (define-key map "\C-ci" 'hol-insert-comment)
  (define-key map "\C-cd" 'hol-definition)
  (define-key map "\C-ca" 'hol-num-asm)
  (define-key map "\C-ch" 'hol-help)
  )


(defvar *hol-mode-map* (make-sparse-keymap))
(hol-mode-commands *hol-mode-map*)

(defun hol-mode ()
  "Major mode for editing Hol code.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Comments are enclosed in 
percent signs.
\\{*hol-mode-map*}
Entry to this mode calls the value of hol-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map *hol-mode-map*)
  (setq major-mode 'hol-mode)
  (setq mode-name "Hol")
  (hol-mode-variables)
  (run-hooks 'hol-mode-hook))

(autoload 'run-hol "xhol"
  "Run an inferior Hol process.
Output goes to the buffer `*hol*'.
With argument, asks for a command line."
  t)


;(defvar *hol-indent-offset* nil "")

;; When asterisks were added to the variable names, they were also added
;; to 'hol-indent-hook inside the variable *hol-indent-hook*, making it
;; '*hol-indent-hook*. <== These asterisks may need to be removed (but not
;; those in the variable name *hol-indent-hook* itself).

;(defvar *hol-indent-hook* '*hol-indent-hook* "")

(defun hol-insert-comment ()
   (interactive)
   (insert-before-markers 
"%----------------------------------------------------------------

----------------------------------------------------------------%"))

;;; hol definition is here

(defun hol-def-string (definiens hol-name ml-name)
  (insert "\nlet " 
	  ml-name 
	  " = new_definition\n   (`" 
	  hol-name 
	  "`,\n    """ 
	  definiens " "))


(defun hol-definition ()
  (interactive)
  (let ((ml-name (get-ml-name)))
    (let ((hol-name (get-hol-name ml-name)))
      (let ((definiens (get-definiens hol-name)))
	(hol-def-string definiens hol-name ml-name)
	(insert "\"\n   );;"))
	(backward-char 8))))


(defun get-definiens (hol-nmue)
  (let ((proto-definiens 
	 (read-from-minibuffer "Definiens: ")))
    (if (equal proto-definiens "")
	hol-name
      proto-definiens)))


(defun get-ml-name ()
  (let ((ml-name (read-from-minibuffer "ML Name: ")))
    (if (equal ml-name "")
	(progn
	  (message "ML Name must not be empty")
	  (sleep-for 1)
	  (get-ml-name))
      ml-name)))

(defun get-hol-name (ml-name)
  (let ((hol-name (read-from-minibuffer 
		     (concat "HOL Name: (default " (downcase ml-name) ") "))))
    (if (equal hol-name "")
	(downcase ml-name)
      hol-name)))

;;; hol assumption numbering is here.

(defun hol-num-asm ()
  "Number HOL assumptions *Note* cursor must be below last assumption.
   To remove numbers, set the argument to negative.  A zero argument
   will try to number all assumptions         TS 6/21/90 15:50"
  (interactive)
  (progn
   (setq pos (point))
   (setq thm 0)
   (search-backward "()")
   (setq beg (point))
   (goto-char pos)
   (setq control (string-to-int (read-string "number of assumptions? " "0")))
   (if (> control 0) 
     (setq num control) 
     (setq num (- control)))
   (if (= control 0)     (setq num 1000))
   (while (< thm num)   
     (search-backward "[")
     (beginning-of-line)
     (setq thm (1+ thm))
     (if (= control 0)
        (and
         (setq tmp (point))
         (if (< tmp beg)
           (setq thm (1+ num))
           (and 
            (princ (format "%2d" thm) (get-buffer(buffer-name)))
            (delete-char 2))) )
        (and 
         (if (> control 0)
           (princ (format "%2d" thm) (get-buffer(buffer-name)))
           (princ "  "  (get-buffer(buffer-name))) ) 
         (delete-char 2))))
   (goto-char pos)
))



;;; HOL Help stuff is here.

(setq hol-help-buffer-name "*HOL Help*")

(defun apropos (string)
  (start-process "lookup" 
		 hol-help-buffer-name 
		 "/usr/pkg/hol/hol201/contrib/Xhelp/hol_apro"
		 string))

(defun full-entry (string)
  (start-process "lookup" 
		 hol-help-buffer-name 
		 "/usr/pkg/hol/hol201/contrib/Xhelp/hol_ref"
		 string))

(defun theorem-entry (string)
  (start-process "lookup" 
		 hol-help-buffer-name 
		 "/usr/pkg/hol/hol201/contrib/Xhelp/hol_thm"
		 string))

(defun no-choice-function (string)
  "Illegal entry")

(defun hol-help (choice search-string)
  "On-Line help for HOL"
  (interactive "ca)propos, f)ull entry, or t)heorem entry:\nsEntry: ")
  (set-buffer (get-buffer-create hol-help-buffer-name))
  (message "Getting HOL help...")
  (setq buffer-read-only nil)
  (erase-buffer)
  (cond ((char-equal choice ?a) (apropos search-string))
	((char-equal choice ?f) (full-entry search-string))
	((char-equal choice ?t) (theorem-entry search-string))
	(t (no-choice-function search-string)))
  (setq buffer-read-only t)
  (if (not (get-buffer-window hol-help-buffer-name))
      (switch-to-buffer-other-window hol-help-buffer-name))
  (message ""))




;; The amount of indentation of blocks
(defconst hol-indent-level 4 "*Indentation of blocks in hol.")


;; The amount of negative indentation of lines beginning with "|"
(defconst hol-pipe-indent -2
  "*Extra (negative) indentation for lines beginning with |.") ;

;; How do we indent case-of expressions.
(defconst hol-case-indent nil
  "*How to indent case-of expressions.
  If t:   case expr              If nil:   case expr of
            of exp1 => ...                     exp1 => ...
             | exp2 => ...                   | exp2 => ...
\nThe first seems to be the standard in NJ-HOL. The second is the default.")

(defconst hol-nested-if-indent nil
  "*If set to t, nested if-then-else expression will have the same
indentation as:
                 if exp1 then exp2
                 else if exp3 then exp4
                 else if exp5 then exp6
                      else exp7")

(defconst hol-type-of-indent t
  "*How to indent `let' `struct' etc.
If t:
          fun foo bar = let
                           val p = 4
                        in
                           bar + p
                        end
If nil:
          fun foo bar = let
              val p = 4
          in
              bar + p
          end
\nWill not have any effect if the starting keyword is first on the line.")

(defconst hol-paren-lookback 5000)

(defun hol-indent-line ()
  "Indent current line of hol code."
  (interactive)
  (let ((indent (hol-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(let ((beg (progn (beginning-of-line) (point))))
	  (skip-chars-forward "\t ")
	  (delete-region beg (point))
	  (indent-to indent))
      ;; If point is before indentation, move point to indentation
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward "\t ")))))

(defconst hol-indent-starters-reg
  "abstraction\\b\\|abstype\\b\\|and\\b\\|case\\b\\|datatype\\b\
\\|else\\b\\|fun\\b\\|functor\\b\\|if\\b\
\\|in\\b\\|infix\\b\\|infixr\\b\\|let\\b\\|local\\b\
\\|nonfix\\b\\|of\\b\\|open\\b\\|sig\\b\\|signature\\b\
\\|struct\\b\\|structure\\b\\|then\\b\\|\\btype\\b\\|val\\b\
\\|while\\b\\|with\\b\\|withtype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst hol-starters-reg
  "\\babstraction\\b\\|\\babstype\\b\\|\\bdatatype\\b\
\\|\\bexception\\b\\|\\bfun\\b\\|\\bfunctor\\b\\|\\blocal\\b\
\\|\\binfix\\b\\|\\binfixr\\b\
\\|\\bnonfix\\b\\|\\bopen\\b\\|\\bsignature\\b\\|\\bstructure\\b\
\\|\\btype\\b\\|\\bval\\b\\|\\bwithtype\\b"
  "The starters of new expressions.")

(defconst hol-end-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bsig\\b\\|\\bstruct\\b"
  "Matching reg-expression for the \"end\" keyword")

(defconst hol-starters-indent-after
  "let\\b\\|local\\b\\|struct\\b\\|in\\b\\|sig\\b\\|with\\b")

(defun hol-calculate-indentation ()
  (save-excursion
    (beginning-of-line)			; Go to first non whitespace
    (skip-chars-forward "\t ")		; on the line.
    (cond
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line. Search only for the
     ;; next "*)", not for the matching.
     ((looking-at "(\\*")
      (if (not (search-forward "*)" nil t))
	  (error "Comment not ended."))
      (skip-chars-forward "\n\t ")
      ;; If we are at eob, just indent 0
      (if (eobp) 0 (hol-calculate-indentation)))
     ;; Are we looking at a case expression ?
     ((looking-at "|.*\\(\\|\n.*\\)=>")
      (hol-skip-block)
      (hol-re-search-backward "=>")
      (beginning-of-line)
      (skip-chars-forward "\t ")
      (cond
       ((looking-at "|") (current-indentation))
       ((and hol-case-indent (looking-at "of\\b"))
	(1+ (current-indentation)))
       ((looking-at "fn\\b") (1+ (current-indentation)))
       ((looking-at "handle\\b") (+ (current-indentation) 5))
       (t (+ (current-indentation) hol-pipe-indent))))
     ((looking-at "and\\b")
      (if (hol-find-matching-starter hol-starters-reg)
	  (current-column)
	0))
     ((looking-at "in\\b")		; Match the beginning let/local
      (hol-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
     ((looking-at "end\\b")		; Match the beginning
      (hol-find-match-indent "end" "\\bend\\b" hol-end-starters-reg))
     ((and hol-nested-if-indent (looking-at "else[\t ]*if\\b"))
      (hol-re-search-backward "\\bif\\b\\|\\belse\\b")
      (current-indentation))
     ((looking-at "else\\b")		; Match the if
      (hol-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
     ((looking-at "then\\b")		; Match the if + extra indentation
      (+ (hol-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	 hol-indent-level))
     ((and hol-case-indent (looking-at "of\\b"))
      (hol-re-search-backward "\\bcase\\b")
      (+ (current-column) 2))
     ((looking-at hol-starters-reg)
      (let ((start (point)))
	(hol-backward-sexp)
	(if (and (looking-at hol-starters-indent-after)
		 (/= start (point)))
	    (+ (if hol-type-of-indent
		   (current-column)
		 (if (progn (beginning-of-line)
			    (skip-chars-forward "\t ")
			    (looking-at "|"))
		     (- (current-indentation) hol-pipe-indent)
		   (current-indentation)))
	       hol-indent-level)
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (if (and (looking-at hol-starters-indent-after)
		   (/= start (point)))
	      (+ (if hol-type-of-indent
		     (current-column)
		   (current-indentation))
		 hol-indent-level)
	    (goto-char start)
	    (if (hol-find-matching-starter hol-starters-reg)
		(current-column)
	      0)))))
     (t
      (let ((indent (hol-get-indent)))
	(cond
	 ((looking-at "|")
	  ;; Lets see if it is the follower of a function definition
	  (if (hol-find-matching-starter
	       "\\bfun\\b\\|\\bfn\\b\\|\\band\\b\\|\\bhandle\\b")
	      (cond
	       ((looking-at "fun\\b") (- (current-column) hol-pipe-indent))
	       ((looking-at "fn\\b") (1+ (current-column)))
	       ((looking-at "and\\b") (1+ (1+ (current-column))))
	       ((looking-at "handle\\b") (+ (current-column) 5)))
	    (+ indent hol-pipe-indent)))
	 (t
	  (if hol-paren-lookback	; Look for open parenthesis ?
	      (max indent (hol-get-paren-indent))
	    indent))))))))

(defun hol-get-indent ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward "\t\n; ")
    (if (looking-at ";") (hol-backward-sexp))
    (cond
     ((save-excursion (hol-backward-sexp) (looking-at "end\\b"))
      (- (current-indentation) hol-indent-level))
     (t
      ;; Go to the beginning of the line and place by first
      ;; non-whitespace, but pass over starting parenthesis
      (beginning-of-line)		
      (skip-chars-forward "\t (|")
      (let ((indent (current-column)))
	(cond
	 ;; Started val/fun/structure...
	 ((looking-at hol-indent-starters-reg) (+ indent hol-indent-level))
	 ;; Indent after "=>" pattern
	 ((looking-at ".*=>") (+ indent hol-indent-level))
	 ;; else keep the same indentation as previous line
	 (t indent)))))))

(defun hol-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) hol-paren-lookback) (point-min)))
          (loop t) (here (point)))
      (while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1) loop)
	(if (re-search-backward "[][{}()]" backpoint t)
	    (if (not (hol-inside-comment-or-string-p))
		(cond
		 ((looking-at "(") (setq levelpar (1+ levelpar)))
		 ((looking-at ")") (setq levelpar (1- levelpar)))
		 ((looking-at "\\[") (setq levelsqr (1+ levelsqr)))
		 ((looking-at "\\]") (setq levelsqr (1- levelsqr)))
		 ((looking-at "{") (setq levelcurl (1+ levelcurl)))
		 ((looking-at "}") (setq levelcurl (1- levelcurl)))))
	  (setq loop nil)))
      (if loop
	  (1+ (current-column))
	0))))

(defun hol-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (progn
		(search-backward "(*")
		(search-forward "*)")
		(forward-char -1)	; A "*)" is not inside the comment
		(> (point) start))
	    (error nil)))
	t
      (let ((numb 0))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (progn (beginning-of-line) (point)) start)
	    (condition-case ()
		(while t
		  (search-forward "\"")
		  (setq numb (1+ numb)))
	      (error (if (and (not (zerop numb))
			      (not (zerop (% numb 2))))
			 t nil)))))))))
		
(defun hol-skip-block ()
  (hol-backward-sexp)
  (cond 
   ;; If what we just passed was a comment, then go backward to
   ;; some code, as code is indented according to other code and
   ;; not according to comments.
   ((looking-at "(\\*")
    (skip-chars-backward "\t\n "))
   ;; Skip over let-in-end/local-in-end etc...
   ((looking-at "end\\b")
    (goto-char (hol-find-match-backward "end" "\\bend\\b"
					hol-end-starters-reg))
    (skip-chars-backward "\n\t "))
   ;; Here we will need to skip backwardd past if-then-else
   ;; and case-of expression. Please - tell me how !!
   ))

(defun hol-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((level 1) (here (point))
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (hol-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun hol-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (hol-find-match-backward unquoted-this this match))
    (if (or hol-type-of-indent indented)
	(current-column)
      (if (progn
	    (beginning-of-line)
	    (skip-chars-forward "\t ")
	    (looking-at "|"))
	  (- (current-indentation) hol-pipe-indent)
	(current-indentation)))))

(defun hol-find-matching-starter (regexp)
  (let ((start-let-point (hol-point-inside-let-etc))
	(start-up-list (hol-up-list))
	(found t))
    (if (hol-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (hol-up-list))
			 (/= start-let-point (hol-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun hol-point-inside-let-etc ()
  (let ((last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (hol-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (hol-find-match-backward "end" "\\bend\\b"
						  hol-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))
		     
(defun hol-re-search-backward (regexpr)
  (let ((found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (hol-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun hol-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun hol-backward-sexp ()
  (condition-case ()
      (progn
	(backward-sexp 1)
	(if (looking-at "(\\*")
	    (backward-sexp 1)))
    (error nil)))

(defun hol-comment-indent ()
  (if (looking-at "^(\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (1+ (max (current-column)		; Else indent at comment column
	       comment-column)))))	; except leave at least one space.

