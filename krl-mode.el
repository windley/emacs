

(provide 'krl-mode)

;; the command to comment/uncomment text
(defun krl-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way. For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) 
	 (comment-start " //") 
	 (comment-end ""))
     (comment-dwim arg)))

;; define several class of keywords
(defvar krl-keywords nil
  "KRL keywords.")

(setq krl-keywords   
      '("ruleset" "meta" "dispatch" "global"
	"rule" "is" "select" "when" "using" "setting" "foreach" "pre" "if" "then" "every" "choose" 
	"callbacks" "success" "failure" "fires" "always"
	"function" "datasource" "dataset" "="
        "set" 
	))

(defvar krl-types nil
  "KRL types.")

(defvar krl-constants nil
  "KRL constants.")

(setq  krl-constants   
       '("active" "inactive" "test"
	 ))

(defvar krl-events nil
  "KRL events.")

(defvar krl-functions nil
  "KRL functions.")

(setq krl-functions
  '("pick"
    "length" "map" "filter" "head" "tail"
    "match" "replace"
    ))


;; create the regex string for each class of keywords
(defvar krl-keywords-regexp (regexp-opt krl-keywords 'words))
(defvar krl-type-regexp (regexp-opt krl-types 'words))
(defvar krl-constant-regexp (regexp-opt krl-constants 'words))
(defvar krl-event-regexp (regexp-opt krl-events 'words))
(defvar krl-functions-regexp (regexp-opt krl-functions 'words))

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq krl-font-lock-keywords
  `(
;    ("\\(<<\\(?:.\\)*?>>\\)" . font-lock-string-face)
    (,krl-type-regexp . font-lock-type-face)
    (,krl-constant-regexp . font-lock-constant-face)
    (,krl-event-regexp . font-lock-builtin-face)
    (,krl-functions-regexp . font-lock-function-name-face)
    (,krl-keywords-regexp . font-lock-keyword-face)
    ("\\(#\\){[^}]*}" . (0 font-lock-constant-face t))
))

(defvar krl-mode-map
  (let ((map (make-keymap))
	)
	(define-key map "\C-c\C-s" 'ruleset-skeleton)
	(define-key map "\C-c\C-r" 'rule-skeleton)
	(define-key map [remap comment-dwim] 'krl-comment-dwim)
	map
	)
  "Keymap for KRL mode"
  )

;; define the mode
(define-derived-mode krl-mode fundamental-mode
  "KRL Mode"
  "Major mode for editing KRL (Kynetx Rule Language)..."
  ;; modify the keymap
  (use-local-map krl-mode-map)

  ;; code for syntax highlighting
  (make-local-variable 'font-lock-defaults)
  (set (make-local-variable (quote font-lock-multiline)) t)
  (setq font-lock-defaults '((krl-font-lock-keywords)))

  ;; C++ style comment: "// ..." 
  (modify-syntax-entry ?\s ". 1b" krl-mode-syntax-table)
  (modify-syntax-entry ?\/ ". 23b" krl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" krl-mode-syntax-table)


  )




(define-skeleton ruleset-skeleton
  "inserts a KRL ruleset skeleton into the current buffer"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 nil)

  "ruleset " (or v1 (setq v1 (skeleton-read "Ruleset name: "))) " {" \n
  "  meta {\n" 
  "    name \"" v1 "\"\n" 
  "    description <<\n" (or v2 (setq v2 (skeleton-read "Ruleset description: "))) "\n>>\n" 
  "    author \"" (or v3 (setq v3 (skeleton-read "Author: "))) "\"\n\n" 
  "    // use module v1_wrangler alias wrangler\n" 
  "    sharing off\n\n" 
  "    // provides \n"  
  "  }\n\n" 
  "  global {" \n "    " _ \n \n "  }" \n \n
  "}"
  )


(define-skeleton rule-skeleton
  "inserts a KRL rule skeleton into the current buffer"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 nil)
  "  rule " (or v1 (setq v1 (skeleton-read "Rule name: "))) " {" \n
  "    select when " _ "\n"
  "  }"
)


