

(provide 'krl-mode)

;; the command to comment/uncomment text
(defun krl-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way. For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "//") (comment-end ""))
     (comment-dwim arg)))

;; define several class of keywords
(defvar krl-keywords nil
  "KRL keywords.")

(setq krl-keywords   
      '("ruleset" "meta" "dispatch" "global"
	"rule" "is" "select" "when" "using" "setting" "foreach" "pre" "if" "then" "every" "choose" 
	"callbacks" "success" "failure" "fires" "always"
	"function" "datasource" "dataset" 
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
;;    ("\\(<<\\(?:.\\)*?>>\\)" . font-lock-string-face)
    (,krl-type-regexp . font-lock-type-face)
    (,krl-constant-regexp . font-lock-constant-face)
    (,krl-event-regexp . font-lock-builtin-face)
    (,krl-functions-regexp . font-lock-function-name-face)
    (,krl-keywords-regexp . font-lock-keyword-face)
    ("\\(#\\){[^}]*}" . (0 font-lock-constant-face t))
))

;; define the mode
(define-derived-mode krl-mode fundamental-mode
  "KRL Mode"
  "Major mode for editing KRL (Kynetx Rule Language)..."
  ;; modify the keymap
  (define-key krl-mode-map [remap comment-dwim] 'krl-comment-dwim)

  ;; code for syntax highlighting
  (make-local-variable 'font-lock-defaults)
  (set (make-local-variable (quote font-lock-multiline)) t)
  (setq font-lock-defaults '((krl-font-lock-keywords)))

  ;; C++ style comment: "// ..." 
  (modify-syntax-entry ?\/ ". 12b" krl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" krl-mode-syntax-table)


  ;; ...
)



