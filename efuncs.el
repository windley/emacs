;; my personal functions and key bindings

;; unbind exit key sequence to prevernt accidental exits
(global-unset-key "\C-x\C-c")

(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\M-4" 'ispell-word)
(global-set-key "\C-cj" 'flyspell-check-previous-highlighted-word)

;; Insert date at this point
(defun insert-date () 
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;; edit the file Journal.txt and put a datastamp at the end
(fset 'journal
   [?\C-x ?\C-f ?\C-f ?\C-a ?\C-k ?~ ?/ ?D ?o ?c tab ?p ?e ?r tab ?J ?o ?u ?r tab ?J ?o ?u ?r tab ?. tab return escape ?> return escape ?x ?i ?n ?s ?e ?  ?d ?  return return return ?\C-x ?\C-s])

;; (defun try-complete-abbrev (old)
;;    (if (expand-abbr`ev) t nil))
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;; 	try-complete-file-name
;; 	try-expand-dabbrev))

(defun mark-buffer-and-copy ()
  "Mark the entire buffer and put it in the kill ring"
  (interactive)
    (kill-ring-save (point-min) (point-max)))

(global-set-key "\C-xh" 'mark-buffer-and-copy)

;; count words in region (from elisp intro)
(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

;; Count the words in the entire document
(defun count-words-buffer ()
  "Count all the words in the buffer"
  (interactive)
  (count-words-region (point-min) (point-max) )
)

;; Key mapping for counting words in buffer
(global-set-key "\C-c\C-cw" 'count-words-buffer)


;; from http://blog.tuxicity.se/elisp/emacs/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

;(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(global-set-key "\C-x\C-i" 'indent-region)

(defun unfill(beg end) 
   (interactive "r") 
   (shell-command-on-region beg end "fmt -w2000" nil t))


(defun entify-region (beginning end)
  "Turn < and > into respective entities"
  (interactive "r")
  (save-excursion
    (narrow-to-region beginning end)
    (beginning-of-buffer)
    (replace-string "<" "&lt;")
    (beginning-of-buffer)
    (replace-string ">" "&gt;")
    (widen)
    )
  )

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))
