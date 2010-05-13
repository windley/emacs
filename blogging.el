;;; blogging stuff

(define-skeleton blogging-skeleton
  "inserts a blog skeleton into the current buffer"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 "newsletter")
  "<!-- title: \n"   (or v1 (setq v1 (skeleton-read "Title: "))) "\n-->\n"
  "<!-- category: newsletter -->\n"
  "<!-- keywords: \n"   (or v2 (setq v2 (skeleton-read "keywords: "))) "\n-->\n"
  "<p>" \n _  \n "</p>")

(defun make-blog-name (name)
    (random t)
    (concat name
	    (format-time-string "-%Y%m%d-") 
	    (number-to-string (random 100))
            ".html"))

(setq *blog-dir* "~/Documents/blogging/")

(defun blog (name)
   "start blogging!"
   (interactive "sname: " "tech")
   (let ((name (make-blog-name name))
	 )
     (find-file (concat *blog-dir* name))
     (blogging-skeleton)))

(global-set-key "\C-x\M-b" 'blog)


(defun unutf ()
  "remove UTF-8 chars from text"
  (interactive)

  (point-to-register 'x)

  (mapcar 
   (lambda (x)
     (beginning-of-buffer)
     (while (search-forward (car x) nil t)
       (replace-match (cdr x) nil t)))
   `((,(string (make-char 'mule-unicode-0100-24ff  114 124)) . "\"")
     (,(string (make-char 'mule-unicode-0100-24ff  114 125)) . "\"")
     (,(string (make-char 'mule-unicode-0100-24ff  114 120)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  114 121)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  114 115)) . "--")
     (,(string (make-char 'mule-unicode-0100-24ff  114 116)) . "---")
     (,(string (make-char 'mule-unicode-e000-ffff  117 61)) . "\'")
     (,(string (make-char 'mule-unicode-0100-24ff  115 38)) . "...")
     ("'" . "'")
     ("&" . "&")
     ("\"" . "\"")))

  (jump-to-register 'x)
  )


(defun remove-brackets (beginning end)
  "remove UTF-8 chars from text"
  (interactive "r")
  (mapcar 
   (lambda (x)
     (goto-char beginning)
     (while (search-forward (car x) nil t)
       (replace-match (cdr x) nil t)))
   `(("<" . "&lt;")
     (">" . "&gt;")
     )
   )
  )

;;(set-face-attribute 'default nil :family "lucida" :height 100)
;;(set-face-attribute 'default nil :family "lucida" :height 120)
;;(set-face-attribute 'default nil :family "lucida" :height 140)

