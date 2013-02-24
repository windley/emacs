;;; blogging stuff

(require 'weblogger)

(define-skeleton blogging-skeleton
  "inserts a blog skeleton into the current buffer"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 nil)
  "<!-- title: \n"   (or v1 (setq v1 (skeleton-read "Title: "))) "\n-->\n"
  "<!-- keywords: \n"   (or v2 (setq v2 (skeleton-read "Keywords: "))) "\n-->\n"
  "<!-- date: "  (format-time-string "%B %d, %Y %H:%M") " -->\n"
  "<!-- author: <a href='http://phil.windley.org'>Phil Windley</a> -->\n"
  "<!-- excerpt: \n"   (or v3 (setq v3 (skeleton-read "Excerpt: "))) "\n-->\n"
  "<p>" \n _  \n "</p>")

(defun make-blog-name (name)
    (random t)
    (let ((dir-name (concat *blog-dir* name (format-time-string "/%Y/%m/%d/"))
		    )
	  )
      (concat 
       dir-name
       name
       "-"
       (format-time-string "%H%M%S")
;;       (number-to-string (random 100))
       ".html")
      ))

;;(find-file (make-blog-name "tech"))

;; must end in /
(setq *blog-dir* "~/Dropbox/Documents/blogging/")

(defun blog ()
   "start blogging!"
   (interactive)
   (let* ((name "tm")
	  (full-name (make-blog-name name))
	  (dir-name (file-name-directory full-name))
	  )
     (make-directory dir-name t)
     (find-file full-name)
     (blogging-skeleton)
     ))

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

