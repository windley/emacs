
(setq sgml-basic-offset 0)

(define-skeleton html-paragraph
  "HTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  \n "<p>" \n _ \n "</p>" \n \n )

(define-skeleton html-emphasis
  "HTML emphasis mode."
  nil
  "<em>" _ "</em>")

(define-skeleton html-bold
  "HTML bold mode."
  nil
  "<b>" _ "</b>")

(define-skeleton html-typewriter
  "HTML typewriter mode."
  nil
  "<tt>" _ "</tt>")


(define-skeleton html-code
  "HTML code mode."
  nil
  "<code>" _ "</code>")

(define-skeleton html-code-block
  "HTML code block."
  nil
  (if (bolp) nil ?\n)
  \n "<pre class='code'>" \n _ \n "</pre>" \n \n )


(define-skeleton html-entity-lt
  "HTML less than entity."
  nil
  "&lt;")

(define-skeleton html-entity-gt
  "HTML greater than entity."
  nil
  "&gt;")

(define-skeleton insert-mdash
  "Insert an mdash entity at point"
  nil
  "&mdash;"
  )

(define-skeleton google-link
  "HTML anchor tag with href attribute."
  "word: "
  '(setq input "")
  "<a href=\"http://www.google.com/search?q=" str "\">" str "</a>")

(define-skeleton twitter-link
  "HTML anchor tag with href attribute."
  "twitter ID: "
  '(setq input "")
  "<a href=\"http://www.twitter.com/" str "\">@" str "</a>")

(define-skeleton wikipedia-link
  "HTML anchor tag with href attribute."
  "word: "
  '(setq input "")
  "<a href=\"http://en.wikipedia.org/wiki/" str "\">" str "</a>")

(define-skeleton twitter-link
  "Twitter link"
  "twitter id without the @: "
  '(setq input "")
  "<a href=\"http://twitter.com/" str "\">@" str "</a>")

(define-skeleton tag-link
  "HTML anchor tag with href attribute."
  "tag: "
  '(setq input "")
  "<a href=\"http://www.windley.com/tags/" str "\">" str "</a>")

(define-skeleton amazon-image
  "HTML image tag for an Amazon book"
  "ASIN: "
  '(setq input "")
  "<a href=\"http://www.amazon.com/exec/obidos/ASIN/" str "/windleyofente-20\"><img src=\"http://images.amazon.com/images/P/" str ".01.MZZZZZZZ.jpg\" border=\"0\" align=\"right\" hspace=\"5\" vspace=\"5\" /></a>"
  )


(define-skeleton amazon-link
  "HTML anchor for an Amazon book"
  "ASIN: "
  '(setq input "")
  "<a href=\"http://www.amazon.com/exec/obidos/ASIN/" str "/windleyofente-20\">" _ "</a>"
  )

(define-skeleton mp3-player
  "HTML image tag for an Amazon book"
  "MP3 URL: "
  '(setq input "")
  "<embed type=\"application/x-shockwave-flash\" flashvars=\"audioUrl=" str "\" src=\"http://www.google.com/reader/ui/3523697345-audio-player.swf\" width=\"400\" height=\"27\" quality=\"best\"></embed>"
  )

(define-skeleton readings
  "HTML for what I'm reading"
  nil
  '(setq v1 nil
	 v2 nil)
  ("Value: "
   "<li><a href=\"" (skeleton-read "URL: ")
   "\">" (skeleton-read "Anchor: ")
   "</a> - "(skeleton-read "Commentary: ")"</li>"
   \n))


(define-skeleton ruleset-image
  "HTML anchor and card image for a Kynetx ruleset"
  "RID: "
  '(setq input "")
  "<a href=\"http://apps.kynetx.com/app/"str"\"><img style=\"margin-top: 10px; margin-left:10px\" src=\"http://appresource.s3.amazonaws.com/appdir/" str "/appimage.jpg\" border=\"0\" hspace=\"3\" vspace=\"3\" align=\"right\" title=\"Kynetx App\" alt=\"Kynetx Logo\" width=\"150px\" /></a>"
  )

(define-skeleton ruleset-link
  "HTML anchor for a Kynetx ruleset in the app directory"
  "RID: "
  '(setq input "")
  "<a href=\"http://apps.kynetx.com/app/"str"\">" _ "</a>"
  )



(define-skeleton picture-table
  "Create a table of pictures"
  nil
  '(setq v1 nil
	 v2 nil)
  "<table cellpadding=\"5\" border=\"0\" width=\"90%\"><tr>"
  ("picture thumbnail: " 
   (if (not (string-equal v2 "")) (identity "<tr>") "")
   \n
   -1
   "<td valign=\"top\" align=\"center\">"
   \n
   " <a href=\"" (setq v1 (skeleton-read "picture url: "))
    "\">" \n "<img src=\"" str
    "\" border=\"1\"  />" \n "</a>"
   " <div class=\"caption\">"
    (skeleton-read "caption: " "")
   "</div>"
   \n
   "</td>"
   (setq v2 (if (y-or-n-p "End of row? ")
	       (funcall skeleton-transformation "</tr>") ""))
   \n
   -2
   \n)
   resume:
   "</table>" 
   \n
   )

(define-skeleton make-event
  "Create HTML for hCalendar formatted event"
  nil
  '(setq v1 nil
	 v2 nil
	 v3 nil
	 y1 nil
	 m1 nil
	 d1 nil
	 h1 nil
	 mi1 nil
	 yc (format-time-string "%Y")
	 mc (format-time-string "%m")
	 dc (format-time-string "%d")
	 hc (format-time-string "%H")
	 mic (format-time-string "%M")
	 duration 1
	 )
  "<div class=\"vevent\">"
  \n
  "<span class=\"summary\"><a href=\"" 
   (or v2 (setq v3 (skeleton-read "URL: ")))
  "\">"
   (or v2 (setq v2 (skeleton-read "What: ")))
  "</a></span> on <abbr class=\"dtstart\" title=\""
  (format "%04d%02d%02dT%02d%02d00" 
	  (or y1 (setq y1 (string-to-number (skeleton-read "Start Year: " yc))))
	  (or m1 (setq m1 (string-to-number (skeleton-read "Start Month: " mc))))
	  (or d1 (setq d1 (string-to-number (skeleton-read "Start Day: " dc))))
	  (or h1 (setq h1 (string-to-number (skeleton-read "Start Hour: " hc))))
	  (or mi1 (setq mi1 (string-to-number (skeleton-read "Start Minute: " mic))))
	  )
   "\">"
   (format-time-string "%B" (encode-time 0 mi1 h1 d1 m1 y1)) " "
   (number-to-string d1) ", " 
   (number-to-string y1) " from " 
   (number-to-string h1) ":" (format "%02d" mi1)
  "</abbr> until <abbr class=\"dtend\" title=\""
  (format "%04d%02d%02dT%02d%02d00" 
	  y1
	  m1
	  d1
	  (setq h2 (+ h1 duration))
	  mi1
	  )
   "\">"
   (number-to-string h2) ":" (format "%02d" mi1)
  "</abbr> at "
  "<span class=\"location\">" 
   (or v1 (setq v1 (skeleton-read "Where: "))) "</span>"
  \n
  "</div>"
  )



(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a '/Applications/Marked 2.app' %s" 
       (shell-quote-argument (buffer-file-name))))
)

(let ((map html-mode-map))
  (define-key map "\C-c\C-ee" 'html-emphasis)
  (define-key map "\C-c\C-eb" 'html-bold)
  (define-key map "\C-c\C-et" 'html-typewriter)
  (define-key map "\C-c\C-ec" 'html-code)
  (define-key map "\C-c>" 'html-entity-gt)
  (define-key map "\C-c<" 'html-entity-lt)
  (define-key map "\C-c-" 'insert-mdash)
  (define-key map "\C-cp" 'html-code-block)
  (define-key map "\C-c\C-cn" 'html-name-anchor)
 )

