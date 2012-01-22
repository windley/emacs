;; example function to call
(defun underline-char (char-point)
  (add-text-properties char-point (1+ char-point)
                       '(face underline)))

(defun do-high-ascii (fn)
     (interactive "aFunction: ")
     (save-excursion
       (goto-char (point-min))
       (while (< (point) (point-max))
         (when (> (char-after) 127)
           (funcall fn (point)))
         (forward-char 1))))