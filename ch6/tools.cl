(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
    (handler-case
      (progn
        (if (functionp prompt)
            (funcall prompt)
            (print prompt))
        (finish-output nil)
        (print (funcall transformer (read))))
      ;; In case of error, do this:
      (error (condition)
        (format t "~&;; Error ~a ignored, back to top level."
                condition)))))

(defun prompt-generator (&optional (num 0) (ctrl-string "[~d]"))
    "Returns a function that prints prompts like [1], [2] etc"
    #'(lambda () (format t ctrl-string (incf num))))

