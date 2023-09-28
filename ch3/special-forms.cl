(defstruct name
    first
    (middle nil)
    last)

(setf b (make-name :first 'Barney :last 'Rubble))

(name-first b)

(let ((x 40)
      (y (+ 1 1)))
  (+ x y))


((lambda (x y) (+ x y)) 40 (+ 1 1))

(defmacro while (test &rest body)
    "Repeat body while test is true"
    (list* 'loop
        (list 'unless test '(return nil))
        body))

(print (macroexpand-1 '(while (< i 10)
                        (print (* i i))
                        (setf i (+i 1)))))

(setf i 7)
(while (< i 10)
    (print (* i i))
    (setf i (+ i 1)))


(defmacro while2 (test &rest body)
    (let ((code '(loop (unless test (return nil) . body))))
        (subst test 'test (subst body 'body code))))


(print (macroexpand-1 '(while2 (< i 10)
                        (print (* i i))
                        (setf i (+i 1)))))


(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))


(defun problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))