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
