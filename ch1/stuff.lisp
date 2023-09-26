(setf p '(John Q public))

(defun last-name (name)
    "get last name"
    (first (last name)))

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)))

; maps fn with arg1_list, arg2_list and so on
(mapcar #'last-name names)
(mapcar #'- '(1 2 3 4))
(mapcar #'+ '(1 2 3 4) '(10 20 30 40))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
    "sekect the first name bar title"
    (if (member (first name) *titles*)
        (first-name (rest name))
        (first name)))

(defun self-and-double (x) (list x (+ x x)))
(self-and-double 3)
; apply converts list to args
(apply #'self-and-double '(3))

; higher order functions
(defun mappend (fn the-list)
    (apply #'append (mapcar fn the-list)))
    
(mappend #'self-and-double '(1 10 300))

; implementation with recursion
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))


; lambdas
((lambda (x) (+ x 2)) 4)
(funcall #'(lambda (x) (+ x 2)) 4) 