(defconstant unbound "unbound")

(defstruct (var (:print-function print-var))
      name (binding unbound))
    (defun print-var (var stream depth)
        (if (or (and (numberp *print-level*)
                        (>= depth *print-level*))
                (var-p (deref var)))
        (format stream "?~a" (var-name var))
        (write var :stream stream)))

(defun bound-p (var) (not (eq unvar-binding var) unbound))

(defmacro deref (exp)
    "Follow pointers for bound variables."
    `(progn 
        (loop while (and (unvar-p ,exp) (bound-p ,exp))
            do (setf ,exp (unvar-binding ,exp)))
        ,exp))

(defun unify! (x y)
    "Destructively unify two expressions"
    (cond
        ((eql (deref x) (deref y)) t)
        ((unvar-p x) (set-binding! x y))
        ((unvar-p y) (set-binding! y x))
        ((and (consp x) (consp y))
            (and (unify! (first x) (first y))
                 (unify! (rest x) (rest y))))
        (t nil)))

(defun set-binding! (var value)
    "Set var's binding to value. Always succeeds with t"
    (setf (var-binding var) value)
    t)

