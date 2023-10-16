(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

; compile the rules

(defun compile-rule (rule)
    "Translate a grammar rule into a LISP function definition"
    (let ((rhs (rule-rhs rule)))
        `(defun ,(rule-lhs rule) ()
            ,(cond 
                ((every #'atom rhs) `(one-of ',rhs))
                ((length=1 rhs) (build-code (first rhs)))
                (t `(case (random ,(length rhs))
                        ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
    "Return a list of case-clauses"
    (when choices
        (cons (list number (build-code (first choices)))
            (build-cases (+ number 1) (rest choices)))))

(defun build-code (choice)
    "Append together multiple constituents"
    (cond 
        ((null choice) nil)
        ((atom choice) (list choice))
        ((length=1 choice) choice)
        (t `(append ,@(mapcar #'build-code choice)))))

(defun length=1 (x)
    "Is x a list of length 1"
    (and (consp x) (null (cdr x))))

(defmacro defrule (&rest rule)
  "Define a grammar rule"
  (compile-rule rule))

(compile-rule '(Noun -> man ball woman table (chow chow)))

