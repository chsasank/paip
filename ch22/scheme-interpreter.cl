(defun interp (x &optional env)
    "Interpret (evaluate) the expression x in the environment env."
    (cond
        ((symbolp x) (get-var x env))
        ((atom x) x)
        ((case (first x)
            (QUOTE (second x))
            (BEGIN (last1 (mapcar #'(lambda (y) (interp y env))
                            (rest x))))
            (SET! (set-var! (second x) (interp (third x) env) env))
            (IF (if (interp (second x) env) 
                    (interp (third x) env)
                    (interp (fourth x) env)))
            (LAMBDA (let ((params (second x))
                          (code (maybe-add 'begin (rest2 x))))
                        #'(lambda (&rest args)
                            (interp code (extend-env params args env)))))
            (t ;; a procedure application
                (apply 
                    (interp (first x) env)
                    (mapcar #'(lambda (v) (interp v env)) (rest x))))))))

(defun set-var! (var val env)
    "Set a variable to a value, in the given or global environment"
    (if (assoc var env)
        (setf (second (assoc var env)) val)
        (set-global-var! var val))
    val)

(defun get-var (var env)
    "Get the value of a variable, from the given or global environment"
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun set-global-var! (var val)
    (setf (get var 'global-val) val))

(defun get-global-var (var)
    (let* ((default "unbound")
           (val (get var 'global-val default)))
        (if (eq val default)
            (error "Unbound scheme variable: ~a" var)
            val)))

(defun extend-env (vars vals env)
    "Add some variables and values to an enviroment"
    (nconc (mapcar #'list vars vals) env))

(defparameter *scheme-procs*
    '(+ - * / = < > <= >= cons car cdr not append list read member
        (null? null) (eq? eq) (equal? equal) (eqv? eql)
        (write prin1) (display princ) (newline terpri)))

(defun init-scheme-interp ()
    "Initialize the scheme interpreter with some global variables"
    ;; define scheme procedures as CL functions:
    (mapc #'init-scheme-proc *scheme-procs*)
    ;; define the boolean constants. Unfortunately this won't
    ;; stop someone from saying: (set! t nil)
    (set-global-var! t t)
    (set-global-var! nil nil))

(defun init-scheme-proc (f)
    "define a scheme procedure as a corresponding CL function"
    (if (listp f)
        (set-global-var! (first f) (symbol-function (second f)))
        (set-global-var! f (symbol-function f))))

(defun maybe-add (op exps &optional if-nil)
    "For example, (maybe-add 'and exps t) returns t 
    if exps is nil, exps if there is only one,
    and (and exp1 exp2...) if there are several exps"
    (cond 
        ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

(defun length=1 (x)
    "Is x a list of length 1"
    (and (consp x) (null (cdr x))))

(defun last1 (list)
    "Return the last element (not last cons cell) of list"
    (first (last list)))

(defun format-flush (&rest args)
    (apply #'format args)
    (finish-output))

(defun scheme ()
    "A scheme read-eval-print loop (using interp)"
    (init-scheme-interp)
    (loop (format-flush t "~&==> ")
        (print (interp (read) nil))))
