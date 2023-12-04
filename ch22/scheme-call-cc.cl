;; scheme with call/cc implementation
(load "scheme-interpreter.cl")

(define scheme ()
    "A scheme read-eval-print loop (using interp)
    Handles call/cc by explicitly passing continuations.
    "
    (init-scheme-interp)
    (loop
        (format-flush t "~&==> ")
        (interp (read) nil #'print)))

(defun interp (x env cc)
    "Evalute the expressionx in the environment env,
    and pass the result to the continuation cc"
    (cond
        ((symbolp x) (funcall cc (get-var x env)))
        ((atom x) (funcall cc x))
        ((scheme-macro (first x))
            (interp (scheme-macro-expand x) env cc))
        ((case (first x)
            (QUOTE
                (funcall cc (second x)))
            (BEGIN
                (interp-begin (rest x) env cc))
            (SET!
                (interp (third x) env #'(lambda (val)
                    (funcall cc (set-var! (second x) val env)))))
            (IF 
                (interp (second x) env
                    #'(lambda (pred)
                        (interp (if pred (third x) (fourth x)) env cc))))
            (LAMBDA
                (let ((parms (second x))
                      (code (maybe-add 'begin (rest2 x))))
                    (funcall cc
                        #'(lambda (cont &rest args)
                            (interp code (extend-env parms args env) cont)))))
            (t (interp-call x env cc))))))


(defun interp-begin (body env cc)
    "Interpret each element of Body passing the last to CC"
    (interp (first body) env
        #'(lambda (val)
            (if (null (rest body))
                (funcall cc val)
                (interp-begin (rest body) env cc)))))

(defun interp-call (call env cc)
    "Interpret the call (f x ...) and pass the result to cc"
    (map-interp call env
        #'(lambda (fn-and-args)
            (apply (first fn-and-args) cc (rest fn-and-args)))))

(defun map-interp (list env cc)
    "Interpret each limit of lisp and pass the list to CC"
    (if (null list)
        (funcall cc nil)
        (interp (first list) env
            #'(lambda (x)
                (map-interp (rest list) env
                    #'(lambda (y) (funcall cc (cons x y))))))))

(defun init-scheme-proc (f)
    "Define a scheme primitive procedure as a CL function"
    (if (listp f)
        (set-global-var! (first f)
            #'(lambda (cont &rest args)))
    )
)