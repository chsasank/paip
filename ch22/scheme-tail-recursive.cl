;; tail recursive interpreter
(load "scheme-interpreter.cl")

(defstruct (proc (:print-function print-proc))
    "Represent a scheme procedure"
    code (env nil) (name nil) (parms nil)
)

(defun print-proc (proc &optional (stream *STANDARD-OUTPUT*) depth)
    (declare (ignore depth))
    (format stream "{~a}" (or (proc-name proc) '??))
)

(defun interp (x &optional env)
    "Evaluate the expression in the environment env
    This version is properly tail-recursive"
    (prog ()
        :INTERP
        (return
            (cond
                ((symbolp x) (get-var x env))
                ((atom x) x)
                ((scheme-macro (first x))
                    (setf x (scheme-macro-expand x)) 
                    (GO :INTERP))
                ((case (first x)
                    (QUOTE
                        (second x))
                    (BEGIN
                        (pop x)  ;; pop off the begin to get at the args
                        ;; now interpret all but the last expression
                        (loop while (rest x) do (interp (pop x) env))
                        ;; finally, rename the last expression as x
                        (setf x (first x))
                        (GO :INTERP)
                    )
                    (SET!
                        (set-var! (second x) (interp (third x) env) env))
                    (IF
                        (setf x (if (interp (second x) env)
                                    (third x)
                                    (fourth x)))
                        ;; that is rename the right expression as x
                        (GO :INTERP))
                    (LAMBDA
                        (make-proc :env env :parms (second x)
                            :code (maybe-add 'begin (rest2 x))))
                    (t  ;; a procedure application
                        (let ((proc (interp (first x) env))
                              (args (mapcar #'(lambda (v) (interp v env))
                                        (rest x))))
                            (if (proc-p proc)
                                ;; execute procedure with rename+goto
                                (progn
                                    (setf x (proc-code proc))
                                    (setf env (extend-env (proc-parms proc) args 
                                                    (proc-env proc)))
                                    (GO :INTERP))
                                ;; else apply primitive procudure
                                (apply proc args))))))))))


;; testing in scheme interp

; (define (traverse lyst)
;   (if lyst (traverse (cdr lyst))))

; (traverse '(a b c d))