(defun fib (n)
    "Compute the nth number in the fibonacci sequence"
    (if (<=  n 1)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(defun memo (fn &key (key #'first) (test #'eql) name)
    "Return a memo-function of fn"
    (let ((table (make-hash-table :test test)))
        (setf (get name 'memo) table)
        #'(lambda (&rest args)
            (let ((k (funcall key args)))
                (multiple-value-bind (val found-p) (gethash k table)
                    (if found-p
                        val
                        (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(trace fib)

(fib 5)

(setf memo-fib (memo #'fib))
(funcall memo-fib 5)

(memoize 'fib)
(fib 5)