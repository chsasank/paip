(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
    "A computation that can be executed later by FORCE"
    `(make-delay :function #'(lambda() . ,body)))

(defun force (x)
    "Find the vaue of x, by comuting if it is a delay"
    (if (not (delay-p x))
        x
        (progn
        (when (delay-function x)
            (setf (delay-value x)
                (funcall (delay-function x)))
            (setf (delay-function x) nil))
        (delay-value x))))

; create pipe data structure

(defmacro make-pipe (head tail)
    "Create a pipe by evaluation the head and delaying tail"
    `(cons ,head (delay ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe) (first pipe))

(defun tail (pipe) (force (rest pipe)))

(defun pipe-elt (pipe i)
    "The i-th element of pipe, 0-based"
    (if (= i 0)
        (head pipe)
        (pipe-elt (tail pipe) (- i 1))))


; second data structure for pipe
; because first one wastes lot of space
(defmacro make-pipe (head tail)
    "Create a pipe by evalating head and delaying tail"
    `(cons ,head #'(lambda () ,tail)))

(defun tail (pipe)
    "Return tail of pipe or list, destructively update
    the tail if it is a function"
    (if (functionp (rest pipe))
        (setf (rest pipe) (funcall (rest pipe)))
        (rest pipe)))

; example pipe
(defun integers (&optional (start 0) end)
    "A pipe of integers from start to end
    If END is null, this is a infinite pipe"
    (if (or (null end) (<= start end))
        (make-pipe start (integers (+ start 1) end))
        nil))

(defun enumerate (pipe &key count key (result pipe))
    "Go through all (or count) elements of pipe
    possibly applying the key function"
    (if (or (eq pipe empty-pipe) (eql count 0))
        result
        (progn
        (unless (null key) (funcall key (head pipe)))
        (enumerate (tail pipe) :count (if count (- count 1))
            :key key :result result))))

(defun filter (pred pipe)
    "Keep only items in pipe satisying pred"
    (if (funcall pred (head pipe))
        (make-pipe (head pipe) (filter pred (tail pipe)))
        (filter pred (tail pipe))))

(defun sieve (pipe)
    (make-pipe (head pipe)
        (filter #'(lambda (x) (/= (mod x (head pipe)) 0))
            (sieve (tail pipe)))))

(defvar *primes* (sieve (integers 2)))

(enumerate *primes* :count 10)