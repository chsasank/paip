(with-open-file (stream "test.txt" :direction :output)
    (print '(hello there) stream)
    (princ 'goodbye stream))


(with-open-file (stream "test.txt" :direction :input)
    (list
        (read stream)
        (read-char stream)
        (read stream)
        (read stream nil 'eof)))
