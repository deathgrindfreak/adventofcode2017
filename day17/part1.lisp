(defun insert (val target n)
  (append (subseq target 0 n)
          (list val)
          (subseq target n)))

(defun spinlock (input &optional (buffer '(0)) (p 0) (next 1))
  (let ((next-position (mod (+ p input) (length buffer))))
    (if (= next 2018)
        (second (member 2017 buffer))
        (spinlock input
                  (insert next buffer next-position)
                  (1+ next-position)
                  (1+ next)))))
