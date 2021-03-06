(defun split-space (line)
  (let ((sp-pos (position #\space line)))
    (if sp-pos
        (cons (subseq line 0 sp-pos)
              (split-space (subseq line (1+ sp-pos))))
        (list line))))

(defun process-line (line)
  (labels ((convert (reg)
             (let ((p (parse-integer reg :junk-allowed t)))
               (or p (read-from-string reg)))))
    (destructuring-bind (label x y) (split-space line)
      (list (read-from-string label) (convert x) (convert y)))))

(defun read-program (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (process-line line))))

(defvar *input* (read-program "input"))

(defun run-input-program (program)
  (let ((registers (make-hash-table))
        (pc 0))
    (labels ((get-register (r)
               (if (numberp r) r (gethash r registers 0)))
             (run-instruction ()
               (destructuring-bind (label x y) (nth pc program)
                 (case label
                   (set (progn (setf (gethash x registers 0) (get-register y))
                               (incf pc)))
                   (sub (progn (decf (gethash x registers 0) (get-register y))
                               (incf pc)))
                   (mul (progn (setf (gethash x registers 0)
                                     (* (get-register x) (get-register y)))
                               (incf pc)))
                   (jnz (if (zerop (get-register x))
                            (incf pc)
                            (incf pc (get-register y))))))))
      (loop while (< pc (length program))
            do (run-instruction)
            counting (let ((instr (nth pc program)))
                       (and instr (eq 'mul (first instr))))))))
