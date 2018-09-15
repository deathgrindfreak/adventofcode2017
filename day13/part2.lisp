(defun process-line (line)
  "Converts a string like \"98: 14\" to '(98 14 0) where 98 is the depth, 14 the range and 0 the current scanner position"
  (let* ((zero (char-code #\0))
         (nine (char-code #\9))
         (proc-chars (reduce #'(lambda (a c)
                                 (cond
                                   ((<= zero (char-code c) nine)
                                    (cons c a))
                                   ((char= c #\:) (list a))
                                   (t a)))
                             line :initial-value '())))
    (concatenate 'list
     (reverse
      (mapcar #'(lambda (c)
                  (read-from-string (coerce (reverse c) 'string)))
              (list
               (remove-if #'consp proc-chars)
               (first (remove-if (complement #'consp) proc-chars)))))
     '(down 1))))

(defun read-firewall (path)
  "Reads in firewall"
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (process-line line))))

(defparameter *input* (read-firewall "input"))
(defparameter *test-input* (read-firewall "test"))
(defvar *firewall* *input*)

(defun number-of-layers (firewall)
  "The number of layers in a firewall"
  (apply #'max (mapcar #'first firewall)))

(defun move-scanners (firewall)
  "Moves the scanners along the layer by 1"
  (mapcar #'(lambda (s)
              (destructuring-bind (d r dir p) s
                (append (list d r)
                        (cond ((= r p) (list 'up (1- p)))
                              ((= 1 p) (list 'down (1+ p)))
                              ((eq 'up dir) (list 'up (1- p)))
                              ((eq 'down dir) (list 'down (1+ p)))))))
          firewall))

(defun is-caught (firewall layer)
  "Determines if moving to a new layer will result in being caught"
  (let ((current-layer (assoc layer firewall)))
    (and current-layer (= 1 (fourth current-layer)))))

(defun find-delay (&optional (firewall *firewall*) (d 0))
  (case (traverse firewall)
    (success d)
    (fail (find-delay (move-scanners firewall) (1+ d)))))

(defun traverse (&optional (firewall *firewall*) (depth 0))
  "Traverse the firewall, counting the severity of the trip"
  (cond ((> depth (number-of-layers firewall)) 'success)
        ((is-caught firewall depth) 'fail)
        (t (traverse (move-scanners firewall) (1+ depth)))))
