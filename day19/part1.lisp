;;; Methods for reading in diagram
(defun to-array (diagram)
  "Convert a list of lists to an array"
  (let ((width (length (first diagram)))
        (height (length diagram)))
    (make-array (list height width) :initial-contents diagram)))

(defun process-line (line)
  "Converts a routing diagram line to symbols"
  (map 'list #'(lambda (c)
                 (case c
                   (#\space nil)
                   (#\+ 'plus)
                   (#\- 'dash)
                   (#\| 'pipe)
                   (otherwise (read-from-string (string c)))))
       line))

(defun read-routing-diagram (path)
  "Reads in a routing diagram"
  (with-open-file (in path)
    (to-array
     (loop for line = (read-line in nil nil)
           while line
           collect (process-line line)))))

;;; Methods for traversing diagram
(defparameter *input* (read-routing-diagram "input"))
(defparameter *test-input* (read-routing-diagram "test-input"))
(defvar *diagram* *input*)

(defun diagram-width ()
  (array-dimension *diagram* 1))

(defun diagram-height ()
  (array-dimension *diagram* 0))

(defun get-position (x y)
  (aref *diagram* y x))

(defun look-right (x y) (and (< x (diagram-width)) (get-position (1+ x) y)))
(defun look-left (x y) (and (> x 0) (get-position (1- x) y)))
(defun look-up (x y) (and (> y 0) (get-position x (1- y))))
(defun look-down (x y) (and (< y (diagram-height)) (get-position x (1+ y))))
(defun look-ahead (direction x y)
  (case direction
    (up (look-up x y))
    (down (look-down x y))
    (left (look-left x y))
    (right (look-right x y))))

;;; Finding paths
(defun find-start ()
  "Finds the x index that the vertical pipe in the diagram starts at"
  (let* ((width (array-dimension *diagram* 1))
         (pipe-start (member-if #'(lambda (p)
                                    (eq 'pipe (get-position p 0)))
                                (loop for n from 0 below width
                                      collect n))))
    (- width (length pipe-start))))

(defun turn-corner (direction x y)
  (case direction
    ((up down) (if (look-left x y)
                   (list 'left (1- x) y)
                   (list 'right (1+ x) y)))
    ((left right) (if (look-up x y)
                      (list 'up x (1- y))
                      (list 'down x (1+ y))))))

(defun continue-travel (direction x y)
  (case direction
    (up (list direction x (1- y)))
    (down (list direction x (1+ y)))
    (left (list direction (1- x) y))
    (right (list direction (1+ x) y))))

(defun determine-next-move (direction x y)
  (let ((current (get-position x y)))
    (if (eq current 'plus)
        (turn-corner direction x y)
        (if (look-ahead direction x y)
            (continue-travel direction x y)
            (list 'done x y)))))

(defun walk (&optional (direction 'down) (x (find-start)) (y 0))
  "Walk along the path collecting letters as they are encountered"
  (let ((current (get-position x y)))
    (if (eq direction 'done)
        '()
        (destructuring-bind (nd nx ny) (determine-next-move direction x y)
          (if (member current '(pipe dash plus))
              (walk nd nx ny)
              (cons current (walk nd nx ny)))))))
