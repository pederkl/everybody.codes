(defparameter *demo-floor-1* "2 3 4 5
3 4 5 2
4 5 2 3
5 2 3 4")

(defparameter *floor-1* "5 2 2 3
5 5 3 4
3 5 2 4
4 5 3 2
3 4 4 2")

(defun make-matrix (str)
  (let* ((lines (lw:split-sequence '(#\newline) str))
         (lines (mapcar #'(lambda (l)
                            (mapcar #'(lambda (c)
                                        (parse-integer c))
                                    (lw:split-sequence '(#\space) l)))
                        lines))
         (extra-line (mapcar #'(lambda (x) (declare (ignore x)) 0) (car lines)))
         (lines (append lines (list extra-line))))
    (make-array (list (length lines) (length (car lines))) :initial-contents lines)))

(defun row (row matrix)
  (make-array (array-dimension matrix 1)
              :displaced-to matrix
              :displaced-index-offset (* row (array-dimension matrix 1))))

(defun print-matrix (matrix)
  (loop for i from 0 below (array-dimension matrix 0)
        do
           (loop for j from 0 below (array-dimension matrix 1)
                 do
                    (princ (aref matrix i j))
                    (princ #\space))
           (terpri)))

(defun matrix-result (matrix)
  (let ((line (row 0 matrix))
        (result 0))
    (loop for d across line
          do
             (setf result (* 10 result))
             (incf result d))
    result))

(defun col (matrix n)
  (mod n (array-dimension matrix 1)))

(defun occupied (matrix col)
  (loop for i downfrom (1- (array-dimension matrix 0))
        unless (zerop (aref matrix i col))
          return (1+ i)))

(defun find-insert-row (matrix col clapper)
  (let ((occupied (occupied matrix col)))
    (cond
      ((<= clapper occupied)
       (1- clapper))
      ((<= clapper (* 2 occupied))
       (1+ (- (* 2 occupied) clapper)))
      (t
       nil))))

(defun take-clapper (matrix col)
  (let ((clapper (aref matrix 0 col)))
    (loop for i from 0 below (1- (array-dimension matrix 0))
          do
             (setf (aref matrix i col) (aref matrix (1+ i) col))
          finally
          do
             (setf (aref matrix i col) 0))
    clapper))

(defun insert-clapper (matrix col clapper &optional (orig-clapper clapper))
  (let ((insert-row (find-insert-row matrix col clapper))
        (occupied (occupied matrix col)))
    (if (null insert-row)
        (insert-clapper matrix
                        (col matrix (1+ col))
                        (- clapper (* 2 occupied))
                        orig-clapper)
        (loop for r downfrom (- (array-dimension matrix 0) 2) to insert-row
              do
                 (setf (aref matrix (1+ r) col) (aref matrix r col))
              finally
              do
                 (setf (aref matrix insert-row col) orig-clapper)))))

(defun do-round (matrix n)
  (let* ((col (col matrix (1- n)))
         (clapper (take-clapper matrix col)))
    (insert-clapper matrix (col matrix (1+ col)) clapper)
    (format t "~d:~t~d~%" n (matrix-result matrix))))

(defun dance-rounds (floor n)
  (let ((matrix (make-matrix floor)))
    (loop for i from 1 to n
          do
             (do-round matrix i))
    matrix))
