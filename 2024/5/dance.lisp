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
  (let ((line (row 0 matrix)))
    (parse-integer (format nil "~{~d~}" (coerce line 'list)))))

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
                        col
                        (- clapper (* 2 occupied))
                        orig-clapper)
        (loop for r downfrom (- (array-dimension matrix 0) 2) to insert-row
              do
                 (setf (aref matrix (1+ r) col) (aref matrix r col))
              finally
              do
                 (setf (aref matrix insert-row col) orig-clapper)))))

(defparameter *verbose* nil)

(defun do-round (matrix n)
  (let* ((col (col matrix (1- n)))
         (clapper (take-clapper matrix col)))
    (insert-clapper matrix (col matrix (1+ col)) clapper)
    (when *verbose*
      (format t "~d:~t~d~%" n (matrix-result matrix)))
    matrix))

(defun dance-rounds (floor n)
  (let ((matrix (make-matrix floor)))
    (loop for i from 1 to n
          do
             (do-round matrix i))
    matrix))

(dance-rounds *floor-1* 10)

(defparameter *demo-floor-2* "2 3 4 5
6 7 8 9")

(defparameter *floor-2* "64 34 28 28
35 14 48 65
43 22 37 96
78 55 88 42
67 47 93 52
42 33 41 32
35 31 10 20
27 37 96 81
23 14 99 83
64 27 38 36
65 40 30 86
33 77 16 92
52 69 54 20
47 79 70 87
28 85 55 94
29 25 36 10
89 39 31 38
14 24 12 41
62 34 97 82
72 64 12 66
80 89 40 17
85 36 48 99
63 75 50 96
12 35 34 61
16 44 61 45
11 13 77 24
52 29 29 34
60 57 11 26
59 27 64 12
87 17 75 81
30 58 77 62
49 80 97 88
73 33 31 20
28 63 17 45
49 98 57 13
18 97 52 19
68 87 10 16
21 29 98 71
84 41 78 71
26 58 23 65
83 19 93 63
51 19 33 95
14 69 93 92
56 51 26 31
86 50 93 66
17 72 81 47
23 16 10 39
79 51 35 39
24 57 90 27
45 18 46 54
59 48 60 67
99 53 21 94
77 66 19 15
17 47 70 15
43 70 53 45
21 63 46 37
23 59 59 95
76 41 50 44
40 68 44 11
94 67 82 67
31 54 76 60
43 91 25 70
92 36 27 91
80 84 30 35
60 85 32 79
81 40 45 15
56 29 37 23
73 22 36 38
18 42 84 91
74 38 55 24
85 79 62 40
44 53 86 42
88 73 38 33
51 46 69 20
18 18 21 99
13 75 30 26
42 41 15 83
22 46 74 74
48 91 43 11
25 24 39 16
95 22 55 76
66 87 97 43
90 50 57 32
56 78 82 58
14 88 20 80
26 39 44 94
25 68 30 28
73 72 78 61
86 62 90 49
34 72 61 15
71 46 92 82
68 98 25 12
71 32 74 58
32 49 13 96
95 83 13 69
21 65 54 11
89 53 76 22
84 75 98 89
56 90 19 37")

(defun dance-until-repeated (floor repeats)
  (let ((matrix (make-matrix floor))
        (result-counters (make-hash-table)))
    (loop for round from 1
          for round-result = (matrix-result (do-round matrix round))
          do
             (incf (gethash round-result result-counters 0))
          while (< (gethash round-result result-counters 0) repeats)
          finally
          return (values (* round round-result)
                         round round-result
                         (gethash round-result result-counters)))))

(dance-until-repeated *floor-2* 2024)
