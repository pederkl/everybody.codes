(defparameter *map*
  "..............................
..............................
..............#...............
.............####.............
...........########.##........
..........#########.##........
........#############.........
........#############.........
.......###.#########..........
.............####.............
...............##.............
...............###............
...............#..............
..............................")

(defparameter *map2* "......................................................................
......................................................................
......................................................................
......................................................................
..................................##..................................
............................##.#######................................
..........................##############..............................
.........................###############..............................
.........................###############..............................
.........................#################..#.........................
........................#################..####.......................
.....................#.###########################....................
.....................#############################....................
...................#################################..................
..................##################################..................
....................##############################....................
.....................##############################...................
....................###############..###########......................
....................#############################.....................
....................#############################.....................
...................##############################.....................
..................###############################.....................
..................#################################...................
...................###############################....................
...................###############################....................
...................##############################.....................
....................############################......................
...................#############################......................
.....................##..#####################.#......................
.........................###.################.........................
..........................##..#.....#########.........................
....................................#########.........................
......................................#####...........................
.......................................####...........................
......................................................................")

(defparameter *map3* "##############..##....................................................................................................########################
#.################.......................................................#............................................######################.#
####################.....................................................##.####.....................................#########################
####################...............................................#.#.#.#########....................................########################
######################..................................#.........#################...................................########################
#######################................................###.......#######.##########...................................########################
#######################..................................#.#.#....#################..................................#########################
#######################..................................###.#######################.................................#..######################
#####################...................................#############################................................#.#######################
#######################..................................###########################.................................#########################
######################...............................#################################...............................#.#######################
######################................................###################################..............................#######################
##################.#..................................###################################..............................#######################
##############..##....................................###################################...............................###.##################
##############........................................##################################......................................################
#############.......................................#####################################......................................###############
###########.........................................##.##################################.#....................................#......###.####
########.#...........................................################################.######............................................######
########..........................................#..#####################################..###.#.............................................
########..........................................#########################################.#####.............................................
#######............................................###############################################............................................
##.####............................................#################################################..........................................
...##..........................................#.##################################################...........................................
....#..........................................######################......########################...........................................
................................................####################.######.####################.#............................................
................................................#####.#############.###..###.######################...........................................
.................................................##################.###..###.######################...........................................
.....#............................................##################.######.#######################...........................................
.....#...#.......................................####################......#######################............................................
...########.......................................#.#############################################.............................................
..#########.......................................##############################################..............................................
..#########.......................................#..#########################################.#...............................###.........#..
.##########........................................###########################################.................................#####.##.######
############.........................................#########################################..................................##############
###########...........................................#######################################.................................################
###########.........................................#########################################................................#################
###########..........................................######################################....................................###############
###########...#......................................######################################....................................###############
###########..##......................................##.##################################....................................################
##################..................................######################################....................................################
##################...................................##.##################################....................................################
##################........................................########.#######################......................................##############
######################....................................#####..#.#######################....................................################
#######################....................................#.......######################..................................###################
########################...................................#.......####################.....................................##################
########################...........................................###########.####.#####...............................######################
########################...........................................##############...###.................................######################
######################.#..........................................####.#######......##..................................######################
######################..............................................#.#####.............................................##...#################
########################............................................##.###...................................................#################
#.#####################.............................................#...........................................................############.#
########################........................................................................................................##############")

(defparameter *royal-rules* t)

(defun map-to-world (&optional (map *map*))
  (let ((lines (lw:split-sequence '(#\newline) map)))
    (make-array (list (length lines) (length (car lines))) :initial-contents lines)))

(defun dig-allowed (pos level world)
  (case level
    (1
     (char= #\# (apply #'aref world pos)))
    (otherwise
     (let ((neighbors (list (list (first pos) (1+ (second pos)))
                            (list (first pos) (1- (second pos)))
                            (list (1+ (first pos)) (second pos))
                            (list (1- (first pos)) (second pos)))))
       (when (char= #\. (apply #'aref world pos))
         (return-from dig-allowed nil))
       (when *royal-rules*
         (setf neighbors (append neighbors
                                 (list (list (1+ (first pos)) (1+ (second pos)))
                                       (list (1- (first pos)) (1+ (second pos)))
                                       (list (1+ (first pos)) (1- (second pos)))
                                       (list (1- (first pos)) (1- (second pos)))))))
       (unless (every #'(lambda (pos)
                          (apply #'array-in-bounds-p world pos))
                      neighbors)
         (return-from dig-allowed nil))
       (unless (every #'(lambda (pos)
                          (let ((val
                                  (- (char-code (apply #'aref world pos))
                                     (char-code #\0))))
                            (or (eql val level)
                                (eql val (1- level)))))
                      neighbors)
         (return-from dig-allowed nil))
       t))))

(defun dig (pos level world)
  (setf (apply #'aref world pos)
        (code-char (+ level
                      (char-code #\0)))))

(defun mine-level (level world)
  (destructuring-bind (rows cols)
      (array-dimensions world)
    (let ((mined 0))
      (loop for r from 0 below rows
            do (loop for c from 0 below cols
                     for pos = (list r c)
                     when (dig-allowed pos level world)
                       do
                          (incf mined)
                          (dig pos level world)))
      (values mined world))))

(defun mine-world (&optional (world (map-to-world)))
  (loop for level from 1
        for mined = (mine-level level world)
        sum mined
        until (< mined 5)))

(defun mine-map (map)
  (let* ((world (map-to-world map))
         (result (mine-world world)))
    (print-world world)
    (values result (count-world world))))

(defun row (row 2d-array)
  (make-array (array-dimension 2d-array 1)
              :displaced-to 2d-array
              :displaced-index-offset (* row (array-dimension 2d-array 1))))

(defun print-world (world)
  (loop for i from 0 below (array-dimension world 0)
        do (format t "~a~%" (coerce (row i world) 'string))))

(defun count-world (world)
  (loop for c across (make-array (array-total-size world) :displaced-to world)
        when (> (char-code c) (char-code #\0))
          sum (- (char-code c) (char-code #\0))))

(defparameter *trace* nil)
(defun dprint (string &rest args)
  (when *trace*
    (apply #'format t string args)))
