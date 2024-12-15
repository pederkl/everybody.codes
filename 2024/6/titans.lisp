(defparameter *demo-tree-1*
  "RR:A,B,C
A:D,E
B:F,@
C:G,H
D:@
E:@
F:@
G:@
H:@")

(defparameter *tree-1* "TL:JJ
PV:LW
KF:MD
HW:TW
MD:FG,TL
PH:JH,FC
NH:PP
SX:RH,TF
JW:GN,@
NB:JW,FT,@
WP:QD,NQ
TB:NJ,WK
SR:NB
TW:MF,PM
GD:@
ZG:CN,DP
WN:XK,GD,@
JJ:PH,WP
MF:QM,RN
LW:NH,HW,@
GN:WN,TB,@
MT:QK,PW,@
TH:LL,XG
XK:@
PM:VB,ZW
QK:BS,GL
PP:KD,TH
KD:QW,QS
PW:NF,VF
JP:ZG,SX
RR:SR,PV,KF
FT:MT,@
FG:JP")

(defun tree-name (tree)
  (car tree))

(defun tree-children (tree)
  (cdr tree))

(defun tree-p (tree)
  (consp tree))

(defun fruit-p (tree)
  (eq '@ tree))

(defun parse-tree (txt)
  (let* ((lines (uiop:split-string txt :separator '(#\newline)))
         (branches nil))
    (loop for line in lines
          for (bname ctxt) = (uiop:split-string line :separator '(#\:))
          for branch = (read-from-string bname)
          for children = (mapcar #'read-from-string (uiop:split-string ctxt :separator '(#\,)))
          do
             (pushnew (list* branch children) branches))
    (loop for branch in branches
          do
             (loop for c-list on (tree-children branch)
                   for branch-def = (find (car c-list) branches :key #'tree-name)
                   when (and (symbolp (car c-list))
                             branch-def)
                     do
                        (rplaca c-list branch-def)))
    (find 'rr branches :key #'car)))

(defun traverse-tree (tree function)
  (let ((result nil))
    (labels ((trav (tree path)
               (if (tree-p tree)
                   (progn
                     (push (tree-name tree) path)
                     (loop for c in (tree-children tree)
                           do
                              (trav c path)))
                   (progn
                     (push tree path)
                     (push (funcall function tree path) result)))))
      (trav tree nil)
      result)))

(defun find-unique-depth (txt-tree)
  (let* ((tree (parse-tree txt-tree))
         (depth-map (make-hash-table)))
    (traverse-tree
     tree
     #'(lambda (node path)
         (when (fruit-p node)
           (push (reverse path) (gethash (length path) depth-map '())))))
    (loop for k being the hash-keys of depth-map
            using (hash-value v)
          ;; do (format t "~d -> ~s~%" k v))))
          when (= 1 (length v))
            do (format t "~d -> ~s~%" k (car v))
            and
              return (values (car v)
                             (format nil "~{~a~}" (car v))))))
