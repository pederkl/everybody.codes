(defparameter *words-1* '("LOR" "LL" "SI" "OR" "ST" "CA" "MO"))

(defparameter *input-1* "LOREM IPSUM DOLOR SIT AMET, CONSECTETUR ADIPISCING ELIT, SED DO EIUSMOD TEMPOR INCIDIDUNT UT LABORE ET DOLORE MAGNA ALIQUA. UT ENIM AD MINIM VENIAM, QUIS NOSTRUD EXERCITATION ULLAMCO LABORIS NISI UT ALIQUIP EX EA COMMODO CONSEQUAT. DUIS AUTE IRURE DOLOR IN REPREHENDERIT IN VOLUPTATE VELIT ESSE CILLUM DOLORE EU FUGIAT NULLA PARIATUR. EXCEPTEUR SINT OCCAECAT CUPIDATAT NON PROIDENT, SUNT IN CULPA QUI OFFICIA DESERUNT MOLLIT ANIM ID EST LABORUM.")

(defun find-words-1 (&optional (input *input-1*) (words *words-1*))
  (loop with result = 0
        with real-end = (length input)
        for pos from 0 to (- (length input) (apply #'min (mapcar #'length words)))
        do
           (loop for w in words
                 when (string= input w :start1 pos :end1 (min real-end (+ pos (length w))))
                   do
                      (format t "~a~%" w)
                      (incf result))
        finally return result))
