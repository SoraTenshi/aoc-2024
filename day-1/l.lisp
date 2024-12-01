#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun split-str (str delimiter)
  (remove "" (uiop:split-string str :separator (string delimiter)) :test #'string=))

(defun read-file-lines (filename &optional (delimiter #\Newline))
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      (split-str contents delimiter))))

(defun parse-line (line)
  (mapcar #'parse-integer (split-str line #\Space)))

(let* ((file-contents (read-file-lines "test.txt"))
       (parsed (mapcar #'parse-line file-contents))
       (left (sort (mapcar #'first parsed) #'<))
       (right (sort (mapcar #'second parsed) #'<))
       ;; ~~~~ part one ~~~~~~ 
       (sorted-pairs (mapcar #'list left right))
       (deltas (mapcar (lambda (pair)
                         (abs (- (first pair) (second pair))))
                       sorted-pairs))
       (result (reduce #'+ deltas)))
  (format t "Total acc delta (1): ~d~%" result)
  ;; ~~~~ part one end ~~~~~~ ;;
  ;; ~~~~ part two start ~~~~ ;;
  (let* ((similarities (mapcar (lambda (lefties)
                                       (* lefties (count lefties right)))
                               left))
         (acc (reduce #'+ similarities)))
    (format t "Total acc sim ~d~%" acc)))
  ;; ~~~~ part two end ~~~~~~ ;;
