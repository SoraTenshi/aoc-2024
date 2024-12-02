#!/usr/bin/env -S sbcl --script

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun find-all-mul (contents)
  (let ((results nil)
        (start 0))
    (loop
       for pos = (search "mul(" contents :start2 start)
       while pos do
       (let* ((open-paren (+ pos 4))
              (comma (position #\, contents :start open-paren))
              (close-paren (position #\) contents :start (when comma (1+ comma)))))
             (when (and comma close-paren
                        (< open-paren comma close-paren)
                        (every #'digit-char-p (subseq contents open-paren comma))
                        (every #'digit-char-p (subseq contents (1+ comma) close-paren)))
                   (let ((num1 (parse-integer (subseq contents open-paren comma)))
                         (num2 (parse-integer (subseq contents (1+ comma) close-paren))))
                     (push (list pos (list num1 num2)) results))))
      (setf start (1+ pos)))
  (nreverse results)))

(defun find-all-don-t-ranges (contents)
  (let ((results nil)
        (start 0))
    (loop
       for pos = (search "don't()" contents :start2 start)
       while pos do
       (let ((end (search "do()" contents :start2 pos)))
         (when end
               (push (list pos end) results)))
       (setf start (1+ pos)))
     (nreverse results)))
 
(defun not-in-range (range target)
  (and (< (first range) target) (> (second range) target)))

(let* ((parsed (read-file "input.txt"))
       (all-mul (find-all-mul parsed))
  ;;~~ part 1 ~~~~~~ ;;
       (acc (reduce #'+ (mapcar (lambda (pair)
                                        (* (first pair) (second pair))) (mapcar #'second all-mul)))))
  (format t "all mul: ~a~%" acc)
  ;;~~ end part 1 ~~ ;;
  ;;~~ part 2 ~~~~~~ ;;
  (let* ((do-range (find-all-don-t-ranges parsed))
         (filtered (remove-if
                   (lambda (entry)
                           (let ((pos (first entry)))
                             (some (lambda (range)
                                           (not-in-range range pos))
                                   do-range)))
                   all-mul))
         (filtered-acc (reduce #'+ (mapcar (lambda (pair)
                                          (* (first pair) (second pair))) (mapcar #'second filtered)))))
    (format t "all filtered mul: ~a~%" filtered-acc)))
  ;;~~ end part 2 ~~ ;;

