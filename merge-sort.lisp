;;sbcl
;; (load "merge-sort.lisp")
;;CL-USER> (merge-sort '(0 10 5 11 2 30 50 15))
;;(0 2 5 10 11 15 30 50)


(defun merge-sort (list)
  (let ((size (length list)))
  (if (eq size 1 )
      list
      (let ((half1 (merge-sort (subseq list 0 (floor (/ size 2))) ))
	    (half2 (merge-sort (subseq list (floor (/ size 2)) size) )))
	(merge-array half1 half2)))))

(defun merge-array(l1 l2)
  (let ((new-arr (make-array (+ (length l1) (length l2))
			     :fill-pointer 0 )))
    (loop for i below (+ (length l1) (length l2))
       do
	 (let ((x (car l1))
	       (y (car l2)) )
	   (when (and (not (null x)) (not (null y)))
	     (cond ((<= x y)(progn
			      (vector-push x new-arr)
			      (setf l1 (cdr l1))))
		   (t (progn
			(vector-push y new-arr)
			(setf l2 (cdr l2 ) ))) ))) )
    (mapcar #'(lambda (w) (vector-push w new-arr)) (append l1 l2))
    (coerce new-arr 'list)))
