(defun quick-sort (lst)
  (let ((pivot (car lst)) )
  (cond ((< (length lst) 1) nil )
	(t
	 (append (quick-sort (lesser pivot (cdr lst)))
		 (cons pivot nil)
		 (quick-sort (greater pivot (cdr lst)))) ))))


(defun lesser (pivot lst)
   "If the first element is lower then the pivot he is saved in a list"
  (cond (( or (null pivot)(null lst)) nil)
	(( < pivot (car lst)) (lesser pivot (cdr lst)))
	(t (cons (car lst) (lesser pivot (cdr lst))) )))

(defun greater (pivot lst)
  "If the first element is greater then the pivot he's saved in a list"
  (cond (( or (null pivot ) (null lst)) nil)
	(( >= pivot (car lst)) (greater pivot (cdr lst)))
	(t (cons (car lst) (greater pivot (cdr lst))) )))

