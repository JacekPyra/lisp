;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun fraction (numr denr)
	(list 'fr numr denr)
)

(defun numr (a) 
	(if (listp a)
		(second a)
		a
	)
)

(defun denr (a) 
	(if (listp a)
		(third a)
		1
	)
)

(defun clauser (a)
	(let ((num_r (second a))
		  (den_r (third a))
		)
		(if (= (mod num_r den_r) 0)
			(/ num_r den_r)
			a
		)
	)
)

(defun fr* (a b)
	(let ((num-a (numr a))
		  (num-b (numr b))
		  (denom-a (denr a))
		  (denom-b (denr b)))
	(clauser (fraction (* num-a num-b) (* denom-a denom-b))))
	)

(defun fr+ (a b)
	(let ((num-a (numr a))
		  (num-b (numr b))
		  (denom-a (denr a))
		  (denom-b (denr b)))
	(clauser 
		(fraction (+ (* num-a denom-b) (* num-b denom-a)) (* denom-a denom-b))
	))
	)

(defun fr/ (a b)
	(let ((num-a (numr a))
		  (num-b (numr b))
		  (denom-a (denr a))
		  (denom-b (denr b)))
	(clauser (fraction (* num-a denom-b) (* denom-a num-b))))
	)

(defun fr- (a b)
	(let ((num-a (numr a))
		  (num-b (numr b))
		  (denom-a (denr a))
		  (denom-b (denr b)))
	(clauser (fraction (- (* num-a denom-b) (* num-b denom-a)) (* denom-a denom-b))))
	)