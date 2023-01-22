(defun polar (mg arg)
	(list 'polr mg arg)
)

(defun recta (rl im)
	(list 'rect rl im)
)

(defun pol_to_rec (a) 
	(let ((mg (second a))
		  (arg (third a)))
          (recta (* mg (cos arg)) (* mg (sin arg)))
          ))


(defun rec_to_pol (a) 
	(let ((rl (second a))
		  (im (third a)))
          (polar (sqrt (+ (* rl rl) (* im im))) 
		  (
			if (> im 0)
			(atan im rl)
			(+ pi (atan im rl))
		  ))
          ))