;inner level
(lambda(f2)(lambda(x2)(f2 (f2 x2))))
;outer level before eval	

(lambda(f1)(lambda(x1)(f1 (f1 x1))))  
;eval inner into outer
(lambda((lambda(f2)(lambda(x2)(f2 (f2 x2)))))
	(lambda(x1)
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1))))  
  
	  
;strip away lambda and expose next var to be filled;
(lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1))) 	  
	  
;try applying inc   

	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) inc))
;a inc with innner lambda 		
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			(lambda(inc)(lambda(x2)(inc (inc x2)))))
;apply inner lambda to outer 
(lambda(x2)((lambda(x2)(inc (inc x2))) ((lambda(x2)(inc (inc x2))) x2)))
; apply 5
((lambda(x2)((lambda(x2)(inc (inc x2))) ((lambda(x2)(inc (inc x2))) x2)))5)

((lambda(x2)(inc (inc x2))) ((lambda(x2)(inc (inc x2))) 5))
;apply 5 to inner lambda
((lambda(x2)(inc (inc x2))) (inc (inc 5)))
; eval incs
((lambda(x2)(inc (inc x2))) 7)
(inc (inc 7))
;result is 9