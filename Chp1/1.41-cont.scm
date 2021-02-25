;inner layer
(lambda(f2)(lambda(x2)(f2 (f2 x2))))
;next
(lambda(f1)(lambda(x1)(f1 (f1 x1))))
;outer
(lambda(f0)(lambda(x0)(f0 (f0 x0))))
;apply 
(lambda(f0)(lambda(x0)(f0 (f0 x0))))
;continue from last part and remember you can evaulate inner to outer
;apply the following to f0
;strip away lambda and expose next var to be filled;
(lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1))) 
;applied
(lambda((lambda(x1)
				((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
					((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1))) )
		(lambda(x0)
			((lambda(x1)
				((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
					((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  
			((lambda(x1)
				((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
					((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  x0))))
					
;strip away lambda

(lambda(x0)
	((lambda(x1)
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  
	((lambda(x1)
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  x0)))
;apply inc to x0; correct
(((lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  
	((lambda(x1)
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  inc))5)				
;apply inc to inner x1; correct
(((lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  

		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) inc)))5)		
;apply inc to inner inner f2 aka f4; correct
(((lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  

		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
			(lambda(x2)( inc ( inc x2)))))5)	
; we can see at this point that by contantly applying this we are going to end up with 16 applications of inc
;apply first x2 to f2; correct
(((lambda(x1)
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))
		
		(lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
			)5)		
;works			
(((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
		(lambda(x2)((lambda(x2)( inc ( inc x2))) 
			((lambda(x2)( inc ( inc x2))) x2)))))  	
				5)		
;works
(((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
	(lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
		((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
		((lambda(x2)( inc ( inc x2))) x2))) x2))) 
	)  	
5)
;works
((lambda(x2)
				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2)))  x2)))		  
5)
;works
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2)))  5))	
;works
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 5)))		  
;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) 5))))		
;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)( inc ( inc x2))) 
					( inc ( inc 5)))))
;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)( inc ( inc x2))) 
					7)))
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) 
					9))
					;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) 9)))						
;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				((lambda(x2)( inc ( inc x2))) 
					( inc ( inc 9))))	
;
((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
			
				13)	
((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					((lambda(x2)( inc ( inc x2))) 
					((lambda(x2)( inc ( inc x2))) 13)))
					;... skip steps
((lambda(x2)((lambda(x2)( inc ( inc x2)))  
					((lambda(x2)( inc ( inc x2))) x2))) 
					17)
;... can see it gets applied 4 more times					