
(picking)
(cherry picking daddies)
(a-friend)

	(defin 1st-rec
		(lambda (newlat seen)
			(a-friend)(cons (cherry) newlat) seen))
			
	(define 2nd-rec
		lambda (newlat seen)
			1st-rec newlat (cons picking seen))
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies newlat) seen))
	
	(define final-rec
		col() ())
		
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies newlat) seen)))(quote())(quote())	
			
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies (quote()) (quote())))
			2nd-rec (daddies)(quote)
	
	define 2nd-rec
		lambda (newlat seen)
			1st-rec newlat (cons picking seen))(daddies)(quote)
	1st-rec daddies picking
	
	(defin 1st-rec
		(lambda (newlat seen)
			(cons (cherry) newlat) seen))(daddies) (picking)
	
	cons(cherry daddies ) (picking)		
	#f
	
	
(picking)
(cherry derp daddies)
(a-friend)

	(defin 1st-rec
		(lambda (newlat seen)
			(a-friend)(cons (cherry) newlat) seen))
			
	(define 2nd-rec
		lambda (newlat seen)
			1st-rec (cons (derp) newlat) seen))
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies newlat) seen))
	
	(define final-rec
		col() ())
		
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies newlat) seen)))(quote())(quote())	
			
	(define 3rd-rec
		lambda(newlat seen)
			2nd-rec (cons daddies (quote()) (quote())))
			;sub
			2nd-rec (daddies)(quote)
	
	define 2nd-rec
		lambda (newlat seen)
			1st-rec (cons derp newlat ) seen )(daddies)(quote)
	1st-rec (derp picking)(quote())
	
	(defin 1st-rec
		(lambda (newlat seen)
			(cons (cherry) newlat) seen))(derp picking)(quote())
	
	(a-friend(cons(cherry derp picking ) (quote())))
	#f