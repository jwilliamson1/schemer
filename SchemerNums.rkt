#lang racket
;quote() is notation for null list
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;LAT = LIST OF ATOMS
;lat returns true if l is a list of atoms
(define lat?
  (lambda (l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]
    )))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;(member? 'meat (list 'mashed 'potatoes' 'and 'meat 'and 'gravy))
;(member? 'tar (list 'mashed 'potatoes' 'and 'meat 'and 'gravy))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat)(quote ()))
      [( eq? (car lat) a)( cdr lat)]
      [else (cons(car lat)
             (rember a
                ( cdr lat)))])))
;(rember 'bacon (list 'bacon 'lettuce 'and 'tomato))
;(rember 'and (list 'bacon 'lettuce 'and 'tomato))
;string functions
(define firsts
  (lambda (l)
    (cond
      [(null? l)(quote())]
      [else (cons (car (car l))
                  (firsts (cdr l)))])))
;(define fruit '(((five plums) four)
;(eleven green oranges)
;((no) more)))
;(firsts fruit)
;number functions
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (add n (sub1 m)))]
      )))

(define subt
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (subt n (sub1 m)))]
      )))

(define multi
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (multi n (sub1 m)))]
      )))

(define divi
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [(< (subt n m) 0) 0]
      [(<= n 0) 0]
      [else (add1 (div (subt n m) m))])))

(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else(add (car tup)(addtup(cdr tup)))])))
;(addtup '(3 1 4))

(define combtup
  (lambda(tup1 tup2)
    (cond
      [(and(null? tup1)(null? tup2) (quote()))]
      [else (cons(add(car tup1)(addtup(cdr tup1)))
                 (add(car tup2)(addtup(cdr tup2))))])))
;(combtup (list 8 2)(list 10 4))            

(define length
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1(length(cdr l)))])))

(define pick
    (lambda (n lat)
      (cond
        [(zero? (sub1 n))(car lat)]
        [else (pick (sub1 n)(cdr lat))])))

(define rempick
  (lambda (n lat)
    (cond      
      [(zero? (sub1 n))(cdr lat)]
      [else
       (cons(car lat)
            (rempick (sub1 n)(cdr lat)))]
      )))
;(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat)(quote())]
      [(number? (car lat))(no-nums(cdr lat))]
      [else (cons (car lat)(no-nums(cdr lat)))])))
;(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [(number? (car lat))
       (cons (car lat)(all-nums(cdr lat)))]
      [else (all-nums(cdr lat))])))
;(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1)(number? a2))
       (= a1 a2)]
      [(or(number? a1)(number? a2)) #f]
      [else (eq? a1 a2)])))

;(eqan? 'r 'r)      
;(eqan? 'r 's)
;(eqan? 1 1)
;(eqan? 1 2)
;(eqan? 'A 1)

(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [else
       (cond
         [(eqan? (car lat) a)
          (add1(occur a (cdr lat)))]
         [else (occur a (cdr lat))])])))
          
;(occur 'to '(to the moon to the stars to beyond))                  

(define tup+
  (lambda (tup1 tup2)
    (cond
      [(and(null? tup1)(null? tup2)) (quote ())]
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (add (car tup1)(car tup2))(tup+ (cdr tup1) (cdr tup2)))])))
(define t1 (list 10 20 30 50 60 70))
(define t2 (list 3 4 5 6))
(define t3 (list 100 200 300))

;(tup+ t1 t3);
;(tup+ t2 t3)
;(tup+ t2 t1)

(define grtr
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (grtr(sub1 n)(sub1 m))])))
;(grtr 4 6)
;(grtr 3 1)
;(grtr 1 1)

(define less
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (less(sub1 n)(sub1 m))])))
;(less 4 6)
;(less 3 1)
;(less 1 1)


(define div
  (lambda (n m)
    (cond
      [(eq? m 0) "Err:div by 0"]
      [(less n m) 0]
      [(add1 (div (subt n m) m))])))
;(div 45 9)
;(div 45 5)
;(div 45 1)
;(div 45 0)

;(define rember*
;  (lambda (a lat)
;    (cond
;      [(null? lat)(quote())]
;      [(eq? (car lat) a) (rember* a (cdr lat))]      
;      [else (cons (car lat)(rember* a (cdr lat)))]))
;    )
;(rember* 'and '(chips and dip and salsa))
;(rember* 'and '(dog and and cat))

;insert to the right of atom
(define insertR
  (lambda (l old new)
    (cond
      [(null? l) (quote())]
      [(eq? (car l) old)(cons old (cons new (cdr l)))]
      [else (cons (car l)(insertR (cdr l) old new))])))
;(insertR  '(tacos tamales and salsa)  'and 'jalepenos) 
;(insertR '(ice cream with fudge for dessert) 'fudge 'topping)

(define insertL
  (lambda (l old new)
    (cond
      [(null? l) (quote())]
      [(eq? (car l) old)(cons new l)]
      [else (cons (car l)(insertL (cdr l) old new))])))
;(insertL '( an abysmally odd task) 'task 'looking)

(define subst
  (lambda (l old new)
    (cond
      [(null? l)(quote())]
      [(eq? (car l) old)(cons new (cdr l))]
      [else (cons (car l)(subst(cdr l) old new))])))
;(subst '(a great green dragon) 'green 'gold)
(define subst2
  (lambda (l new o1 o2)
    (cond
      [(null? l)(quote())]
      [(or (eq? (car l) o1)(eq? (car l) o2))(cons new (cdr l))]      
      [else (cons (car l)(subst2(cdr l) new o1 o2))])))
;(subst2 '(banana ice cream with chocolate topping) 'vanilla 'chocolate 'banana)
;(subst2 '(Mint ice cream with chocolate topping) 'vanilla 'chocolate 'banana)
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat)(quote())]
      [(eq? (car lat) a)(multirember a (cdr lat))]
      [else (cons (car lat)(multirember a (cdr lat)))])))
;(multirember 'cup '(coffee cup tea cup hick cup))      


(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat)(quote())]
      [(eq?(car lat) old)(cons old(cons new (multiinsertR new old (cdr lat))))]
      [else(cons (car lat)(multiinsertR new old (cdr lat)))])))
;(multiinsertR 'of 'cup '(cup coffee cup tea cup hick cup))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat)(quote())]
      [(eq?(car lat) old)(cons new(cons old (multiinsertL new old(cdr lat))))]
      [else(cons (car lat)(multiinsertL new old(cdr lat)))])))
;(multiinsertL 'a 'cup '(cup coffee cup tea cup hick cup))

(define multiSubst
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq?(car lat) old)(cons new (multiSubst new old (cdr lat)))]
      [else (cons (car lat)(multiSubst new old (cdr lat)))]
      )))
;(multiSubst 'the 'a '(a dark place for a dark mind))
(define one?
  (lambda (n)
    (= n 1)))
;(one? 3)
;(one? 0)
;(one? 1)
(define rempick?
  (lambda (n lat)
    (cond
      [(null? lat)(quote())]
      [(one? n)(cdr lat)]
      [else (cons (car lat)(rempick? (sub1 n)(cdr lat)))])))
;(rempick? 3 '(lemon meringue salty pie))
;(rempick? 1 '(lemon meringue salty pie))
;(rempick? 4 '(lemon meringue salty pie))
;(rempick? 3 '())
(define rember*
  (lambda (a l)
    (cond
      [(null? l)(quote())]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) (rember* a (cdr l)) ]
         [else (cons (car l)(rember* a (cdr l)))])]
      [else(cons (rember* a (car l))(rember* a (cdr l)))])))
;(rember* 'sauce '(((tomato sauce))((bean) sauce)(and ((flying)) sauce)) )
;(rember* 'cup '((coffee) cup ((tea) cup)(and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l)(quote())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)(cons old (cons new (insertR* new old (cdr l))))]
         [else(cons(car l)(insertR* new old (cdr l)))]
         )]
      [else(cons(insertR* new old (car l))(insertR* new old (cdr l)))])))
;(insertR* 'roast 'chuck '(((how much (wood))could((a (wood) chuck))(((chuck)))(if (a) ((wood chuck)))could chuck wood) ))

(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? (car l) a)(add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))])]
      [else(add(occur* a (car l))(occur* a (cdr l)))])))
;(occur* 'banana '((banana)(split ((((banana ice)))(cream (banana))sherbet))(banana)(bread)(banana brandy)) )

(define subst*
  (lambda (new old l)
    (cond
      [(null? l)(quote())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)(cons new (subst* new old (cdr l)))]
         [else(cons(car l)(subst* new old (cdr l)))])]
      [else(cons(subst* new old (car l))(subst* new old (cdr l)))])))
;(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l)(quote())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)(cons new(cons old (insertL* new old (cdr l))))]
         [else(cons(car l)(insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l))(insertL* new old (cdr l)))])))
;(insertL* 'pecker 'chuck '(((how much (wood))could((a (wood) chuck))(((chuck)))(if (a) ((wood chuck)))could chuck wood) ))

(define member*
  (lambda (a l)
    (cond
      [(null? l)#f]
      [(atom?(car l))
       (or(eq?(car l) a)
         (member* a (cdr l)))]
      [else(or(member* a (car l))
               (member* a (cdr l)))])))

;(member* 'chips '((potato) (chips ((with) fish) (chips))))
;(member* 'dip '((potato) (chips ((with) fish) (chips))))
(define leftmost
  (lambda (l)
    (cond
      [(atom?(car l))(car l)]
      [else(leftmost (car l))])))

;(leftmost '((potato) (chips ((with) fish) (chips))))
;ERROR(leftmost '(((() four)) 17 (seventeen)))
;ERROR(leftmost (quote()))

(define eqlist-v1?
  (lambda (l1 l2)
    (cond
      [(and(null? l1)(null? l2))#t]
      ; elim null null
      [(and(null? l1)(atom? (car l2)))#f]
      ;null null and null 1 atom 2 elim
      [(null? l1) #f]
      ;only 3rd case left where l2
      ;also check null before checking atom car l1
      [(and(null? l2)(atom? (car l1)))#f]
      ;need to check null2 because we only know null null, null atom, and null list are elim
      [(and(atom? (car l1))(atom? (car l2)))
       (and
        (eqan? (car l1)(car l2))(eqlist-v1? (cdr l1)(cdr l2)))]
      ;atom atom gone
      [(atom? (car l1))#f]       
      ;because atom null and atom atom are gone one more option; already checked null
      [(null? l2)]
      ;check null l2 before checking car on it
      ;we know if atom car l1 only thing left for l2 is a list
      [(atom? (car l2))#f]      
      ;null and atom elim, atom atom elim; l1 is list
      [else(and(eqlist-v1?(car l1)(car l2))
               (eqlist-v1?(cdr l1)(cdr l2)))])))
                       
;(eqlist-v1? '(beef ((sausage)) (and (soda)))  '(beef ((sausage)) (and (soda))) )
;(eqlist-v1? '(beef ((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(eqlist-v1? '(((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(eqlist-v1? '(((sausage)) (and (soda)))  '(((pepperoni)) (and (soda))) )

(define eqlist-v2?
  (lambda (l1 l2)
    (cond
      [(and(null? l1)(null? l2))#t]
      ; elim null null
      [(or(null? l1)(null? l2))#f]
      ;elim null and any other conidtion
      [(and(atom? (car l1))(atom? (car l2)))
       (and
        (eqan? (car l1)(car l2))(eqlist-v2? (cdr l1)(cdr l2)))]
      ;atom atom gone
      [(or(atom? (car l1))(atom?(car l2)))#f]
      ;because atom null and atom atom are gone one more option; already checked null
      ;we know if atom car l1 only thing left for l2 is a list
      
      ;null and atom elim, atom atom elim; l1 is list
      [else(and(eqlist-v2?(car l1)(car l2))
               (eqlist-v2?(cdr l1)(cdr l2)))])))

;(eqlist-v2? '(beef ((sausage)) (and (soda)))  '(beef ((sausage)) (and (soda))) )
;(eqlist-v2? '(beef ((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(eqlist-v2? '(((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(eqlist-v2? '(((sausage)) (and (soda)))  '(((pepperoni)) (and (soda))) )

(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1)(atom? s2))
       (eqan? s1 s2)]
      [(or(atom? s1)(atom? s2)
          )#f]
      [else(eqlist-v3? s1 s2)])))

;(equal? '(beef ((sausage)) (and (soda)))  '(beef ((sausage)) (and (soda))) )
;(equal? '(beef ((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(equal? '(((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
;(equal? '(((sausage)) (and (soda)))  '(((pepperoni)) (and (soda))) )

(define eqlist-v3?
  (lambda (l1 l2)
    (cond
      [(and(null? l1)(null? l2))#t]
      [(or(null? l1)(null? l2))#f]
      [else
       (and(equal?(car l1)(car l2))
               (eqlist-v3?(cdr l1)(cdr l2)))])))

(eqlist-v3? '(beef ((sausage)) (and (soda)))  '(beef ((sausage)) (and (soda))) )
(eqlist-v3? '(beef ((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
(eqlist-v3? '(((sausage)) (and (soda)))  '(beef ((pepperoni)) (and (soda))) )
(eqlist-v3? '(((sausage)) (and (soda)))  '(((pepperoni)) (and (soda))) )

(define remberS
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      
              (( equal? ( car l) s) ( cdr l))
              (else (cons ( car l)
                          (remberS s
                                   ( cdr l)))))))

(remberS '((sausage)) '(((sausage)) (and (soda))))