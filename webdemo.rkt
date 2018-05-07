#lang web-server/insta
(define (start request)
  (define a-blog
    (cond[(can-parse-post? (request-bindings request))
          (cons(parse-post (request-bindings request))
               BLOG)]
         [else BLOG]))
  (render-blog-page a-blog request))

(define (message-box message)
  (cons "alert('" (cons message (cons "')" null))))

(define my-form `(form
                  (input ((name "title")(value "title")(onclick ,(message-box "this is js"))))
                  (input ((name "body")))            
                  (input ((type "submit")))))



(define (render-blog-page a-blog request)
  (response/xexpr
   `(html
     (head (title "Scheme Test"))
     (body (h1 "Scheming and Running Rackets")
           ,(render-posts a-blog)
          ,(map-form-elements my-form (λ (x)(cons x (cons `(br) null))))
           ))))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (flatmap proc seq) 
  (accumulate append null (map proc seq)))

(define (map-form-elements form proc)
  (let ((form-elements (cdr form)))
    (cons `form (flatmap
     proc
     form-elements))))

(struct post (title body))

(define BLOG (list (post "First post" "This is my first post!")
                   (post "Second post" "we are doomed. Yay!")))


(define (render-post a-post)
  `(div ((class "post"))
        ,(post-title a-post)
        (p ,(post-body a-post))))

(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))     

;can-parse-post? : (bindings? . -> . boolean?)

(define (can-parse-post? bindings)
 (and (exists-binding? 'title bindings)
      (exists-binding? 'body bindings)))

;parse-post : bindings -> post
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

