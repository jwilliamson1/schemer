#lang web-server/insta

(struct post (title body))

(define BLOG (list (post "first" "first entry")
(post "world ending" "we are doomed. Yay!")))


(define (render-post a-post)
  `(div ((class "post"))
        ,(post-title a-post)
        (p ,(post-body a-post))))

(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))

(define (start request)
  (define a-blog
    (cond[(can-parse-post? (request-bindings request))
          (cons(parse-post (request-bindings request))
               BLOG)]
         [else BLOG]))
  (render-blog-page a-blog request))

      
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html
     (head (title "Scheme Test"))
     (body (h1 "scheming and running rackets")
           ,(render-posts a-blog)
           (form
            (input ((name "title")))
            (input ((name "body")))
            (input ((type "submit"))))))))

;can-parse-post? : (bindings? . -> . boolean?)

(define (can-parse-post? bindings)
 (and (exists-binding? 'title bindings)
      (exists-binding? 'body bindings)))

;parse-post : bindings -> post
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

