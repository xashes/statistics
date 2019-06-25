#lang typed/racket

(require math
         plot)

(provide my/mean)
(: my/mean (-> (Listof Real) Real))
(define (my/mean xs)
  (/ (apply + xs)
     (length xs))
  )
(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  )

(provide my/var)
(: my/var (-> (Listof Real) Real))
(define (my/var xs)
  (let ([mu : Real (my/mean xs)])
    (/ (apply + (map (lambda ([x : Real]) : Real
                             (diff-sqr x mu))
                     xs))
       (length xs)))
  )

(: diff-sqr (-> Real Real Nonnegative-Real))
(define (diff-sqr x y)
  (sqr (- x y))
  )
(module+ test
  (define xs '(1 2 3 4 5 6))
  (check-equal? (my/mean xs) (mean xs))
  (check-equal? (my/var xs) (variance xs))
  (check-equal? (diff-sqr 2 8) 36)
  )

(: my/std (-> (Listof Real) Number))
(define (my/std xs)
  (sqrt (my/var xs)))
(module+ test
  (check-equal? (my/std xs) (stddev xs))
  )
