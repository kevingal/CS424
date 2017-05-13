#lang racket

;; 2015A, q1
(define reverse-with-count (lambda (xs ns) (reverse (reverse-with-count-aux xs ns))))

(define reverse-with-count-aux
  (lambda (xs ns)
    (if (null? xs)
        '()
        (if (= 0 (car ns))
            (reverse-with-count-aux (cdr xs) (cdr ns))
            (cons (car xs)
                  (reverse-with-count-aux xs (cons (- (car ns) 1) (cdr ns))))))))

;; 2015J, q1
(define after-filter
  (lambda (p xs)
    (if (or (null? xs) (null? (cdr xs)))
        '()
        (if (p (car xs))
            (cons (car (cdr xs)) (after-filter p (cdr xs)))
            (after-filter p (cdr xs))))))

;; 2014A, q1
(define add-numbers
  (lambda (x)
    (cond
      [(list? x)
       (sum (map add-numbers x))]
      [(number? x) x]
      [else 0])))

(define sum
  (lambda (xs)
    (if (null? xs)
        0
        (+ (car xs) (sum (cdr xs))))))

;; 2014J, q1
(define deep-fetch
  (lambda (p xs)
    (cond
      [(null? xs)
       '()]
      [(list? (car xs))
       (append (deep-fetch p (car xs)) (deep-fetch p (cdr xs)))]
      [else
       (if (p (car xs))
           (cons (car xs) (deep-fetch p (cdr xs)))
           (deep-fetch p (cdr xs)))])))