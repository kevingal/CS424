#lang racket

;; 1) Last item of list.
(define my-last
  (lambda (xs)
    (if (null? (cdr xs))
        (car xs)
        (my-last (cdr xs)))))

;; 2) Last but one.
(define my-but-last
  (lambda (xs)
    (if (null? (cdr (cdr xs)))
        (car xs)
        (my-but-last (cdr xs)))))

;; 3) Kth element of list.
(define element-at
  (lambda (xs n)
    (if (= n 1)
        (car xs)
        (element-at (cdr xs) (- n 1)))))

;; 4) # elements of list.
(define my-len
  (lambda (xs)
    (foldl add-one 0 xs)))

(define add-one (lambda (x y) (+ y 1)))

;; 5) Reverse a list.
(define my-rev (lambda (xs) (my-rev-aux xs '())))
(define my-rev-aux
  (lambda (xs xsr)
    (if (null? xs)
        xsr
        (my-rev-aux (cdr xs) (cons (car xs) xsr)))))

;; 6) Palindrome check.
(define is-pally
  (lambda (xs)
    (equal? (my-rev xs) xs)))

;; 7) Flatten nested list structure.
(define my-flatten
  (lambda (xs)
    (if (null? xs)
        '()
        (if (list? (car xs))
            (my-flatten (append (car xs) (cdr xs)))
            (cons (car xs) (my-flatten (cdr xs)))))))

;; 8) Eliminate consecutive duplicates.
(define compress
  (lambda (xs)
    (if (or (null? xs) (null? (cdr xs)))
        xs
        (if (equal? (car xs) (car (cdr xs)))
            (compress (cons (car xs) (cdr (cdr xs))))
            (cons (car xs) (compress (cdr xs)))))))

;; 9) Pack consecutive elements.
(define pack
  (lambda (xs)
    (if (null? xs)
        xs
        (cons (take-while (curry equal? (car xs)) xs)
              (pack (drop-while (curry equal? (car xs)) xs))))))

(define take-while
  (lambda (p xs)
    (if (or (null? xs) (not (p (car xs))))
        '()
        (cons (car xs) (take-while p (cdr xs))))))

(define drop-while
  (lambda (p xs)
    (if (or (null? xs) (not (p (car xs))))
        xs
        (drop-while p (cdr xs)))))
        
;; 10) Run-length encoding.
(define encode
  (lambda (xs)
    (map (lambda (xs) (list (my-len xs) (car xs))) (pack xs))))