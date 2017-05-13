#lang racket

;; PART A: Finger Exercises.
(define setify 
  (lambda (not-set)
    (setify-aux '() not-set)))

(define setify-aux
  (lambda (set not-set)
    (if (null? not-set)
        set
        (let ((hd (car not-set))
              (tl (cdr not-set)))
          (if (member hd set)
              (setify-aux set tl)
              (setify-aux (cons hd set) tl))))))

(define set-cardinality
  (lambda (set)
    (if (null? set)
        0
        (+ 1 (set-cardinality (cdr set))))))

(define set-union
  (lambda (s1 s2)
    (setify (append s1 s2))))

(define set-intersection
  (lambda (s1 s2)
    (set-combiner s1 s2 member)))

(define set-difference
  (lambda (s1 s2)
    (set-combiner s1 s2 (compose not member))))

(define set-combiner
  (lambda (s1 s2 member-condition)
    (if (null? s1)
        '()
        (let ((hd (car s1))
              (tl (cdr s1)))
          (if (member-condition hd s2)
              (cons hd (set-combiner tl s2 member-condition))
              (set-combiner tl s2 member-condition))))))

(define set-equal?
  (lambda (s1 s2)
    (let* ((intersect (set-intersection s1 s2))
           (intersect-size (set-cardinality intersect)))
      (and (equal? intersect-size (set-cardinality s1))
           (equal? intersect-size (set-cardinality s2))))))

(define set-map-join
 (lambda (f s)
   (setify (apply append (map f s)))))

;; PART B: λ Calculus Manipulation.
(define free-variables
  (lambda (e)
    (cond
      [(is-var? e)
       (list e)]
      [(is-application? e)
       (set-union (free-variables (car e))
                  (free-variables (cadr e)))]
      [(is-lambda? e)
       (set-difference (free-variables (caddr e))
                       (list (cadr e)))]
      [else (error "invalid expression!")])))

(define is-var?
  (lambda (e)
    (not (list? e))))

(define is-application?
  (lambda (e)
    (and (list? e)
         (not (equal? (car e) 'λ)))))

(define is-lambda?
  (lambda (e)
    (and (list? e)
         (equal? (car e) 'λ))))

(define is-lambda-application?
  (lambda (e)
    (and (is-application? e)
         (is-lambda? (car e)))))

(define get-param
  (lambda (lambda-e)
    (cadr lambda-e)))

(define get-inner-expr
  (lambda (lambda-e)
    (caddr lambda-e)))

(define β-reduce
  (lambda (e)
    (if (not (is-lambda-application? e))
        #f
        (let ((param (cadr (car e)))
              (value (cadr e))
              (inner-expr (get-inner-expr (car e))))
          (if (α-rename-required? param value inner-expr)
              #f
              (β-substitute param value inner-expr))))))

(define α-rename-required?
  (lambda (param value e)
    (cond
      [(is-var? e)
       #f]
      [(is-application? e)
       (or (α-rename-required? param value (car e))
           (α-rename-required? param value (cadr e)))]
      [(is-lambda? e)
       (or (member (get-param e) (free-variables value))
           (α-rename-required? param value (get-inner-expr e)))])))

(define β-substitute
  (lambda (param value e)
    (cond
      [(is-var? e)
       (if (equal? e param)
           value
           e)]
      [(is-application? e)
       `(,(β-substitute param value (car e)) ,(β-substitute param value (cadr e)))]
      [(is-lambda? e)
       (if (equal? param (get-param e))
           e
           `(λ ,(get-param e) ,(β-substitute param value (get-inner-expr e))))])))

;; Test suite.
(define test-it
  (lambda ()
    (list
     (set-equal? (set-union '(a b c d e) '(c d e f g))
                 '(b d a c e g f))
     (set-equal? (set-map-join (λ (e) (list (+ e 1) (* e 10))) '(1 2 3 4))
                 '(3 2 10 4 40 5 20 30))
     (set-equal? (free-variables '((a b) (λ c ((d c) (e b)))))
                 '(a b d e))
     (equal? (β-reduce '((λ x (((λ x (x y)) x) (x b))) z))
             '(((λ x (x y)) z) (z b)))
     (equal? (β-reduce '((λ x (((λ y (x y)) x) (x b))) y))
             #f))))