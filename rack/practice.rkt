#lang racket

;;; Exam question.
(define pick-elements
  (lambda (p li)
    (if (< (length li) 2)
        null
        (if (p (car li))
            (cons (cadr li) (pick-elements p (cdr li)))
            (pick-elements p (cdr li))))))

;(pick-elements zero? (list 0 1 2 0 1))

;;; Flat recursion.
(define multi-assoc
  (lambda (obj alist)
    (if (null? alist)
        #f
        (if (is-match obj (car alist))
            (car alist)
            (multi-assoc obj (cdr alist))))))

(define is-match
  (lambda (obj pair)
    (equal? obj (car pair))))

;(multi-assoc 3 (list (list 0 1) (list 2 3)))

;;; Recursion.
(define filter
  (lambda (p li)
    (if (null? li)
        null
        (if (p (car li))
            (cons (car li) (filter p (cdr li)))
            (filter p (cdr li))))))

; (filter odd? '(1 2 3 4 5 6 7 8 9))

;;; Strings.
(define variable?
  (lambda (obj)
    (and (symbol? obj)
         (equal? #\*
                 (string-ref (symbol->string obj) 0)))))

;;; Additional String handling.
(define str-car
  (lambda (s)
    (substring s 0 1)))

(define str-cdr
  (lambda (s)
    (substring s 1)))

(define non-empty-string?
  (lambda (s)
    (and (string? s)
         (not (equal? s "")))))

(define explode
  (lambda (sym)
    (explode-aux (symbol->string sym))))

(define explode-aux
  (lambda (str)
    (if (non-empty-string? str)
        (cons (string->symbol (str-car str))
              (explode-aux (str-cdr str)))
        '())))

(define implode
  (lambda (list-of-symbols)
    (implode-aux "" list-of-symbols)))

(define implode-aux
  (lambda (symbol-str list-of-symbols)
    (if (null? list-of-symbols)
        (string->symbol symbol-str)
        (implode-aux (string-append symbol-str (symbol->string (car list-of-symbols)))
                     (cdr list-of-symbols)))))

;; there are two trickier problems here.

;;; Higher order functions.
(define cxr
  (lambda (operations)
    (if (non-empty-string? operations)
        (compose (get-operator (str-car operations))
                 (cxr (str-cdr operations)))
        (lambda (x) x))))

(define get-operator
  (lambda (key)
    (if (equal? key "a")
        car
        cdr)))

;;; Accumulators and continuations.
(define split
  (lambda (li)
    (split-aux '() '() li)))

(define split-aux
  (lambda (symbols numbers li)
    (if (null? li)
        (list symbols numbers)
        (let ((hd (car li))
              (tl (cdr li)))
          (if (symbol? hd)
              (split-aux (append symbols (list hd)) numbers tl)
              (split-aux symbols (append numbers (list hd)) tl))))))

;; re-implement using functional continuations.

;;; Map.
(define mu-map
  (lambda (fn . lists)
    (if (null? (car lists))
        '()
        (cons (apply fn (get-first-items lists))
              (apply mu-map fn (drop-first-items lists))))))

(define get-first-items
  (lambda (lists)
    (if (null? lists)
        '()
        (cons (car (car lists))
              (get-first-items (cdr lists))))))

(define drop-first-items
  (lambda (lists)
    (if (null? lists)
        '()
        (cons (cdr (car lists))
              (drop-first-items (cdr lists))))))