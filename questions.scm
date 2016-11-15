(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
    (cond ((null? items) nil)
    (else (cons (proc (car items)) (map proc (cdr items))))))

(define (cons-all first rests)
  (cond ((null? rests) nil)
        (else (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests))))))

(define (zip pairs)
  (define one (map (lambda (x) (car x)) pairs))
  (define two (map (lambda (x) (cadr x)) pairs))
  (list one two))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  (define (enumerate-so-far index s)
      (cond ((null? s) nil)
            (else (cons (cons index (cons (car s) nil)) (enumerate-so-far (+ index 1) (cdr s))))))
  (enumerate-so-far 0 s))

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((= total 0) (cons nil nil))
    ((null? denoms) nil)
    ((>= (/ total (car denoms)) 1) (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
    (else (list-change total (cdr denoms)))))
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr)
        ((quoted? expr) expr)
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
        (cons form (cons params (cons body nil)))))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
        (append (cons 'lambda (cons (car (zip values)) body)) (cdr (zip values)))))
        (else
          (map let-to-lambda expr))))
