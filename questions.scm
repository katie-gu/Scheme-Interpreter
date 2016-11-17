(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond ((null? items) nil)
    (else (cons (proc (car items)) (map proc (cdr items)))
    )
  )
)

(define (cons-all first rests)
  (cond ((null? rests) nil)
    (else (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests)))
    )
  )
)

(define (zip pairs)
  (define one (map (lambda (x) (car x)) pairs))
  (define two (map (lambda (x) (cadr x)) pairs))
  (list one two)
)

    ;;;make sure all functions in pair are simultaneously iterated through

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerate-so-far index s)
    (cond ((null? s) nil)
      (else (cons (cons index (cons (car s) nil)) (enumerate-so-far (+ index 1) (cdr s))))
      )
    )
  (enumerate-so-far 0 s)
)


;;;basically return ((index s[index]) (index + 1 s[index + 1]) ...)
  ; END PROBLEM 17


;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((null? denoms) nil)
    ((= total 0) (cons nil nil))
    ((>= (/ total (car denoms)) 1) (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
    (else (list-change total (cdr denoms)))
    )
  )
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

          (cons form (cons params (let-to-lambda body)))
          ))


          ((let? expr)
           (let ((values (cadr expr))
                 (body   (car (cddr expr))))

              (cons (list 'lambda (map car values) (let-to-lambda body))
                (map let-to-lambda (map cadr values)))
              ))

        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)

         ; END PROBLEM 19
         )
    )
  )
