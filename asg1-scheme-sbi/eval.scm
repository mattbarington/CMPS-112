#!/Applications/Racket/bin/mzscheme -qr
(define *variable-table* (make-hash))

(hash-set! *variable-table* 'i 3)

(display (hash-ref *variable-table* 'i))

(define *function-table* (make-hash))

(for-each
	(lambda (pair)
		(hash-set! *function-table* (car pair) (cadr pair)))
	`(
		(+	,+)
		(-	,-)
		(%	,(lambda (x y) (- x(* trunc (/ xy)) y)))
		(*	,*)
		(^	,(lambda (x y) (expt x y)))
		(/	,/)
		(=	,=)
		(<	,<)
		(>	,>)
		(>=	,>=)
		(<=	,<=)
		(atan ,atan)
		; (<>	,<>)
 		(log10_2 0.301029995663981195213738894724493026768189881)
 		(sqrt_2 1.414213562373095048801688724209698078569671875)
		(div ,(lambda (x y) (floor (/ x y))))
		(log10 ,(lambda (x) (/ (log x) (log 10.0))))
		(mod ,(lambda (x y) (- x (* (div x y) y))))
		(quot ,(lambda (x y) (truncate (/ x y))))
		(rem ,(lambda (x y) (- x (* (quot x y) y))))
		(ceil ,ceiling)
	 	(exp ,exp)
		(floor ,floor)
		(log ,log)
	 	(sqrt ,sqrt))
	)


(define (eval expr)
	(if (number? expr)
		expr
		(if (hash-has-key? *variable-table* expr)
			(begin
				(hash-ref *variable-table* expr)
			)
			(begin
				(if (null? (cddr expr))
					((hash-ref *function-table* (car expr)) (eval (cadr expr)))
					((hash-ref *function-table* (car expr)) (eval (cadr expr)) (caddr expr))
				)
			)
		)
	)
)

(define (show label item)
        (newline)
        (display label) (display " = ") (display item)
        (newline))

(show "log10 (85 + 15)" (eval '(log10 (+ 85 15))))
(show "(3 * 4) + (10 * 2)" (eval '(+ (* 3 4) (* 10 2))))
;(show "(+2)" (eval '(+2)))
