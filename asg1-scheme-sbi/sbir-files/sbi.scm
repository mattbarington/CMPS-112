#!/Applications/Racket/bin/mzscheme -qr
;#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s Is a list element.~n" line)) program)
    (printf ")~n"))

(define *function-table* (make-hash))

(define *variable-table* (make-hash))

(hash-set! *variable-table* 'e 2.718281828459045235360287)
(hash-set! *variable-table* 'pi 3.141592653589793238462643)

(for-each
	(lambda (pair)
		(hash-set! *function-table* (car pair) (cadr pair)))
	`(
		(+	,+)
		(-	,-)
		(%	,(lambda (x y) (- x(* trunc (/ xy)) y)))
		(*	,*)
		(^	,(lambda (x y) (expt x y)))
		(/	,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
		(=	,=)
		(<	,<)
		(>	,>)
		(>=	,>=)
		(<=	,<=)
		(sin	,sin)
		(cos	,cos)
		(tan	,tan)
		(asin	,asin)
		(acos	,acos)
		(abs	,abs)
		(atan	,atan)
		(round	,round)
		(<>	,(lambda (a b) (not (= a b))))
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
		(log ,(lambda (x) (log (evaluate (+ x 0.0)))))
	 	(sqrt ,sqrt))
	)


(define (evaluate expr)
	;(show expr "in eval")
	(cond [(or (number? expr) (string? expr)) expr]									;number literal or string literal
		[(and (symbol? expr) (hash-has-key? *variable-table* expr)) (hash-ref *variable-table* expr)] ;single variable
		[(and (pair? expr) (hash-has-key? *variable-table* (car expr))) 							;vector reference
							(vector-ref (hash-ref *variable-table* (car expr)) (evaluate (cadr expr)))]
		[(and (pair? expr) (hash-has-key? *function-table* (car expr)))								;function
					;(show expr "in function set thing")
					(if (null? (cddr expr))											;function w/ one argument
						((hash-ref *function-table* (car expr)) (evaluate (cadr expr)))		;function w/ two arguments
						((hash-ref *function-table* (car expr)) (evaluate (cadr expr)) (evaluate (caddr expr))))]
			
		)
	)

; (define (evaluate expr)
; 	(show "variable-table in eval" *variable-table*)
; 	(show "eval: expr" expr)
; 	(show "eval: expr in variable table?" (hash-has-key? *variable-table* expr))
; 	(when (and (pair? expr) (hash-has-key? *variable-table* (car expr)))
; 		(hash-ref *variable-table* (car expr))
; 		(display "This statement should be like rlly unreachable")
; 		(newline))
; 		; (show "expr is a pair it's car" (car expr))
; 		; (show "car is in var-table" (hash-has-key? *variable-table* (car expr))))
; 	(if (or (number? expr) (string? expr))
; 		expr
; 		(if (hash-has-key? *variable-table* expr)	
; 			(begin
; 			(if (and (pair? expr) (vector? (hash-has-key? *variable-table* (car expr))))
; 				(vector-ref (hash-ref *variable-table* (car expr)) (cadr expr))
; 				(hash-ref *variable-table* expr)
; 				)
; 			(show "SLIDING INTO THE DMS" "")
; 			)
; 			(if (null? (cddr expr))
; 				(begin
; 					(show "SLiding into the function-table" "")
; 				((hash-ref *function-table* (car expr)) (evaluate (cadr expr))))
; 				((hash-ref *function-table* (car expr)) (evaluate (cadr expr)) (evaluate (caddr expr)))
; 			)
; 		)
; 	)
; )

(define *label-table* (make-hash))

(define *statement-table* (make-hash))

(define (..input args acc)
	(let ((input (read)))
		(if (eof-object? input)
			(hash-set! *variable-table* 'inputcount (- 1))
			(if (null? args)
				(hash-set! *variable-table* 'inputcount acc)
				(begin
					(hash-set! *variable-table* (car args) input)
					(..input (cdr args) (+ 1 acc)))))))
; (if (null? args)
	; 	(hash-set! *variable-table* 'inputcount acc)
	; 	(begin
	; 		(hash-set! *variable-table* (car args) (read))
	; 		(..input (cdr args) (+ 1 acc)))))


(for-each
	(lambda (pair)
		(hash-set! *statement-table* (car pair) (cadr pair)))
		`(
			(dim	,(lambda (array)
					(hash-set! *variable-table* (caar array) (make-vector ( + (evaluate (cadar array)) 1 )))
					) ;lambda
			) ;dim
			(let	,(lambda (mem/expr)
					(if (symbol? (car mem/expr))
						(hash-set! *variable-table* (car mem/expr) (evaluate (cadr mem/expr))) ;variable assignment
						(vector-set! (hash-ref *variable-table* (caar mem/expr))		;array assignment
							;(hash-ref *variable-table* (evaluate (cadar mem/expr)))
							(evaluate (cadar mem/expr))
							(evaluate (cadr mem/expr)))
					) ;if
				)) ;lambda let
			(goto	,(lambda (label)
					(if (hash-has-key? *label-table* (car label))
						(interpret (hash-ref *label-table* (car label)))
						(die `("Label:" ,label " does not exist in the label-table"))
					))) ;if lambda goto
			(if		,(lambda (args)
					(when (and (evaluate (car args))
							(hash-has-key? *label-table* (cadr args)))
						(interpret (hash-ref *label-table* (cadr args)))
						))) ;when lambda if

			(print	,(lambda (printable) 
					(for-each (lambda (x) (display (evaluate x)) (display " ")) printable)
					(newline)
				))
			(input	,(lambda (args) (..input args 0)))
							
			))

;Given a pointer to a line in the program list, adds all succeeding labels recursively
(define (build-label-table line*)
	(unless (or (null? line*) (null? (getLabel (car line*))))
		(hash-set! *label-table* (getLabel (car line*)) line*))
	(if (null? line*) '() (build-label-table (cdr line*)))
	)

(define (show label item)
        (newline)
        (display label) (display " = ") (display item)
        (newline))

;Given a line, will return the lineNumber
(define (getLineNumber list)
    (when (not (null? list))
        (if (null? (car list)) #f (caar list))))

;Given a line, will return the label, if there is a label, '() otherwise
(define (getLabel line)
    (cond [(null? line) '()]
          [(null? (cdr line)) '()]
          [else (if (symbol? (cadr line)) (cadr line) '())]))

;Given a line, will return the statement. If there is no statement, will return '()
(define (getStatement line)
	(cond [(null? line) '()]
          [(null? (cdr line)) '()]
          [(and (symbol? (cadr line)) (not (null? (cddr line)))) (caddr line)]
          [(not (symbol? (cadr line))) (cadr line)]
	))

(define (show-all-labels line*)
	(unless (or (null? line*) (null? (getLabel (car line*))))
		(show (getLabel (car line*)) line*))
	(if (null? line*) #f (show-all-labels (cdr line*))))


(define (interpret-statement statement)
	;(show "statement"statement)
	;(show statement (hash-ref *statement-table* (car statement)))
	;(show ((hash-ref *statement-table* (car statement)) (cdr statement)) (car statement))
	((hash-ref *statement-table* (car statement)) (cdr statement))
	)


(define (interpret lineHead)
	(if (or (null? lineHead) (not (pair? lineHead)))
		(exit 0)
		(let ((statement (getStatement(car lineHead))))
			(when (and (not (null? statement)) (not (void? statement)))	
				(interpret-statement statement)
			)
			(interpret (cdr lineHead))
		)		
	))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
            (program (readlist-from-inputfile sbprogfile)))
            ;(write-program-by-line sbprogfile program)
            (build-label-table program)

            ;(display *label-table*)
            (interpret program)
            )))

(main (vector->list (current-command-line-arguments)))
