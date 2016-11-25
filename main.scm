
#lang racket

(require (lib "yacc.ss" "parser-tools")
	 (lib "lex.ss" "parser-tools")
	 (prefix-in : parser-tools/lex-sre))
(require (lib "pretty.ss"))
(require racket/bool)
(require racket/set)

(define-tokens value-tokens (PNAME PROPVAR PHI))

(define-empty-tokens op-tokens
   (OP
    CP
    NOT
    AND
    CONJUNCTION
    OR
    QUOTING
    IMPLIES
    SPEAKSFOR
    CONGRUENT
    SAYS
    CONTROLS
    REPS
    ON
    TRUE
    FALSE
    EOF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ "A" "Z"))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  (pname (:: upper-letter (:* letter)))
  (propvar (:: lower-letter (:* letter)))
  (phi (:: "\"" (:+ letter) "\""))
  (number (:+ digit)))

(define get-token
  (lexer
    ((eof) 'EOF)
    ("(" 'OP)
    (")" 'CP)
    ("!" 'NOT)
    ("&" 'CONJUNCTION)
    ("^" 'AND)
    ("|" 'QUOTING)
    ("||" 'OR)
    ("->" 'IMPLIES)
    ("=>" 'SPEAKSFOR)
    ("==" 'CONGRUENT)
    ("says" 'SAYS)
    ("controls" 'CONTROLS)
    ("reps" 'REPS)
    ("on" 'ON)
    ("true" 'TRUE)
    ("false" 'FALSE)
    (pname (token-PNAME (string->symbol lexeme)))
    (propvar (token-PROPVAR (string->symbol lexeme)))
    (phi (token-PHI (string->symbol lexeme)))
    (whitespace (get-token input-port))))

; Identifiers and helpers here

(define ident? symbol?)

(define (not? a) (and (pair? a) (eq? (car a) 'NOT)))
;(eq? (not? '(NOT a)) #t)
;(eq? (not? '3) #f)

(define (make-not expr)
  (list 'NOT expr))
;(equal? (make-not 2) '(NOT 2))


(define (and? a) (and (pair? a) (eq? (car a) 'AND)))

(define (make-and a b)
  (list 'AND a b))

(define (conjunction? a) (and (pair? a) (eq? (car a) 'CONJUNCTION)))

(define (make-conjunction a b)
  (list 'CONJUNCTION a b))

(define (or? a) (and (pair? a) (eq? (car a) 'OR)))

(define (make-or a b)
  (list 'OR a b))

(define (quoting? a) (and (pair? a) (eq? (car a) 'QUOTING)))

(define (make-quoting a b)
  (list 'QUOTING a b))

;(define (implies? a) (and (pair? a) (eq? (car a) 'IMPLIES)))

(define (make-implies a b)
  (make-or (make-not a) b))

(define (speaksfor? a) (and (pair? a) (eq? (car a) 'SPEAKSFOR)))

(define (make-speaksfor a b)
  (list 'SPEAKSFOR a b))

;(define (congruent? a) (and (pair? a) (eq? (car a) 'CONGRUENT)))

(define (make-congruent a b)
  (make-or (make-and a b) (make-and (make-not a) (make-not b))))

(define (says? a) (and (pair? a) (eq? (car a) 'SAYS)))

(define (make-says a b)
  (list 'SAYS a b))

(define (pname? a) (and (pair? a) (eq? (car a) 'PNAME)))

(define (make-pname a) (list 'PNAME a))

(define (propvar? a) (and (pair? a) (eq? (car a) 'PROPVAR)))

(define (make-propvar a) (list 'PROPVAR a))

;(define (controls? a) (and (pair? a) (eq? (car a) 'CONTROLS)))

(define (make-controls a b)
  (make-implies (make-says a b) b))

;(define (reps? a) (and (pair? a) (eq? (car a) 'REPS)))

(define (make-reps a b c)
  (make-implies (make-says (make-quoting a b) c) (make-says b c)))

;parser / grammar
(define parse-lang
  (parser
    (start form)
    (end EOF)
    (tokens value-tokens op-tokens)
    (error (lambda (a b c) (error 'parse-lang "error occured, ~v ~v ~v" a b c)))
    (grammar
      (form ((comp) $1)
	    ((princ SPEAKSFOR princ) (make-speaksfor $1 $3)))
      (comp ((impform) $1)
	    ((comp CONGRUENT impform) (make-congruent $1 $3)))
      (impform ((orform) $1)
	       ((impform IMPLIES orform) (make-implies $1 $3)))
      (orform ((andform) $1)
	      ((orform OR andform) (make-or $1 $3)))
      (andform ((acform) $1)
	       ((andform AND acform) (make-and $1 $3)))
      (acform ((negform) $1)
	      ((princ SAYS negform) (make-says $1 $3))
	      ((princ CONTROLS negform) (make-controls $1 $3))
	      ((princ REPS princ ON negform) (make-reps $1 $3 $5)))
      (negform ((NOT negform) (make-not $2))
	       ((simple) $1))
      (simple ((PROPVAR) (make-propvar $1)) 
	      ((TRUE) #t) 
	      ((FALSE) #f)
	      ((OP form CP) $2))
      (princ ((simprinc) $1)
	     ((simprinc CONJUNCTION princ) (make-conjunction $1 $3))
	     ((simprinc QUOTING princ) (make-or $1 $3)))
      (simprinc ((PNAME) (make-pname $1))
		((OP simprinc CP) $2)))))

;(define (to-cnf expr)
;  (cond ((boolean? expr) expr)
;	((pname? expr) expr)
;	((propvar? expr) expr)
;	((and? expr)
;	 (cond ((not (and? (cadr expr))) (to-cnf (make-and (to-cnf (cadr expr)) (caddr expr))))
;	       ((not (and? (caddr expr))) (to-cnf (make-and (cadr expr) (to-cnf (caddr expr)))))
;	       (else (make-and (cadr expr) (caddr expr)))))
;	((or? expr)
;	 (

(define (check-statement str)
  (let* ((i (open-input-string str)))
    (parse-lang (lambda () (get-token i)))))

;(let* ((example "avar ^ Aprinc says bvar")
;       (i (open-input-string example)))
;  (equal? (parse-lang (lambda () (get-token i))) '(AND avar (SAYS Aprinc bvar))))


(define (init-kb) (list
		    ;(make-congruent a (make-says b a))
		    ;(make-congruent (make-and a (make-implies a b)) b)
		    (make-implies (make-implies (make-says 'p (make-implies 'a 'b)) (make-implies (make-says 'p 'a) (make-says 'p 'b))) #t)
		    (make-implies (make-implies (make-speaksfor 'p 'q) (make-implies (make-says 'p 'a) (make-says 'q 'a))) #t)
		    (make-implies (make-congruent (make-says (make-and 'p 'q) 'a) (make-and (make-says 'p 'a) (make-says 'q 'a))) #t)
		    (make-implies (make-congruent (make-says (make-quoting 'p 'q) 'a) (make-says 'p (make-says 'q 'a))) #t)
		    (make-implies (make-speaksfor 'p 'p) #t)))

(define (insert-statement kb new)
  (cons new kb))

(define init-k (lambda (v) v))

(define (find-vars expr)
  (cond ((pname? expr) (list expr))
	((boolean? expr) (list expr))
	((propvar? expr) (list expr))
	(else (foldl (lambda (x y) (set-union (find-vars x) y)) '() (cdr expr)))))

;(define (substitute exp1 exp2)

(define (unify expr k)
  (cond ((boolean? expr) (k expr))
	((pname? expr) (k expr))
	((conjunction? expr) (k expr)) ;pass it through
	((quoting? expr) (k expr)) ;pass it through
	((says? expr) (k expr)) ;pass it through
	((speaksfor? expr) (k expr)) ;pass it through
	((propvar? expr) (k expr)) ;pass it through, used in substitution
	((not? expr)
	 (unify (cadr expr)
		  (lambda (v1)
		    (k (not v1)))))
	((and? expr)
	 (unify (cadr expr)
		  (lambda (v1)
		    (unify (caddr expr)
			     (lambda (v2)
			       (k (and v1 v2)))))))
	((or? expr)
	 (unify (cadr expr)
		  (lambda (v1)
		    (unify (caddr expr)
			     (lambda (v2)
			       (k (or v1 v2)))))))
	(else (error 'unify "Unknown expr"))))

(define (query kb q) 
  (insert-statement kb (check-statement q)))

(define (access-check filename q)
    (query 
      (foldl (lambda (x y) (insert-statement y (check-statement x))) 
	     (init-kb)
	     (file->lines filename))
      q))

