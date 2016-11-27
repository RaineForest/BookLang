;#! /usr/bin/env racket
;#lang racket

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
    (pname (token-PNAME (string->symbol lexeme)))
    (propvar (token-PROPVAR (string->symbol lexeme)))
    (phi (token-PHI (string->symbol lexeme)))
    (whitespace (get-token input-port))))

; Identifiers and helpers here

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
  (make-controls a (make-says b c)))

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
	      ((OP form CP) $2))
      (princ ((simprinc) $1)
	     ((simprinc CONJUNCTION princ) (make-conjunction $1 $3))
	     ((simprinc QUOTING princ) (make-quoting $1 $3)))
      (simprinc ((PNAME) (make-pname $1))
		((OP simprinc CP) $2)))))

(define (to-cnf expr)
  (cond ((boolean? expr) expr)
	((pname? expr) expr)
	((propvar? expr) expr)
	((says? expr) 
	 (cond ((quoting? (cadr expr)) ;quoting expansion
		(make-says (cadr (cadr expr)) (to-cnf (make-says (caddr (cadr expr)) (to-cnf (caddr expr))))))
	       ((conjunction? (cadr expr)) ;conjunction expansion
		(make-and (make-says (cadr (cadr expr)) (to-cnf (caddr expr))) 
			  (to-cnf (make-says (caddr (cadr expr)) (to-cnf (caddr expr))))))
	       (else (make-says (cadr expr) (to-cnf (caddr expr)))))) ;make sure the inner statement is cnf
	((speaksfor? expr) (expr))
	((quoting? expr) expr)
	((conjunction? expr) expr)
	((and? expr) ;all is right with the world, recurse
	 (make-and (to-cnf (cadr expr)) (to-cnf (caddr expr))))
	((or? expr)
	 (cond ((and (and? (cadr expr)) (and? (caddr expr))) ;foil
		(make-and (make-or (to-cnf (cadr (cadr expr))) (to-cnf (caddr (caddr expr)))) 
			  (make-or (to-cnf (caddr (cadr expr))) (to-cnf (cadr (caddr expr))))))
	       ((and? (cadr expr)) ;distribution
		(make-and (make-or (to-cnf (cadr (cadr expr))) (to-cnf (caddr expr))) 
			  (make-or (to-cnf (caddr (cadr expr))) (to-cnf (caddr expr)))))
	       ((and? (caddr expr)) ;distribution
		(make-and (make-or (to-cnf (cadr (caddr expr))) (to-cnf (cadr expr)))
			  (make-or (to-cnf (caddr (caddr expr))) (to-cnf (cadr expr)))))
	       (else (make-or (to-cnf (cadr expr)) (to-cnf (caddr expr)))))) ;all is right with the world; recurse
	((not? expr)
	 (cond ((and? (cadr expr)) ;demorgans
		(make-or (to-cnf (make-not (cadr (cadr expr)))) (to-cnf (make-not (caddr (cadr expr))))))
	       ((or? (cadr expr)) ;demorgans
		(make-and (to-cnf (make-not (cadr (cadr expr)))) (to-cnf (make-not (caddr (cadr expr))))))
	       ((not? (cadr expr)) ;double negation
		(to-cnf (cadr (cadr expr))))
	       (else (make-not (cadr expr))))))) ;all is right with the world; recurse


(define (check-statement str)
  (let* ((i (open-input-string str)))
    (parse-lang (lambda () (get-token i)))))

;(let* ((example "avar ^ Aprinc says bvar")
;       (i (open-input-string example)))
;  (equal? (parse-lang (lambda () (get-token i))) '(AND avar (SAYS Aprinc bvar))))


;initial knowledge base
(define (init-kb) '())

;find statements separated by ands, so we can split them and insert them as separate statements
(define (find-clauses expr)
  (cond ((pname? expr) (list expr))
	((boolean? expr) (list expr))
	((propvar? expr) (list expr))
	((or? expr) (list expr))
	((not? expr) (list expr))
	((says? expr) (list expr))
	((speaksfor? expr) (list expr))
	((quoting? expr) (list expr))
	((conjunction? expr) (list expr))
	(else (foldl (lambda (x y) (set-union (find-clauses x) y)) '() (cdr expr)))))

;find literals in a clause so we can eliminate opposing statements
(define (find-literals expr)
  (if (or? expr) 
    (foldl (lambda (x y) (set-union (find-literals x) y)) '() (cdr expr))
    (list expr)))

;add a statement to the knowledge base, set-union for no dups
(define (insert-statement kb new)
  (set-union (find-clauses (to-cnf new)) kb))

;do the reduction
(define (kb-literal-elim kb-ltrls accum)
  (if (empty? kb-ltrls)
    accum
    (let* ((stmt (car kb-ltrls))
	   (notstmt (to-cnf (make-not stmt)))
	   (removed (set-intersect (list notstmt) (cdr kb-ltrls))))
      (if (empty? removed)
	(kb-literal-elim (cdr kb-ltrls) (cons stmt accum))
	(kb-literal-elim (set-subtract (cdr kb-ltrls) removed) accum)))))

;get the all literals in the knowledge base
(define (kb-literals kb accum)
  (if (empty? kb)
    accum
    (kb-literals (cdr kb) (set-union accum (find-literals (car kb))))))

;set up the resolution
(define (search-kb kb query mygoal)
  (if (not (says? query))
    (error 'search-kb "Query must be a says statement! I'm not letting you inject your own facts!")
    (let ((newkb (insert-statement (insert-statement kb (to-cnf query)) (to-cnf (make-not mygoal)))))
      (empty? (set-intersect (list query)
			     (kb-literal-elim (kb-literals newkb '()) '()))))))

;main function
(define (access-check filename user permission)
  (if (search-kb 
	(foldr (lambda (x y) (insert-statement y (check-statement x))) 
	       (init-kb)
	       (file->lines filename))
	(check-statement (string-append user " says " permission))
	(check-statement permission))
    (display "Granted")
    (display "Denied")))

;(access-check "ex5-1test" "Carla" "execfoo")
;(access-check "ex7-4" "Dawn | Beth" "acceptRafflePrize")

