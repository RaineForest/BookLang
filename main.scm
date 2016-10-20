
(require (lib "yacc.ss" "parser-tools")
	 (lib "lex.ss" "parser-tools")
	 (prefix-in : parser-tools/lex-sre))

(require (lib "pretty.ss"))

(define-tokens value-tokens (PNAME PROPVAR PHI))

(define-empty-tokens op-tokens
   (OP
    CP
    NOT
    AND
    OR
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
  (pname (:: upper-letter (:+ letter)))
  (propvar (:+ letter))
  (phi (:: "\"" (:+ letter) "\""))
  (number (:+ digit)))

(define get-token
  (lexer
    ((eof) 'EOF)
    ("(" 'OP)
    (")" 'CP)
    ("!" 'NOT)
    ("&" 'AND)
    ("|" 'OR)
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


(define (or? a) (and (pair? a) (eq? (car a) 'OR)))

(define (make-or a b)
  (list 'OR a b))


(define (implies? a) (and (pair? a) (eq? (car a) 'IMPLIES)))

(define (make-implies a b)
  (list 'IMPLIES a b))


(define (speaksfor? a) (and (pair? a) (eq? (car a) 'SPEAKSFOR)))

(define (make-speaksfor a b)
  (list 'SPEAKSFOR a b))


(define (congruent? a) (and (pair? a) (eq? (car a) 'CONGRUENT)))

(define (make-congruent a b)
  (list 'CONGRUENT a b))


(define (says? a) (and (pair? a) (eq? (car a) 'SAYS)))

(define (make-says a b)
  (list 'SAYS a b))


(define (controls? a) (and (pair? a) (eq? (car a) 'CONTROLS)))

(define (make-controls a b)
  (list 'CONTROLS a b))

(define (reps? a) (and (pair? a) (eq? (car a) 'REPS)))

(define (make-reps a b c)
  (list 'REPS a b c))

;parser / grammar
(define parse-lang
  (parser
    (start form)
    (end EOF)
    (tokens value-tokens op-tokens)
    (error (lambda (a b c) (error 'parse-lang "error occured, ~v ~v ~v" a b c)))
    (grammar
      (form ((PROPVAR) ($1))
	    ((NOT form) (make-not $2))
	    ((form AND form) (make-and $1 $3))
	    ((form OR form) (make-or $1 $3))
	    ((form IMPLIES form) (make-implies $1 $3))
	    ((form CONGRUENT form) (make-congruent $1 $3))
	    ((princ SPEAKSFOR princ) (make-speaksfor $1 $3))
	    ((princ SAYS form) (make-says $1 $3))
	    ((princ CONTROLS form) (make-controls $1 $3))
	    ((princ REPS princ ON PHI) (make-reps $1 $3 $5)))
      (princ ((PNAME) ($1)) ;pname
	     ((princ AND princ) (make-and $1 $3))
	     ((princ OR princ) (make-or $1 $3))))))



