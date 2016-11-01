#lang racket

; Ngoc N. Tran
; CSC-121-02
; Professor Turner

; FINAL PROJECT: SCHEME CALCULATOR
; BUGS: none by far
; POTENTIAL UPGRADE: all executed

; commented out codes are either obsolete after an update
; or test code used to print out useful information during debugging

; constant/semi-constant
(define toprint #f)       ; the string to output to file
(define expr #f)          ; processed char list of the expression
(define temp #f)          ; global temporary variable - not a good idea, not a bad one either
(define success #f)       ; if the operation is successful, i.e., is it alive?
(define errname "")       ; the error if something goes bad
(define input #f)         ; batch mode/interactive mode
(define output #f)        ; output mode on/off
(define ip #f)            ; input port
(define op #f)            ; output port
(define inf "in.ss")      ; input filename
(define outf "out.ss")    ; output filename

; char search, basically. assume that str is either a string or a list
(define (in? obj str)
  (if (string? str)
      (if (member obj (string->list str)) #t #f)
      (if (member obj str) #t #f)))

; read the expression. brackets error handling is in here.
; this maybe overkill, but I created this before finding out about (read-line)
(define (readexpr p)
  (set! temp 0)
  (let ((r (call/cc
            (lambda(break)
              (let f ()
                (let ((c (peek-char p)))
                  (if (eof-object? c)
                      (if (> temp 0) (begin (set! errname "unbalanced brackets. ") (break '())) '())
                      (begin (read-char p)
                             (if (or (in? c "1234567890().+-*/^%") (equal? c #\space))
                                 (cond
                                   [(equal? c #\() (set! temp (+ temp 1)) (cons c (f))]
                                   [(equal? c #\)) (if (= temp 0)
                                                       (begin (set! errname "unbalanced brackets. ") (break '()))
                                                       (begin (set! temp (- temp 1)) (cons c (f))))]
                                   [(equal? c #\space) (f)]
                                   [(in? c "1234567890.") (cons c (f))]
                                   [(in? c "+-*/%^")
                                    (cond
                                      [(equal? c #\+) (cons '+ (f))]
                                      [(equal? c #\-) (cons '- (f))]
                                      [(equal? c #\*) (cons '* (f))]
                                      [(equal? c #\/) (cons '/ (f))]
                                      [(equal? c #\^) (cons '^ (f))]
                                      [(equal? c #\%) (cons '% (f))])])
                                 (begin (set! errname "illegal characters inserted. ") (break '())))))))))))
    (if (= temp 0) r (begin (set! errname "unbalanced brackets. ") '()))))

; combine numeric character to full-fledged numbers
(define (numset ls lst)
  (if (null? ls)
      (if (null? lst) '() (list (string->number (list->string lst))))
      (begin (set! temp (car ls))
             (if (in? temp "1234567890.")
                 (numset (cdr ls) (append lst (list temp)))
                 (if (null? lst)
                     (cons temp (numset (cdr ls) '()))
                     (append (list (string->number (list->string lst)) temp) (numset (cdr ls) '())))))))

; chop the string into list of number and functions
(define (chop ls lst count)
  (if (null? ls)
      (if (null? lst) '() (list (chop lst '() 0)))
      (let ((p (car ls)) (q (cdr ls)))
        (if (not (in? p "()"))
            (if (= count 0)
                (if (null? lst)
                    (cons p (chop q '() 0))
                    (cons (chop lst '() 0) (cons p (chop q '() 0))))
                (chop q (append lst (list p)) count))
            (if (equal? p #\()
                (if (= count 0)
                    (if (null? lst)
                        (chop q '() (+ count 1))
                        (cons (chop lst '() 0) (chop q '() (+ count 1))))
                    (chop q (append lst (list p)) (+ count 1)))
                (if (= count 1)
                    (chop q lst (- count 1))
                    (chop q (append lst (list p)) (- count 1))))))))

; get the expr
(define (getExpr)
  ; '.' is not the only choice to quit. + - * / % ^ are all accepted.
  (if input 
      (set! toprint (read-line ip 'return-linefeed))
      (set! toprint (read-line)))
  (if (eof-object? toprint)
      (set! expr '(#\.))
      (set! expr (numset (readexpr (open-input-string toprint)) '()))))

;; evaluate the list
;; ONLY WORK WITH PROPER BINARY OPERATIONS - I.E. LIKE A BINARY TREE SORTA THING
;(define (evaluate ls)
;  (if (number? ls) ls
;      (case (length ls)
;        [(3) (if (and (or (number? (car ls)) (list? (car ls)))
;                      (or (number? (caddr ls)) (list? (caddr ls))))
;                 ((cadr ls) (evaluate (car ls)) (evaluate (caddr ls)))
;                 (set! success #f))]
;        [(1) (if (number? (car ls))
;                 (car ls)
;                 (if (list? (car ls)) (evaluate (car ls)) "game over"))]
;        [else (set! success #f)])))

(define (haslist? ls)
  (if (null? ls) #f
      (or (list? (car ls)) (haslist? (cdr ls)))))

(define (debracket ls)
  (if (list? ls)
      (if (not (haslist? ls)) (evaluate ls)
          (let reduce ((ls1 ls) (ls2 '()))
            (if (null? ls1) (debracket ls2)
                (reduce (cdr ls1) (append ls2 (list (debracket (car ls1))))))))
      ls))

(define (evaluate ls)
  (with-handlers ([exn:fail? (λ (e) (set! success #f)
                               (set! errname (exn-message e)))])
    (if (number? ls) ls
        (if (haslist? ls) (debracket ls)
            (if (= 1 (length ls))
                (if (number? (car ls)) (car ls) "game over")
                (eval-nols ls))))))

(define (qual ls b)
  (if (null? ls) #t
      (and (if b (number? (car ls)) (in? (car ls) (list '+ '- '* '/ '^ '%))) (qual (cdr ls) (not b)))))

(define (eval-nols ls)
  (if (= (length ls) 2)
      (if (and (in? (car ls) (list '+ '-)) (number? (cadr ls)))
          (eval-nols (cons 0 ls))
          (error
           ;'Error@Length2
           "syntax error. "))
      (if (qual ls #t)
          (let check^ ((ls (reverse ls)))
            ;(display "check^ ")(displayln (reverse ls))
            (match ls
              [(list head ... x '^ y tail ...) (check^ (append head (list (expt y x)) tail))]
              [_ (let checkrest ((ls (reverse ls)))
                   ;(display "checkrest ")(displayln ls)
                   (match ls
                     [(list head ... x '% y tail ...) (checkrest (append head (list (remainder x y)) tail))]
                     [(list head ... x '/ y tail ...) (checkrest (append head (list (/ x y)) tail))]
                     [(list head ... x '* y tail ...) (checkrest (append head (list (* x y)) tail))]
                     [(list head ... x '- y tail ...) (checkrest (append head (list (- x y)) tail))]
                     [(list head ... x '+ y tail ...) (checkrest (append head (list (+ x y)) tail))]
                     [(list x) x]
                     [else (error
                            ;'Error@PatternMatching
                            "syntax error. ")]))]))
          (error
           ;'Error@Evaluation
           "syntax error. "))))

; main method
(define (prompt)
  (set! success #t)
  (getExpr)
  (if (equal? expr '())
      (set! success #f)
      (set! temp (evaluate (chop expr '() 0))))
  (if (and input (not (eof-object? toprint))) (displayln toprint) (display ""))
  (if success
      (if (string? temp)
          (if input (display "End of file. ") (display "Escape sequence detected. "))
          (begin (display "= ")(display temp) (display "\n\n")))
      (begin (display "Error: ") (display errname) (display "Try again.\n\n")))
  (if output
      (begin (if success
                 (if (string? temp) (close-output-port op)
                     (begin (write-string toprint op)
                            (write-string "\n= " op)
                            (write temp op)
                            (write-string "\n\n" op)))
                 (begin (write-string toprint op)
                        (write-string "\n= " op)
                        (write-string "ERROR: " op)
                        (write-string errname op)
                        (write-string "\n\n" op))))
      (display ""))
  (if (equal? temp "game over") (display "Exited successfully.") (prompt)))

; ask for stuff
(define (ioprompt)
  (display "Enable output?:        ")
  (set! temp (read-line))
  (if (eof-object? temp)
      (display "EOF detected, output off.\n")
      (case temp
        [("T" "t" "True" "true" "#t" "1" "Y" "y" "yes" "Yes")
         (set! output #t) (display "Output mode on.\n")
         (display "Enter output filename: ")
         (set! temp (read-line))
         (if (eof-object? temp)
             (display "EOF detected, default file 'out.ss' set.\n")
             (set! outf temp))]
        [("F" "f" "False" "false" "#f" "0" "N" "n" "No" "no")
         (set! output #f) (display "Output mode off.\n")]
        [else (display "Error. Output mode off by default.\n")]))
  (display "Enable file input?:    ")
  (set! temp (read-line))
  (if (eof-object? temp)
      (display "EOF detected, batch mode off.\n")
      (case temp
        [("T" "t" "True" "true" "#t" "1" "Y" "y" "yes" "Yes")
         (set! input #t) (display "Batch mode on.\n")
         (display "Enter input filename:  ")
         (set! temp (read-line))
         (if (eof-object? temp)
             (display "EOF detected, default file 'in.ss' set.\n")
             (set! inf temp))]
        [("F" "f" "False" "false" "#f" "0" "N" "n" "No" "no")
         (set! input #f) (display "Interactive mode on.\n")]
        [else (display "Error. Interactive mode on by default.\n")])))

(define (start)
  (display "RACKET CALCULATOR v1.0\n")
  (ioprompt)
  ; should have implemented in ioprompt, but the code line is long enough already
  (if output 
      ; output mode on - open the file
      (with-handlers ([exn:fail? (λ (e)
                                   (display "Directory read-only or does not exist. Output disabled.\n")
                                   (set! output #f))])
        (set! op (open-output-file outf #:mode 'text #:exists 'replace)))
      ; output mode off -  do nothing
      (display ""))
  (if input
      ; file input mode on - open the file
      (with-handlers ([exn:fail? (λ (e)
                                   (display "File does not exist. Reverting to interactive input.\n")
                                   (set! input #f))])
        (set! ip (open-input-file inf)))
      ; file input mode off - do nothing
      (display ""))
  (if input
      ; batch mode
      (begin (display "Batch mode/file input activated successfully.\n")
             (display "Starting evaluating line-by-line.\n\n"))
      ; user prompt mode
      (begin (display "Enter a valid expression and hit Enter to evaluate or press EOF to exit.\n")
             (display "For example: (12 + 21) % (1 ^ (9 * (9 * 6)))\n\n")))
  (prompt))

(start)
