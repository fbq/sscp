(case-sensitive #t)
(optimize-level 2)

(load "match.ss")
(define driver
  (lambda (program)
    (with-output-to-file "t.s"
                         (lambda ()
                           (generate-x86-64 (verify-scheme program))))))

(define verify-scheme
  (lambda (program)
    (verify-program program)))

(define verify-program
  (lambda (Program)
    (match Program
           [(begin ,[verify-assignment -> FirstStatement] ,[verify-assignment -> Statement*] ...) 
            `(begin ,FirstStatement ,Statement* ...)]
           [(begin)
            (error 'Program "No statement in a program")]
           [,x (error 'Program "A program must start with (begin")])))


(define verify-assignment
  (lambda (Statement)
    (match Statement
           [(set! ,Var1 (,Binop ,Var1 ,Var2))
            `(set! ,(verify-var Var1) (,(verify-binop Binop) ,Var1 ,(verify-var-or-int32 Var2)))] 
           [(set! ,Var1 ,Var2)
            (guard (atom? Var2))
            `(set! ,(verify-var Var1) ,(verify-var-or-int64 Var2))]
           [,x (error 'Statement "Not a well formed assignment" x)])))


(define verify-var
  (lambda (Var)
    (if (Var? Var)
      Var
      (error 'Var "Not an acceptable Var" Var))))

(define verify-var-or-int64
  (lambda (Int64)
    (if (or (Int64? Int64) (Var? Int64))
      Int64
      (error 'Int64 "Not an acceptable Var or Int64" Int64))))

(define verify-var-or-int32
  (lambda (Int32)
    (if (or (Int32? Int32) (Var? Int32))
      Int32
      (error 'Int32 "Not an acceptable Var or Int32" Int32))))

(define verify-int64
  (lambda (Int64)
    (if (Int64? Int64)
      Int64
      (error 'Int64 "Not an acceptable Int64" Int64))))

(define verify-int32
  (lambda (Int32)
    (if (Int32? Int32)
      Int32
      (error 'Int32 "Not an acceptable Int32" Int32))))

(define verify-binop
  (lambda (Binop)
    (if (Binop? Binop)
      Binop
      (error 'Binop "Not an acceptable Binop" Binop))))

(define Var?
  (lambda (Var)
    (let
      ([var-list
         '(rax
           rcx
           rdx
           rbx
           rbp
           rsi
           rdi
           r8
           r9
           r10
           r11
           r12
           r13
           r14
           r15)])
      (memq Var var-list))))

(define Int64?
  (lambda (Int64)
    (and (integer? Int64)
         (and (<= Int64 (- (expt 2 63) 1))
              (>= Int64 (- (expt 2 63)))))))

(define Int32?
  (lambda (Int32)
    (and (integer? Int32)
         (and (<= Int32 (- (expt 2 31) 1))
              (>= Int32 (- (expt 2 31)))))))

(define Binop?
  (lambda (Binop)
    (let
      ([binop-list '(+ - *)])
      (memq Binop binop-list))))

(define generate-x86-64
  (lambda (Program)
    (define program
      (lambda (Program)
        (match Program
               [(begin ,Statement* ...)
                (label 'begin)
                (statements Statement*)
                ])))
    (define label
      (lambda (Label)
        (match Label
               [begin (printf ".globl _scheme_entry~%_scheme_entry:~%")])))
    (define statements
      (lambda (Statements)
        (cond
          ((null? Statements)
           (printf "ret~%"))
          (else
            (set!-statement (car Statements))
            (statements (cdr Statements))))))
    (define set!-statement
      (lambda (Set!-Statement)
        (match Set!-Statement
               [(set! ,Var1 ,Int64)
                (guard (Int64? Int64))
                (printf "movq $~s, %~s ~%" Int64 Var1)]
               [(set! ,Var1 ,Var2)
                (guard (atom? Var2))
                (printf "movq %~s, %~s ~%" Var2 Var1)]
               [(set! ,Var1 (,Binop ,Var1 ,Int32))
                (guard (Int32? Int32))
                (printf "~s $~s, %~s ~%" (binop Binop) Int32 Var1)]
               [(set! ,Var1 (,Binop ,Var1 ,Var2))
                (printf "~s %~s, %~s ~%" (binop Binop) Var2 Var1)])))
    (define binop
      (lambda (x)
        (cond
          ((eq? x '+) 'addq)
          ((eq? x '-) 'subq)
          ((eq? x '*) 'imulq))))
    (program Program)))
