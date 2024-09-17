; ba compiler

; arrays
(define constants '())
(define globals '())

(define org 32768)

(define mem (make-bytevector 65536))
(define nmem 0)

(define (in? l el)
  (find (lambda (x) (eq? el x)) l))

(define (string-in? l el)
  (find (lambda (x) (string=? el x)) l))

(define (index l el)
  (let index ((l l) (el el) (i 0))
    (if (null? l) -1
      (if (eq? (car l) el) i
        (index (cdr l) el (+ i 1))))))

(define delim (string->list "!\"$%^&*()-+=[]{};:'~<>,.?/|"))

(define joins
  (append
    (map (lambda (x) (string-append (string x) (string x)))
      (string->list "&|<>=/+-"))
    (map (lambda (x) (string-append x "="))
      '("+" "-" "*" "/" "%" "/" "%" "&" "|" "^" "<<" ">>" "<" ">" "!" "="))
    '("/*" "*/" "$[")))

(define singles (string->list "(){}];:?~"))

(define (oct-digit c)
  (let ((c (char->integer c)))
    (if (<= 48 c 55) (- c 48) #f)))

(define (hex-digit c)
  (let ((c (char->integer c)))
    (cond
      ((<= 48 c 58) (- c 48))
      ((<= 64 c 70) (- c 54))
      ((<= 97 c 102) (- c 87))
      (#t #f))))

(define (read-escape p)
  (let ((c (read-char p)))
    (cond
      ((eq? c #\") #\")
      ((eq? c #\') #\')
      ((eq? c #\\) #\\)
      ((eq? c #\n) #\newline)
      ((eq? c #\t) #\tab)
      ((eq? c #\e) (integer->char 0))
      ((eq? c #\x)
       (let ((c1 (hex-digit (read-char p))) (c2 (hex-digit (read-char p))))
         (if (and c1 c2) (integer->char (+ (* 256 c1) c2))
           (error 'read-escape "invalid hex escape sequence"))))
      ((oct-digit c)
       (let ((c2 (oct-digit (read-char p))) (c3 (oct-digit (read-char p))))
         (if (and c2 c3) (integer->char (+ (* c 64) (* c2 8) c3))
           (error 'read-escape "invalid octal escape sequence"))))
      (#t (error 'read-escape "invalid escape char")))))

(define (read-string p t)
  (let read ((s '()) (c (read-char p)))
    (cond
      ((eof-object? c) (error 'read-string "unterminated string"))
      ((eq? c t) (list->string (reverse s)))
      (#t
       (read
         (cons (if (eq? c #\\) (read-escape p) c) s)
         (read-char p))))))

(define (read-squote p)
  (let ((s (read-string p #\')))
    (if (= 1 (string-length s))
      (char->integer (string-ref s 0))
      (error 'read-squote "invalid character"))))

(define (dec-digit c)
  (let ((c (char->integer c)))
    (if (<= 48 c 57) (- c 48) #f)))

(define (number s)
  (let
    ((number
      (lambda (s digit b)
        (let num ((n 0) (s (cdr s)) (d (digit (car s))))
          (if d
            (let ((n (+ (* n b) d)))
              (if (null? s) n
                (num n (cdr s) (digit (car s)))))
            #f)))))
    (cond
      ((and (>= (string-length s) 3) (eq? (string-ref s 0) #\0)
            (eq? (string-ref s 1) #\x))
       (number (cddr (string->list s)) hex-digit 16))
      ((eq? (string-ref s 0) #\0) (number (string->list s) oct-digit 8))
      (#t (number (string->list s) dec-digit 10)))))

(define (cons-token s t)
  (let ((n (number s)))
    (cons (if n n (string->symbol s)) t)))

(define (read-tokens-0 p)
  (let read ((t '()) (s "") (c (read-char p)) (dep 0))
    (cond
      ((eof-object? c)
       (reverse (if (zero? (string-length s)) t (cons-token s t))))
      ((in? delim c)
       (if (zero? (string-length s))
         (cond 
           ((or (and (= dep 0) (eq? c #\;)) (and (= dep 1) (eq? c #\})))
            (reverse (cons (string->symbol (string c)) t)))
           ((eq? c #\")
            (let ((s (read-string p #\")))
              (read (cons s t) "" (read-char p) dep)))
           ((eq? c #\')
            (let ((c (read-squote c)))
              (read (cons c t) "" (read-char p) dep)))
           ((in? singles c)
            (read (cons (string->symbol (string c)) t) "" (read-char p)
              (+ dep (cond ((eq? c #\{) 1) ((eq? c #\}) -1) (#t 0)))))
           (#t
             (let dl ((s (string c)) (c (read-char p)))
               (let ((in (string-in? joins (string-append s (string c)))))
                 (cond
                   (in (dl in (read-char p)))
                   ((string=? s "/*")
                    (let cm ((c (read-char p)))
                      (if (eq? c #\*)
                        (let ((c (read-char p)))
                          (if (eq? c #\/)
                            (read t "" (read-char p) dep)
                            (cm c)))
                        (if (eof-object? c)
                          (err "unterminated comment")
                          (cm (read-char p))))))
                   ((string=? s "//")
                    (let cm ((c (read-char p)))
                      (if (or (eof-object? c) (eq? c #\newline))
                        (read t "" (read-char p) dep)
                        (cm (read-char p)))))
                   (#t (read (cons (string->symbol s) t) "" c dep)))))))
         (read (cons-token s t) "" c dep)))
      ((<= (char->integer c) 32)
       (read (if (zero? (string-length s)) t (cons-token s t))
         "" (read-char p) dep))
      (#t (read t (string-append s (string c)) (read-char p) dep)))))

(define (read-tokens p)
  (let read ((d '()) (s (read-tokens-0 p)))
    (if (null? s) (reverse d)
      (let ((c (assoc (car s) constants)))
        (read
          (if c (append (cdr c) d) (cons (car s) d))
          (cdr s))))))

(define (match? t m)
  (let ((m (map
             (lambda (x)
               ((lambda (x) (string->symbol x))
                (if (eq? x #\$) "$[" (string x))))
             (string->list (string-append m "/")))))
    (let match ((s (car m)) (t t) (m (cdr m)))
      (cond
        ((eq? s '\/) (null? t))
        ((null? t) #f)
        ((eq? s '\~) #t)
        ((eq? s '\.) (if (null? m) #t (match (car m) (cdr t) (cdr m))))
        ((eq? s '\*)
         (if (eq? (car t) (car m))
           #f
           (match #\* (cdr t) m)))
        ((eq? s #\*)
         (if (eq? (car t) (car m))
           (match (car m) t (cdr m))
           (match #\* (cdr t) m)))
        ((eq? s (car t))
         (if (null? m) #t (match (car m) (cdr t) (cdr m))))
        (#t #f)))))

(define err-context 'test)

(define (to-string s)
  (cond
    ((symbol? s) (symbol->string s))
    ((number? s) (number->string s))
    ((char? s) (string s))
    (#t s)))

(define (err . x)
  (let err ((s "") (x x))
    (if (null? x)
      (error err-context s)
      (err (string-append s (to-string (car x))) (cdr x)))))

(define (expect a b)
  (unless (eq? a b)
    (err "expected " b ", got " a)))

(define (expect-length l ln)
  (unless (>= (length l) ln)
    (err "syntax error")))

(define (ex t all fall)
  ;(unless (null? all)
    ;(display t) (display " ") (display (car all)) (newline))
  (if (null? all)
    (fall t)
    (let ((t (ex t (cdr all) fall)))
      (if (and (>= (length t) 3) (in? (car all) (cadr t)))
        (let ((d (ex (cddr t) all fall)))
          (cons (list (cadr t) (car t) (car d)) (cdr d)))
        t))))

(define (ex-post t)
  ;(display t) (newline)
  (cond
    ((null? (cdr t)) t)
    ((in? '(++ --) (cadr t))
     ;(cons (cadr t) (ex-post (cons (car t) (cddr t)))))
     (ex-post (cons (list (cadr t) (car t)) (cddr t))))
    ((in? '(\[ $\[) (cadr t))
     (let ((b (cadr t)) (nm (car t)) (t (expression (cddr t))))
       ;(display (list b nm t)) (newline)
       (if (and (>= (length t) 2) (eq? (cadr t) '\]))
         (ex-post (cons (list b nm (car t)) (cddr t)))
         (err "expected closing ]"))))
    ((eq? (car t) '\()
     (let ((t (expression (cdr t))))
       (if (and (>= (length t) 2) (eq? (cadr t) '\)))
         (ex-post (cons (car t) (cddr t)))
         (err "expected closing )"))))
    ((eq? (cadr t) '\()
     (let ((f (car t)) (t (expression (cddr t))))
       (if (and (>= (length t) 2) (eq? (cadr t) '\)))
         (cons (cons 'call (cons f (comma-list (car t)))) (cddr t))
         (err "expected closing )"))))
    (#t t)))

(define (ex-pre t)
  (cond
    ((in? '(++ -- + - * &) (car t))
     (let ((d (ex-pre (cdr t))))
       (cons (list
               (string->symbol (string-append (symbol->string (car t)) "x"))
               (car d))
         (cdr d))))
    ((in? '(! ~) (car t))
     (let ((d (ex-pre (cdr t))))
       (cons (list (car t) (car d)) (cdr d))))
    (#t (ex-post t))))

(define (ex-tern t)
  (let ((t (ex t '((\,) (\|\|) (&&) (\|) (^) (&) (== !=) (< <= > >=)
                    (<< >>) (+ -) (* / %)) ex-pre)))
    ;(display t) (display " ") (display '(?)) (newline)
    (if (and (>= (length t) 5) (eq? (cadr t) '\?))
      (let ((a (car t)) (d (ex-tern (cddr t))))
        (expect (cadr d) '\:)
        (list (cons '\? (cons (car t) (cons (car d) (ex-tern (cddr d)))))))
      t)))

(define (expression t)
  (ex t '((\,) (= += -= *= /= %= <<= >>= &= ^= \|=)) ex-tern))

(define (expression-1 t)
  (let ((t (expression t)))
    (if (= (length t) 1)
      (car t)
      (begin (display t) (newline) (err "invalid operator syntax")))))

(define (eval-expression t)
  (if (atom? t)
    t
    (let ((t (cons (car t) (map eval-expression (cdr t)))))
      (let ((bi (assoc (car t)
                  `((+ ,+) (- ,-) (* ,*) (/ ,(lambda (x y) (floor (/ x y))))
                    (% ,mod) (& ,bitwise-and) (\| ,bitwise-ior)
                    (^ ,bitwise-xor)
                    (&& ,(lambda (x y) (if (and x y) 1 0)))
                    (\|\| ,(lambda (x y) (if (or x y) 1 0))))))
            (un (assoc (car t)
                  `((+x ,(lambda (x) x)) (-x ,-) (~ ,bitwise-not)
                    (! ,(lambda (x) (if x 0 1)))))))
        (cond
          (bi
           (if (and (number? (cadr t)) (number? (caddr t)))
             (apply (cadr bi) (cdr t))
             t))
          (un
           (if (number? (cadr t))
             ((cadr un) (cadr t))
             t))
          (#t t))))))

(define (comma-list t)
  (if (and (list? t) (not (eq? (car t) '\,))) t
    (let comma-list ((t t))
      (if (atom? t) (list t)
        (if (eq? (car t) '\,)
          (cons (cadr t) (comma-list (caddr t)))
          (list t))))))

(define (expression+semi t)
  (let ((t (expression t)))
    (expect-length t 2)
    (expect (cadr t) '\;)
    (eval-expression (car t))))

(define (drop-last t)
  (reverse (cdr (reverse t))))

(define (ast-dim t)
  (let ((t (expression t)))
    (expect-length t 2)
    (expect (cadr t) '\;)
    (cons 'dim (eval-expression (cdar t)))))

(define (ast-dimc t)
  (cons 'dimc (cdr (ast-dim t))))

(define (ast-var t)
  (list 'var (car t) (expression+semi (cdr t))))

(define (ast-arr t)
  (cons 'arr
    (cons (car t)
      (comma-list (expression+semi (cdddr t))))))

(define (ast-arrc t)
  (cons 'arrc (cdr (ast-arr t))))

(define (after l el)
  (if (null? l)
    (err "expected " el)
    (if (eq? (car l) el)
      (cdr l)
      (after (cdr l) el))))

(define (ast-code t)
  ;(display t) (newline)
  (case (car t)
    ('{
     (let ast ((body '()) (t (cdr t)))
       (if (eq? (car t) '})
         (cons (cons 'body (reverse body)) (cdr t))
         (let ((t (ast-code t)))
           (ast (cons (car t) body) (cdr t))))))
    ('auto
     (let ((t (expression (cdr t))))
       (expect (cadr t) '\;)
       (cons (cons 'auto (comma-list (eval-expression (car t)))) (cddr t))))
    ('static
     (let* ((old err-context) (t (cons (ast t) (after t '\;))))
       (set! err-context old)
       t))
    ('if
     (expect (cadr t) '\()
     (let ((t (expression (cddr t))))
       (expect (cadr t) '\))
       (let ((x (eval-expression (car t))) (t (ast-code (cddr t))))
         (if (eq? (cadr t) 'else)
           (let ((d (ast-code (cddr t))))
             (cons (list 'if-else x (car t) (car d)) (cdr d)))
           (cons (list 'if x (car t)) (cdr t))))))
    ('while
     (expect (cadr t) '\()
     (let ((t (expression (cddr t))))
       (expect (cadr t) '\))
       (let ((x (eval-expression (car t))) (t (ast-code (cddr t))))
         (cons (list 'while x (car t)) (cdr t)))))
    ('\; (cons '(-x 69) (cdr t)))
    (else
     (let ((t (expression t)))
       (expect (cadr t) '\;)
       (let ((x (eval-expression (car t))))
         (if (atom? x)
           (cons '(-x 69) (cddr t))
           (cons x (cddr t))))))))

(define (ast-fun t)
  (let ((nm (car t)) (t (expression (cddr t))))
    (expect (cadr t) '\))
    (let ((t (cons nm (cons (list (car t)) (ast-code (cddr t))))))
      (expect-length t 2)
      (cons 'fun
        (if (eq? (caadr t) '*nil)
          (cons (car t) (cons '() (cddr t)))
          t)))))

(define (ast-const t)
  (cons 'const (cons (car t) (drop-last (cddr t)))))

(define (ast t) ;(display t) (newline)
  (set! err-context (car t))
  (unless (null? t)
    (cond
      ((eq? (car t) 'static) (cons 'static (ast (cdr t))))
      ((eq? (car t) 'include) (drop-last t))
      ((match? t ".;") (list 'dimvar (car t)))
      ((match? t ".[*];") (ast-dim t))
      ((match? t ".$*];") (ast-dimc t))
      ((match? t ".[]~") (ast-arr t))
      ((match? t ".$]~") (ast-arrc t))
      ((match? t ".(*){~") (ast-fun t))
      ((match? t ".(){~")
       (ast-fun (cons (car t) (cons '\( (cons '*nil (cddr t))))))
      ((match? t ".=~") (ast-const t))
      (#t (ast-var t)))))

(define (db b)
  (bytevector-u8-set! mem nmem b)
  (set! nmem (+ nmem 1)))

(define (dh h)
  (bytevector-u16-set! mem nmem h 'little)
  (set! nmem (+ nmem 2)))

(define (dw w)
  (bytevector-u32-set! mem nmem w 'little)
  (set! nmem (+ nmem 4)))

(define (get-auto t)
  (if (eq? (car t) 'body)
    (let get ((a '()) (t (cdr t)))
      (if (eq? (caar t) 'auto)
        (get (append a (cdar t)) (cdr t))
        (cons a (cons 'body t))))
    (cons '() t)))

(define (set-jump! m j l)
  (let ((o (- l m 2)))
    (bytevector-u8-set! mem m
      (+ j (bitwise-and (bitwise-arithmetic-shift-right o 8) #x1f)))
    (bytevector-u8-set! mem (+ m 1) (bitwise-and o #xff))))

(define (compile-number n)
  (if (<= -16 n 15)
    (db (+ #x40 (bitwise-and n #x1f)))
    (if (<= -128 n 127)
      (begin (db #xe0) (db n))
      (if (<= -32768 n 32767)
        (begin (db #xe1) (dh n))
        (begin (db #xe2) (dw n))))))

(define (dglobal op g)
  (let ((gi (index globals g)))
    (let ((g (if (= gi -1) (length globals) gi)))
      (db (+ op (bitwise-and (floor (/ g 256)) #x1f)))
      (db g))
    (when (= gi -1)
      (set! globals (append globals (list (cons g #f)))))))

(define (ready-lval t a l)
  (cond
    ((number? t) (compile-number n))
    ((atom? t)
     (let* ((ai (index a t)) (li (index l t))
           (gi (if (= ai li) (index globals (car t)) -1)))
       (cond
        ((not (= ai -1)) (db (+ #x60 (bitwise-and (- -3 ai) #x1f))))
        ((not (= li -1)) (db (+ #x60 li)))
        (#t (dglobal #x80 g)))))
    ((eq? (car t) '\[)
     (compile-ex (cadr t) a l #t)
     (compile-ex (caddr t) a l #t)
     (db #xed))
    ((eq? (car t) '$\[)
     (compile-ex (cadr t) a l #t)
     (compile-ex (caddr t) a l #t)
     (db #xec))
    (#t (compile-ex t a l #t))))

(define (byte/word t b w)
  (if (and (list? t) (eq? (car t) '$\[)) b w))

(define (compile-load t)
  (db (byte/word t #xfd #xfb)))

(define (compile-store t)
  (db (byte/word t #xfc #xfa)))

(define (global? g a l)
  (cond
    ((list? g) #f)
    ((in? a g) #f)
    ((in? l g) #f)
    (#t #t)))

(define (compile-ex t a l keep)
  (let ((ops '((+ #xec) (- #xee) (& #xef) (\| #xf0) (^ #xf1)
               (<< #xf2) (>> #xf3) (* #xf4) (/ #xf5) (% #xf6)
               (== #xf7) (!= #xf7 #xe7) (< #xf8) (> #xf9)
               (>= #xf8 #xe7) (<= #xf9 #xe7)
               (! #xe7) (-x #xe6) (+x) (~ #xe6 #x41 #xee)))
        (ass '((=) (+= #xec) (-= #xee) (&= #xef) (\|= #xf0) (^ #xf1)
               (<<= #xf2) (>>= #xf3) (*= #xf4) (/= #xf5) (%= #xf6))))
    (cond
      ((number? t) (when keep (compile-number t)))
      ((atom? t) (when keep (ready-lval t a l) (compile-load t)))
      ((eq? (car t) 'call) ;(display "call.... whatever") (newline))
       (for-each (lambda (x) (compile-ex x a l #t)) (reverse (cddr t)))
       (if (global? (cadr t) a l)
         (dglobal #xa0 (cadr t))
         (begin
           (compile-ex (cadr t) a l #t)
           (db #xea)))
       (when keep (db #xe9)))
      ((eq? (car t) '=)
       (ready-lval (cadr t) a l)
       (when keep (db #xeb))
       (compile-ex (caddr t) a l #t)
       (compile-store (cadr t))
       (when keep (compile-load (cadr t))))
      ((eq? (car t) '++)
       (ready-lval (cadr t))
       (db #xeb)
       (when keep (db #xeb))
       (compile-load (cadr t))
       (db #x41) (db #xec)
       (compile-store (cadr t))
       (when keep (compile-load (cadr t))))
      ((eq? (car t) '--)
       (ready-lval (cadr t))
       (db #xeb)
       (when keep (db #xeb))
       (compile-load (cadr t))
       (db #x41) (db #xee)
       (compile-store (cadr t))
       (when keep (compile-load (cadr t))))
      ((eq? (car t) 'x++)
       (ready-lval (cadr t))
       (db #xeb)
       (when keep (db #xeb))
       (compile-load (cadr t))
       (db #x41) (db #xec)
       (compile-store (cadr t))
       (when keep
         (compile-load (cadr t)) (db #x41) (db #xee)))
      ((eq? (car t) 'x--)
       (ready-lval (cadr t))
       (db #xeb)
       (when keep (db #xeb))
       (compile-load (cadr t))
       (db #x41) (db #xee)
       (compile-store (cadr t))
       (when keep
         (compile-load (cadr t)) (db #x41) (db #xec)))
      ((eq? (car t) '&&)
       (compile-ex (cadr t) a l #t)
       (let ((b1 nmem))
         (dh 0)
         (compile-ex (caddr t) a l keep)
         (if keep
           (begin
             (dh 0)
             (db #x41)
             (dh 0)
             (set-jump! b1 #x20 nmem)
             (set-jump! (- nmem 5) #x20 nmem)
             (db #x40)
             (set-jump! (- nmem 3) #x00 nmem))
           (set-jump! b1 #x20 nmem))))
      ((eq? (car t) '\|\|)
       (compile-ex (cadr t) a l #t)
       (db #xe7)
       (let ((b1 nmem))
         (dh 0)
         (compile-ex (caddr t) a l keep)
         (if keep
           (begin
             (db #xe7)
             (dh 0)
             (db #x40)
             (dh 0)
             (set-jump! b1 #x20 nmem)
             (set-jump! (- nmem 5) #x20 nmem)
             (db #x41)
             (set-jump! (- nmem 3) #x00 nmem))
           (set-jump! b1 #x20 nmem))))
      (#t
        (let ((op (assoc (car t) ops)) (as (assoc (car t) ass)))
          (cond
            (op
             (for-each (lambda (x) (compile-ex x a l keep)) (cdr t))
             (when keep
               (let d ((op (cdr op)))
                 (unless (null? op)
                   (db (car op))
                   (d (cdr op))))))
            (as
             (ready-lval (cadr t) a l)
             (db #xeb)
             (when keep (db #xeb))
             (compile-load (cadr t))
             (compile-ex (caddr t) a l #t)
             (unless (null? (cdr as)) (db (cadr as)))
             (compile-store (cadr t))
             (when keep (compile-load (cadr t))))
            (#t
             (when keep
               (ready-lval (cadr t) a l)
               (compile-load (cadr t))))))))))

(define (compile-code t args locals)
  ;(display args) (newline)
  ;(display locals) (newline)
  ;(display t) (newline)
  (case (car t)
    ('if
     (compile-ex (cadr t) args locals #t)
     (let ((m nmem))
       (dh 0)
       (compile-code (caddr t) args locals)
       (set-jump! m #x20 nmem)))
    ('if-else
     (compile-ex (cadr t) args locals #t)
     (let ((m nmem))
       (dh 0)
       (compile-code (cadr t) args locals)
       (let ((n nmem))
         (dh 0)
         (compile-code (caddr t) args locals)
         (set-jump! m #x20 (+ n 2))
         (set-jump! n #x00 nmem))))
    ('body
     (for-each (lambda (x) (compile-code x args locals)) (cdr t)))
    (else (compile-ex t args locals #f))))

(define (compile-fun t)
  (when (assoc (car t) globals)
    (err (car t) " already defined"))
  (set! globals (append globals (list (cons (car t) (+ org nmem)))))
  (let* ((args (cadr t))
         (t (get-auto (caddr t))) (locals (car t)) (t (cdr t)))
    (when (> (length args) 14)
      (err "too many args"))
    (when (> (length locals) 16)
      (err "too many locals"))
    (db #xe3)
    (db (length locals))
    (let local ((locals1 '()) (locals locals))
      (if (null? locals)
        (compile-code t args (reverse locals1))
        (let ((l (car locals)) (locals (cdr locals)))
          (cond
            ((atom? l) (local (cons l locals1) locals))
            ((eq? (car l) '\[)
             (db #xe3) (db (caddr l))
             (db #xe4) (db (+ #x60 (length locals1)))
             (db #xfa)
             (local (cons (cadr l) locals1) locals))
            ((eq? (car l) '$\[)
             (db #xe3)
             (db (+ (floor (/ (caddr l) 4))
                   (if (integer? (/ (caddr l) 4)) 0 1)))
             (db #xe4) (db (+ #x60 (length locals1)))
             (db #xfa)
             (local (cons (cadr l) locals1) locals))
            (#t (err "invalid local variable"))))))
    (db (+ #xc0 (length args)))))

(define (compile-outer t)
  (display t) (newline)
  (case (car t)
    ('fun (compile-fun (cdr t)))
    ('const
     (set! constants
       (cons (cons (cadr t) (reverse (cddr t))) constants)))
    (else (display "whatever...") (newline))))

(define (compile-file name)
  (call-with-input-file name
    (lambda (p)
      (let read ((t (read-tokens p)))
        (unless (null? t)
          (display "BLOCK") (newline)
          (for-each (lambda (x) (display x) (display " ")) t) (newline)
          (compile-outer (ast t))
          (read (read-tokens p)))))))

(define (bytes->list v i0 n)
  (let bytes ((l '()) (i i0))
    (if (< i n)
      (bytes (cons (bytevector-u8-ref v i) l) (+ i 1))
      (reverse l))))

(define (digitc n)
  (integer->char (+ n (if (<= 0 n 9) 48 55))))

(define (bytestr b)
  (let ((b (bitwise-and b #xff)))
    (string-append
      (string (digitc (floor (/ b 16))))
      (string (digitc (bitwise-and b 15))))))

(define (ext n bn)
  (let* ((neg (bitwise-arithmetic-shift-left -1 bn))
         (n (bitwise-and n (bitwise-not neg))))
    (if (zero? (bitwise-and n (bitwise-arithmetic-shift-left 1 (- bn 1))))
      n
      (+ n neg))))

(define (bytes . b)
  (let bytes ((t 0) (b (reverse b)))
    (if (null? b)
      t
      (bytes (+ (* t 256) (car b)) (cdr b)))))

(define (sbytes . b)
  (ext (apply bytes b) (* 8 (length b))))

(define (dasm l)
  (let dasm ((l1 '()) (l l))
    (cond
      ((null? l) (reverse l1))
      ((< (car l) #x40)
       (dasm
         (cons (string-append (if (< (car l) #x20) "jmp " "jz ")
                 (number->string (ext (+ (* (car l) 256) (cadr l)) 13)))
           l1)
         (cddr l)))
      ((< (car l) #x80)
       (dasm
         (cons (string-append (if (< (car l) #x60) "im " "adrl ")
                 (number->string (ext (car l) 5)))
           l1)
         (cdr l)))
      ((< (car l) #xc0)
       (dasm
         (cons (string-append (if (< (car l) #xa0) "adrg " "callg ")
                 (number->string
                   (bitwise-and (+ (* (car l) 256) (cadr l)) #x1fff)))
           l1)
       (cddr l)))
      ((< (car l) #xe0)
       (dasm
         (cons (string-append "ret "
                 (number->string (bitwise-and (car l) #x1f)))
           l1)
       (cdr l)))
      (#t
       (case (car l)
         (#xe0
          (dasm
            (cons (string-append "imb "
                    (number->string (ext (cadr l) 8))) l1)
            (cddr l)))
         (#xe1
          (dasm
            (cons (string-append "imh "
                    (number->string (sbytes (cadr l) (caddr l)))) l1)
            (cdddr l)))
         (#xe2
          (dasm
            (cons
              (string-append "imw "
                (number->string
                  (sbytes (cadr l) (caddr l) (cadddr l) (caddr (cddr l)))))
              l1)
            (cddddr l)))
         (#xe3
          (dasm
            (cons (string-append "alloc " (number->string (cadr l))) l1)
            (cddr l)))
         (else
           (dasm
             (cons (vector-ref
                     (vector
                       "sp" "sys" "neg" "not" "setr" "getr" "callt" "dup"
                       "add" "add4" "sub" "and" "or" "xor" "shl" "shr"
                       "mul" "div" "rem" "eq" "lt" "gt" "stw" "ldw"
                       "stb" "lb" "ifdup" "drop")
                     (- (car l) #xe4))
               l1)
             (cdr l))))))))

(define (grange g)
  (let ((g (assoc g globals)))
    (if (and g (cdr g))
      (let ((ub (find (lambda (x) (and (cdr x) (> (cdr x) (cdr g))))
                  globals)))
        (cons (- (cdr g) org)
          (if ub
            (- (cdr ub) org)
            nmem)))
      #f)))

(define (global-bytes g)
  (let ((r (grange g)))
    (if r
      (bytes->list mem (car r) (cdr r))
      #f)))

(define (dasm-global g)
  (let ((b (global-bytes g)))
    (if b
      (dasm b)
      #f)))
