(include "bac.scm")

(define (test f . g)
  (display f) (display ":") (newline)
  (compile-file f)
  (let dg ((g g))
    (unless (null? g)
      (let ddas ((g (dasm-global (car g))))
        (display (car g))
        (unless (null? (cdr g)) (display ", ") (ddas (cdr g))))
      (newline)
      (dg (cdr g))))
  (newline))

(test "test.b" 'f)
(test "test1.b" 'main)
(exit)

