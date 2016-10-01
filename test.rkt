#lang axe

;;; Test all the overall language which could not be test otherwise
;;; Mainly for the reader and #%app.
;;;
(module+ test
  (require rackunit)

  (test-case
    "~> threading and applicable dict"

    (check-equal? (~> {'a {'b {'c 10}}} 'a 'b 'c) 10)
    (check-equal? (~> 'a {'a 10}) 10)
    (check-equal? (~> 'a {'a {'b 10}} 'b) 10))

  (test-case
    "~> and lambda literal"

    (check-equal? (~> 10 #(+ % 10)) 20)
    (check-equal? (~> 10 #(+ % 10) #(* % %)) 400)

    (check-equal? ((~> 10 #(- % _)) 20) 10)
    (check-equal? (~> 10 #(- % _) (_ 20)) 10)

    (check-equal? (let ([lambda -]) (~> 10 #(+ % 10))) 20))

  (test-case
    "lambda literal #(...): normal usage"

    (check-equal? (map #(+ % 1) '(1 2 3)) '(2 3 4))
    (check-equal? (map #(+ % %2) '(1 2 3) '(1 2 3)) '(2 4 6))
    (check-equal? (#(apply list* % %&) 1 '(2 3)) '(1 2 3))

    (check-equal? (map #fn(+ % 1) '(1 2 3)) '(2 3 4))
    (check-equal? (map #fn(+ % %2) '(1 2 3) '(1 2 3)) '(2 4 6))
    (check-equal? (#fn(apply list* % %&) 1 '(2 3)) '(1 2 3))

    (check-equal? (map #lambda(+ % 1) '(1 2 3)) '(2 3 4))
    (check-equal? (map #lambda(+ % %2) '(1 2 3) '(1 2 3)) '(2 4 6))
    (check-equal? (#lambda(apply list* % %&) 1 '(2 3)) '(1 2 3))

    (check-equal? (map #λ(+ % 1) '(1 2 3)) '(2 3 4))
    (check-equal? (map #λ(+ % %2) '(1 2 3) '(1 2 3)) '(2 4 6))
    (check-equal? (#λ(apply list* % %&) 1 '(2 3)) '(1 2 3))

    ;; #fn doesn't interfere with #f
    (check-equal? #f #f)
    (check-false #f)

    ;; keyword arguments
    (check-equal? (#(* 1/2 %:m (* %:v %:v)) :m 2 :v 1) 1))

  (test-case
    "% and %1 refer to the same argument"

    (check-equal? (#(+ % %1) 2) 4)
    ; they refer to the same thing even when one of them is set! to another value.
    (check-equal? (#(begin (set! % "%") %1) "%1") "%"))

  (test-case
    "lambda literal should handle arbitrary number of arguments"
    (check-equal? (apply #(list %1 %42) (build-list 42 add1))
                  (list 1 42)))

  (test-case
    "lambda should be hygienic"
    (check-equal? (let ([lambda "not lambda"]
                        [define-syntax "not define-syntax"])
                    (#(+ % 1) 0))
                  1))

  (test-case
    "quotes should not be parsed in lambda literals"
    (check-equal? (#(list '%2 '%3 '%&))
                  (list '%2 '%3 '%&)))
  )

