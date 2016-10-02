#lang racket/base

(require racket/contract)

(provide
  (contract-out
    [dict-extend (-> (dict-implements/c 'dict-set) any/c ...  dict?)]
    [dict-extend! (-> (dict-implements/c 'dict-set!) any/c ...  void?)]
    [dict-merge (-> (dict-implements/c 'dict-set) dict? ...  dict?)]
    [dict-merge! (-> (dict-implements/c 'dict-set!) dict? ...  void?)]
    [dict-merge-with (-> (any/c any/c . -> . any/c)
                         (dict-implements/c 'dict-set)
                         dict? ...
                         dict?)]
    [dict-merge-with! (-> (any/c any/c . -> . any/c)
                          (dict-implements/c 'dict-set!)
                          dict? ...
                          void?)]
    ))


(require racket/dict
         racket/match)

(module+ test
  (require rackunit))

;;; (dict-extend dict? . kvs)
(define (dict-extend dictionary . kvs)
  (when (not (zero? (remainder (length kvs) 2)))
    (raise-arguments-error 'dict-extend
                           "key and value should appear in pairs"
                           "kvs" kvs))
  (match-define (list keys vals) (partition-key-values kvs))
  (for/fold ([d dictionary])
            ([k (in-list keys)]
             [v (in-list vals)])
    (dict-set d k v)))

(module+ test
  (check-equal? (dict-extend #hash()) #hash())
  (check-equal? (dict-extend #hash() 'a 1) #hash((a . 1)))
  (check-equal? (dict-extend #hash() 'a 1 'b 2) #hash((a . 1) (b . 2)))
  (check-equal? (dict-extend #hash() 'a 1 'a 2) #hash((a . 2)))
  (check-equal? (dict-extend '()) '())
  (check-equal? (dict-extend '() 'a 1) '((a . 1)))
  (check-equal? (dict-extend '() 'a 1 'b 2) '((a . 1) (b . 2)))
  (check-equal? (dict-extend '() 'a 1 'a 2) '((a . 2)))
  )

(define (dict-extend! dictionary . kvs)
  (when (not (zero? (remainder (length kvs) 2)))
    (raise-arguments-error 'dict-extend
                           "key and value should appear in pairs"
                           "kvs" kvs))
  (match-define (list keys vals) (partition-key-values kvs))
  (for ([k (in-list keys)]
        [v (in-list vals)])
    (dict-set! dictionary k v)))

(module+ test
  (check-equal? (dict-extend! #hash()) (void))
  (check-equal? (let ([d (make-hash)])
                  (dict-extend! d 'a 1)
                  d)
                (make-hash '((a . 1))))
  (check-equal? (let ([d (make-hash)])
                  (dict-extend! d 'a 1 'b 2)
                  d)
                (make-hash '((a . 1) (b . 2))))
  (check-equal? (let ([d (make-hash)])
                  (dict-extend! d 'a 1 'a 2)
                  d)
                (make-hash '((a . 2)))))

;;; (partition-key-values '(k v k2 v2)) => '((k k2) (v v2))
(define (partition-key-values kvs)
  (let loop ([kvs kvs] [keys '()] [vals '()])
    (if (null? kvs)
        (list (reverse keys) (reverse vals))
        (loop (cddr kvs) (cons (car kvs) keys) (cons (cadr kvs) vals)))))

(module+ test
  (check-equal? (partition-key-values '()) '(() ()))
  (check-equal? (partition-key-values '(1 2 3 4)) '((1 3) (2 4))))

(define (dict-merge-one f dict-target dict-source)
  (for/fold ([dict-target dict-target])
            ([(key new-val) (in-dict dict-source)])
    (if (not (dict-has-key? dict-target key))
        (dict-set dict-target key new-val)
        (dict-update dict-target key (lambda (old-val) (f old-val new-val))))))

(module+ test
  (check-equal? (dict-merge-one + '((a . 1)) '((a . 2))) '((a . 3)))
  (check-equal? (dict-merge-one + '((a . 1)) '((b . 2))) '((a . 1) (b . 2)))
  (check-equal? (dict-merge-one + '((a . 1)) '()) '((a . 1)))
  (check-equal? (dict-merge-one + '((a . 1)) '((a . 2) (b . 3))) '((a . 3) (b . 3)))
  (check-equal? (dict-merge-one + #hash((a . 1)) #hash((a . 2))) #hash((a . 3)))
  (check-equal? (dict-merge-one + #hash((a . 1)) #hash((b . 2))) #hash((a . 1) (b . 2)))
  (check-equal? (dict-merge-one + #hash((a . 1)) #hash()) #hash((a . 1)))
  (check-equal? (dict-merge-one + #hash((a . 1)) #hash((a . 2) (b . 3))) #hash((a . 3) (b . 3)))
  )

(define (dict-merge-one! f dict-target dict-source)
  (for ([(key new-val) (in-dict dict-source)])
    (if (not (dict-has-key? dict-target key))
        (dict-set! dict-target key new-val)
        (dict-update! dict-target key (lambda (old-val) (f old-val new-val))))))

(module+ test
  (check-equal? (let ([d (make-hash '((a . 1)))])
                  (dict-merge-one! + d '((a . 2)))
                  d)
                (make-hash '((a . 3))))
  (check-equal? (let ([d (make-hash '((a . 1)))])
                  (dict-merge-one! + d '((b . 2)))
                  d)
                (make-hash '((a . 1) (b . 2))))
  (check-equal? (let ([d (make-hash '((a . 1)))])
                  (dict-merge-one! + d '())
                  d)
                (make-hash '((a . 1))))
  (check-equal? (let ([d (make-hash '((a . 1)))])
                  (dict-merge-one! + d '((a . 2) (b . 3)))
                  d)
                (make-hash '((a . 3) (b . 3)))))

(define (dict-merge-with f dict-target . dicts)
  (for/fold ([dict-target dict-target])
            ([dict-source dicts])
    (dict-merge-one f dict-target dict-source)))

(define (dict-merge dict-target . dicts)
  (apply dict-merge-with (lambda (x y) y) dict-target dicts))

(define (dict-merge-with! f dict-target . dicts)
  (for ([dict-source dicts])
    (dict-merge-one! f dict-target dict-source)))

(define (dict-merge! dict-target . dicts)
  (apply dict-merge-with! (lambda (x y) y) dict-target dicts))
