#lang info
(define collection "axe")
(define deps '("base"
               "data/collection"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "scribble-code-examples" "scribble-doc"))
(define scribblings '(("scribblings/axe.scrbl" ())))
(define pkg-desc "Handy defaults to bootstrap racket program")
(define version "0.1")
(define pkg-authors '(jinzhouz))
