#lang axe/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/require)

(require (except-in (subtract-in racket data/collection) _)  ; `_` will be used in axe/threading
         "app.rkt"
         data/collection
         "threading.rkt"
         "escape.rkt"
         "conditionals.rkt"
         "dict.rkt"
         (for-syntax racket/base)
         )

(provide (except-out (all-from-out racket) #%app)
         (rename-out [-#%app #%app])
         (except-out (all-from-out "app.rkt") -#%app)
         (all-from-out data/collection)
         (all-from-out "threading.rkt")
         (all-from-out "escape.rkt")
         (all-from-out "conditionals.rkt")
         (all-from-out "dict.rkt")
         (for-syntax (all-from-out racket/base))
         )

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
