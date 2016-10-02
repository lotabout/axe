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


(require (except-in racket _) ; `_` will be used in axe/threading
         "threading.rkt"
         "escape.rkt"
         "app.rkt"
         "conditionals.rkt"
         "dict.rkt"
         )

(provide (except-out (all-from-out racket) #%app)
         (rename-out [-#%app #%app])
         (except-out (all-from-out "app.rkt") -#%app)
         (all-from-out "threading.rkt")
         (all-from-out "escape.rkt")
         (all-from-out "conditionals.rkt")
         (all-from-out "dict.rkt")
         )

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
