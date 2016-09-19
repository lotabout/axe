#lang s-exp syntax/module-reader
axe
#:read /-read
#:read-syntax /-read-syntax
#:language-info #(axe/lang/language-info get-language-info #f)

(require (prefix-in /- "../regex-reader.rkt"))
