#lang s-exp syntax/module-reader
axe/base
#:read /-read
#:read-syntax /-read-syntax
#:language-info #(axe/base/lang/language-info get-language-info #f)

(require (prefix-in /- "../regex-reader.rkt"))
