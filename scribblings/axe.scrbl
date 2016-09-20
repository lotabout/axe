#lang scribble/manual
@require[racket/sandbox
         scribble/eval
         @for-label[axe]]
@(define EVAL
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'axe)))

@title{#lang axe}
@author{jinzhouz}


Some handy tools that you might need. Just like the axe in your hand.

@hyperlink["https://github.com/lotabout/axe" "Source"]

@[table-of-contents]

@;====================================================================
@section{Reader Extension}

@defmodulelang[axe]

Reader extension is enabled by @tt{#lang axe} which also export all
identifiers from @tt{#lang racket}. Thus you can normally use @tt{#lang axe}
wherever you need @tt{#lang racket}.

@;--------------------------------------------------------------------
@subsection[#:tag "raw-regexp"]{Raw regexp}

Read raw regular expressions. In python, we wrote a regular expression:
@tt{r"(\t*)\1"}. When translated into racket, we have @racket{#px"(\t*)\\1"}. Note
that we need to add another @tt{\} character to escape @tt{\\1}.

It is inconvenient. Luckily we have @tt{#lang at-exp} so that we can do things like this:

@racketmod[
at-exp racket/base
(regexp-match @#,elem{@tt["@"]@racket[regexp]@racketparenfont["{"](.*)\1@racketparenfont["}"]} "123123")]

reports @racket['("123123" "123")]

@tt{\1} is one example that you do NOT want something to be escaped by racket
reader. But sometimes you do need it: such as when type @racket{@px"\t"}, you
actually wants to match the tab character. If we use at-exp for it, it will
treat @tt{\t} as two characters: @tt{\\} and @tt{t}. Which will not be
recognized as tab character in racket's @racket{pregexp} compiler, nor
@racket{regexp} of course.
