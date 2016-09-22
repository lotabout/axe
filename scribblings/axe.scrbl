#lang scribble/manual
@require[racket/sandbox
         scribble/eval
         @for-label[axe]]

@(define axe-eval
   (make-eval-factory '(axe)))

@title{#lang axe}
@author{jinzhouz}


Some handy tools that you might need. Just like the axe in your hand. @hyperlink["https://github.com/lotabout/axe" "Source"]

@[table-of-contents]

@defmodulelang[axe]
@;====================================================================
@section{Reader Extension}

Reader extension is enabled by @tt{#lang axe} which also export all
identifiers from @tt{#lang racket}. Thus you can safely replace @tt{#lang
racket} with @tt{#lang axe}.

@;--------------------------------------------------------------------
@subsection[#:tag "raw-string"]{Raw String}
@racketmod[
axe
@#,elem{r"raw string"}
@#,elem{r'raw string'}
]

With this reader extension, you can compose raw strings just like you did in
python. The @tt{\\} character is interpreted as raw. But note that @tt{\"} is
interpreted as @tt{"} in @tt{r"\""} form. The same goes to @tt{'} in
@tt{r'\''}.

Note that even with raw string, times would still be hard with regular
expressions, so now we provide regular expression utilities that work well
with raw strings.

@racketmod[
axe
(regexp-match (pregexp-raw @#,elem{r"(.*)\1"}) "123123")]

i.e. you can build regular expression via @racket[pregexp-raw] with raw
strings.

@;--------------------------------------------------------------------
@subsection[#:tag "raw-regexp"]{Raw regexp}

@racketmod[
axe
@#,elem{#/raw regexp/}
]

Typing regular expressions could be difficult in racket. In python, we can write
a regular expression in raw mode: @tt{r"(\t*)\1"}. When translated into racket,
we have @tt{#px"(\t*)\\1"}. Note that we need to add another @tt{\} character to
escape @tt{\\1}.

It is inconvenient. Luckily we have @tt{#lang at-exp} so that we can do things
like this:

@racketmod[
at-exp racket/base
(regexp-match @#,elem{@tt["@"]@racket[regexp]@racketparenfont["{"](.*)\1@racketparenfont["}"]} "123123")]

reports @racket['("123123" "123")]

@tt{\1} is one example that you do NOT want something to be escaped by racket
reader. But sometimes you do need it: such as when type @tt{@"@"px"\t"}, you
actually wants to match the @racket[#\tab]. If we use at-exp for it, it will
treat @tt{\t} as two characters: @tt{\\} and @tt{t}. Which will not be
recognized as @racket[#\tab] character in racket's @racket{pregexp} compiler,
nor @racket{regexp} of course.

Thus we introduce the new form:

@racketmod[
axe
(regexp-match @#,elem{#/(\t*)\1/} "\t\t")
]

reports @racket['("\t\t" "\t")]. That means the you can write raw regexp and
enclose it with @tt{#/} and @tt{/}. Now try it!

@;====================================================================
@section{Handy Macros}

@;--------------------------------------------------------------------
@subsection[#:tag "threading"]{Threading Macros}

@defmodule[axe/threading]

@hyperlink["http://clojure.org/guides/threading_macros" "Threading macros"] are
first introduced in clojure. It helps us to change nested function calls into
"pipelines". For example: we are calculating some values with a complicated
nested function calls:

@(interaction
#:eval (axe-eval)
(- (bytes-ref (string->bytes/utf-8 (symbol->string 'abc)) 1) 2))

It would be hard to understand the data flow in this expression. It would be
more clear if we convert it into pipelines using the threading macro:
@(interaction
#:eval (axe-eval)
(~> 'abc
    symbol->string
    string->bytes/utf-8
    (bytes-ref 1)
    (- 2)))

Note that @racket[symbol->string] and @racket[string->bytes/utf-8] are not
enclosed in parenthesis.

@racket[~>] macro also enables you to use placeholder(@racket[_]) to specify the
position of the arguments you want to place. So that you can achieve something
like this:

@(interaction
#:eval (axe-eval)
(~> '(1 2 3)
     (map sqrt _)
     (apply + _)
     (- 20 (* _ 2))))

You can also change the symbol for place holder to any identifier you like:

@(interaction
#:eval (axe-eval)
(require (rename-in axe [_ %]))
(~> '(1 2 3)
     (map sqrt %)
     (apply + %)
     (- 20 (* % 2))))

@defform[(~> expr clause ...)]{
    Threads @emph{expr} through @emph{clause}. Insert @emph{expr} as the
    @bold{second} item in the first @emph{clause}. @racket[(~> expr (function arg))]
    is transformed into @racket[(~> expr (function _ arg))] and that
    results in @racket[(function expr arg)].

    If there are multiple clauses, thread the first clause as the second item in the
    second clause, recursively.
}

@defform[(~>> expr clause ...)]{
    Like @racket[~>] but insert @emph{expr} as the @bold{last} item in
        @emph{clause}. So that it equals to @racket[(~>> expr (function arg _))].
}

@defform[(and~> expr clause ...)]{
    Works like @racket[~>], but immediatly returns @racket[#f] if any of the
    clause returns @racket[#f]. Like @racket[and], this is short-circuiting, so
    the remaining steps will not be executed.

    Examples:

    @(examples
      #:eval (axe-eval)
      (and~> '(2 4 5)
       (map add1 _)
       (findf even? _)
       (* 2))

      (and~> '(2 4 6)
       (map add1 _)
       (findf even? _)
       (* 2)))
}

@defform[(and~>> expr clause ...)]{
    Combines the threading behavior of @racket[~>>] and the short-circuiting
    behavior of @racket[and~>].
}

@deftogether[(@defform[(lambda~> clause ...)]
              @defform[(λ~> clause ...)]
             )]{
    Handy wrapper for @racket[(λ (arg) (~> arg clause ...))].
    @(examples
      #:eval (axe-eval)
      (map (λ~> add1 (* 2)) (range 5)))
}

@deftogether[(@defform[(lambda~>> clause ...)]
              @defform[(λ~>> clause ...)])]{
    Like @racket[lambda~>] but for @racket[~>>].
}

@deftogether[(@defform[(lambda~>* clause ...)]
              @defform[(λ~>* clause ...)])]{
    Equivalent to @racket[(λ args (~> args clause ...))].
}

@deftogether[(@defform[(lambda~>>* clause ...)]
              @defform[(λ~>>* clause ...)])]{
    Like @racket[lambda~>*] but for @racket[~>>].
}

@deftogether[(@defform[(lambda-and~> clause ...)]
              @defform[(λ-and~> clause ...)]
              @defform[(lambda-and~>> clause ...)]
              @defform[(λ-and~>> clause ...)]
              @defform[(lambda-and~>* clause ...)]
              @defform[(λ-and~>* clause ...)]
              @defform[(lambda-and~>>* clause ...)]
              @defform[(λ-and~>>* clause ...)])]{
    Like @racket[lambda~>], but for @racket[and~>].
}

The @hyperlink["https://docs.racket-lang.org/threading/index.html"
"threading module"], and
@hyperlink["http://docs.racket-lang.org/rackjure/index.html#(part._.Threading_macros)"
"rackjure"] had already provide such functionality and good documents. But
rackjure do not support placeholder and threading module do not support
placeholder in nested function calls like @racket[(~> expr (fun1 (func2 _
arg)))].
