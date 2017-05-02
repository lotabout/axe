#lang scribble/manual
@require[scribble/eval
         scribble/bnf
         scribble-code-examples]

@(define axe-eval
   (make-eval-factory '(axe)))


@title{#lang axe}
@author{jinzhouz}

Some handy tools that you might need. Just like the axe in your hand. @hyperlink["https://github.com/lotabout/axe" "Source"]

@[table-of-contents]

@defmodulelang[axe]

@itemlist[
  @item{@tt{#lang axe} export all identifiers from @tt{#lang racket}. Thus you can
        safely replace @tt{#lang racket} with @tt{#lang axe}.}
  @item{@tt{#lang axe} re-exports lexi-lambda's @secref["top" #:doc '(lib
        "scribblings/data/collection/collections.scrbl")] which provides a generic
        interface for all racket collections.}
]

@;====================================================================
@section{Reader Extension}

Reader extension is enabled by the @tt{#lang axe} line on the top of your source file.
@;--------------------------------------------------------------------
@subsection[#:tag "raw-string"]{Raw String}
@racketmod[
axe
@#,elem{r"raw string"}
@#,elem{r'raw string'}
]

This syntax is borrowed from Python. If the string literals you are trying to
write contains lots of (mixed) quotes, or escaped forms, raw string can help a lot.

For example if we want to match a single digit we have to compose regular
expression: @racket[(pregexp "\\d")]. Note that we have to write @litchar{\\} to
make racket's reader happy and make us unhappy.

With this reader extension, the @litchar{\\} character is interpreted as
raw. But note that @litchar{\"} is interpreted as @litchar{"} in @litchar{r"\""}
form. The same goes to @litchar{'} in @litchar{r'\''}.

@code-examples[#:lang "axe" #:context #'here #:show-lang-line #t]|{
(list r'\t\a\b\c\d\1 \'quote\'')
}|

If your your string contains lots of mixed quotes, the three quotes version
might be more convenient:
@racketmod[
axe
@#,elem{r"""raw string"""}
@#,elem{r'''raw string'''}
]

Here's an example:
@code-examples[#:lang "axe" #:context #'here #:show-lang-line #t]|{
(list r'''mixe 'single quote' with "double quote""''')
}|

Note that @litchar{"""} or @litchar{''''} are matched eagerly. So that in
@litchar{r"""ends with""""}, racket will treat the last quote as the begin of
another quoted string.

Now we have an easy way to read in raw strings, we still got some troubles
building up regular expressions. Thus we provide @racket[pregexp-raw] and
@racket[regexp-raw] to build up regular expressions from raw strings.

@code-examples[#:lang "axe" #:context #'here #:show-lang-line #t]|{
(regexp r"(\t*)\1")
(pregexp-raw r"(\t*)\1")
}|

@defform[(regex-escape-raw raw-string)]{
    Parse the escaped characters in raw strings into. For example If you
    write @litchar{r"\t"}, it is equivalent to string @racket["\\t"].
    @racket[regex-escape-raw] will parse it into @racket["\t"]. While raw
    strings like @racket["\\1"] will be used by @racket[regexp], so it will
    remains the same.

    @(examples
      #:eval (axe-eval)
      (regex-escape-raw "\\a\\b\\t\\n\\1")
      )
}

@deftogether[(@defform[(pregexp-raw raw-string)]
              @defform[(regexp-raw raw-string)]
              )]{

    It is a wrapper of @racket[regex-escape-raw], so that you can create
    regular expressions directly from raw strings.
    @racketblock[
      (regexp (regex-escape-raw raw-string))
      (pregexp (regex-escape-raw raw-string))
    ]
}


@;--------------------------------------------------------------------
@subsection[#:tag "raw-regexp"]{Raw regexp}

@racketmod[
axe
@#,elem{#rx/raw regexp/}
@#,elem{#px/raw regexp/}
@#,elem{r/raw regexp string/}
]

Typing regular expressions could be difficult in racket. In python, we can write
a regular expression in raw mode: @litchar{r"(\t*)\1"}. When translated into racket,
we have @litchar{#px"(\t*)\\1"}. Note that we need to add another @litchar{\} character to
escape @litchar{\\1}.

It is inconvenient. Luckily we have @litchar{#lang at-exp} so that we can do things
like this:

@code-examples[#:lang "at-exp racket/base" #:context #'here #:show-lang-line #t]|{
(regexp-match @regexp{(.*)\1} "123123")
}|


@litchar{\1} is one example that you do NOT want something to be escaped by racket
reader. But sometimes you do need it: such as when type @litchar{@"@"px"\t"}, you
actually wants to match the @racket[#\tab]. If we use at-exp for it, it will
treat @litchar{\t} as two characters: @litchar{\\} and @litchar{t}. Which will not be
recognized as @racket[#\tab] character in racket's @racket{pregexp} compiler,
nor @racket{regexp} of course.

Thus we introduce the new form:

@racketmod[
axe
(regexp-match @#,elem{#px/(\t*)\1/} "\t\t")
]

reports @racket['("\t\t" "\t")]. That means the you can write raw regexp and
enclose it with @litchar{#px/} and @litchar{/}. The same goes to @litchar{#rx/raw/}

The "raw regexp string" is like raw regular expressions, but they can be used
as replace string in @racket[regexp-replace]. The following two forms are equal:

@racketmod[
axe

@#,elem{#px/(\t*)\1/}
(pregexp @#,elem{r/(\t*)\1/})
]

And the @litchar{r/raw regex string/} can be use where regexp strings are needed:

@racketmod[
axe
(regexp-replace @#,elem{#px/(\d+)/} "abc123xyz" @#,elem{r/-\1-/})
]

We got result @racket["abc-123-xyz"].

@;--------------------------------------------------------------------
@subsection[#:tag "quick-keyword"]{Quick Keyword}

This one is simple. You can replace @racket[#:key] with @racket[:key].

@;--------------------------------------------------------------------
@subsection[#:tag "app-dict"]{Applicable Dictionary}

Borrowed from @litchar{#lang rackjure}, we redefines @racket[#%app] to make dictionaries applicable:

@#reader scribble/comment-reader
(racketblock
;; When (dict? d) is #t

(d key)         => (dict-ref d key #f)
(d key default) => (dict-ref d key default)
)

Note here that we don't support rackjure's set syntax: @racket[(d key value)].
We prefer clojure style. You can also use key as a procedure to retrieve the
contents of a dict:

@#reader scribble/comment-reader
(racketblock
(key d)  => (dict-ref d key)
(key #f) => #f  ; unless (or (procedure? `key`) (dict? `key`))
)

The reason that @racket[#f] is returned for the @racket[(key #f)] is that we
can use it together with the threading macro to fetch the contents of nested
dicts:

@codeblock{
    (~> dict 'a 'b 'c)
}

expands to:

@codeblock{
    ('c ('b ('a dict)))
}

And is applied as:

@codeblock{
    (dict-ref (dict-ref (dict-ref dict 'a) 'b) 'c)
}

@;--------------------------------------------------------------------
@subsection[#:tag "dict-reader"]{Dictionary initialization with @tt{{}}}

Racket supports writting dict literals as:

@racketblock[
#hash((k0 . v0) (k1 . v1))
]

It is straightforward but requires more typing than:

@racketblock[
{k0 v0 k1 v1 ...}
]

Especially when typing nested lists:

@racketblock[
{k0 v0,
 k1 {key value,
     key value}}
]

Note the character @litchar{,} is optional here. In @racket[axe] @litchar{,} is treated as
whitespace if followed by other whitespaces. Thus you can use it as a
delimiter whitespace or use it as @racket[unquote] normally.

Borrowed form rackjure, we provide @racket[current-curly-dict] parameter to
specify the type of dict it expands to.

@defparam[current-curly-dict v procedure?]{
    Defaults to @racket[hash]. Can be set to @racket[hasheq] or anything with the
    same @racket[(f k v ... ...)] signature.

    Examples:

    @code-examples[#:lang "axe" #:context #'here]|{
        (parameterize ([current-curly-dict hasheq])
           {'k0 0, 'k1 1})
        (parameterize ([current-curly-dict hasheqv])
           {'k0 0 'k1 1})
    }|
}

@;--------------------------------------------------------------------
@subsection[#:tag "lambda-literal"]{Lambda literals}

Thanks to
@hyperlink["http://docs.racket-lang.org/rackjure/index.html#%28part._func-lit%29"
"rackjure"] and @hyperlink["http://docs.racket-lang.org/curly-fn/index.html"
"curly-fn"] for the idea and implementation.

The clojure reader lets us to define function literals through:

@racketblock[
#(+ % %2)
]

It is equivalent to this in clojure:

@racketblock[
(fn [% %2] (+ % %2))
]

Or in racket:

@racketblock[
(λ (% %2) (+ % %2))
(lambda (% %2) (+ % %2))
]

In the @racket[#(...)] form, arguments are denoted using identifiers prefixed
with @litchar{%}.

@itemlist[
 @item{@litchar{%} @kleeneplus{@nonterm{digit}} is a positional argument.}
 @item{@litchar{%} on its own is an alias for @litchar{%1}.}
 @item{@litchar{%&} is a rest argument.}
 @item{@litchar{%:} @nonterm{id} is a keyword argument.}]

Racket uses @litchar{#( )} for vector literals by default. It is overwritten
by @racket[axe]. You can use @litchar{#[ ]} for that if needed.  Besides,
@racket[axe] provides other forms for lambda literals: @litchar{#fn()},
@litchar{#λ( )} and @litchar{#lambda( )}.

@code-examples[#:lang "axe" #:context #'here]|{
(sequence->list (map #(+ 1 %) '(1 2 3)))
(sequence->list (map #fn(+ % %2) '(1 2 3) '(1 2 3)))
(#lambda(apply list* % %&) 1 '(2 3))
(#(* 1/2 %:m (* %:v %:v)) #:m 2 #:v 1)
(#(* 1/2 %:m (* %:v %:v)) :m 2 :v 1)
(#(begin %2) 'ignored 'used)
}|

Remember in the above example, we use @litchar{:m} to replace @litchar{#:m},
see @secref{quick-keyword}.

There are some pitfalls about lambda literals that normally you should not
care about:

@itemlist[
 @item{Nested lambda literals are not supported, the @litchar{#( ... )} will
 be treated as vectors as it is in racket. Other forms such as @litchar{#fn( ... )} will cause error.}
 @item{It is safe to write @racket['%] and its variants, but not @racket[`%].
 That means quoted @litchar{%} is not treated as argument but not the
 quasiquote.}
 @item{You should not use it to write complicated functions. It will be hard to read, use lambda instead.}
]

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

Threading macro in @racket[axe] will handle @racket[lambda] well, so that the
following example works correctly.

@(interaction
#:eval (axe-eval)
(~> 10 (lambda (x) (- x 1)))
(~> 10 (lambda (x) (- x 1)) (lambda (x) (* x x)))
)


If the placeholder @litchar{_} is included in the body of @racket[lambda]
expression, the @racket[~>] macro will instead replace the placeholder with
the result of previous "pipeline" and generate a new lambda function.

@#reader scribble/comment-reader
(racketblock
(~> 10 (lambda (x) (- x 1))) => ((lambda (x) (- x 1)) 10)
(~> 10 (lambda (x) (- x _))) => (lambda (x) (- x 10))
)

So that you can do the following:

@(interaction
#:eval (axe-eval)
((~> 10 (lambda (x) (- x _))) 1)
)

Finally, we have some litte hack in threading macro, now you can NOT use
@litchar{[...]} or @litchar{{...}} to represent function applications. But we
already use @litchar{{...}} for dictionaries, so hope it won't be too strange
for you.

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
       (index-where even?)
       (* 2))

      (and~> '(2 4 6)
       (map add1 _)
       (index-where even?)
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

@;--------------------------------------------------------------------
@subsection[#:tag "conditionals"]{Conditionals}
@defmodule[axe/conditionals]

This module provides alternatives to built in @racket[if], @racket[when] and @racket[cond].

@defform[(if-let [binding-expr test-expr] then-expr else-expr)]{
    combines @racket[if] and @racket[match-let]:
    @racketblock[
    (let ([val test-expr])
     (if val
         (match-let ([binding-expr test-expr])
           then-expr)
         else-expr))]
}

@defform[(when-let [binding-expr test-expr] body ...+)]{
    combines @racket[when] and @racket[match-let]:
    @racketblock[
    (let ([val test-expr])
     (when val
         (match-let ([binding-expr test-expr])
           body ...)))]
}

@defform[(if-not test-expr then-expr else-expr)]{
    shotcut for:
    @racketblock[
    (if (not test-expr)
        then-expr
        else-expr)]
}

@defform[(when-not test-expr body ...+)]{
    Just another implementation of @racket[unless]. To make it consistent in the wording.
    @racketblock[
    (when (not test-expr)
        body ...+)]
}


@defform/subs[#:literals (else =>)
              (cond-let binding-expr cond-clause ...)
              ([cond-clause [test-expr then-body ...+]
                            [else then-body ...+]
                            [test-expr => proc-expr]
                            [test-expr]])]{
    As you can see in the signature, it is almost identical to the built in
    @racket[cond], except it allows an @tt{binding-expr} for the test-expr,
    so that in the @tt{then-body} for @tt{proc-expr}, you can refer to the
    result of @tt{test-expr} with it.

    @(examples
      #:eval (axe-eval)
      (let ([lst '(x y z a b c)])
        (cond-let it
          [(member 'a lst) (length it)]
          [else 0])))

    @racket[match-let] pattern can be applied to descruct the values:
    @(examples
      #:eval (axe-eval)
      (let ([dct #hasheq((a . #f) (b . (1 2)) (c . (10 20)))])
        (cond-let (list a b)
          [(dict-ref dct 'a) (+ a b)]
          [(dict-ref dct 'b) (- a b)]
          [(dict-ref dct 'c) (* a b)]
          [else 0])))
}

@;====================================================================
@section{Utility Functions}

@;--------------------------------------------------------------------
@subsection[#:tag "dictionary"]{Dictionary}

@defmodule[axe/dict]

Now axe depends on @racket[data/collection], you are safely to use it instead.

@racket[axe] provide some wrappers over the dictionary related functions
provided by @racket[racket/dict].

@defproc[(dict-extend [dict (and/c dict? immutable?)] [key any/c] [value any/c] ...)
         (and/c dict? immutable?)]{
Iterate over all the key-value pairs and update the dictionary accordingly.

@(examples
#:eval (axe-eval)
(dict-extend '((a . 1)) 'b 2 'a 3)
(dict-extend #hash() 'a 1 'b 2 'a 3)
)}

@defproc[(dict-extend! [dict (and/c dict? (not/c immutable?))] [key any/c] [value any/c] ...)
         (and/c dict? (not/c immutable?))]{

The mutable version of @racket[dict-extend].

@(examples
#:eval (axe-eval)
(dict-extend! (make-hash) 'b 2 'a 3)
)}

@deftogether[(
@defproc[(dict-merge [d0 (and/c dict? immutable?)] [d1 dict?] ...)
         (and/c dict? immutable?)]
@defproc[(dict-merge! [d0 (and/c dict? (not/c immutable?))] [d1 dict?] ...)
         (and/c dict? (not/c immutable?))]
)]{
Merge several dicts into @tt{d0}. @racket[dict-merge!] is the mutable version.

@(examples
#:eval (axe-eval)
(dict-merge '((a . 1)) '((b . 2) (c . 3)) '((a . 4) (c . 5)))
(define d (make-hash '((a . 1))))
(dict-merge! d '((b . 2) (c . 3)) #hash((a . 4) (c . 5)))
d
)}


@deftogether[(
@defproc[(dict-merge-with [f (any/c any/c . -> . any/c)] [d0 (and/c dict? immutable?)] [d1 dict?] ...)
         (and/c dict? immutable?)]
@defproc[(dict-merge-with! [f (any/c any/c . -> . any/c)] [d0 (and/c dict? (not/c immutable?))] [d1 dict?] ...)
         (and/c dict? (not/c immutable?))]
)]{
Like @racket[dict-merge] and @racket[dict-merge!]. But when both dicts contain
the same key, merge function @tt{f} is called with arguments the value in the
old dict and value in the new dict.

@(examples
#:eval (axe-eval)
(dict-merge-with list #hash((a . 1)) '((b . 2) (c . 3)) '((a . 4) (c . 5)))
(dict-merge-with - '((a . 5)) '((b . 2) (c . 3)) '((a . 4) (c . 5)))
)
}
