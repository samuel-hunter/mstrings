# M-strings for Common Lisp
[![builds.sr.ht status](https://builds.sr.ht/~shunter/mstrings/commits/master/test.yml.svg)](https://builds.sr.ht/~shunter/mstrings/commits/master/test.yml)

Reader macro for friendlier multiline strings.

M-strings defines a reader macro for multiline strings with various
quality-of-life features, including whitespace elimination and folding.

When writing strings that take multiple lines, for example an extensive
docstring, it feels natural to align them up. However, the Lisp reader will
keep all leading whitespace in the string:

```lisp
* (defun grab-config (use-default)
    (if use-default
        "foo=1
         bar=2
         qux=3"
        (read-file-into-string +config-filepath+)))

* (grab-config t)
"foo=1
         bar=2
         qux=3"
```

The workaround is to shove all subsequent lines to the beginning of the file,
which could appear quite unsightly:

```lisp
* (defun grab-config (use-default)
    (if use-default
        "foo=1
bar=2
qux=3"
        (read-file-into-string +config-filepath+))

* (grab-config t)
"foo=1
bar=2
qux=3"
```

M-strings allow a plain method to remove the whitespace while keeping content
aligned in the source code:

```lisp
* (defun grab-config (use-default)
    (if use-default
        #M"foo=1
           bar=2
           qux=3"
        (read-file-into-string +config-filepath+)))

* (grab-config t)
"foo=1
bar=2
qux=3"
```

## Contributions

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/mstrings/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/mstrings).

## Usage

M-strings uses [cl-syntax](https://github.com/fukamachi/cl-syntax) to expose the
reader macro. Install it through Quicklisp, or manually alongside its
dependents [trivial-types](https://github.com/m2ym/trivial-types) and
[named-readtables](https://github.com/kmizumar/named-readtables/).

Install M-strings locally, until it is added to Quicklisp:

```sh
$ cd ~/common-lisp/ # Or wherever you store your definitions
$ git clone https://git.sr.ht/~shunter/parsnip
```

To use M-string macros, call `(syntax:use-syntax '#:mstrings)` at the beginning
of every file:

```lisp
* (require :mstrings)
* (syntax:use-syntax '#:mstrings)

* #M"Hello, world!"
* "Hello, world!"
```

## Features

M-strings remove leading whitespace:
```lisp
* (princ #M"Hello
            World!")
Hello
World!
```

M-strings read empty lines as a single newline:

```lisp
* (princ #M"Hello

            World!")
Hello

World!
```

M-strings respect escaped characters and, outside for newlines, treats them line non-whitespace characters:

```lisp
* (princ #M"keys:
           \  foo: \"apple\"
           \  bar: \"banana\"")
keys:
  foo: "apple"
  bar: "banana"
```

M-strings concatenate together two lines if they're separated by an escaped newline - useful for very long single-line values:

```lisp
* (princ #M"NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW\
            2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===")
NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===
```

By default, M-strings are read in "literal-block mode", where newlines are read
as literal newlines. Prefixing a string with a `>` sets the reader to
"folding-block mode", where multiple lines in the string are folded into one
line:

```lisp
* (princ #M>"The quick brown fox
             jumps over
             the lazy dog.

             Sphinx of black quartz,
             judge my vow!")
The quick brown fox jumps over the lazy dog.
Sphinx of black quartz, judge my vow!
```

## Shorthand notation: `#"..." and #>"..."`

The reader macro function understands `#"..."` and `#>"..."` as shorthands for
`#M"..."` and `#M>"..."`, respectively. They're defined separately to prevent
macro collisions from other systems:

```lisp
* (syntax:use-syntax 'mstrings:shorthand-mstrings)
  ;; Or, for both:
* (syntax:use-syntax '(#:mstrings mstrings:shorthand-mstrings))

*    #"Literal-block
       Mode"
"Literal-block
Mode"
*    #>"Folding-block
        Mode"
"Folding-block Mode"
```

Be aware, `#"` is also used by the string-escape library, and `#>` is used by
Clozure CL 1.2 and later (and therefore elided from the shorthand-mstrings
syntax).

## API

### [Function] **mstring-reader** *stream subchar arg*

Reader macro accepts multiline strings. It ignores and warns on any provided
*arg*, and provides a few quality-of-life features depending on the value of
*subchar*:

- If *subchar* is `#"`, it is always literal-block mode (`#M"..."`) and reads
  the rest of the string.
- If *subchar* is `#>`, it is always folding-block mode (`#M>"..."`).
- If *subchar* is anything else, it follows the default behavior: it assumes
  literal-block mode unless there is a greater-than-sign preceding the string,
  in which case it switches to folding-block mode.
