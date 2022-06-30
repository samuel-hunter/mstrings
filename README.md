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

```lisp
$ cd ~/common-lisp/ # Or wherever you store your definitions
$ git clone https://git.sr.ht/~shunter/parsnip
```

To use Y-string macros, call `(syntax:use-syntax :mstrings)` at the beginning
of every file:

```lisp
* (require :mstrings)
* (syntax:use-syntax :mstrings)

* #M"Hello, world!"
* "Hello, world!"
```

## Features

M-strings reads empty lines as a single newline:

```lisp
* (princ #M"Hello

            World")
Hello

World
```

M-strings respects escaped characters and, outside for newlines, treats them line non-whitespace characters:

```lisp
* (princ #M"keys:
           \  foo: \"apple\"
           \  bar: \"banana\"")
keys:
  foo: "apple"
  bar: "banana"
```

A backslash followed by a newline merges two lines together, useful for very
long lines:

```lisp
* (princ #M"NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW\
            2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===")
NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===
```

Preceding the string with a `>` can set the reader into "folding mode", where
multiple lines in source are folded into one line, joined with spaces:

```lisp
* (princ #M>"The quick brown fox
             jumps over
             the lazy dog.

             Sphinx of black quartz,
             judge my vow!")
The quick brown fox jumps over the lazy dog.
Sphinx of black quartz, judge my vow!
```

## API

### [Function] **use-mstrings** *&optional subchar readtable*

Add the M-string read macro to the sharpsign dispatch character. If not provided, *subchar* is `#\Y`, and *readtable* is the current readtable, `*readtable*`.
