# Y-Strings for Common Lisp

When writing strings that take multiple lines, for example within an extensive
docstring, it feels natural to align them up; however, the Lisp reader will
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

Y-strings allow a plain method to remove the whitespace while keeping content
aligned in the source code:

```lisp
* (defun grab-config (use-default)
    (if use-default
        #Y"foo=1
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
I do my main development on [Sourcehut](https://sr.ht/~shunter/ystrings/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/ystrings).

## Usage

Y-strings uses [cl-syntax](https://github.com/fukamachi/cl-syntax) to expose the
reader macro. Install it through Quicklisp, or manually alongside its
dependents [trivial-types](https://github.com/m2ym/trivial-types) and
[named-readtables](https://github.com/kmizumar/named-readtables/).

Install Y-strings locally, until it is added to Quicklisp:

```lisp
$ cd ~/common-lisp/ # Or wherever you store your definitions
$ git clone https://git.sr.ht/~shunter/parsnip
```

To use Y-string macros, call `(syntax:use-syntax :ystrings)` at the beginning
of every file:

```lisp
* (require :ystrings)
* (syntax:use-syntax :ystrings)

* #Y"Hello, world!"
* "Hello, world!"
```

## Features

Y-strings reads empty lines as a single newline:

```lisp
* (princ #Y"Hello

            World")
Hello

World
```

Y-strings respects escaped characters and, outside for newlines, treats them line non-whitespace characters:

```lisp
* (princ #Y"keys:
           \  foo: \"apple\"
           \  bar: \"banana\"")
keys:
  foo: "apple"
  bar: "banana"
```

A backslash followed by a newline merges two lines together, useful for very
long lines:

```lisp
* (princ #Y"NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW\
            2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===")
NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===
```

Preceding the string with a `>` can set the reader into "folding mode", where
multiple lines in source are folded into one line, joined with spaces:

```lisp
* (princ #Y>"The quick brown fox
             jumps over
             the lazy dog.

             Sphinx of black quartz,
             judge my vow!")
The quick brown fox jumps over the lazy dog.
Sphinx of black quartz, judge my vow!
```

## API

### [Function] **use-ystrings** *&optional subchar readtable*

Add the ystring read macro to the sharpsign dispatch character. If not provided, *subchar* is `#\Y`, and *readtable* is the current readtable, `*readtable*`.
