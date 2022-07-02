# Mstrings - Pretty Multliline Strings for Common Lisp
[![builds.sr.ht status](https://builds.sr.ht/~shunter/mstrings/commits/master/test.yml.svg)](https://builds.sr.ht/~shunter/mstrings/commits/master/test.yml)

**Mstrings** defines a reader macro for strings with a small handful of
features to provide visually appealing multiline blocks. An M-string:

- Trims leading whitespace from lines
- Respects escaped whitespace characters and keeps them untrimmed
- Concatenates lines together when joined by an escaped Newline
- Alternative mode to fold multiple non-empty lines into one joined by spaces

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

## Install and Enable It

M-strings' sole dependency is [named-readtables](https://named-readtables.common-lisp.dev/), available on Quicklisp.

Install M-strings locally, until it is added to Quicklisp:

```sh
$ cd ~/common-lisp/ # Or wherever you store your systems
$ git clone https://git.sr.ht/~shunter/mstrings
```

This library uses named readtables to expose the reader:

```lisp
* (use-package :named-readtables)
* (in-readtable mstrings:mstring-syntax)

* (princ #M"Hello
            World!")
Hello
World!
```

## Features

M-strings trim leading whitespace from lines:

```lisp
* (princ #M"Hello
            World!")
Hello
World!
```

M-strings respect escaped characters and keeps them untrimmed:

```lisp
* (princ #M"keys:
           \  foo: \"apple\"
           \  bar: \"banana\"")
keys:
  foo: "apple"
  bar: "banana"
```

M-strings concatenates lines together when they're joined by an escaped Newline - useful for very long single-line values:

```lisp
* (princ #M"NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW\
            2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===")
NB2HI4DTHIXS653XO4XHS33VOR2WEZJOMNXW2L3XMF2GG2B7OY6WIULXGR3TSV3HLBRVC===
```

By default, M-strings are read in "literal-block mode", where newlines are read
as literal newlines. Prefixing a string with a `>` sets the reader to
"folding-block mode", where multiple non-empty lines are folded into one,
joined by spaces:

```lisp
* (princ #M>"The quick brown fox
             jumps over
             the lazy dog.

             Sphinx of black quartz,
             judge my vow!")
The quick brown fox jumps over the lazy dog.
Sphinx of black quartz, judge my vow!

* (princ #M>"Escaped newlines
             \
             still prevent
             \
             line breaks")
Escaped Newlines still prevent line breaks
```

## Shorthand notation: `#"..." and #>"..."`

The reader macro function understands `#"..."` and `#>"..."` as shorthands for
`#M"..."` and `#M>"..."`, respectively. They're defined separately to prevent
macro collisions from other systems:

```lisp
* (in-readtable mstrings:shorthand-mstring-syntax)
* ;; Or, for both shorthand and long-hand:
* (in-readtable mstrings:full-mstring-syntax)

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

### [Readtable] **mstring-syntax**

The standard readtable with the longhand mstring syntax:

- `#M"..."` - Literal-block mstring
- `#M>"..."` - Folding-block mstring

### [Readtable] **shorthand-mstring-syntax**

The standard readtable with both `#"` and `#>` dispatch character pairs bound
to **mstring-reader**:

- `#"..."` - Literal-block mstring
- `#>"..."` - Folding-block mstring

### [Readtable] **full-mstring-syntax**

The standard readtable with additions from both **mstring-syntax** and
**shorthand-mstring-syntax**:

- `#M"..."`, `#"..."` - Literal-block mstring
- `#M>"..."`, `#>"..."` - Folding-block mstring
