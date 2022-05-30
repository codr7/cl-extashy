## cl-extashy
#### Extensible hash tables in Common Lisp

```
  (let ((tbl (new-table))
        (n 100))
    (dotimes (i n)
      (setf (find-key tbl i) i))

    (assert (= (key-count tbl) n))

    (dotimes (i n)
      (assert (= (find-key tbl i) i)))

    (dotimes (i n)
      (remove-key tbl i))

    (dotimes (i n)
      (assert (null (find-key tbl i))))

    (assert (zerop (key-count tbl))))
```

### intro
cl-extashy aims to provide an exstensible and flexible hash table implementation in Common Lisp.

### design
Tables are implemented as vectors of vectors of pairs.
The first level is slots, which has a static size set at table instantiation time, alternatively resize time.
The second level, which is used for chaining, is lazily allocated and adjustable.

### equality
cl-estashy provides an extensible, universal equality predicate named `^=`, which is the default for new tables but may be overridden by passing an `:eq`-argument to the constructor.

### hashing
cl-extashy provides a generic `hash`-function that may be implemented to override the behavior, it defaults to `sxhash`.

### speed
cl-extashy is currently around twice as slow as built in hash tables in SBCL, but I suspect it's possible to squeeze that further by profiling and declaring types.

```
CL-USER> (extashy:bench 1000000)
Evaluation took:
  0.681 seconds of real time
  0.678242 seconds of total run time (0.635942 user, 0.042300 system)
  [ Run times consist of 0.126 seconds GC time, and 0.553 seconds non-GC time. ]
  99.56% CPU
  1,766,664,986 processor cycles
  120,218,896 bytes consed
  
Evaluation took:
  0.261 seconds of real time
  0.259561 seconds of total run time (0.217786 user, 0.041775 system)
  [ Run times consist of 0.121 seconds GC time, and 0.139 seconds non-GC time. ]
  99.62% CPU
  676,659,146 processor cycles
  114,202,720 bytes consed
```

### todo
- Add `do-table`-macro.
- Add support for manual resizing via `(setf slot-count)`.