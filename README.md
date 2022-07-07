# lispxmp.elisp
Automagical annotation of Lisp values like Ruby's xmpfilter.

## How to use

## Example
from: https://rubikitch.hatenadiary.org/entry/20090313/lispxmp

``` sample-before.el
(+ 3 4) ; =>
(current-buffer) ; =>

(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; =>
      (+ i 1) ; =>
)
```

After `M-x lispxmp`
``` sample-after.el
(+ 3 4) ; => 7
(current-buffer) ; => #<buffer 13-044055.el>

(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; => 10, 20, 30
      (+ i 1) ; => 2, 3, 4
)
```

``` more-example.el
(setq i 0)
(progn
  1                                     ; => 1
  )
i                                       ; => 0
(dotimes (x 3)
  i                                     ; => 0, 1, 2
  (incf i)
  i                                     ; => 1, 2, 3
  )

(+ 1                                    ; => 1
   (+ 3
      ;; => 3
      4)
   ;; => 7
   )
;; => 8
```
