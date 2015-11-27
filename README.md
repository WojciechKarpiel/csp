It's like Go.
```lisp
(define chan (make-channel))
(go (-> chan (+ 1 2 3)))
(<- chan)
; => 6

(-> chan (* 2 4)) ; deadlock
``` 

TODO:  
implement proper go-like "select" that can handle
recieving, sending and default case.
