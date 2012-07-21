(
 (fix (lambda (f a)
        (cond (< a 3)       ;; if a==1 or a==2
              1               ;; return 1
              (+
                (f (- a 2)) ;; recurs
                (f (- a 1))
                )
              )
        ))
 5
 )

