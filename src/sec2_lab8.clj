 (ns sec2-lab8)
;Write a macro (avg n1 n2 n3..) to compute the average of the numbers passed to it. It should work like this:
;user=> (avg 1 2 3 4 5 6)
;3.5
;user=> (avg 1)
;1.0
 (defmacro avg [lis]
  `(/ (reduce + ~lis) (count ~lis)))

 (println (avg  '(1 2 3 4 5 6)))
;Write a macro (avg-vect v) to compute the average of a vector passed to it. It should work like this:
;user=> (avg-vect [1 2 3 4 5 6])
;3.5
;user=> (avg [0])
;0.0
 (defmacro avg-vect [vec]
  `(/ (reduce + ~vec) (count ~vec)))
 (println (avg-vect '[1 2 3 4 5 6]))
;Write a macro (numif e p z n) to compute a three-way numeric if.
; If the expression e is positive computes p,
; if it is zero computes z and
; if it is negative computes n. It should work like this:
;user=> (map (fn [x] (numif x "pos" "zero" "neg")) (list 2 -4 5.6 (- 2 2)))
;("pos" "neg" "pos" "zero")
 (defmacro numif [e p z n]
  `(cond (pos? ~e) ~p
        (zero? ~e) ~z
        :else ~n))
 (println (map (fn [x] (numif x "pos" "zero" "neg")) (list 2 -4 5.6 (- 2 2))))

;Write a macro (ntimes n & body) that computes the value of body for n times.
; Make sure you avoid the "variable capture problem". It should work like this:
;user=> (ntimes 5 (print "hurra! ") (print "done "))
;hurra! done hurra! done hurra! done hurra! done hurra! done nil
 (defmacro ntimes [n & body]
  `(loop [c 0]
    (if (== c (- ~n 1))
     (do ~body)
     (recur (inc c)))))

 (println (ntimes 5 (print "hurra! ") (print "done ")))
;Write a macro (my-dotimes [x init end] & body) that computes the value of body for x going from init to end-1 in increments of 1. Here you again have to make sure to avoid the "variable capture problem". It should work like this:
;user=> (my-dotimes [x 0 4] (print x))
;0123nil

