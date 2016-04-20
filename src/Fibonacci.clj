(ns Fibonacci)
;compute nth of fibonacci
(defn fibonacci [n]
 (cond (== n 1) 1
       (== n 0) 1
       :else (+ (fibonacci (dec n)) (fibonacci (- n 2)))))

(println (fibonacci 4))
(println (fibonacci 10))
(println (fibonacci 37))

