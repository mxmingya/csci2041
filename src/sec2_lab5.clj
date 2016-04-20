 (ns sec2-lab5)
;Write a function (num-digit n) to compute the number of digits of a positive integer. For example:
(defn num-digits [num]
 (loop [c 0 num num]
  (if (< num 1)
   c
   (recur (inc c) (/ num 10)))))
(println "example from question1~~~~")
(println (num-digits 234))
(println (num-digits 2))

 (println "________________________________")


;For this question we will convert numbers to diffeent bases. We will represent any number not in decimal base as a list of its digits. So for the example, we will have
;(5 5) base 6 = 35 decimal = (3 8) base 9
;(1 1 3 1) base 4 = 93 decimal = (1 6 2) base 7
;There are two parts to this question:
;1. Write (from-dec n base) to convert an integer n from base 10 to the base specified, representing it as a list.
;2. Write (to-dec n base) to convert a list of digits in base base to an integer in base 10 (printed in decimal but stored internally in binary). It should work like this:


 (defn from-dec [n base]
  (let [ result '()]
  (loop [exp 0] (comment "this is the num of exponential part")
   (if (> (Math/pow base exp))
    (if (< (/ n (Math/pow base exp)) base)
     (conj result (/ n (Math/pow base exp)))
     (conj result base))
    ) (recur (inc exp)))))

 (defn to-dec [vecn base]
  )

(to-dec [1 1 3 1] 4)

(from-dec (to-dec [1 1 3 1] 4) 7)
(1 6 2)
(from-dec 1131 10)
(1 1 3 1)