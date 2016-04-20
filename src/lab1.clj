 (ns lab1)
 (def a 4)
 (+ (cond(and(odd? a)(> a 3)) a
     (< a 2) (* 10 a)
     :else (* a a)) a)

(cond(and(< a 10)(even? a)) (inc a):else a)


;question 3
 (ns user (:import java.lang.Math))

;compute the value of pi as 4 times atan (1)
