(ns HW.homework1.homework1)
;[10 points] Write a function which returns the largest integer which is a multiple of 7 in a given range,
; extremes included, and nil if there is no multiple of 7.
; The range should be expressed as low end and high end, with both ends positive integer numbers.
; It should work like this:
;(maxmul7 1 20) = 14
;(maxmul7 20 35) = 35
;(maxmul7 1 6) = nil
;(maxmul7 7 10) = 7
(defn maxmul7 [min max]
  (cond
    (zero? (mod max 7)) max
    (= max min) (if (mod min 7)
                  min)
    :else (maxmul7 min (dec max)))
  )

(println (maxmul7 1 20))
(println (maxmul7 8 35))
(println (maxmul7 30 1000))

;Write a function to sum the cubes of the integers in a given range, extremes included.
; The range should be expressed as low end and high end, with both ends positive integer numbers.
; It should work like this:
;(sumcube 1 3) = 36
(defn cube [x]
  (reduce * (repeat 3 x)))

(defn sumcube [min max]
  (loop [m max rs 0]
    (if (= m min)
      rs
      (recur (dec m) (+ rs (cube m))))                      ;it was (recur (dec max) (+ rs (cube max))) attention!!! should use the variable in loop function!!
    ))

(println (sumcube 1 2))
(println (sumcube 2 4))
(println (sumcube 3 10))

;Write a function that takes a vector of at least three elements and
; returns a vector (or a list) consisting of all the elements except the second one.
; It should work like this:
;(drop2nd [1 2 3 4])= [1 3 4]
; or (drop2nd [1 2 3 4])= (1 3 4)
(defn drop2nd
  [vector]
  (concat [(first vector)] (rest (rest vector))))

(println (drop2nd [1 2 3 4]))
(println (drop2nd [100 39 37 45 5]))
(println (drop2nd [120 119 110 911 121]))

;Write a program to check if a positive integer is perfect.
; A number is perfect if it is equal to the sum of its divisors,
; including 1 but not including the number itself. It should work like this:
;(perfect 6) = true
;(perfect 10) = false


(defn sumOfDivisor [n]
  (loop [s 0 div 1]
    (if (= div (dec n))
      s
       (recur (if (zero? (mod n div))
                (+ s div)
                s) (inc div)))))


(defn perfect [n]
  (if (= (sumOfDivisor n) n)
    "true"
    "false"))

(println (perfect 6))
(println (perfect 28))
(println (perfect 29))

;Write a function to compute the average of a sequence of numbers
; and one to compute the standard deviation.
; For the standard deviation use the sample standard deviation. They should work like this:
;(ave [1 2 3 4]) = 5/2 or
; (ave [1 2 3 4] = 2.5
; (stddev [1 2 3 4]) =1.29
(defn  ave [vector]
  (/ (apply + vector) (count vector)) )

(println (ave [1 2 3 4 5 6]))
(println (ave [10 2 3 4 5 6]))
(println (ave [189 33 47]))

(defn square [x]
  (reduce * (repeat 2 x)))

(println (square 2))                                        ;square works well

(defn step1 [vector]
  "compute the sum of the difference between average and each element"
  (loop [n 0 rs 0]
    (if (= n (count vector))
      rs
    (recur (inc n) (+ rs (square (- (nth vector n) (ave vector))))))
    ))

(defn stddev [vector]
  (Math/sqrt (/ (step1 vector) (count vector))))

(println (stddev [1 2 3 4 5]))
(println (stddev [19 25 37 46 55]))
(println (stddev [54 265 363 48 369]))

;Write a function that takes as input an element and a vector and
;returns the position of the first occurrence of the element in the vector
; or nil if the element is not in the vector. It should work like this:
;(posel [10 20 30 40] 30) = 2

(defn posel [vector n]
  (if (not(contains? vector n))
   nil;n stands for the number we are looking for
  (loop [x 0]                                        ;x stands for the position
    (if (= (get vector x) n) x
                (recur (inc x))))))

(println (posel [1 2 3 4 5] 2))                             ;this works!!!
(println (posel [1 2 3 4 5] 4))                             ;this works!!!
(println (posel [1 2890 33 4 56 9 117] 4))                             ;this works!!!








