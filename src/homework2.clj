(ns homework2)
;replace with sum
;user=> (replace-with-sum '[a 4 3 b])
;(a b 7)   ; 4=3=7
;user=> (replace-with-sum '[a b c])
;(a b c)
;user=> (replace-with-sum '[2 5 1])
;(8)
(defn replace-with-sum [vec]
  (concat (remove integer? vec) (conj '() (reduce + (filter integer? vec)))))
(println (replace-with-sum ["a" "b" "c" 1 2 3]))
;[15 points] Write a procedure running-sum that given a vector of numbers produces a list
; where each element in the list is the sum of all corresponding elements up to that
; point in the original vector. It should work like this:
;user=> (running-sum [1 3 9 2])
;(1 4 13 15) ;where 1+3 =4, 1+3+9=13, 1+3+9+2=15

;user=> (running-sum [2 2 2 2 2])
;(2 4 6 8 10)

      (defn running-sum [vec]
        (reductions + vec))
(println (running-sum [1 3 9 2]))
(println (running-sum [2 4 6 8 10]))


; Write a procedure (expand t) that takes as argument a list.
; The elements of the list are either symbols or lists of two elements,
; a strictly positive integer (i.e. greater than 0) and a symbol.
; The integer is a repeat count, meaning that the symbol following it should be repeated that many times.
; The procedure should return a list where the repeated symbols are expanded,
; i.e. each sublist (< number > < symbol >) is replaced by a list with the symbol repeated < number > times. You cannot use any global variable, but you are free to use as many auxiliary procedures as you like. It should work like this:
;user=> (expand '(the (2 big) boat))
;(the (big big) boat)
;user=> (expand '((3 oh) wow))
;((oh oh oh) wow)
;user=> (expand '((1 this) is (2 very) nice))
;((this) is (very very) nice)


(defn expand [lis]
  (reverse (loop [c 0 result ()]
             (if (== c (count lis))
               result
               (recur (inc c) (if (not (seq? (nth lis c)))
                                (cons (nth lis c) result)
                                (cons (repeat (nth (nth lis c) 0) (nth (nth lis c) 1)) result))
                      ))
             )
           ))

(println (expand '((3 oh) wow)))
(println (expand '((1 this) is (2 very) nice)))


;[15 points] Write a procedure (factors range num),
; that takes a range and an integer as arguments
; and returns the elements in the range that are factors of num.
;user=>(factors (range 3 6) 12)
;(3 4)=
;user=>(factors (range 2 13) 50)
;(2 5 10)
;user=>(factors (range 4 7) 9)
;()


(defn factors [lis target]
  (loop [c 0
         output []]
    (if (== c (count lis))
      output
      (if (zero?  (mod target (nth lis c)))
        (recur (inc c) (conj output (nth lis c)))
        (recur (inc c) output)
        )
      )
    )
  )

(println (factors (range 3 6) 12))
(println (factors (range 2 13) 50))
(println (factors (range 4 7) 9))

;This part of the homework is to get you to start working on encoding words as numbers
; so we can later do encryption. For simplicity, the problem is broken into small parts.

;[10 points] Write a function (string->ascii str) to convert a word,
; written as a string, into a sequence integer as follows:
; transform the string into a list of the corresponing characters,
; then transform each character into the corrsponding ascii code and
; subtract 32 from each ascii code,
; 32 is the first printable ascii character that represents a blank space.
;user=> (string->ascii "ouch")
;(79 85 67 72)
;user=> (string->ascii "gulp")
;(71 85 76 80)
;user=> (string->ascii " ")
;(0)
(require '[clojure.string :as str])
(defn string->ascii [str]
  (map (fn [str] (Character/getNumericValue str)) str))



(println (string->ascii "ouch"))
(println (string->ascii "gulp"))
(println (string->ascii " "))


;[10 points] write a function (ascii->num96 seq) to transform each list of numbers into a single number
; by considering each of the numbers in the sequence as a digit in a representation with base 96.
; Suppose your number is (n1 n2 n3 n4).
; You will compute n1 * b^3 + n2 * b^2 +n3 * b + n4 or, equivalently,
;
;((n1*b + n2) *b + n3) *b + n4
  ;user=> (ascii-num96 '(79 85 67 72))
  ;70684008

(defn ascii-num96 [lis]
  (loop [c 0 result 0]
    (if (== c (count lis))
      result
     (recur (inc c)
            (+ result (* (nth lis c) (Math/pow 96 (- (count lis) (inc c)))))
            ))
    )
  )
(println (ascii-num96 '(79 85 67 72)))
  ;[10 points] Given a vector of words write a function (make-dict words) that
  ; encodes each word as done in the previous problem and returns a map of the words and
  ; their corresponding numbers.
  ; It should work like this:
  ;user=> (make-dict ["ouch" "gulp"])
  ;{"ouch" 70684008, "gulp" 63606992}
(defn make-dict [vec]
  (loop [c 0 result {}]
    (if (== c (count vec))
      result
    (recur (inc c)
           (assoc result
             (nth vec c)
             (ascii-num96 (string->ascii (nth vec c))))
           )
      )
    )
  )
(println (make-dict ["ouch" "gulf"]))
  ;[5 points] Finally, write a function (in-dict dictmap word) that
  ; searches for a word in the map created by the previous step
  ; and returns the encoding if the word is in the map or nil otherwise.
  ;user=> (in-dict (make-dict ["ouch" "gulp"]) "gulp")
  ;63606992
  ;user=> (in-dict (make-dict ["ouch" "gulp"]) "noway")
  ;nil
(defn in-dict [map str]
  (if (contains? map str)
    (ascii-num96 (string->ascii str))
    nil))
(println (in-dict (make-dict ["ouch" "gulp"]) "noway"))
(println (in-dict (make-dict ["ouch" "gulp"]) "gulp"))


;output:
;all test works as the sample

;(a b c 6)
;((oh oh oh) wow)
;((this) is (very very) nice)
;[3 4]
;[2 5 10]
;[]
;(24 30 12 17)
;(16 30 21 25)
;(-1)
;7.0684008E7
;{ouch 2.1511313E7, gulf 1.4434287E7}
;nil
;1.4434297E7













