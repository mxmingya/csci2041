 (ns sec2-lab10)
;1.write a macro to define car to be a synomym for first
;example: (car (list 1 2 3)) => 1
(defmacro car [lst]
  `(nth ~lst 0))

(println (car '(1 2 3)))

 ;write a macro for if-else structure, including if-structure without else statement
(defmacro my-if [con then a & args]
  (if (= then 'then)
     ;"drama starts here"
     (if (= 'else (first args))
       `(if ~con
          ~a
           ~(second args))
       `(if ~con ~a)) ;which is same as (if con a)
     "error!!!"))

(println (my-if (odd? 1) then 1 else 2))
(println (my-if (odd? 2) then 1 else 2))
(println (my-if (odd? 2) then 1))

 ;3. a lazy-seq to interleave two lists
(defn lazy-interleave [lst1 lst2]
  (concat (list (first lst1) (first lst2)) (lazy-seq (lazy-interleave (rest lst1) (rest lst2)))))

(println (take 10 (lazy-interleave '(1 2 3) '(a b c))))

 ;4. a lazy-seq of random number smaller than n
(defn lazy-rand [n]
  (lazy-seq (cons (rand-int n) (lazy-rand n))))

(println (take 10 (lazy-rand 123)))


