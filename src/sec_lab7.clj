 (ns sec-lab7)
(defn lazy-interleave [lst1 lst2]
  (lazy-seq (concat (list (first lst1) (first lst2)) (lazy-interleave (rest lst1)(rest lst2)))))

;(println (take 5 (lazy-interleave '(1 2 3 4 5) '(1 2 3 4 5))))

(defn lazy-interleave1 [lst1 lst2]
  (if (not (or  (empty? lst1) (empty? lst2)))
    (cons (first lst1) (cons (first lst2) (lazy-seq (lazy-interleave (rest lst1) (rest lst2)))))))
;(println (take 17 (lazy-interleave1 '[1 2 3] '[a b c])))

(defn lazy-random [n]
  (lazy-seq (cons (rand-int n) (lazy-random n))))

;(println (take 5 (lazy-random 123)))

;a lazy-seq that each element is the summation of all elements that ahead of it

(defn lazy-scale [lst factor]
  (map #(* factor %) lst)) ;does it create a lazy-sequence? no because it doesnt return nil after out-of-range

(defn lazy-scale1 [lst factor]
  (cons (#(* factor %) (first lst)) (lazy-seq (lazy-scale (rest lst) factor))))
(println (lazy-scale1 '(1 2 3 4 5) 2))

;(println (take 6 (lazy-scale '(1 2 3) 2)))

(defn lazy-running-sum [v]
  (if (not (empty? v))
  (lazy-seq (cons (first v) (lazy-running-sum (cons (reduce + (take 2 v)) (next (next v))))))))

(defn lazy-running-sum1 [v]
  (if (not (empty? v))
  (cons (first v) (lazy-seq (lazy-running-sum1 (cons (reduce + (take 2 v)) (next (next v))))))))
;(println (take 50 (lazy-running-sum1 '[1 2 3 4 5 6 8 9])))

 ;use lazy-seq to remove all the duplicates in a list
(defn lazy-remove-dup [lst]
  (if (not (empty? lst))
  (cons (first lst) (lazy-seq (lazy-remove-dup (filter (fn [x] (not= x (first lst))) (rest lst)))))))

(defn lazy-remove-dup2 [lst]
  (if (not (empty? lst))
    (cons (first lst) (lazy-seq (lazy-remove-dup2 (remove (fn [x] (= x (first lst))) lst))))))
;(println (take 10 (lazy-remove-dup '(1 2 2 3 3 4 4))))

;(println (take 5 (lazy-remove-dup '(1 2 2 3 3 4 4))))

(defn lazy-fib [a b]
  (cons b (lazy-seq (fib a (+ a b)))))