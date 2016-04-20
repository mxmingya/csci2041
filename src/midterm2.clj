 (ns midterm2)
;2
(defn half-even [v]
  (loop [c 0 n []]
    (if (= c (count v))
      n
      (if (even? (get v c))
        (do (cons (/ (get v c) 2) n) (println "n is " n) (recur (inc c) n))
        (do (cons (get v c) n) (recur (inc c) n))))))

(println (half-even '[1 2 3 4 5 6 7]))