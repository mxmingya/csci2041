 (ns lab2)
(:import [java.lang.Math])
(defn prime [num]
 (loop [div 2]
  ((when (< div (Math/sqrt num))
    ((if (not= (mod num div) 0))
      (inc num)
      :true)))
  (recur (inc div))))

 (prime 27)