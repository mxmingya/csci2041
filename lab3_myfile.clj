 (ns lab3_myfile)
;write a function that takes two sequences (lists or vectors) and
; returns a sequence with the elements not in common to the two original sequences.
; Assume there are no duplicate elements in each individual sequence.
; The order of the elements in the resulting list is not important. It should work like this:
;(remove-dupl [1 4 7 10 16][2 7 18 4]) = (1 10 16 2 18)
;(remove-dupl [1 8 16][2 18]) = (1 8 16 2 18)
;(remove-dupl [][2 18]) = (2 8)
 (defn remove-dupl [seq1 seq2]
  (loop [index 0]
   (if (not (contains? seq1 (get seq2 index)))
     (concat seq1 (get seq2 index))
     (recur (inc index))))
  seq1)

 (remove-dupl [1 2 3] [3 4 5])
 (println (remove-dupl [1 2 3] [3 4 5]))