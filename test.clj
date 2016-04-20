 (ns test)
(defn string->ascii [str]
 (map (fn [str] (Character/getNumericValue str)) str))

 (println (string->ascii "s"))