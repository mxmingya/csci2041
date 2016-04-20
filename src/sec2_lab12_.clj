 (ns sec2-lab12-)
(with-open [rdr (clojure.java.io/input-stream "tmpdata.txt")]
  (doseq [line (line-seq rdr)]
    (prn line)))
(with-open [r (clojure.java.io/input-stream "tmpdata.txt")]
  (doseq [line (line-seq r)]
    (prn (re-seq #"%d+ . -" line))))
(with-open [r (clojure.java.io/input-stream "tmpdata.txt")]
  (doseq [line (line-seq r)]
    (prn (re-seq #"\d{1,6}$" line))))