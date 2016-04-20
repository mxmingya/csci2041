 (ns sec2-hw6)
(use 'clojure.java.io)

 (def countOfIP (atom 0))
 (defn count-IP [line]
  "count the number of request that does not have a hostname"
  (if (not (nil? (re-find #"^\d{1,3}[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}\s[-]\s[-]" line)))
   (do
    ;(println line)
       (swap! countOfIP inc))))

 (def PIP (atom '()))
 (defn find-private-IP [line]
  "Find in the file all the requests that were made from a private IP address,
  and print them out"
  (cond
   (not (nil? (re-find #"^127[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}\s[-]\s[-]" line))) (swap! conj PIP line)
   (not (nil? (re-find #"^10[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}\s[-]\s[-]" line))) (swap! conj PIP line)
   (not (nil? (re-find #"^192[.]168[.]\d{1,3}[.]\d{1,3}\s[-]\s[-]" line))) (swap! conj PIP line)
   (not (nil? (re-find #"^192[.]88[.]99[.]\d{1,3}\s[-]\s[-]" line))) (swap! conj PIP line)
   ))


(with-open [rdr (reader "/home/maxxx950/csci2041-s16/NASA_access_log_Jul95_short")]
 (doseq [line (line-seq rdr)]
  (count-IP line)))

(with-open [rdr (reader "/home/maxxx950/csci2041-s16/NASA_access_log_Jul95_short")]
 (doseq [line (line-seq rdr)]
  (find-private-IP line)))

 (println countOfIP)
(println PIP)