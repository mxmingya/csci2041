 (ns sec2-hw6)
(use 'clojure.java.io)

 (def countOfIP (atom 0))
 (defn count-IP [line]
  "count the number of request that does not have a hostname"
  (if (not (nil? (re-find #"^\d{1,3}[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}\s[-]\s[-]" line)))
   (do
    ;(println line)
       (swap! countOfIP inc))))

(def requestOnDate (atom 0)) ;;a global variable to record the requests received from 4th to 5th July
(defn count-request-on-dates [line]
 "count the number of request that received on 4th to 5th July"
 (if (not (nil? (re-find #"\d[4-5][/]Jul[/]1995[:]\d{2}[:]\d{2}[:]\d{2}/s[-]0400" line)))
  (do
   ;(println line)
   (swap! requestOnDate inc))))

(def requestOnDate_ (atom 0)) ;;a global variable to record the requests received between 10am and 10:30 am on 5th of July
(defn count-hosts [line]
 "count the number of request that between 10:00pm and 13:30pm on 5th of July"
 (if (not (nil? (re-find #"05[/]Jul[/]1995[:]22[:]\d[00-30][:]\d{2}\s[-]0400" line)))
  (do
   ;(println line)
   (swap! requestOnDate_ inc))))

(def server_errors (atom ()))
(defn find-server-error [line]
 "count the number of request that between 10:00pm and 13:30pm on 5th of July"
 (if (not (nil? (re-find #"HTTP[/]1[.]0[/]\s5\d{1,2}\s\d{1,6}$" line)))
  (do
   ;(println line)
   (swap! conj server_errors line))))

(def request_redirect (atom 0))
(defn count-redirect [line]
 "count the number of request that were redirected"
 (if (not (nil? (re-find #"HTTP[/]1[.]0[/]\s3\d{1,2}\s\d{1,6}$" line)))
  (do
   ;(println line)
   (swap! request_redirect inc))))

(def big_requiest (atom 0))
(defn count-morethan50000 [line]
 "count the number of request that between 10:00pm and 13:30pm on 5th of July"
 (if (not (nil? (re-find #"HTTP[/]1[.]0[/]\s5\d{1,2}\s\d[5-9]\d{1,5}$" line)))
  (do
   ;(println line)
   (swap! conj server_errors line))))

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
  (count-IP line)
  (find-private-IP line)
  (count-request-on-dates line)
  (count-hosts line)
  (find-server-error line)
  (count-redirect line)
  (count-morethan50000 line)))

 (println countOfIP)
(println PIP)