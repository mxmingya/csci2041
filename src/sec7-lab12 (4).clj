(with-open [r (clojure.java.io/input-stream "tmpdata.txt")]
        (doseq [line (line-seq r)] (prn line)))
;;OUtput
;ArityException Wrong number of args (1) passed to: core$re-seq  clojure.lang.AFn.throwArity (AFn.java:437)
;user=> (with-open [rdr (reader "tmpdata.txt")](doseq [line (line-seq rdr)](prn line)))
;"199.72.81.55 - - [01/Jul/1995:00:00:01 -0400] \"GET /history/apollo/ HTTP/1.0\" 200 6245"
;"unicomp6.unicomp.net - - [01/Jul/1995:00:00:06 -0400] \"GET /shuttle/countdown/ HTTP/1.0\" 200 3985"
;"199.120.110.21 - - [01/Jul/1995:00:00:09 -0400] \"GET /shuttle/missions/sts-73/mission-sts-73.html HTTP/1.0\" 200 4085"
;"burger.letters.com - - [01/Jul/1995:00:00:11 -0400] \"GET /shuttle/countdown/liftoff.html HTTP/1.0\" 304 0"
;"199.120.110.21 - - [01/Jul/1995:00:00:11 -0400] \"GET /shuttle/missions/sts-73/sts-73-patch-small.gif HTTP/1.0\" 200 4179"
;"burger.letters.com - - [01/Jul/1995:00:00:12 -0400] \"GET /images/NASA-logosmall.gif HTTP/1.0\" 304 0"
;"burger.letters.com - - [01/Jul/1995:00:00:12 -0400] \"GET /shuttle/countdown/video/livevideo.gif HTTP/1.0\" 200 0"
;"205.212.115.106 - - [01/Jul/1995:00:00:12 -0400] \"GET /shuttle/countdown/countdown.html HTTP/1.0\" 200 3985"
;"d104.aa.net - - [01/Jul/1995:00:00:13 -0400] \"GET /shuttle/countdown/ HTTP/1.0\" 200 3985"
;"129.94.144.152 - - [01/Jul/1995:00:00:13 -0400] \"GET / HTTP/1.0\" 200 7074"
;"unicomp6.unicomp.net - - [01/Jul/1995:00:00:14 -0400] \"GET /shuttle/countdown/count.gif HTTP/1.0\" 200 40310"
;"unicomp6.unicomp.net - - [01/Jul/1995:00:00:14 -0400] \"GET /images/NASA-logosmall.gif HTTP/1.0\" 200 786"
;"unicomp6.unicomp.net - - [01/Jul/1995:00:00:14 -0400] \"GET /images/KSC-logosmall.gif HTTP/1.0\" 200 1204"
;"d104.aa.net - - [01/Jul/1995:00:00:15 -0400] \"GET /shuttle/countdown/count.gif HTTP/1.0\" 200 40310"
;"d104.aa.net - - [01/Jul/1995:00:00:15 -0400] \"GET /images/NASA-logosmall.gif HTTP/1.0\" 200 786"
;"d104.aa.net - - [01/Jul/1995:00:00:15 -0400] \"GET /images/KSC-logosmall.gif HTTP/1.0\" 200 1204"
;"129.94.144.152 - - [01/Jul/1995:00:00:17 -0400] \"GET /images/ksclogo-medium.gif HTTP/1.0\" 304 0"
;"199.120.110.21 - - [01/Jul/1995:00:00:17 -0400] \"GET /images/launch-logo.gif HTTP/1.0\" 200 1713"
;"ppptky391.asahi-net.or.jp - - [01/Jul/1995:00:00:18 -0400] \"GET /facts/about_ksc.html HTTP/1.0\" 200 3977"
;"net-1-141.eden.com - - [01/Jul/1995:00:00:19 -0400] \"GET /shuttle/missions/sts-71/images/KSC-95EC-0916.jpg HTTP/1.0\" 200 34029"







(with-open [r (clojure.java.io/input-stream "tmpdata.txt")]
  (doseq [line (line-seq r)]
    (prn (re-seq #"%d+ . -" line))))

;;Test case
;("199.72.81.55 - -")
;("unicomp6.unicomp.net - -")
;("199.120.110.21 - -")
;("burger.letters.com - -")
;("199.120.110.21 - -")
;("burger.letters.com - -")
;("burger.letters.com - -")
;("205.212.115.106 - -")
;("d104.aa.net - -")
;("129.94.144.152 - -")
;("unicomp6.unicomp.net - -")
;("unicomp6.unicomp.net - -")
;("unicomp6.unicomp.net - -")
;("d104.aa.net - -")
;("d104.aa.net - -")
;("d104.aa.net - -")
;("129.94.144.152 - -")
;("199.120.110.21 - -")
;("ppptky391.asahi-net.or.jp - -")
;("net-1-141.eden.com - -")
;nil


 (with-open [r (clojure.java.io/input-stream "tmpdata.txt")]
   (doseq [line (line-seq r)]
     (prn (re-seq #"\d{1,6}$" line))))
;;output
;("6245")
;("3985")
;("4085")
;("0")
;("4179")
;("0")
;("0")
;("3985")
;("3985")
;("7074")
;("40310")
;("786")
;("1204")
;("40310")
;("786")
;("1204")
;("0")
;("1713")
;("3977")
;("34029")
;nil


