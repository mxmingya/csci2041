 (ns sec2-lab12)
;read the file from home directory
(def mytest (slurp "/home/maxxx950/csci2041-s16/test.txt"))

;Transform the text you have read into upper-case
; by calling clojure.string/upper-case.
(clojure.string/upper-case mytest)

;Write the upper-case version of the file
; you have read to a new file named copy-test.txt using the instruction spit.
;spit takes two arguments, the file name, written as a string, and a string containing the text to be written.
;After you have written the file copy-test.txt, check that its contents have been written correctly by looking at the file outside of clojure.
(spit "copy-test.txt" mytest)

 (spit "copy-test.txt" "to be continued" :append true)

(use 'clojure.java.io)

 (with-open [rdr (reader "test.txt")]
  (doseq [line (line-seq rdr)]
   (println line)))