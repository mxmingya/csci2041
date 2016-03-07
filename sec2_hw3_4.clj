 (ns sec2-hw3-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modular inverse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn egcd "return list (s g t) s.t. a s + b t = g = gcd(a,b)" [a b]
  (letfn [(sign [x] (cond (pos? x) 1 (neg? x) -1 (zero? x) 0 :else nil))
          (abs [x] (if (neg? x) (- x) x))]
    ;; extended GCD
    (cond (or (< a 0) (< b 0)) (map * (list (sign a) 1 (sign b)) (egcd (abs a) (abs b)))
          (< a b) (reverse (egcd b a))
          (== b 0) (list 1 a 0)
          (== b 1) (list 0 1 1)
          :else (let [q (quot a b)
                      r (mod a b)
                      w (egcd b r)]
                  (list (last w)
                        (second w)
                        (- (first w) (* q (last w))))))))

(defn modular-inverse "compute b s.t. a*b = 1 mod n, if it exists." [a n]
  (if (== n 0)
    (/ a)
    (let [result (egcd n a)]
      (if (== (second result) 1) ;; check if inverse exists...
        (mod (last result) n)
        nil))))

;; fast-prime?

(defn modz "like mod, but if n=0, use infinity" [k n] (if (== n 0) k (mod k n)))
(defn expmod
  "compute base^exp mod m, fast. exp must be a non-neg integer.
  if m=0, then just compute base^exp (no mod)."
  [base exp m]
  (cond (== exp 0)   1
        (neg? exp)  'error
        (even? exp) (modz ((fn [x] (* x x)) (expmod base (quot exp 2) m))
                          m)
        :else       (modz (* base (expmod base (- exp 1) m))
                          m)))

(defn rand-bigint "crude random number of type bigint" [n]
  (bigint (bigdec (rand n))))

(defn fermat-test-once "do a Fermat prime test once" [n]
  (let [a (+ 1 (rand-bigint (- n 1)))
        e (expmod a n n)]
    (== a e)))

(defn fast-prime? "randomized prime tester, default 40 times" [n & opt-times]
  (loop [times (or (first opt-times) 40)]
    (cond (== times 0)          true
          (fermat-test-once n) (recur (- times 1))
          :else                false)))

(defn strong-prime? "check if n & (n-1)/2 are both prime" [n]
  (and (fast-prime? n) (fast-prime? (quot (- n 1) 2))))

(defn find-first "Starting at N, applying update each time, find first number satisfying pred"
  ([pred N ] (find-first pred N inc))
  ([pred N update] (if (pred N) N (recur pred (update N) update))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def p 6240322667N)    ; need long integer
(def q 6240323147N)    ; need long integer
(def n (* q p))
(def m (* (- p 1) (- q 1)))
;;;part 1
(defn find-3 [m]
  (let [e (+ 2 (rand-bigint (- m 2)))]
    ;;wait to be modified
    (if (and (== (second (egcd e m)) 1) (not= (modular-inverse n m) nil))
      (if (not= e 0)
        e
        (find-3 m))
      (find-3 m))))

(def e (find-3 m))

(def d (modular-inverse e m))

;;;;;part 2 key distribution

 (defn make-public-key [n e] {:mod n :exp e})

 (defn public-mod [pk] (get pk :mod))

 (defn public-exp [pk] (get pk :exp))

 (defn publickey (make-public-key n e))

 (defn make-private-key [ n e] {:mod n :exp d})

 (defn private-mod [pk] (get make-public-key :mod))

 (defn private-exp [pk] (get make-public-key :exp))

 (defn privatekey (make-private-key n d))


;;part 3 Encrption of a word

;helper
(defn string->ascii [str]
  (map (fn [str] (Character/getNumericValue str)) str))
;helper
(defn ascii-num91 [lis]
  (loop [c 0 result 0]
    (if (== c (count lis))
      result
      (recur (inc c)
             (+ result (* (nth lis c) (Math/pow 91 (- (count lis) (inc c)))))
             ))))


 (defn encrypt-num [number publickey]
   (let  [pn (expmod number (public-exp publickey) (public-mod publickey))]
     (if (> pn (public-mod publickey))
       nil
       pn)))

 (defn encrypt-word [plaintext publickey]
   (let [pn (ascii-num91 (string->ascii plaintext))]
     (encrypt-num pn publickey)))

;part 4 Decryption of a word
(defn decrypt-word [cyphertext private-key]
  (let [ct (expmod cyphertext (private-exp privatekey) (private-mod privatekey))]
    ct))

;part 5 Encryption of a sentence
 (defn encrypt-msg [msg publickey]
   (let [msg (seq msg)]
     (loop [c 0 result 0]
       (if (== c count msg)
         result
         (recur (inc c) (+ result (encrypt-word (nth msg c) publickey)))))))

;part 6 Decryption of a sentence
 (defn decrypt-msg [msg privatekey]
   (loop [c 0 current (nth msg c) result (list nil)]
     (if (== (count msg) c)
       (apply str result)
       (recur (inc c) current (concat result (decrypt-word current privatekey))))))
