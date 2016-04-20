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
