(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n] 
                 (if (= 0 n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [f r] 
                 (if (empty? r)
                   f
                   (recur (first r) (rest r))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [s1 seq1
         s2 seq2] 
    (cond
     (and (empty? s1) (empty? s2)) true
     (not (= (first s1) (first s2))) false
	 :else (recur (rest s1) (rest s2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) index
     :else (recur (inc index) (rest s)))))

(defn avg [a-seq]
  (/
   (loop [sum 0
          s a-seq]
    (if (empty? s)
      sum
      (recur (+ sum (first s)) (rest s))))
   (count a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                (if (contains? a-set elem) 
                 (disj a-set elem)
                 (conj a-set elem)))]
    (loop [ps #{}
           s a-seq]
      (if (empty? s)
        ps
        (recur (toggle ps (first s)) (rest s))))))

(defn fast-fibo [n]
 (if (= n 0) 0
  (loop [f-2 0
         f-1 1
         loops (dec n)]
    (if (= loops 0)
      f-1
      (recur f-1 (+ f-2 f-1) (dec loops))))))

(defn cut-at-repetition [a-seq]
 (loop [vc []
        sq a-seq]
   (let [f (first sq)
         isf (fn [x] (= x f))]
    (if (or (some isf vc) (empty? sq))
      vc
      (recur (conj vc f) (rest sq))))))