(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp result]
                 (cond 
                   (zero? exp) 1
                   (= exp 1) result
                   :else (recur base (dec exp) (* result base))))]
    (helper base exp base)))
        
(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (count seq1) (count seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [indx 0
         s a-seq]
	  (cond
	    (empty? s) nil
	    (pred (first s)) indx
	    :else (recur (inc indx) (rest s)))))

(defn avg [a-seq]
  (let [total (count a-seq)]
    (loop [sum 0
           the-seq a-seq]
     (if (empty? the-seq)
       (/ sum total)
       (recur (+ sum (first the-seq)) (rest the-seq)))))) 

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
							  (if (contains? a-set elem)
							    (disj a-set elem)
							    (conj a-set elem)))]
    (loop [toggle-set #{}
           the-seq a-seq]
      (if (empty? the-seq)
        toggle-set
        (recur (toggle toggle-set (first the-seq))
               (rest the-seq))))))

(defn fast-fibo [n]
  (loop [i 2
         f-n 1
         f-n-1 1]
    (cond
      (== n 0) 0
      (== n 1) 1
      (== i n) f-n
      :else (recur (inc i) 
                    (+ f-n f-n-1) 
                    f-n))))
      

(defn cut-at-repetition [a-seq]
  (loop [seen-values #{}
         the-seq a-seq
         result []]
    (cond 
      (empty? the-seq) result
      (contains? seen-values (first the-seq)) result
      :else (recur (conj seen-values (first the-seq))
                   (rest the-seq)
                   (conj result (first the-seq))))))

