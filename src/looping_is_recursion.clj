(ns looping-is-recursion)

(defn power [base exp]
       (let [helper (fn [acc base exp]
             (if (zero? exp)
              acc
              (recur (* acc base) base (dec exp))))]
       (helper 1 base exp)))


(defn last-element [a-seq]
  (let [helper (fn [a-seq elem]
                (if (empty? a-seq)
                  elem
                  (recur (rest a-seq) (first a-seq))))]
    (helper a-seq nil)))


(defn seq= [seq1 seq2]
  (let [helper (fn [sq1 sq2]
                 (if (not (= (count sq1) (count sq2)))
                   false
                   (cond
                    (and (empty? sq1) (empty? sq2)) true
                    (not (= (first sq1) (first sq2))) false
                    :else (recur (rest sq1) (rest sq2)))))]
        (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [index 0
         sq    a-seq]
    (if (empty? sq)
      nil
      (cond
       (= (pred (first sq)) true) index
       :else (recur (inc index) (rest sq))))))


(defn avg [a-seq]
  (loop [sq   a-seq
         sum  0
         cnt  0]
    (if (empty? sq)
      (cond
       (= cnt 0) nil
       :else (/ sum cnt))
      (recur (rest sq) (+ sum (first sq)) (inc cnt)))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))


(defn parity [a-seq]
  (loop [odd-set  #{}
         sq       a-seq]
    (if (empty? sq)
      odd-set
      (recur (toggle odd-set (first sq)) (rest sq)))))


(defn fast-fibo [n]
  (loop [fibo-seq [0 1]]
    (cond
     (> (count fibo-seq) n) (nth fibo-seq n)
     :else (recur (conj fibo-seq (+ (first (take-last 2 fibo-seq)) (second (take-last 2 fibo-seq))))))))


(defn contains-duplicates? [a-seq]
  (cond
   (= (count (set a-seq)) (count a-seq)) false
   :else true))

(defn cut-at-repetition [a-seq]
  (loop [new-vec []
         sq      a-seq]
    (if (empty? sq)
      new-vec
      (cond
       (contains-duplicates? (conj new-vec (first sq))) new-vec
       :else (recur (conj new-vec (first sq)) (rest sq))))))


