(ns looping-is-recursion)

;(defn power [base exp]
;  ":(")

(defn power-helper [base r n]
  (if (= n 0)
    r
    (recur base (* r base) (- n 1))))

;(defn power [base exp]
;  (cond
;    (= base 0)
;    0
;    (= exp 0)
;    1
;    (= exp 1)
;    base
;    (>= exp 2)
;    (power-helper base 1 exp)))

(defn power [base exp]
  (cond
    (= base 0)
    0
    (= exp 0)
    1
    (= exp 1)
    base
    (>= exp 2)
    (loop [n exp r 1]
      (if (= n 0)
        r
        (recur (- n 1) (* r base))))
    ))

;(defn last-element [a-seq]
;  ":(")

(defn last-element [a-seq]
  (if
      (empty? a-seq) nil
      (loop [n (count a-seq)
             a a-seq]
        (if (= n 1)
          (first a)
          (recur (- n 1) (next a))
          )))
  )

;(defn seq= [seq1 seq2]
;  ":(")

(defn seq= [seq1 seq2]
  (loop [a seq1 b seq2]
    (cond
      (and (empty? a) (empty? b))
      true
      (or (empty? a) (empty? b))
      false
      (not= (first a) (first b))
      false
      :else
      (recur (next a) (next b))))
  )

;(defn find-first-index [pred a-seq]
;  ":(")

(defn find-first-index [pred a-seq]
  (loop [n 0 s a-seq]
    (cond
      (empty? s)
      nil
      (pred (first s))
      n
      :else
      (recur (+ n 1) (rest s))
      )))

;(defn avg [a-seq]
;  -1)

(defn avg [a-seq]
  (loop [n 0 ps 0 s a-seq]
    (cond
      (empty? s)
      (/ ps n)
      :else
      (recur (+ n 1) (+ ps (first s)) (rest s))
      ))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

;(defn parity [a-seq]
;  ":(")

(defn parity [a-seq]
  (loop [r #{} s a-seq]
    (cond
      (empty? s)
      r
      :else
      (recur (toggle r (first s)) (rest s))
      ))
  )

;(defn fast-fibo [n]
;  ":(")

(defn fast-fibo [n]
  (cond
    (= n 0N)
    0N
    (= n 1N)
    1N
    (>= n 2N)
    (loop [x 0 y 1N z 1N i 2N]
      (cond
        (= n i)
        z
        :else
        (recur y z (+ y z) (+ i 1N)))))
  )

;(defn cut-at-repetition [a-seq]
;  [":("])

(defn cut-at-repetition [a-seq]
  (loop [a (vec a-seq) v [] s #{}]
    (cond
      (empty? a)
      v
      (> (count (conj s (first a))) (count s))
      (recur (rest a) (conj v (first a)) (conj s (first a)))
      :else
      v
      )
    )
  )

