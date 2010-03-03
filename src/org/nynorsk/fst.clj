(ns org.nynors.fst
  (:use [clojure.set]))

(defn generate
  "An FST generator"
  [inp transd]
  :hello)

(defn epsilon
  "The null transducer"
  [in]
  (if (empty? in)
    #{()}
    #{in}))

(defn fst-sym [sym]
  (fn [in]
    (if (= (first in) sym)
      #{(rest in)}
      nil)))

(defn fst-or
  "Disjunction operator"
  ([] epsilon)
  ([t] t)
  ([t & ts]
     (fn [in]
       (if-let [tails (t in)]
	 (let [result (mapcat (apply fst-cat ts) tails)]
	   (if (seq result)
	     (set result)))))))

(defn fst-cat
  "Concatenation operator"
  ([] epsilon)
  ([t] t)
  ([t & ts]
     (fn [in]
       (if-let [tails (t in)]
	 (let [result (mapcat (apply fst-cat ts) tails)]
	   (if (seq result)
	     (set result)))))))

