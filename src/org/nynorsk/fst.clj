(ns org.nynorsk.fst
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

(defn fst-cat
  "Concatenation operator"
  ([] epsilon)
  ([t] t)
  ([t & ts]
     (fn [in]
       (if-let [tails (t in)]
	 (let [result (mapcat (apply fst-cat ts) (seq tails))]
	   (if (seq result)
	     (set result)))))))

(defn fst-or
  "Disjunction operator"
  ([] (fn [_] nil))
  ([t] t)
  ([t & ts]
     (fn [in]
       (set (filter identity
		    (for [t (cons t ts)]
		      (if-let [result (t in)]
			(apply concat result))))))))

(def a (fst-sym \a))
(def b (fst-sym \b))
(def c (fst-sym \c))
(def d (fst-sym \d))
(def ab (fst-cat a b))
(def cool (fst-cat c (fst-sym \o) (fst-sym \o) (fst-sym \l)))
