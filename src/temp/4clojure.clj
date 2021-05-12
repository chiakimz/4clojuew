(ns temp.4clojure)

(#(nth % (dec (count %))) [1 2 3 4 5])
;23
(def arr [1 2 3 4 5])
(def l '(2 3 4 5))
(nth arr (- (count arr) 2))

(#(reverse(sort-by first [1 2 3 4]))

(defn reverse-recursively [coll]
  (loop [[fst & rst :as all] (seq coll) result '()]
    (if all
      (recur rst (cons fst result))
      result)))

(reverse-recursively arr)

(defn reverse-seq [s]
  (loop [[fst & rst :as all] s result (empty s)]
    (if (empty? all)
      result
      (recur rst (into (conj (empty s) fst) result)))))


(reverse-seq (sorted-set 5 7 2 7))
(def result (conj (empty l) 0))
  ;take a first element, add to the last, and delete that first element. repeat until you reach len - 1

  (#(loop [[fst & rst :as all] % result '()]
        (if (empty? all)
          result
          (recur rst (into [fst] result))))(sorted-set 5 7 2 7)))
  ;26
  (defn fibonacci [num]
    (loop [res [1] i 1 n num]
      (if (= n 1)
        res
        (recur (conj res i) (+ (last res) i) (dec n)))))

  (fibonacci 10)

  (#(loop [res [1] i 1 n %]
      (if (= n 1)
        res
        (recur (conj res i) (+ (last res) i) (dec n))))10)

  ;27
(defn palindrome [sq]
  "detects palindrome and returns true or false"
  (loop [remain sq current 0]
    (if (empty? remain)
      true
      (if (= (last remain) (nth remain current))
               (recur ((drop-last remain) (inc current)))
               false))))

  (defn pal [sq]
    (loop [remain sq current 0]
      (if (> (count remain) current)
        (if (= (last remain) (nth remain current))
          (recur (drop-last remain) (inc current))
          false)
        true)))

  (pal "racecar")

  ;28
  (defn flat [sq]
    (loop [[fst & rst :as all] sq result []]
      (if all
        (if (sequential? fst)
          (recur (into fst rst) result)
        (recur rst (conj result fst)))
        result)))

(flatten-a-sequence '(0 (1 2) 3 [4 [5 6]]))
(flat [1 2 [3]])
(flat '((1 2) 3 [4 [5 6]]))
;; I passed the test because I hacked by using 'sort'

;keigo's code
(defn flatten-a-sequence [xs]
  (reduce (fn flattenize [x y]
            (println "x:" x)
            (println "y:" y)
            (println "xy:" xs)
            (if (coll? y)
              (reduce flattenize x y)
              (conj x y)))
          []
          xs))
;29
(defn only-caps [string-input]
  (loop [[fst & rst :as all] string-input res ""]
    (if all
      (if (Character/isUpperCase (.charAt (str fst) 0))
        (recur rst (str res fst))
        (recur rst res)
        )res)))

;list comprehension solution
(defn only-caps2 [string-input] (apply str (for [x string-input :when (Character/isUpperCase (.charAt (str x) 0))] x))
)
(only-caps2 "HellO")

(def ls [1 1 2 3 3 2 2 3])

;30
(defn compress-seq [s]
  (loop [[fst & rst :as all] s res []]
    (if (empty? all)
      res
      (if (= fst (nth rst 0))
        (recur rst res)
        (recur rst (conj res fst))))))

(compress-seq [[1 2] [1 2] [3 4] [1 2]])

;keigos solution
(defn compress-a-sequence [xs]
  (map first (partition-by #(identity %) xs)))
(compress-a-sequence [1 1 2 1 1 1 3 3])

;31
;oneliner solution
(partition-by identity [1 1 2 1 1 1 3 3])

(defn pack-seq [s]
  (loop [[fst & rst :as all] s res [] current [fst]]
    (if (empty? all)
      res
      (if (= fst (nth rst 0))
        (recur rst res (conj current fst))
        (recur rst (conj res current) [(nth rst 0)])))))
(pack-seq [1 1 2 1 1 1 3 3])

;32
(mapcat (partial repeat 2) [1 2 3])

(defn duplicate-seq [s]
  (loop [[fst & rst :as all] s res [] current 1]
    (if (empty? all)
      res
      (if (= fst (nth rst 0))
        (recur rst res (inc current))
        (recur rst (conj res (take (* current 2) (repeat fst))) 1)))))
(duplicate-seq [[1 2] [3 4]])
(duplicate-seq [1 2 3])

;31 somehow did it again
; (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(defn pack-a-seq [s]
  (loop [[fst & rst :as all] s result [] current []]
    (if (empty? all)
      result
      (if (= fst (nth rst 0))
        (recur rst result (conj current fst))
        (if-not (empty? current)
          (recur rst (conj result (conj current fst)) [])
          (recur rst (conj result [fst]) []))))))

(pack-a-seq [1 1 1 1 1 2 3 3 3])

;33 (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
(defn replicate-a-seq [s n]
  (mapcat (partial repeat n) s))

(replicate-a-seq [1 2 3] 2)

#(mapcat (partial repeat %2) %)
((fn [s n] (mapcat (partial repeat n) s)) [1 2 3] 3)

;34
(defn implement-a-range
  [n m]
  (if (> n m)
    (println "Error: the first input has to be the same or smaller than the second")
    (loop [current n return-val [] ]
      (if (= current m)
        return-val
        (recur (inc current) (conj return-val current)))))
  )
(implement-a-range -2 2)


((fn [n m]
  (if (> n m)
    (println "Error: the first input has to be the same or smaller than the second")
    (loop [current n return-val [] ]
      (if (= current m)
        return-val
        (recur (inc current) (conj return-val current)))))
  )-2 2)

(fn [x y] (take (- y x) (iterate inc x)))

(apply str (re-seq #"[A-Z]+" "bA1B3Ce "))

;38 (= (__ 1 8 3 4) 8)
((fn [c & coll]
  (nth (sort (into [c] coll)) (dec (count (into [c] coll))))) 4 1)
((fn [& coll]
   (nth (sort coll) (dec (count coll)))) 4 1)

;39 (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))

((fn [sq1 sq2]
  (loop [[fst1 & rst1 :as all1] sq1 [fst2 & rst2 :as all2] sq2 res []]
    (if (or (empty? all1) (empty? all2))
      res
    (recur rst1 rst2 (conj (conj res fst1) fst2)))))[1 2 3] [:a :b :c])
(mapcat list [1 2 3] [:a :b :c])

;40 (= (__ 0 [1 2 3]) [1 0 2 0 3])

((fn [i sq]
   (loop [[fst & rst :as all] sq result [fst]]
     (if (= 1 (count all))
       result
       (recur rst (conj (conj result i) (nth rst 0))))
     ))0 [1 2 3])

;41 (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])

((fn [sq n]
   (loop [[fst & rst :as all] sq result [] current 1]
     (if (empty? all)
       result
       (if (= current n)
         (recur rst result 1)
         (recur rst (conj result fst) (inc current))))))[1 2 3 4 5 6 7 8] 3)
;42 (= (__ 8) 40320)

((fn [n]
   (loop [current n result 1]
     (if (= current 0)
       result
       (recur (dec current) (* result current))))) 8)
