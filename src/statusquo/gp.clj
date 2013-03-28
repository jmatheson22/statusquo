(ns statusquo.gp
  (:require [clojure.zip :as zip]))


(def fn-table (zipmap '(+ * pd -) 
                      '(2 2 2 2 ))) 

(defn eval-strat
  [strat]
  (let [func (eval (list 'fn '[{coin :coin life :life age :age}] (:riskfn strat)))]
    (let [result (func strat)]
      (Math/abs (float (func strat))))))

(defn rand-fn
  "Creates a random function from the function-table."
  []
  (rand-nth (keys fn-table)))

(defn rand-var
  []
  ;(rand-nth (list 'x (- (rand 10) 5))))
  (rand-nth (list 'coin 'life '(- (rand 10) 5))))

(defn pd
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

;lee's code for creating longer fns, nested based on depth
(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2))) ;2 at least!!
    (rand-var)
    (let [f (rand-fn)]
      (cons f (repeatedly (get fn-table f)
                          #(random-code (dec depth)))))))


(defn codesize [c] ;lee's code for this fn
  (if (seq? c)
    (count (flatten c))
    1))

(defn at-index ;from lee
  "Returns a subtree of tree indexed by point-index in a depth first traversal."
  [tree point-index]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/node z)
        (if (seq? (zip/node z)) 
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn insert-at-index ;from lee
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) replaced by new-subtree."
  [tree point-index new-subtree]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/root (zip/replace z new-subtree))
        (if (seq? (zip/node z))
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn mutate
  "Given the risk function, this substitutes random code in risk for new random 
   code produced by random-code."
  [risk-fn]
  (let [chunk (rand-int 2)
        newCode (random-code chunk)
        startPt (rand-int (codesize risk-fn))] 
    (insert-at-index risk-fn startPt newCode)))

(defn crossover
  "Given two risk-fns, it preforms crossover on a chunk of code."
  [risk-fn risk-fn2]
  (insert-at-index risk-fn
                   (rand-int (codesize risk-fn)) 
                   (at-index risk-fn2 (rand-int (codesize risk-fn2)))))
