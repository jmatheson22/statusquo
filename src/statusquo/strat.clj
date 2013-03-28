(ns statusquo.strat
  (:use statusquo.coins
        statusquo.gp
        [clojure.math.numeric-tower :only [expt]]))

(def init-age 0)
(def init-life 25)

(def depreciation-amt 0.1)
(def penalty-const -5.0)
(def risk-cost 0.1)

;;; strat order

(defstruct strat :coin :life :riskfn :age)

(defn rand-strat
  "Creates a random new strategy with fixed start coin."
  []
  (struct strat (first @coins) init-life (random-code 4) init-age))

(defn strat-for-risk
  [riskfn]
  (struct strat (first @coins) init-life riskfn init-age))

(defn only-alive
  [strat func]
  (if (zero? (:life strat))
    strat
    (func strat)))

(defn life-loss
  [strat]
  (let [{life :life riskfn :riskfn} strat
        life (- life (* risk-cost (eval-strat strat)))]
    (assoc strat :life (max 0 (dec life)))))

(defn flip-penalty
  "How much of a penalty to life for a strat"
  [strat]
  (let [c (expt Math/E (- (:coin strat))) ;e^-coin, the absolute risk adversion function
        penalty (* c penalty-const 
                   (if (> (:coin strat) 0.5) 1/2 1))]   ; half penalty for good coin
    (assoc strat :life (- (:life strat) (if (> penalty 0) 
                                          penalty (- penalty))))))

(defn best-coin-choice
  [strat]
  "select a new coin"
  (let [{coin :coin} strat
        n (min (count @coins) (int (eval-strat strat)))
        new-coins (conj (take n @coins) coin)]
    (assoc strat :coin (apply max new-coins))))


(defn depreciate
  "Depreciate coin value, cap at 0"
  [strat]
  (assoc strat :coin (max 0 (- (:coin strat) depreciation-amt))))

(defn inc-age
  [strat]
  (assoc strat :age (inc (:age strat))))

(defn update-strat
  [strat]
  (-> strat
    (only-alive inc-age)
    (only-alive life-loss)
    (only-alive flip-penalty)
    (only-alive depreciate)
    (only-alive best-coin-choice)))
