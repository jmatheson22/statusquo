(ns statusquo.core
  (:use statusquo.gp
        statusquo.strat
        [statusquo.coins :only [new-coins]]))

(def pop-size 25)
(def num-turns 20.0)

(defn round
  [strats]
  (doall (map update-strat strats)))

(defn run-simulations
  [strats]
  (nth (iterate round strats) num-turns))

(defn next-gen
  [risks]
  (let [half (take (* 1/2 pop-size) risks)
        quarter (take (* 1/4 pop-size) risks)]
    (concat
      (map mutate half)
      (map crossover quarter (shuffle quarter))
      quarter)))
    

(defn evolve
  [n]
  (println "Starting evolution...")
  (let [init-strats (repeatedly pop-size rand-strat)]
    (loop [strats init-strats generation 0]
      (let [results (reverse (sort-by :age (run-simulations strats)))
            best (first results)]
        (println "Turn:" generation 
                 "Length:" (:age best)

                 "Code:" (:riskfn best)
                 "Risk:" (eval-strat (strat-for-risk (:riskfn best)))
                 "Life:" (:life best)
                 "Coin:" (:coin best))     
        (when (< generation n)  
          (new-coins)
          (recur 
            (map strat-for-risk (next-gen (map :riskfn results)))
            (inc generation)))))))
