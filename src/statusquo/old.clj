(ns statusquo.old)
(ns SQcode.core
  (:require [clojure.zip :as zip])
  (:require [clojure.math.numeric-tower :as math]))

;want this for expt, but it can't be found

;This code will follow this structure:
;1. Create random strategies, coins, and risk-taking functions
;2. Simulate lifetimes of strategies
;3. Evolve strategies with error function based on lifetime (mutate risk fns)

(def coins (repeatedly 25 coinMaker)) ;populate coins

;vectors of [lastCoin, life, risk-taking fn, lifetime]


(defn listChoices
  "Creates a list of choices for a strategy."
  [strat]
  (let [bestCoin (first strat)
        coinPop (take 15 coins)
        risk-taking (nth strat 2)]
    (conj (take (int (Math/abs (eval-strat risk-taking))) coinPop) bestCoin)))




;right now we can get things like (eval (2 (* 2 5)))

(defn select
  [reduction-size]
  (take reduction-size @strats))

(defn select-risk
  "Grabs the risk function."
  [reduction-size]
  (map #(nth % 2) (select reduction-size))) 






(defn updateStrats
  "Returns a vector of updated strategies, called each turn."
  []
  ;(println "updatestrats called")
  ;this is returning a single strat, not all of them
  (reset! strats (map updateStrat @strats)))



  
(defn simulate
  "Finds the length strategies will survive."
  []
  ;(println "simulate called")
  (dotimes [_ num-turns] (updateStrats)))





(defn extractLifetime
  []
  (map last @strats))
  
(defn extractCoins
  []
  (map first @strats))
  
(defn extractLife1
  []
  (map second @strats))


(defn third
  "Grabs the third in a sequence."
  [x]
  (first (next (next x))))
  


(defn best [] (apply max (extractLifetime)))

(defn next-strat
  [strat]
  [(nth coins fixed-coin) init-life (third strat) init-lifetime])
  
(defn next-gen
  []  
  (let [oldStrats (reverse (sort-by last @strats))]
    (reset! strats (map next-strat oldStrats))))

(defn risk-update
  []
  ;(println "calling risk-update")
  (concat
    (map mutate (select-risk (* 1/2 pop-size)))
    (map crossover 
         (select-risk (* 1/4 pop-size)) 
         (select-risk (* 1/4 pop-size)))
    (select-risk (* 1/4 pop-size))))

(defn updated-strats
  [strats]
  (let [risks (risk-update)]
    (map (fn [s v] (assoc s 2 v)) strats risks)))

(defn nextStep
  []
  (let [x (doall (updated-strats @strats))]
    (reset! strats x)))

(defn fullUpdate 
  [] 
  ;(println "calling fullUpdate")
  (next-gen)
  (nextStep))
  
(defn evolve
  "1.create strats, risk-fns
   2.each round run extractLifetime w/ modified risk-fns
   3.extractLifetime is the 'error' function for risk-fns"
  []
  (println "Starting evolution...")
  (populateStrats) ;init strats
  (loop [generation 0]
    ;(reset! strats (populateStrats))
    ;(println "set up complete")
    (let [risk-fns (select-risk pop-size)]
    
    (simulate)
    (println "Got through simulate")
    ;reporting results:
    (let [best (best)
          risk (nth (nth @strats 0) 2)])
    (println "let worked")
    (println "Turn:" generation 
             "Risk:" (int (Math/abs (eval (nth (nth @strats 0) 2)))) 
             "Length:" (apply max (extractLifetime))
             "Coin:" (nth (nth @strats 0) 0))
    ;(println "penalty counter is: " @penaltyCount)
    ;(println "Life is: " (nth (nth @strats 0) 1))
    
    (println "to full update")
    (fullUpdate)
      
    (if (< generation 10)
      (recur
        (inc generation)))))
  
  ;more reporting at end
  (println "Best strategy coin: " (best))
  (println "Best risk function: " (nth (nth @strats 0) 2))
  ;(println "Eval'd risk fn" (eval (nth (nth @strats 0) 2)))
  (println "Final coin chosen: " (nth (nth @strats 0) 0)))
  
;(evolve)  (let [expVal (* -1.0 (first strat)) ;a double from 0 to -1
        c (- 1 (expt 2.71828 expVal)) ;1-e^-coin, the absolute risk adversion function
        converted (+ 0.1 (pd 0.2 c))] ;transformed to reflect proper values
    (if (> (first strat) 0.5)
      (* 1/2 (* penalty converted)) ;much less penalty if good coin
      (* penalty converted)))) ;full penalty if bad coin  (let [expVal (* -1.0 (first strat)) ;a double from 0 to -1
        c (- 1 (expt 2.71828 expVal)) ;1-e^-coin, the absolute risk adversion function
        converted (+ 0.1 (pd 0.2 c))] ;transformed to reflect proper values
    (if (> (first strat) 0.5)
      (* 1/2 (* penalty converted)) ;much less penalty if good coin
      (* penalty converted)))) ;full penalty if bad coin