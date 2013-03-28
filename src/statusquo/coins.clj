(ns statusquo.coins)

(def n-coins 15)
(defn rand-coin [] (+ 0.1 (rand 0.85)))

(def coins (atom nil))

(defn new-coins
  []
  (reset! coins (repeatedly n-coins rand-coin)))
  ;(println "First coin is: " (first @coins)))

(new-coins)