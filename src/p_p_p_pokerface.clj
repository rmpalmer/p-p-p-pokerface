(ns p-p-p-pokerface)


(defn rank [card]
  (let [r (get card 0)
        replacements {\A 14 \K 13 \Q 12 \J 11 \T 10}]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
