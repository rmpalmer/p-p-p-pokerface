(ns p-p-p-pokerface)


(defn rank [card]
  (let [r (get card 0)
        replacements {\A 14 \K 13 \Q 12 \J 11 \T 10}]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (let [counts (sort (vals (frequencies (map rank hand))))]
    (= counts (range 2 4))))

(full-house? ["2H" "4S" "2C" "3H" "3D"])

(defn two-pairs? [hand]
  (let [counts (sort (vals (frequencies (map rank hand))))
        table (frequencies counts)]
    (= (table 2) 2)))

(two-pairs? ["2H" "2S" "2C" "3H" "3D"])

(defn straight? [hand]
  (let [fvals (sort (map rank hand))
        gvals (sort (replace {14 1} (map rank hand)))
        fv (first fvals)
        gv (first gvals)
        fcomp (range fv (+ fv 5))
        gcomp (range gv (+ gv 5))]
    (or (= fcomp fvals) (= gcomp gvals))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)


(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        score  (fn [checker] (second checker))
        accept (fn [checker] ((first checker) hand))]
    (apply max (map score (filter accept checkers)))))

