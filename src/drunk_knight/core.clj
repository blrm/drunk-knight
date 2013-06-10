(ns drunk-knight.core
  (:require [clojure.math.numeric-tower :as math]))

(def knight-counter (atom 0))

(defn plane [[x y]]
  (vec (repeat y (vec (repeat x 0)))))

(def blank-checker (plane [8 8]))

(defn display [board]
  (clojure.pprint/pprint (for [row board]
                           (for [item row]
                             (format "%02d" item)))))

(defn update [board [x y] val]
  (assoc-in board [x y] val))

(defn board-get [board x y]
  (get-in board [x y]))

(defn dims [board]
  [(count board) (count (first board))])

(defn knight-moves
  "for a given [x y], returns the 8 knight moves from that location"
  [[x y]]
  (for [dx [-2 -1 1 2] dy [-2 -1 1 2]
        :when (not= (math/abs dx) (math/abs dy))]
   [(+ dx x) (+ dy y)]))

(defn possible-moves
  "filters the knight moves down to what is available on the given board"
  [board loc]
  (filter #(get-in board %) (knight-moves loc)))

(defn visited? [board loc]
  (> (get-in board loc) 0))

(defn unvisited-moves
  "filters the knight moves down to what is available and unvisited so far."
  [board loc]
  (filter #((complement visited?) board %) (possible-moves board loc)))

(defn find-next-move [board knight-loc]
  (first (sort (fn [x y] (reduce - (map #(count (unvisited-moves board %)) [x y]))) (unvisited-moves board knight-loc))))

(defn walk-board [board start-loc]
    (if (empty? (unvisited-moves board start-loc))
      board
    (let [next-move (find-next-move board start-loc)]
      (swap! knight-counter inc)
      (recur (update board next-move @knight-counter) next-move))))

(defn knights-tour [board-dims starting-point]
  (reset! knight-counter 1)
  (walk-board (update (plane board-dims) starting-point 1) starting-point))



(defn draw
  ([w h board] (draw w h board 30))
  ([w h board scale]
     (doto (javax.swing.JFrame. "Knight Moves")
       (.setContentPane
        (doto (proxy [javax.swing.JPanel] []
                (paintComponent [^java.awt.Graphics g]
                  (let [g (doto ^java.awt.Graphics2D (.create g)
                                (.scale scale scale)
                                (.setStroke (java.awt.BasicStroke. 0.4))
                                (.setFont (java.awt.Font. nil 1 2))
                                (.setColor (java.awt.Color. 0))
                                (.fillRect 0 0 (* w 5) (* h 5)))]
                    (doseq [x (range w)
                            y (range h)]
                      (let [item (get-in board [x y])]
                        (.setColor g (java.awt.Color. (mod  (* item 23) 255)
                                                      (mod  (* item 27) 255)
                                                      (mod  (* item 11) 255)))
                        (.fillRect g (* 5 x) (* 5 y) 5 5)
                        (.setColor g (java.awt.Color. (mod  (* item 13) 255)
                                                      (mod  (* item 72) 255)
                                                      (mod  (* item 29) 255)))
                        (.fillOval g (+ 1 (* 5 x)) (+ 1 (* 5 y)) 3 3)
                        (.setColor g (java.awt.Color. (mod  (* item 27) 255)
                                                      (mod  (* item 19) 255)
                                                      (mod  (* item 33) 255)))
                        #_(.fillOval g (+ 2 (* 5 x)) (+ 2 (* 5 y)) 1 1)
                        (.drawString g (str item) (+ 0 (* 5 x)) (* 5 (+ 1 y))))))))
          (.setPreferredSize (java.awt.Dimension. (* scale w 5) (* scale h 5)))))
       .pack
       (.setVisible true))))

(defn draw-demo [dimensions scale]
  (let [[x y] dimensions]
    (draw y x (knights-tour [x y] [0 0]) scale)
    (println @knight-counter " / "(* x y))))
