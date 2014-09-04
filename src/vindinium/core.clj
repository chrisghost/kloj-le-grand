(ns vindinium.core
  (:gen-class)
  (:use [slingshot.slingshot :only [try+, throw+]])
  (:use [clojure.core.match :only (match)]))

(require '[clj-http.client :as http])

(def server-url "http://10.0.25.125:9000/")

(defn neigb [ [x y] ]
  ;(prn "neigb args :")
  ;(prn x)
  ;(prn y)
  ;(prn "neigb args ////")
  (filter (fn [e] (and (>= (e 0) 0) (>= (e 1) 0)))
    [
      [(- x 1) y]
      [x (+ y 1)]
      [(+ x 1) y]
      [x (- y 1)]
    ])
  )

(defn at [[x y] tiles size]
  ;(prn ">at")
  ;(prn (+ (* y size) x))
  (nth tiles (+ (* y size) x))
  )

(defn bfs [tovisit search tiles size visited]
  (let [
        t (first tovisit)
        next-to-visit (drop 1 tovisit)
        cur (last t)
        n (neigb cur)
        _ (map #(prn %) tovisit)
        ]
       (if (search cur)
         t ;curpath
         (bfs
           (if (and (not= (at cur tiles size) {:tile :air}) (not= (get (at cur tiles size) :tile) :hero))
             (vec next-to-visit)
             (into
               (vec next-to-visit)
               (vec
                 (map
                      (fn [e] (conj t e))
                      ;(filter (fn [e] (prn (at e tiles size)) (prn e) (= {:tile :air} (at e tiles size)))
                        (filter #(not (contains? (set visited) %)) n))))
             )
           search
           tiles
           size
           (conj visited t)
           )
         )
    )
  )

(defn getdir [[x y] [tx ty]]
  (if (< x tx)
    "east"
    (if (> x tx)
      "west"
      (if (< y ty)
        "south"
        "north"
        )
      )
  )
  )

(defn bot [{:keys [game hero] :as input}] ;[id turn maxTurns heroes [size tiles] finished] hero token viewUrl playUrl] }]
  "Implement this function to create your bot!"
  (let [
        heroes (get game :heroes)
        board (get game :board)
        hpos (get hero :pos)
        hlife (get hero :life)
        size (get board :size)
        tiles (get board :tiles)
        aim (if (< hlife 50)
              :tavern
              :mine)
        path (bfs
               [[hpos]]
               (fn [e]
                 (= (at e tiles size) {:tile aim}))
               tiles
               size
               [hpos])
        ]
    ;(prn hpos)
    ;(prn (first (drop 1 path)))
    ;(prn (at (last path) tiles size))
    (prn (getdir hpos (first (drop 1 path))))
    ;(prn "================")
    (getdir hpos (first (drop 1 path)))
  ))

(defn at [[x y] tiles size]
 (tiles (+ (* y size) x)))

; Because the (y,x) position of the server is inversed. We fix it to (x,y).
(defn fix-pos [{:keys [x y]}] [y x])

(defn fix-hero [hero]
  (-> hero
      (update-in [:pos] fix-pos)
      (update-in [:spawnPos] fix-pos)))

(defn improve-input [input]
  (-> input
      (update-in [:hero] fix-hero)
      (update-in [:game :heroes] #(map fix-hero %))
      (update-in [:game :board :tiles] vec)))

(defn parse-tile [tile]
  (match (vec tile)
         [\space \space] {:tile :air}
         [\# \#] {:tile :wall}
         [\[ \]] {:tile :tavern}
         [\$ \-] {:tile :mine}
         [\$ i] {:tile :mine :of i}
         [\@ i] {:tile :hero :id (Integer/parseInt (str i))}))

(defn parse-tiles [tiles] (map parse-tile (partition 2 (seq tiles))))

(defn parse-input [input] (update-in input [:game :board :tiles] parse-tiles))

(defn request [url, params]
  "makes a POST request and returns a parsed input"
  (try+
    (-> (http/post url {:form-params params :as :json})
        :body
        parse-input
        improve-input)
    (catch map? {:keys [status body]}
      (println (str "[" status "] " body))
      (throw+))))


(defn step [from]
  (loop [input from]
    (print ".")
    (let [next (request (:playUrl input) {:dir (bot input)})]
      (if (:finished (:game next)) (println "") (recur next)))))

(defn training [secret-key turns]
  (let [input (request (str server-url "/api/training") {:key secret-key :turns turns})]
    (println (str "Starting training game " (:viewUrl input)))
    (step input)
    (println (str "Finished training game " (:viewUrl input)))))

(defn arena [secret-key games]
  (loop [it 1]
    (let [p #(println (str "[" it "/" games "] " %))
          _ (p "Waiting for pairing...")
          input (request (str server-url "/api/arena") {:key secret-key})]
      (p (str "Starting arena game " (:viewUrl input)))
      (step input)
      (p (str "Finished arena game " (:viewUrl input)))
      (when (< it games) (recur (+ it 1))))))

(def usage
  "Usage:
   training <secret-key> <number-of-turns>
   arena <secret-key> <number-of-games")

(defn -main [& args]
  (match (vec args)
         ["training", secret-key, nb] (training secret-key nb)
         ["arena", secret-key, nb] (arena secret-key nb)
         :else (println usage)))
