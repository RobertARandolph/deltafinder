(ns abb.deltafinder
  (:gen-class)
  (:require
   [oz.core :as oz]
   [clojure.data.csv :as csv]
   [clojure.data.json :as json]
   [clojure.java.io :as io]))

(defn signal-outset-old
  "Takes a vector of an n+1 data structure, where the first value is time in seconds and the rest are channels of data.
  Find the :time and :sample-number of the first time the signal is below the threshold (double) for tail samples."
  [xs threshold tail]
  (loop [last-spike 0
         x          0
         xs         xs]
    (if xs 
      (let [cur (first xs)]
        (if (< cur threshold)
          (if (and (> 0 last-spike)
                   (> (- x last-spike) tail))
            (inc last-spike)
            (recur last-spike (inc x) (next xs)))
          (recur x (inc x) (next xs))))
      (inc last-spike))))

(defn signal-onset-old
  "Takes a vector of an n+1 data structure, where the first value is time in seconds and the rest are channels of data.
  Find the :time and :sample-number of the first violation of the threshold (double)."
  [xs threshold]
  (loop [x  0 
         xs xs]
    (let [cur (first xs)]
      (if (and xs (> cur threshold))
        (inc x)
        (recur (inc x) (next xs))))))



(defn signal-onset
  "Finds the number of items until the signal exceeds the threshold"
  [xs threshold]
  (count (take-while #(< % threshold ) xs)))

;; For this use, there's no need for the "last over threshold for n samples" method
(defn signal-last-over-threshold
  "Finds the number of items before the last time the signal violated the thresold"
  [xs threshold]
  (let [data (reverse xs)]
    (count (drop-while #(< % threshold) data))))


(defn extract-column
  [x xs]
  (mapv #(nth % x) xs))


(defn get-signals
  "takes a csv file and returns a vector of a vector of doubles corresponding to the columns specified in the vector cols."
  [file cols]
  (let [xs (csv/read-csv (io/reader file))]
    (mapv
     (fn [x]
       (->> (extract-column x xs)
            (mapv #(Double/parseDouble %))))
     cols)))


(defn setup-signal
  "takes a path to a csv file, a vector of numbers corresponding to columns to extract, a vector of names of those columns
  a vector of the on/last-over thresholds for each extracted column."
  [file cols names thresholds]
  (let [c      (get-signals file cols)
        starts (map #(signal-onset % %2) c thresholds)
        ends   (map #(signal-last-over-threshold % %2) c thresholds)]
    (map (fn [v n s e]
           {:values v
            :name   n
            :start  s
            :end    e})
         c names starts ends)))


(defn delta
  "returns the distance between the end of the (first cols) signal in xs and the start of the (second cols) signal in xs in samples"
  [xs]
  (let [first-end  (:end (first xs))
        last-start (:start (second xs))]
    (- last-start first-end)))

(defn delta-in-ms
  "Returns the delta between signals in milliseconds, given a sampling rate in khz"
  [xs scope-sr]
  (-> (delta xs)
      (/ scope-sr)
      float))

(defn delta-in-samples
  "Returns the delta between signals in samples related to the signal sample rate, given sampling rates in khz"
  [xs scope-sr signal-sr]
  (-> (delta xs)
      (* (/ signal-sr scope-sr))
      Math/floor
      int))

(defn signal->oz-data
  "Takes a signal map, sample rate and title and returns a map for plotting."
  [signal scope-sr source-sr title index]
  (let [main-values (apply concat
                           (map (fn [x]
                                  (let [values (:values x)
                                        name   (:name x)]
                                    (map-indexed (fn [idx y]
                                                   {:time    idx
                                                    :item    name 
                                                    :voltage y})
                                                 values)))
                                signal))
        midi-end    (:end (first signal))
        audio-start (:start (second signal))]
    {:signal       main-values
     :title        title
     :index        index
     :gap          [{:time midi-end}
                    {:time audio-start}]
     :delay-ms     (delta-in-ms signal scope-sr)
     :delay-sample (delta-in-samples signal scope-sr source-sr)}))


(defn plot
  [{:keys [title index signal delay-ms delay-sample gap]}]
  {:$schema    "https://vega.github.io/schema/vega-lite/v4.json"
   :background "white"
   :config     {:padding 20
                :concat  {:spacing -100}}
   :vconcat    [{:title    {:text     (str title " - " index)
                            :fontSize 64
                            :orient   "top"
                            :align    "center"}
                 :width    1800
                 :height   1200
                 :data     {:values signal} 
                 :mark     {:type        "line"
                            :interpolate "natural"}
                 :encoding {:color {:field  "item"
                                    :type   "nominal"
                                    :title  ""
                                    :legend {:orient            "top-right"
                                             :values            ["audio" "midi"]
                                             :labelFontSize     36
                                             :symbolSize        300
                                             :symbolStrokeWidth 20}}
                            :x     {:title ""
                                    :field "time"
                                    :scale {:domain [0 7000]}
                                    :type  "quantitative"}
                            :y     {:title ""
                                    :field "voltage"
                                    :scale {:domain [-2.5 8]}
                                    :type  "quantitative"}}}
                {:width    1800
                 :height   50
                 :title    {:text     (format "Total Delay: %.2fms - %d samples" delay-ms delay-sample)
                            :fontSize 48
                            :orient   "bottom"                          
                            :align    "center"
                            :dy       10}
                 :data     {:values gap}
                 :mark     {:type        "line"
                            :strokeWidth 50}
                 :encoding {:color {:value "#C44"}
                            :x     {:scale {:domain [0 7000]}
                                    :field "time"
                                    :type  "quantitative"}}}]})


(defn -main
  [& args]
  (println (first *command-line-args*))
  (doseq [x (filter #(.isFile %) (file-seq (io/file (first *command-line-args*))))]
    (let [{:keys [cols names thresholds scope-sr source-sr daw buffer setting version indexs]} (clojure.edn/read-string (slurp x))
          first-cols                                                                           (take 2 cols)
          dir                                                                                  (first *command-line-args*)]
      (doseq [x indexs]
        (let [filename (clojure.string/replace (str dir daw buffer setting x) #" " "")
              title    (format "%s v%s - Buffer: %s - %s" daw version buffer setting)
              oz-data  (signal->oz-data (setup-signal (str filename ".csv") first-cols names thresholds) scope-sr source-sr title x)]
          (println (str "Reading: " (str filename ".csv") " - Outputting: " (str filename ".png")))
          (let [f (str dir "images/" filename ".png")]
            (io/make-parents f)
            (with-open [w (io/output-stream f)]
              (.write w (oz/compile (plot oz-data) {:to-format :png})))))))))


#_(comment
    (:title (signal->oz-data (setup-signal "StudioOne512Medium.csv" [0 1] [:midi :audio] [2 2] ) [0 1] "Test Title"))
    (oz/view! (plot (signal->oz-data (setup-signal "StudioOne128Minimum1.csv" [0 1] [:midi :audio] [2 2] ) [0 1] "Test Title")))
    (oz/start-server!)
    (-main "samplesettings.edn")
    (with-open [writer (clojure.java.io/writer "test.json")]
      (json/write test-plot writer))
    {:cols [0 1] :names [:midi :audio] :thresholds [2 2] :sr 500 :daw "Studio One" :buffer 128 :setting "Minimum" :version "5.0.2" :index 1}

    
    )
