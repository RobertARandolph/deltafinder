(ns abb.deltafinder
  (:gen-class)
  (:require
   [oz.core :as oz]
   [clojure.data.csv :as csv]
   [clojure.data.json :as json]
   [clojure.java.io :as io]))

(defn signal-outset
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

(defn signal-onset
  "Takes a vector of an n+1 data structure, where the first value is time in seconds and the rest are channels of data.
  Find the :time and :sample-number of the first violation of the threshold (double)."
  [xs threshold]
  (loop [x  0 
         xs xs]
    (let [cur (first xs)]
      (if (and xs (> cur threshold))
        (inc x)
        (recur (inc x) (next xs))))))

(defn setup-signal
  "takes a path to a csv file, a vector of numbers corresponding to columns to extract, a vector of names of those columns
  a vector of the on/outset thresholds for each extracted columl and a vector of tails for outset detection of each column."
  [file cols names thresholds tails]
  (let [c      (get-signals file cols)
        starts (map #(signal-onset % %2) c thresholds)
        ends   (map #(signal-outset % %2 %3) c thresholds tails)]
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
  [xs sr]
  (float (/ (delta xs) sr)))

(defn signal->oz-data
  "I really should document this."
  [signal sr title]
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
    {:signal main-values
     :title  title
     :gap    [{:time midi-end}
              {:time audio-start}]
     :delay  (delta-in-ms signal sr)}))


(defn plot
  [data]
  {:$schema    "https://vega.github.io/schema/vega-lite/v4.json"
   :background "white"
   :config     {:concat {:spacing -100}}
   :vconcat    [{:title    {:text     (:title data)
                            :fontSize 64
                            :orient   "top"
                            :align    "center"}
                 :width    1800
                 :height   1200
                 :data     {:values (:signal data)} 
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
                                    :type  "quantitative"}
                            :y     {:title ""
                                    :field "voltage"
                                    :type  "quantitative"}}}
                {:width    1800
                 :height   200
                 :title    {:text     (str "Total Delay: " (:delay data) "ms")
                            :fontSize 48
                            :orient   "bottom"                          
                            :align    "center"}
                 :data     {:values (:gap data)}
                 :mark     {:type        "line"
                            :strokeWidth 200}
                 :encoding {:title "test"
                            :color {:value "#C44"}
                            :x     {:title ""
                                    :scale {:domain [0 7000]}
                                    :field "time"
                                    :type  "quantitative"}}}]})

(defn -main
  [& args]
  (let [{:keys [cols names thresholds tails sr daw buffer setting version index]} (clojure.edn/read-string (slurp (first *command-line-args*)))
        first-cols                                                                (take 2 cols)
        filename                                                                  (clojure.string/replace (str daw buffer setting index) #" " "")
        title                                                                     (format "%s v%s - Buffer: %s - %s " daw version buffer setting)
        oz-data                                                                   (signal->oz-data (setup-signal (str filename ".csv") first-cols names thresholds tails) sr title)]
    
    (with-open [w (io/output-stream (str filename ".png"))]
      (.write w (oz/compile (plot oz-data) {:to-format :png})))))


(comment

  (oz/view! (plot (signal->oz-data (setup-signal "StudioOne512Medium.csv" [0 1] [:midi :audio] [2 30] [500 500]) [0 1] 500 "Test Title")))
  (oz/start-server!)
  (-main "samplesettings.edn")
  (with-open [writer (clojure.java.io/writer "test.json")]
    (json/write test-plot writer))
  {:cols [0 1] :names [:midi :audio] :thresholds [2 2] :tails [500 500] :sr 500 :daw "Studio One" :buffer 128 :setting "Minimum" :version "5.0.2" :index 1}

  (with-open [w (io/output-stream "/users/robertrandolph/Desktop/test.png")]
    (.write w (oz/compile test-plot {:to-format :png})))
  )
