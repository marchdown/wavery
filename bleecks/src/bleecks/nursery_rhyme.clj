
(ns bleecks.nursery-rhyme
  (:gen-class))

(require '[overtone.live :as overtone])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn note [timing pitch] {:time timing :pitch pitch})
(def melody 
	(let [pitches
  			[0 0 0 1 2
			; Row, row, row your boat, 
			 2 1 2 3 4 
			; Gently down the stream, 
			 7 7 7 4 4 4 2 2 2 0 0 0
			; (take 4 (repeat "merrily"))
			 4 3 2 1 0]
; Life is but a dream!
			durations [1 1 2/3 1/3 1
		               2/3 1/3 2/3 1/3 2
        		       1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
              		   2/3 1/3 2/3 1/3 2]
            times (reductions + 0 durations)]
          (map note times pitches)))


(defn where [k f notes] (map #(update-in % [k] f) notes))
(defn scale [intervals] (fn [degree] (apply + (take degree intervals))))
(def major (scale [2 2 1 2 2 2 1])) (defn from [n] (partial + n))
(def minor (scale [2 1 2 2 1 2 2])) (defn from [n] (partial + n))

(def A (from 69))
    (->> melody
      (where :pitch (comp A major)))
    ;; -> ({:time 0, :pitch 69} ; Row,
    ;;     {:time 1, :pitch 69} ; row,
    ;;     ...)

(defn bpm [beats] (fn [beat] (/ (* beat 60 1000) beats)))
    (->> melody
      (where :time (comp (from (overtone/now)) (bpm 90))))
    ;; -> ({:time 1383316072169,    pitch 0}
    ;;     {:time 4149948218507/3, :pitch 0}
    ;;     ...)

(require '[overtone.live :refer [definst line sin-osc FREE midi->hz at]])
(definst beep [freq 440]
(let [envelope (line 1 0 0.5 :action FREE)]
        (* envelope (sin-osc freq))))
(defn play [notes]
(doseq [{ms :time midi :pitch} notes]
        (at ms (beep (midi->hz midi)))))
    ;; Make sure your speakers are on...
    (->> melody
      (where :pitch (comp A minor))
      (where :time (comp (from (overtone/now)) (bpm 90)))
      play)
    ;; -> <music playing on your speakers>



; cadence is a sequence of relative pitches.
; melody is a sequence of pairs of a base-pitch and a cadence
; (defn cadence)          	