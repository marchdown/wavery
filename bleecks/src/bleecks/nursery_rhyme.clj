(ns bleecks.nursery-rhyme
  (:gen-class))

(require '[overtone.live :as overtone])

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

(def freylekh-em
  "from http://d-ch.chat.ru/740/7-40.gif"
  (let [pitches
  			[3   0 3 0 3 0 3
                        ; er bin a hiz in paravoz
                        ;(map (comp (partial #(rem % 7)) (partial + 3)) [4 0 4 0 4 0 4 6])
                                        ;
                         5   2 5 2 5 2 5
                         8   5 8 7   4 7
                         5   3 6 4   8

			; (take 4 (repeat "merrily"))
			 ]
        
                        ; Life is but a dream!
              durations [h   q q q q q q
                         h   q q q q q q 
;                         1/2 1/4 1/4 1/4 1/4
;                         1/2 1/4 1/4 1/4 1/4
                         h   q q h   q q
                         h   q q h   q q 
                         h   q q h   h  ]
            times (reductions + 0 durations)]
    (map note times pitches)))

(def freylekh-a-phrygian
  "from http://muzon-muzon.ru/Noti/sem_sorok7.png"
  (let [pitches
                                        ; A b C D E F G A'
                                        ; 0 1 2 3 4 5 6 7
  			[3   0 3 0 3 0 3
                         5   2 5 2 5 2 5
                         7   5 7 6   4 6
                         5   4 3 4
                         
                         3   0 3 0 3 0 3
                         5   2 5 2 5 2 5
                         7   5 7 6   4 6
                         5   4 3 4  2
			 ]
              durations [h   q q q q q q
                         h   q q q q q q 
                         h   q q h   q q
                         h   q q 1
                         
                         h   q q q q q q
                         h   q q q q q q 
                         h   q q h   q q
                         h   q q h   h  ]
            times (reductions + 0 durations)]
    (map note times pitches)))

(def scales
  (let [pitches
        [0 1 2 3 4 5 6 7 8
         7 6 5 4 3 2 1 0
         1 2 3 4 5 6 7 8 9
         t e o 13 14 15 16]
        
        durations
        [h q q q q q q q h
         q q q q q q q h
         q q q q q q q h q
         q q h q  q  h  1 ]
            times (reductions + 0 durations)]
          (map note times pitches)))


;;;; freylech pitches in semitone values, not in scale steps
;; pitches
;;   			[5   0 5 0 5 0 5
;;                         ; er bin a hiz in paravoz
;;                         ;(map (comp (partial #(rem % 7)) (partial + 3)) [4 0 4 0 4 0 4 6])
;;                                         ;
;;                          8   3 8 3 8 3 8
;;                          o   8 o t   7 t
;;                          8   5 8 7   o 

;; 			; (take 4 (repeat "merrily"))
;; 			 ]


(defn where [k f notes] (map #(update-in % [k] f) notes))
(defn scale [intervals] (fn [degree] (apply + (take degree (cycle intervals)))))

(def ionian (scale [2 2 1 2 2 2 1]))
(def aeolian (scale [2 1 2 2 1 2 2]))
(def phrygian (scale [1 2 2 2 1 2 2]))

(def major ionian)
(def minor aeolian)

(defn from [n] (partial + n))

(def q 1/4)
(def h 1/2)
(def t 10)
(def e 11)
(def o 12)
(def A (from 69))
(def D (from 62))
(def E (from 64))
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
    (->> freylekh-em
      (where :pitch (comp A minor))
      (where :time (comp (from (overtone/now)) (bpm 90)))
      play)

(defn p [melody] (->> melody
                      (where :pitch (comp A phrygian))
                      (where :time (comp (from (overtone/now)) (bpm 80)))
                      play))
    ;; -> <music playing on your speakers>





; cadence is a sequence of relative pitches.
; melody is a sequence of pairs of a base-pitch and a cadence
; (defn cadence)          	
