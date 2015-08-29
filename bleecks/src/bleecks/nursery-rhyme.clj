
(ns bleecks.nursery-rhyme
  (:gen-class))

(require '[overtone.live :as overtone])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn note [timing pitch] {:time timing :pitch pitch})
(def melody (let [pitches
[00012
; Row, row, row your boat, 21234
; Gently down the stream, 777444222000
; (take 4 (repeat "merrily")) 43210]
; Life is but a dream!
durations [112/31/31
              2/3 1/3 2/3 1/3 2
              1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
              2/3 1/3 2/3 1/3 2]
            times (reductions + 0 durations)]
          (map note times pitches)))
