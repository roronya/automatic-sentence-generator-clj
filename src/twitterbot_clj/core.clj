(ns twitterbot-clj.core
  (:import [org.atilika.kuromoji Token Tokenizer])
  (:use [clojure.string :only (split)]))

(defn split-by-period [paragraph]
  (map #(str %1 "。") (split paragraph #"。")))

(defn morphological-analysis-sentence [sentence]
  (let [tokenizer (-> (Tokenizer/builder) .build)
        token (.tokenize tokenizer sentence)]
    (concat ["-start" ] (map #(.getSurfaceForm %1) token) ["-end"])))

(defn create-markov-chunk [morphemes]
  (let [chunkes (ref [])]
    (loop [morphemes morphemes]
      (let [chunk (take 3 morphemes)]
        (dosync (alter chunkes conj chunk))
        (when-not (= (last chunk) "-end")
          (recur (rest morphemes)))))
    @chunkes))

(def frequency-list (ref {}))

(defn add-frequency-list [markov-chunk]
  (doseq [markov-chunk-element markov-chunk]
    (cond (nil? (get @frequency-list (first markov-chunk-element)))
          (dosync (alter frequency-list assoc (first markov-chunk-element) {markov-chunk-element 1}))
          (nil? (get (get @frequency-list (first markov-chunk-element)) markov-chunk-element))
          (dosync (alter frequency-list update-in [(first markov-chunk-element)] assoc markov-chunk-element 1))
          :else
          (dosync (alter frequency-list update-in [(first markov-chunk-element) markov-chunk-element] inc)))))

(get @frequency-list "-start")
(map #(add-frequency-list (create-markov-chunk (morphological-analysis-sentence %1))) (split-by-period "吾輩は猫である。名前はまだない。"))


