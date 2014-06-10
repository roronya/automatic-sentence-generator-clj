(ns twitterbot-clj.core
  (:import [org.atilika.kuromoji Token Tokenizer])
  (:use [clojure.string :only (split)]))

(defn morphological-analysis-sentence [sentence]
  (let [tokenizer (-> (Tokenizer/builder) .build )
        token (.tokenize tokenizer sentence)]
    (concat ["-start"] (map #(.getSurfaceForm %1) token) ["-end"])))

(defn create-markov-chunk [morphemes]
  (let [chunkes (ref [])]
    (loop [morphemes morphemes]
      (let [chunk (take 3 morphemes)]
        (dosync (alter chunkes conj chunk))
        (when-not (= (last chunk) "-end")
          (recur (rest morphemes)))))
    @chunkes))

(defn create-frequency-list [morkov-chunk]
  (let [frequency-list (ref {})]
    (doseq [chunk morkov-chunk]
      (when (nil? (keyword (first chunk)) frequency-list)
        (dosync alter frequency-list assoc (keyword (first chunk)) [(chunk 0)])))))


(first (create-markov-chunk (morphological-analysis-sentence "吾輩は猫である。")))
(comment
  ("-start" "吾輩" "は") => {"start" {'("-start" "吾輩" "は") 1}})

(assoc {} :a 1)
