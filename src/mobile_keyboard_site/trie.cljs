(ns mobile-keyboard-site.trie
  (:require [clojure.string :refer (split lower-case)]))

(enable-console-print!)

(defn add-to-trie [trie x]
  (print "In add to trie " x)
  (let [depth     (count x)
        max-depth (:max-depth trie 0)
        exists    (get-in trie x)
        count     (inc (or (:count exists) 0))]
    (let [trie (if (> depth max-depth)
                 (assoc trie :max-depth depth)
                 trie)]
      (assoc-in trie x (merge (get-in trie x) {:val x :terminal true :count count})))))

(defn in-trie? [trie x]
  "Returns true if the value x exists in the specified trie."
  (:terminal (get-in trie x) false))

(defn prefix-matches [trie prefix]
  "Returns a list of matches with the prefix specified in the trie."
  (filter (comp not nil?) (map :val (tree-seq map? vals (get-in trie prefix)))))

(defn prefix-matches-confidence
  "Returns a list of match-confidence pairs"
  [trie prefix]
  (let [branch (tree-seq map? vals (get-in trie prefix))
        result   (filter (comp not nil?)
                         (map #(if (nil? (:val %))
                                 nil
                                 (vector (:val %)(:count %))) branch))
        ] result))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))

;; This function needs to
;; take the text, and grab trie-depth characters out of it.
;; each of those characters must be fed into the scan function.
;; Any hits should be recorded locally, and returned when
;; scan is finished processing.
(defn largest-match
  "Returns the largest match of a subsequence in a trie
  Ex: 
  (largest-match (build-trie ['abc' 'abcd'] 'abcde') => 'abcd'"
  [trie seq]
  (loop [s seq
         trie trie
         largest-match nil]
    (let [branch (get trie (first s) false)]
      (if (not branch)
        largest-match
        (recur (rest s) branch (if (:terminal branch)
                          (:val branch)
                          largest-match))))))

;; inputs, a trie and text
;; outputs: a list of matches like ("a" "ba" "a")
;; where prefixes are linked to number of occurances in the text.
(defn scan
  "Scans the text given using the trie for better performance, returns any matches 
in the text. Useful for when you're using a trie, but can't separate out tokens by 
splitting on whitespace"
  [trie text]
  (loop  [text text matches []]
    (if (empty? text)
      (remove nil? matches)
      (let [max-depth (:max-depth trie)
            working-set (take max-depth text)
            biggest-match (largest-match trie working-set)]
        (conj matches biggest-match)
        (recur (if biggest-match
                 (drop  (count biggest-match) text)
                 (drop  1 text)) (conj matches biggest-match))))))

(defn clean-input
 [text]
  (let [punctuation-removed (filter #(not (#{\.\,\!\?} %)) text)
        lower-case          (apply str (map lower-case punctuation-removed))]
    lower-case))
