(ns mobile-keyboard-site.core
  (:require [figwheel.client :as fw]
            [sablono.core :as html :refer-macros [html]]
            [quiescent :as q :include-macros true]
            [mobile-keyboard-site.trie :as trie]
            [clojure.string :refer (split)]))

(enable-console-print!)

;; This is all of the mutable state stored in the application. Everything that ever gets
;; modified exists here. Everything else is data derived from this through functional
;; transformations
(defonce app-state (atom {:text ""
                          :prefix ""
                          :trie {}
                          :output []
                          }))

;; Produce a list of works that can be clicked
;; Include confidence (count)
(defn word-list
  "Generate a sequence of span elements, which are each clickable words. When clicked, 
each element adds itself to text area."
  [data]
  (print "In word list")
  (let [stored-prefix (:prefix data)
        trie   (:trie data)
        prefix-matches (sort-by second > (trie/prefix-matches-confidence trie (seq stored-prefix)))]
    (into [:span] (mapv (fn [[prefix count]]
                          [:span {:on-click #(do (swap! app-state assoc :text prefix)
                                                 (trie/add-to-trie trie prefix))
                                  :class "word"} (str prefix "(" (js/parseInt count) ")")])
                        prefix-matches))))

(defn last-word
  "Gets the last space separated token from text"
  [text]
  (last (split text #" ")))

(defn key-press-handler
  "Function responsible for handling key presses in the input field"
  [data curr-letter contents]
  (if (or (= curr-letter "Enter") (= curr-letter " "))
    (swap! app-state assoc :trie (trie/add-to-trie (:trie data) (last-word contents))
           :prefix  (last-word contents)
           :text    ""
           :output (conj (:output data) (last-word contents)))
    (swap! app-state assoc :prefix (last-word (str contents curr-letter))
                           :text (str contents curr-letter))))

(q/defcomponent display-output
  "This component is responsible for displaying the text generated by the auto-complete widget"
  [data]
  (html [:div
         (into [:span] (map-indexed
                        (fn [idx output-word]
                          [:span
                           {:on-click #(swap! app-state assoc :output (subvec (:output data) 0 idx))
                            :class "word"} output-word])
                            (:output data)))]))

(q/defcomponent input-text
  "This is the code responsible for the input box component"
  [data]
  (html
   [:div
    [:div (word-list data)]
    [:div  [:input {:on-key-press #(key-press-handler data (.-key %) (.-value (.-target %)))
                    :value (:text data)}]]]))

(defn render [data]
  "Top level rendering function"
  (q/render (input-text data)
           (.getElementById js/document "input"))
  (q/render (display-output data)
            (.getElementById js/document "output")))

(add-watch app-state ::render
           (fn [_ _ _ data] (render data)))

(defonce start (render @app-state)) ;; initial render

