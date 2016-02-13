
(ns repodoc.core
  (:require
    [cognitect.transit :as t]
    [m]
    [goog.i18n.DateTimeFormat :as dtf]
    [repodoc.data :refer [REPO]]
    [repodoc.fathom :refer [nm]]
    ))

(enable-console-print!)

;; Transito functions

(defn read [data]
  (let [r (t/reader :json)]
    (t/read r data)))

(defn write [data]
  (let [w (t/writer :json)]
    (t/write w data)))


;; App

(defn annotate
  [item index]
  (if (= (get item "type") "blob")
    (nm "button.pure-button" "Annotate")))

(defn node
  "Renders one node from tree with indentation"
  [index item]
  (let [path (get item "path")
        parts (clojure.string/split path "/")
        level (count parts)
        base (last parts)]
    (nm (str "div.level" level) [base (annotate item index)])))

(defn reporender
  "Render repository trees"
  [data]
  (map-indexed node (get data "tree")))

(defn ctrl []
  {:db REPO})

(defn viewer
  [ctrl]
  (println "rendering")
  (nm "div" [(nm "h1" "RepoDoc App")
             (nm "div" (reporender (:db ctrl)))]))


;; Setup

(def app {:controller ctrl :view viewer})

(defn setup []
  (.render js/m
           (.getElementById js/document "app")
           (clj->js app)))

(setup)

; To get REPL running
; (require 'cljs.repl)
; (require 'cljs.repl.node)
;
; (cljs.repl/repl (cljs.repl.node/repl-env))

