
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

;; Data

(def db {:data REPO
         :start 0
         :editor {}})

(defn ^:export updatedb [fields value]
  (set! db (assoc-in db fields value))
  (.redraw js/m true))

(defn querydb [fields]
  (get-in db fields))

(defn edit [item pos]
  (updatedb [:editor] (assoc (querydb [:editor]) pos item)))

(defn update_field [id value]
  (set! db (assoc-in db [:editor id] value)))

;; App

(defn text [id name value size]
  (nm "input[type=text]"
      {:value value :placeholder name :size size
       :onchange #(update_field id (-> % .-target .-value))}))

(defn annotate-editor
  [index item editing]
  (let [title (get editing "title")]
    (nm "div" [(text index "title" title 40)])))

(defn annotate-button
  [index item]
  (if (= (get item "type") "blob")
    (nm "button.pure-button" {:onclick #(edit item index)} "Annotate")))

(defn annotate
  [index item]
  (let [editor (querydb [:editor])
        editing (get editor index)]
    (if (nil? editing)
      (nm "div.pure-u-1.pure-u-sm-1-2" [(annotate-button index item)])
      (nm "div.pure-u-1-1.pure-u-sm-1" [(annotate-editor index item editing)]))))

(defn node
  "Renders one node from tree with indentation"
  [index item]
  (let [path (get item "path")
        parts (clojure.string/split path "/")
        level (count parts)
        base (last parts)]
    (nm "div.pure-g" [(nm "div.pure-u-1.pure-u-sm-1-2" [(nm (str "div.level" level) base)])
                      (annotate index item)])))

(defn reporender
  "Render repository trees"
  []
  (let [tree (get (querydb [:data]) "tree")]
    (map-indexed node tree)))


(defn ctrl [])

(defn viewer
  [ctrl]
  (nm "div" [(nm "h1" "RepoDoc App")
             (reporender)]))


;; Setup

(def app {:controller ctrl :view viewer})

(defn setup []
  (.mount js/m
           (.getElementById js/document "app")
           (clj->js app)))

(setup)

; To get REPL running
; (require 'cljs.repl)
; (require 'cljs.repl.node)
;
; (cljs.repl/repl (cljs.repl.node/repl-env))

