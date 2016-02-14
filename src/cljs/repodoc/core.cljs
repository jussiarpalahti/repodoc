
(ns repodoc.core
  (:require
    [cognitect.transit :as t]
    [m]
    [goog.i18n.DateTimeFormat :as dtf]
    [repodoc.data :refer [REPO FILETYPES]]
    [repodoc.fathom :refer [nm]]
    [repodoc.editor :as e]
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
         :editing #{}})

(defn ^:export updatedb [fields value]
  (set! db (assoc-in db fields value))
  (.redraw js/m true))

(defn querydb [fields]
  (get-in db fields))

(defn edit [pos]
  (let [editing (querydb [:editing])]
    (updatedb [:editing] (conj editing pos))))

(defn update_field [pos key value]
  (let [item (assoc (querydb [:data pos]) key value)]
    (set! db (assoc-in db [:data pos] item))))

(defn save [item pos]
  "Save item to db :data into position"
  (let [doc (assoc item "mtime" (str (new js/Date)))
        data (querydb [:data])]
    (updatedb [:data] (assoc data pos doc))
    (println "Saved!" (get doc "path"))))

(defn close
  [pos]
  (let [editing (querydb [:editing])]
    (updatedb [:editing] (disj editing pos))))

;; Utils

(defn blob?
  [item]
  (if (= (get item "type") "blob") true false))

(defn editing?
  [pos]
  (contains? (querydb [:editing]) pos))

;; App

(defn ftype-to-icon
  [base]
  (let [suffix (last (clojure.string/split base "."))
        ftype (get FILETYPES suffix)]
    (if (not (nil? ftype))
      (nm (str "i.ftype." ftype)))))

(defn annotate-editor
  [pos item]
  (let [title (get item "title")]
    (nm "div" [(e/text "title" title 40 #(update_field "title" pos %))])))

(defn annotate-button
  [pos item]
  (if (blob? item)
    (nm "button.pure-button" {:onclick #(edit pos)} "Annotate")))

(defn annotate
  [index item]
  (if (false? (editing? index))
      (nm "div.pure-u-1.pure-u-sm-1-2" [(annotate-button index item)])
      [(nm "div.pure-u-1.pure-u-sm-1-2"
           [(nm "button.pure-button" {:onclick #(close index)} "Close")])
       (nm "div.pure-u-1-1.pure-u-sm-1"
           [(annotate-editor index item)])]))

(defn node
  "Renders one node from tree with indentation"
  [index item]
  (let [path (get item "path")
        parts (clojure.string/split path "/")
        level (count parts)
        base (last parts)
        icon (if (blob? item) (ftype-to-icon base) (nm "i.ftype.fa.fa-folder-open-o"))]
    (nm "div.pure-g" [(nm "div.pure-u-1.pure-u-sm-1-2" [(nm (str "div.level" level) [icon base])])
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

