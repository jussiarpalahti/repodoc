
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

(def db {:data (get REPO "tree")
         :start 0
         :editing #{}
         :opened #{}})

(defn ^:export updatedb [fields value]
  (set! db (assoc-in db fields value))
  (.redraw js/m true))

(defn querydb [fields]
  (get-in db fields))


;; Utils

(defn blob?
  [item]
  (if (= (get item "type") "blob") true false))

(defn editing?
  [pos]
  (contains? (querydb [:editing]) pos))

(defn opened?
  [pos]
  (contains? (querydb [:opened]) pos))

;; Handlers

(defn edit [pos]
  (let [editing (querydb [:editing])]
    (updatedb [:editing] (conj editing pos))))

(defn save [item pos]
  "Save item to db :data into position"
  (let [doc (assoc item "mtime" (str (new js/Date)))
        data (querydb [:data])]
    (updatedb [:data] (assoc data pos doc))
    (println "Saved!" (get doc "path"))))

(defn update_field [pos key value]
  (let [prev (querydb [:data pos])
        item (assoc prev key value)]
    (println "Editing: " pos key value)
    (save item pos)))

(defn toggle-item-detail
  [pos]
  (let [opened (querydb [:opened])]
    (if (contains? opened pos)
      (updatedb [:opened] (disj opened pos))
      (updatedb [:opened] (conj opened pos)))))

(defn close-editor
  [pos]
  (let [editing (querydb [:editing])]
    (updatedb [:editing] (disj editing pos))
    (if (opened? pos) (toggle-item-detail pos))))

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
    (if (not (opened? pos)) (toggle-item-detail pos))
    (nm "div" [(e/text "title" title 40 #(update_field pos "title" %))])))

(defn annotate-button
  [pos item]
  (if (blob? item)
    (nm "button.pure-button" {:onclick #(edit pos)} "Annotate")))

(defn annotate
  [pos item]
  (if (false? (editing? pos))
      (nm "div.pure-u-1.pure-u-sm-1-2" [(annotate-button pos item)])
      [(nm "div.pure-u-1.pure-u-sm-1-2"
           [(nm "button.pure-button" {:onclick #(close-editor pos)} "Close")])
       (nm "div.pure-u-1-1.pure-u-sm-1"
           [(annotate-editor pos item)])]))

(defn item-detail
  [pos item]
  (if (and (get item "mtime" false) (opened? pos))
    (let [title (get item "title")]
      (nm "div.pure-u-1-1.pure-u-sm-1" title))))

(defn node
  "Renders one node from tree with indentation"
  [index item]
  (let [path (get item "path")
        parts (clojure.string/split path "/")
        level (count parts)
        base (last parts)
        icon (if (blob? item) (ftype-to-icon base) (nm "i.ftype.fa.fa-folder-open-o"))]
    (nm "div.pure-g" [(nm "div.pure-u-1.pure-u-sm-1-2"
                          [(nm (str "div.level" level) {:onclick #(toggle-item-detail index)} [icon base])])
                      (annotate index item)
                      (item-detail index item)])))

(defn reporender
  "Render repository trees"
  []
  (let [tree (querydb [:data])]
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

