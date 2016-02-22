
(ns repodoc.core
  (:require
    [cognitect.transit :as t]
    [m]
    [goog.i18n.DateTimeFormat :as dtf]
    [repodoc.data :refer [REPO FILETYPES]]
    [repodoc.fathom :refer [nm request]]
    [repodoc.utils :refer [serialize-md serialize-edn serialize-html serialize format_time]]
    [repodoc.editor :as e]
    [devtools.core :as devtools]
    ))

(enable-console-print!)

(devtools/set-pref! :install-sanity-hints true)
(devtools/install!)

;
; Constants
;
(def ^:const DATEFORMAT "d.M.yyyy H:mm")
(declare querydb)

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

(defn render-html
  "This does render, but subsequent
  redraws probably break stuff...
  Use the pattern for other things."
  [data el init? ctx]
  (aset el "innerHTML" data))

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
         :opened #{}
         :serialized false
         :serializer serialize-edn})

(defn ^:export updatedb [fields value]
  (set! db (assoc-in db fields value))
  (.redraw js/m true))

(defn querydb [fields]
  (get-in db fields))

(defn serialize-db
  [serializer]
  (serialize serializer
             (filter #(get % "mtime") (querydb [:data]))))

(defn initialize-db
  "Creates closure based database
  of the map given as data"
  [data]
  (let [cdb (atom data)]
           {:update (fn [key val]
                  (swap! cdb #(assoc % key val)))
            :query (fn [key]
                 (get @cdb key))}))

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

(defn toggle-serialization
  "Open serialization modal"
  []
  (if (querydb [:serialized])
    (updatedb [:serialized] false)
    (updatedb [:serialized] true)))

(defn set_serialize
  [serializer]
  (updatedb [:serializer] serializer))

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
      (nm "div.pure-u-1.pure-u-sm-1-5" [(annotate-button pos item)])
      [(nm "div.pure-u-1.pure-u-sm-1-5"
           [(nm "button.pure-button" {:onclick #(close-editor pos)} "Close")])
       (nm "div.pure-u-1-1.pure-u-sm-1"
           [(annotate-editor pos item)])]))

(defn item-detail
  [pos item]
  (if (and (get item "mtime" false) (opened? pos))
    (let [mtime (get item "mtime")]
      (nm "div.pure-u-1-1.pure-u-sm-1"
          ["Modification time:" (format_time (new js/Date mtime) DATEFORMAT)]))))

(defn node
  "Renders one node from tree with indentation"
  [index item]
  (let [path (get item "path")
        parts (clojure.string/split path "/")
        level (count parts)
        base (last parts)
        icon (if (blob? item) (ftype-to-icon base) (nm "i.ftype.fa.fa-folder-open-o"))
        title (get item "title")]
    (nm "div.pure-g" [(nm "div.pure-u-1.pure-u-sm-2-5"
                          [(nm (str "div.level" level) {:onclick #(toggle-item-detail index)} [icon base])])
                      (nm "div.pure-u-1-2.pure-u-sm-2-5" {:onclick #(toggle-item-detail index)} title)
                      (annotate index item)
                      (item-detail index item)])))

(defn serialization
  []
  (let [serialize (querydb [:serialized])
        serializer (querydb [:serializer])]
    (if serialize
      (nm "#serialization" [(nm "div" [(nm "button.pure-button"
                                           {:onclick #(toggle-serialization)} "Close")
                                       " Export as: "
                                       (nm "button.pure-button"
                                           {:onclick #(set_serialize serialize-edn)} "EDN")
                                       (nm "button.pure-button"
                                           {:onclick #(set_serialize serialize-md)} "Markdown")
                                       (nm "button.pure-button"
                                           {:onclick #(set_serialize serialize-html)} "HTML")
                                       " Click the text area and select text"])
                            (if (= serializer serialize-html)
                              (nm "div" (m/trust (serialize-db serializer)))
                              (nm "textarea" {"key" "ser"} (serialize-db serializer)))]))))

(defn reporender
  "Render repository trees"
  []
  (let [tree (querydb [:data])]
    (map-indexed node tree)))

(defn toolbar
  "Toolbar contains buttons for app wide functions"
  []
  (nm "div.toolbar" [(nm "button.pure-button" {:onclick #(toggle-serialization)} "Export")]))

(defn ctrl [])

(defn viewer
  [ctrl]
  (nm "div" [(nm "h1" "RepoDoc App")
             (toolbar)
             (reporender)
             (serialization)]))

(defn fetch_repositories
  [username cb]
  (println "hmm" username)
  (let [repourl (str "https://api.github.com/users/" username "/repos")]
    (request {:url repourl}  #(cb %))))

(defn index_view
  [{:keys [update query]} ctrl]
  (let [username (query :username)
        repositories (query :repositories)]
    (nm "div" [(nm "h1" "RepoDoc App")
               (nm "h2" "Give username and choose your repository")
               (nm "div"
                   [(nm "input"
                        {"placeholder" "Github username"
                         "value" username
                         :onchange (e/text "username" username 40 #())})
                    (nm "div" [(nm "button"
                                   {:onclick #(fetch_repositories
                                               username
                                               (fn [data] (update :repositories data)))}
                                   "Fetch")
                               ])])
               (if repositories
                 (nm "div" [(nm "h2" "Repositories")
                            (nm "ul" [(map #(nm "li" (get % "name"))
                                           repositories)])]))])))

;; Setup

(def repodoc {:controller ctrl :view viewer})

(def index {:controller #(initialize-db {:username "jussiarpalahti" :repositories nil})
            :view index_view})

;; Routing mode
(aset (.-route js/m) "mode" "hash")

(defn setup []
  "Mount app"
  (do
    (.route js/m
            (.getElementById js/document "app")
            "/"
            (clj->js {"/" index
                      "/repo/:user/:repository" repodoc}))))


(setup)
(.route js/m "/")

; To get REPL running
; (require 'cljs.repl)
; (require 'cljs.repl.node)
;
; (cljs.repl/repl (cljs.repl.node/repl-env))

