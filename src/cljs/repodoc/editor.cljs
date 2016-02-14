
(ns repodoc.editor
  (:require
    [goog.i18n.DateTimeFormat :as dtf]
    [repodoc.fathom :refer [nm]]
    ))

(defn text [name value size cb]
  (nm "input[type=text]"
      {:value value :placeholder name :size size
       :onchange #(cb (-> % .-target .-value))}))
