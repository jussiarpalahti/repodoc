(ns repodoc.utils)

(defn dsu-sort
  "Decorate - sort - undecorate
  from http://gettingclojure.wikidot.com/cookbook:sequences
  Also used in Python Cookbook
  This version sorts only on decorated values directly"
  [sortable decorator]
  (map last
     (sort (fn [[v1 a] [v2 b]]
             (< v1 v2))
           (map (fn [x]
                  [(decorator x) x])
                sortable))))
