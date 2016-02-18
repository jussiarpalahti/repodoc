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

(defn sapling
  "Function that reduces into acc
  a map of given items' basenames
  using directory paths as keys"
  [acc item]
  (let [path (get item "path")
        parts (clojure.string/split path #"/")
        dirs (clojure.string/join "/" (butlast parts))
        dir (if (empty? dirs) "/" dirs)
        leaf (get acc dir)]
    (if leaf
      (assoc acc dir (conj leaf item))
      (assoc acc dir [item]))))

(defn path-reduce [paths]
  (reduce sapling  {} paths))

(defn vector-compare [[value1 & rest1] [value2 & rest2]]
  "From http://stackoverflow.com/questions/15716894/"
  (let [result (compare value1 value2)]
    (cond
      (not (= result 0)) result
      (nil? value1) 0 ; value2 will be nil as well
      :else (recur rest1 rest2))))
