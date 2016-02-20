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

(defn add-to-map
  "Addind values into :children of keys path
  trying to keep whatever other values
  the target map has"
  [data keys val]
  (update-in data
             keys
             (fn [old]
               (if (nil? old)
                 {:children [val]}
                 (if (:children old)
                   (update old :children
                           (fn [older & arg]
                             (conj older val))
                   (assoc old :children [val])))))))

;; Binary tree from JoC 2nd edition listing 9.1
(defrecord TreeNode [val l r])

(defn xconj [t v]
  (println t v)
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else          (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)

(defrecord GitTree [name tree])

(defn build-git-tree [t name tree]
  "Build a tree node for given name
  and add given list of names from the tree under it"
  (cond
    (nil? tree) (GitTree. name nil "blob")
    :else (GitTree. name (map build-git-tree tree) "tree")))

(defn min-vec
  "Return largest vectors length"
  [vecs]
  (apply min (map count vecs)))

(defn cut-vec
  "Returns vectors cut to the size
  of smallest vector"
  [& vecs]
  (let [size (min-vec vecs)]
    (map #(subvec % 0 (min (count %) size)) vecs)))

(defn sort-paths
  "Sort paths lexicographically by comparing
  path items directly and cutting vec length to
  equal"
  [paths]
  (sort #(apply compare (cut-vec %1 %2)) paths))

(defn string-rsplit
  "Split string from right to left
  returning items original order"
  [s & args]
  (reverse
    (map clojure.string/reverse
         (apply clojure.string/split (clojure.string/reverse s) args))))

(defn parent-paths
  "Return a list of given path's
  parents starting from root downward
  /a/b/c -> [/a /a/b /a/b/c]"
  [path]
  (let
    [[_ & dirs] (clojure.string/split path #"/")] ;; Root path turns to ""
    (reduce
      (fn [acc p]
        (cond
          (nil? acc) [[p]]
          :else (conj acc (conj (last acc) p))))
      nil
      dirs)))

(defn expand-paths
  "Generates a list of unique
  paths from given paths
  [/a/b/c /a/c /a/d] ->
  [/a /a/b /a/b/c /a/c /a/d]"
  [paths]
  (distinct
    (apply concat
      (map
        parent-paths
        paths))))

(defn pathify
  "Generate actual paths from components
  by adding path separator"
  [paths]
  (map #(str "/" (clojure.string/join "/" %))
       (sort-paths
         (expand-paths paths))))

(defn create-git-tree
  "Creates a list of maps for
  paths and their files"
  [paths]
  (let [dirfiles (path-reduce paths)
        paths (pathify (keys dirfiles))]
    (map (fn [p]
           {p (get dirfiles p)})
         paths)))
