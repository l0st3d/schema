(ns l0st3d.util.schema.internal)

(defn get-args [type-def]
  (let [m (meta type-def)
        ks (:l0st3d.util.schema/map-keys m)
        vs (:l0st3d.util.schema/map-vals m)
        ts (zipmap ks vs)]
    (->> ks 
      (map #(let [k (name %)]
              (if-let [sub-args (not-empty (-> ts (get %) get-args))]
                {:keys sub-args :as (symbol k)}
                (if (re-matches #".*[-_]id" k)
                  {(symbol k) :id}
                  (symbol k)))))
      (into []))))

(defn get-arg-values [type-def]
  (->> type-def meta :l0st3d.util.schema/map-keys
       (map #(symbol (name %)))
       (into [])))
