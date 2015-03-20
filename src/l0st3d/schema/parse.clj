(ns l0st3d.util.schema.parse
  (:require [l0st3d.util.schema :as s]))

(defn parse
  ([element]
   (cond (map? element)
         (apply s/map-of (interleave (keys element) (apply parse (vals element))))
         
         (and (sequential? element) (seq (next element)))
         (apply s/all-of (map parse element))

         (and (sequential? element) (seq element))
         (s/list-of (parse (first element)))

         (and (set? element) (seq element))
         (apply s/one-of (map parse element))

         (class? element)
         (s/is-a element)

         (decimal? element)
         (s/is-decimal)

         (number? element)
         (s/is-integer)

         (instance? java.util.Date element)
         (s/is-date)

         (keyword? element)
         (s/equals element)

         (or (true? element) (false? element))
         (s/is-boolean)

         (string? element)
         (s/is-string)

         (nil? element)
         (s/is-nil)

         (fn? element)
         element

         :else
         (throw (ex-info "Cannot parse element" {:element element}))))
  ([el & elements]
   (map parse (concat [el] elements))))
