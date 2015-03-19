(ns l0st3d.util.schema.macros
  (:require [l0st3d.util.schema.internal :as i]
            [l0st3d.util.schema :as s]))

(defmacro def-constructor [constructor-name type-def]
  (let [t (eval type-def)
        ctr (s/get-constructor t)]
    `(let [ctr# (s/get-constructor ~type-def)]
       (defn ~constructor-name ~(i/get-args t)
         (apply ctr# ~(i/get-arg-values t))))))
