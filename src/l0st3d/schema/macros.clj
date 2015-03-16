(ns l0st3d.util.schema
  (:require [l0st3d.util.schema :as s]))

(defmacro def-constructor [constructor-name type-def]
  (let [t (eval type-def)
        ctr (s/get-constructor t)]
    `(let [ctr# (s/get-constructor ~type-def)]
       (defn ~constructor-name ~(s/get-args t)
         (apply ctr# ~(s/get-arg-values t))))))
