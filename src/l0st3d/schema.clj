(ns l0st3d.util.schema
  (:require [clj-time.core :as cdt])
  (:import [org.joda.time.format DateTimeFormat]
           [java.util Date])) 

(defn- collect-errors []
  (let [errors (atom [])
        fkey-paths (atom [])
        unique-paths (atom [])]
    {:error-handler (fn [err data-path data]
                      (swap! errors conj (str data-path (when data-path " ") err))
                      data)
     :fkey-handler (fn [data-path] (swap! fkey-paths conj data-path))
     :unique-handler (fn [data-path] (swap! unique-paths conj data-path))
     :get-errors (fn [] @errors)
     :get-unique-paths (fn [] @unique-paths)
     :get-forgien-key-paths (fn [] @fkey-paths)}))

(defn- check-valid [e validation-fn error-handler data-path err-msg]
  (cond (and (fn? validation-fn) (not (validation-fn e)))
        (error-handler err-msg data-path e)

        (and (instance? java.util.regex.Pattern validation-fn) (not (re-matches validation-fn (str e))))
        (error-handler err-msg data-path e)

        :else
        e))

(defn- validate-element [type-def data data-path error-handler unique-handler fkey-handler]
  (if (fn? type-def)
    (type-def data data-path error-handler unique-handler fkey-handler)
    (throw (ex-info "unknown type-def" {:type-def type-def}))))

;; validation fns
(defn is-string
  ([] (is-string nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler unique-handler fkey-handler]
       (if (string? e)
         (check-valid e validation-fn error-handler data-path err-msg)
         (error-handler "should be a string" data-path e)))))

(defn is-keyword
  ([] (is-keyword nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler unique-handler fkey-handler]
       (if (keyword? e)
         (check-valid e validation-fn error-handler data-path err-msg)
         (error-handler "should be a keyword" data-path e)))))

(defn is-boolean []
  (fn [e data-path error-handler unique-handler fkey-handler]
    (if (contains? #{true false nil "true" "false" "TRUE" "FALSE"} e)
      (contains? #{true "true" "TRUE"} e)
      (error-handler "should be a boolean" data-path e))))

(defn is-integer
  ([] (is-integer nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler unique-handler fkey-handler]
       (cond (number? e) (check-valid e validation-fn error-handler data-path err-msg)
             (string? e) (try (-> e Long/parseLong (check-valid validation-fn error-handler data-path err-msg)) (catch Exception ex (error-handler "should be a number" data-path e)))
             :else (error-handler "should be a number" data-path e)))))

(defn is-decimal
  ([] (is-decimal nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler unique-handler fkey-handler]
       (cond (number? e) (check-valid (bigdec e) validation-fn error-handler data-path err-msg)
             (string? e) (try (-> e BigDecimal. (check-valid validation-fn error-handler data-path err-msg)) (catch Exception ex (error-handler "should be a decimal" data-path e)))
             :else (error-handler "should be a decimal" data-path e)))))

(defn is-date []
  (let [parse-fmts [(DateTimeFormat/forPattern "yyyy-MM-dd'T'HH:mm:ss.SSSZ") (DateTimeFormat/forPattern "EEE, dd MMM yyyy HH:mm:ss zzz")]]
    (fn [e data-path error-handler unique-handler fkey-handler]
      (cond (instance? java.util.Date e) e
            (string? e) (if (re-matches #"[0-9]+" e)
                          (Date. (Long. (str e "000")))
                          (some #(try (.toDate (.parseDateTime % e)) (catch IllegalArgumentException ex)) parse-fmts))
            :else (error-handler "should be a date" data-path e)))))

(defn should-exist []
  (fn [data data-path error-handler unique-handler fkey-handler]
    (if (nil? data)
      (error-handler "should exist" data-path data)
      data)))

(defn is-nil []
  (fn [data data-path error-handler unique-handler fkey-handler]
    (when-not (nil? data)
      (error-handler "should be nil" data-path data))))

(defn equals
  ([v] (equals v (str "Should be '" v "'")))
  ([v error-message]
     (fn [data data-path error-handler unique-handler fkey-handler]
       (if (not= v data)
         (error-handler error-message data-path data)
         v))))

;; collection validation fns
(defn map-of [& keys-and-vals]
  (with-meta
    (let [keys-and-vals (apply hash-map keys-and-vals)]
      (fn [data data-path error-handler unique-handler fkey-handler]
        (if (map? data)
          (reduce (fn [m [k val-type-def]]
                    (let [d (validate-element val-type-def (get data k) (conj data-path k) error-handler unique-handler fkey-handler)]
                      (if (contains? data k)
                        (assoc m k d)
                        m))) {} keys-and-vals)
          (error-handler "should be a map" data-path data))))
    (let [p (partition 2 keys-and-vals)]
      {::map-keys (map first p)
       ::map-vals (map second p)})))

(defn list-of [type-def]
  (fn [data data-path error-handler unique-handler fkey-handler]
    (if (sequential? data)
      (->> data
        (map #(validate-element type-def %2 (conj data-path %1) error-handler unique-handler fkey-handler) (iterate inc 0))
        (into []))
      (error-handler "should be a list" data-path data))))

(defn one-of [& type-defs]
  (fn [data data-path error-handler unique-handler fkey-handler]
    (let [one-of-errors (atom [])
          one-of-error-handler-called (atom nil)
          one-of-error-handler (fn [err data-path data]
                                 (swap! one-of-error-handler-called (constantly true))
                                 (swap! one-of-errors conj err)
                                 data)]
      (loop [tds type-defs]
        (if (seq tds) 
          (let [td (first tds)
                r (validate-element td data data-path one-of-error-handler unique-handler fkey-handler)]
            (if @one-of-error-handler-called
              (do (swap! one-of-error-handler-called (constantly nil))
                  (recur (next tds)))
              r))
          (error-handler (->> @one-of-errors (interpose " or ") (apply str)) data-path data))))))

(defn all-of [& type-defs]
  (fn [data data-path error-handler unique-handler fkey-handler]
    (let [c (map #(validate-element % data data-path error-handler unique-handler fkey-handler) type-defs)]
      (when (every? identity (doall c))
        (first c)))))

(defn count-is [message func & args]
  (fn [data data-path error-handler unique-handler fkey-handler]
    (when-not (apply func (count data) args)
      (error-handler (str "count should be " message) data-path data))
    data))

(defn with-defaults [type-def & {:as defaults}]
  (with-meta (fn [data data-path error-handler unique-handler fkey-handler]
               (validate-element type-def (merge data (apply dissoc defaults (keys data))) data-path error-handler unique-handler fkey-handler))
    (let [ks (::map-keys (meta type-def))
          vs (::map-vals (meta type-def))
          kvs (remove nil? (map #(when-not (get defaults %1) [%1 %2]) ks vs))]
      {::map-keys (map first kvs)
       ::map-vals (map second kvs)})))

(defn with-modifications [type-def & modifier-fns]
  (with-meta
    (fn [data data-path error-handler unique-handler fkey-handler]
      (try
        (reduce #(%2 %1) (validate-element type-def data data-path error-handler unique-handler fkey-handler) modifier-fns)
        (catch Throwable t
          (error-handler (str "Modification Exception : " t) data-path data)
          data)))
    (meta type-def)))

;; constraints
(defn unique []
  (fn [data data-path error-handler unique-handler fkey-handler] ;TODO change all these fns so that we can collect unique cols and search for them in repository ... also do foreign key checks??
    (when (fn? unique-handler)
      (unique-handler data-path))
    data))

;; data cleaners
(defn trimmed [f]
  (comp #(if (string? %) (.trim %) %) f))

(defn lower-cased [f]
  (comp #(if (string? %) (.toLowerCase %) %) f))

;; API
(defn get-errors [data type-def]
  (let [{:keys [error-handler unique-handler fkey-handler get-errors get-forgien-key-paths get-unique-paths]} (collect-errors)]
    (validate-element type-def data [] error-handler unique-handler fkey-handler)
    (get-errors)))

(defn get-data-ignoring-errors [data type-def]
  (let [{:keys [error-handler unique-handler fkey-handler get-errors get-forgien-key-paths get-unique-paths]} (collect-errors)
        d (validate-element type-def data [] #(-> %& (nth 2)) unique-handler fkey-handler)]
    (with-meta d {::valid nil ::unique-paths (get-unique-paths) ::forgien-key-paths (get-forgien-key-paths)})))

(defn validate [data type-def]
  (let [{:keys [error-handler unique-handler fkey-handler get-errors get-forgien-key-paths get-unique-paths]} (collect-errors)
        d (if (-> data meta ::valid) data (validate-element type-def data [] error-handler unique-handler fkey-handler))]
    (if-let [errs (not-empty (get-errors))]
      (throw (ex-info "there were validation errors" {:message "there were validation errors" :errors errs :handle :validation-errors :data data}))
      (with-meta d {::valid true ::unique-paths (get-unique-paths) ::forgien-key-paths (get-forgien-key-paths)}))))

(defn merge-and-validate [new data type-def]
  (validate (merge data new) type-def))

(defn- get-args [type-def]
  (let [m (meta type-def)
        ks (::map-keys m)
        vs (::map-vals m)
        ts (zipmap ks vs)]
    (->> ks 
      (map #(let [k (name %)]
              (if-let [sub-args (not-empty (-> ts (get %) get-args))]
                {:keys sub-args :as (symbol k)}
                (if (re-matches #".*[-_]id" k)
                  {(symbol k) :id}
                  (symbol k)))))
      (into []))))

(defn- get-arg-values [type-def]
  (->> type-def meta ::map-keys
       (map #(symbol (name %)))
       (into [])))

(defn get-constructor [type-def]
  (let [ks (::map-keys (meta type-def))
        args (get-args type-def)
        arg-vals (get-arg-values type-def)
        ctr (comp #(validate % type-def)
                  (eval `(fn ~args
                           ~(zipmap ks arg-vals))))]
    ctr))

(defmacro def-constructor [constructor-name type-def]
  (let [t (eval type-def)
        ctr (get-constructor t)]
    `(let [ctr# (get-constructor ~type-def)]
       (defn ~constructor-name ~(get-args t)
         (apply ctr# ~(get-arg-values t))))))
