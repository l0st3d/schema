(ns l0st3d.util.schema
  (:require [clj-time.core :as cdt]
            [l0st3d.util.schema.internal :as i])
  (:import [org.joda.time.format DateTimeFormat]
           [java.util Date])) 

(defn- collect-errors []
  (let [errors (atom [])
        metadata (atom {})]
    {:error-handler (fn [err data-path data]
                      (swap! errors conj (str data-path (when data-path " ") err))
                      data)
     :metadata-handler (fn [f] (swap! metadata f))
     :get-errors (fn [] @errors)
     :get-metadata (fn [] @metadata)}))

(defn- check-valid [e validation-fn error-handler data-path err-msg]
  (cond (and (fn? validation-fn) (not (validation-fn e)))
        (error-handler err-msg data-path e)

        (and (instance? java.util.regex.Pattern validation-fn) (not (re-matches validation-fn (str e))))
        (error-handler err-msg data-path e)

        :else
        e))

(defn- validate-element [type-def data data-path error-handler metadata-handler]
  (if (fn? type-def)
    (type-def data data-path error-handler metadata-handler)
    (throw (ex-info (str "unknown type-def " type-def) {:type-def type-def}))))

;; validation fns
(defn is-string
  ([] (is-string nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler metadata-handler]
       (if (string? e)
         (check-valid e validation-fn error-handler data-path err-msg)
         (error-handler "should be a string" data-path e)))))

(defn is-keyword
  ([] (is-keyword nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler metadata-handler]
       (if (keyword? e)
         (check-valid e validation-fn error-handler data-path err-msg)
         (error-handler "should be a keyword" data-path e)))))

(defn is-boolean []
  (fn [e data-path error-handler metadata-handler]
    (if (contains? #{true false nil "true" "false" "TRUE" "FALSE"} e)
      (contains? #{true "true" "TRUE"} e)
      (error-handler "should be a boolean" data-path e))))

(defn is-integer
  ([] (is-integer nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler metadata-handler]
       (cond (number? e) (check-valid e validation-fn error-handler data-path err-msg)
             (string? e) (try (-> e Long/parseLong (check-valid validation-fn error-handler data-path err-msg)) (catch Exception ex (error-handler "should be a number" data-path e)))
             :else (error-handler "should be a number" data-path e)))))

(defn is-decimal
  ([] (is-decimal nil nil))
  ([validation-fn err-msg]
     (fn [e data-path error-handler metadata-handler]
       (cond (number? e) (check-valid (bigdec e) validation-fn error-handler data-path err-msg)
             (string? e) (try (-> e BigDecimal. (check-valid validation-fn error-handler data-path err-msg)) (catch Exception ex (error-handler "should be a decimal" data-path e)))
             :else (error-handler "should be a decimal" data-path e)))))

(defn is-date []
  (let [parse-fmts [(DateTimeFormat/forPattern "yyyy-MM-dd'T'HH:mm:ss.SSSZ") (DateTimeFormat/forPattern "EEE, dd MMM yyyy HH:mm:ss zzz")]]
    (fn [e data-path error-handler metadata-handler]
      (cond (instance? java.util.Date e) e
            (string? e) (if (re-matches #"[0-9]+" e)
                          (Date. (Long. (str e "000")))
                          (some #(try (.toDate (.parseDateTime % e)) (catch IllegalArgumentException ex)) parse-fmts))
            :else (error-handler "should be a date" data-path e)))))

(defn should-exist []
  (fn [data data-path error-handler metadata-handler]
    (if (nil? data)
      (error-handler "should exist" data-path data)
      data)))

(defn is-nil []
  (fn [data data-path error-handler metadata-handler]
    (when-not (nil? data)
      (error-handler "should be nil" data-path data))))

(defn equals
  ([v] (equals v (str "Should be '" v "'")))
  ([v error-message]
     (fn [data data-path error-handler metadata-handler]
       (if (not= v data)
         (error-handler error-message data-path data)
         v))))

(defn matches
  ([error-message test-func]
   (matches error-message test-func identity))
  ([error-message test-func coerce-func]
   (fn [data data-path error-handler metadata-handler]
     (when-not (test-func data)
       (error-handler error-message data-path data))
     (coerce-func data))))

(defn is-a [class]
  (matches (str "Should be an instance of " class) #(instance? class %)))

;; collection validation fns
(defn map-of [& keys-and-vals]
  (with-meta
    (let [keys-and-vals (apply hash-map keys-and-vals)]
      (fn [data data-path error-handler metadata-handler]
        (if (map? data)
          (reduce (fn [m [k val-type-def]]
                    (let [d (validate-element val-type-def (get data k) (conj data-path k) error-handler metadata-handler)]
                      (if (contains? data k)
                        (assoc m k d)
                        m))) {} keys-and-vals)
          (error-handler "should be a map" data-path data))))
    (let [p (partition 2 keys-and-vals)]
      {::map-keys (map first p)
       ::map-vals (map second p)})))

(defn map-of-values [key-func val-type-def]
  (fn [data data-path error-handler metadata-handler]
    (if (map? data)
      (->> data vals
           (reduce #(let [v (validate-element val-type-def %2 (conj data-path "value") error-handler metadata-handler)]
                      (assoc %1 (key-func v) v)) {}))
      (error-handler "should be a map" data-path data))))

(defn list-of [type-def]
  (fn [data data-path error-handler metadata-handler]
    (if (sequential? data)
      (->> data
        (map #(validate-element type-def %2 (conj data-path %1) error-handler metadata-handler) (iterate inc 0))
        (into []))
      (error-handler "should be a list" data-path data))))

(defn one-of [& type-defs]
  (fn [data data-path error-handler metadata-handler]
    (let [one-of-errors (atom [])
          one-of-error-handler-called (atom nil)
          one-of-error-handler (fn [err data-path data]
                                 (swap! one-of-error-handler-called (constantly true))
                                 (swap! one-of-errors conj err)
                                 data)]
      (loop [tds type-defs]
        (if (seq tds) 
          (let [td (first tds)
                r (validate-element td data data-path one-of-error-handler metadata-handler)]
            (if @one-of-error-handler-called
              (do (swap! one-of-error-handler-called (constantly nil))
                  (recur (next tds)))
              r))
          (error-handler (->> @one-of-errors (interpose " or ") (apply str)) data-path data))))))

(defn all-of [& type-defs]
  (fn [data data-path error-handler metadata-handler]
    (->> type-defs
      (map #(validate-element % data data-path error-handler metadata-handler))
      doall
      first)))

(defn count-is [message func & args]
  (fn [data data-path error-handler metadata-handler]
    (when-not (apply func (count data) args)
      (error-handler (str "count should be " message) data-path data))
    data))

(defn with-defaults [type-def & {:as defaults}]
  (with-meta (fn [data data-path error-handler metadata-handler]
               (validate-element type-def (merge data (apply dissoc defaults (keys data))) data-path error-handler metadata-handler))
    (let [ks (::map-keys (meta type-def))
          vs (::map-vals (meta type-def))
          kvs (remove nil? (map #(when-not (get defaults %1) [%1 %2]) ks vs))]
      {::map-keys (map first kvs)
       ::map-vals (map second kvs)})))

(defn with-modifications [type-def & modifier-fns]
  (with-meta
    (fn [data data-path error-handler metadata-handler]
      (try
        (reduce #(%2 %1) (validate-element type-def data data-path error-handler metadata-handler) modifier-fns)
        (catch Throwable t
          (error-handler (str "Modification Exception : " t) data-path data)
          data)))
    (meta type-def)))

(defn compose-with [merge-fn & type-defs]
  (with-meta
    (fn [data data-path error-handler metadata-handler]
      (->> type-defs
        (map #(validate-element % data data-path error-handler metadata-handler))
        (apply merge-fn)))
    (->> type-defs
      (map meta)
      (apply merge-with concat))))

(defn compose [& type-defs]
  (apply compose-with merge type-defs))

;; constraints
(defn unique []
  (fn [data data-path error-handler metadata-handler]
    (metadata-handler #(assoc % ::unique-paths (conj (or (::unique-paths %) []) data-path)))
    data))

(defn foreign-key [entity]
  (fn [data data-path error-handler metadata-handler]
    (metadata-handler #(assoc-in % (into [::foreign-key-paths] data-path) entity))
    data))

;; data cleaners
(defn trimmed [f]
  (comp #(if (string? %) (.trim %) %) f))

(defn lower-cased [f]
  (comp #(if (string? %) (.toLowerCase %) %) f))

;; API
(defn get-errors [data type-def]
  (let [{:keys [error-handler metadata-handler get-errors get-metadata]} (collect-errors)]
    (validate-element type-def data [] error-handler metadata-handler)
    (get-errors)))

(defn get-data-ignoring-errors [data type-def]
  (let [{:keys [error-handler metadata-handler get-errors get-metadata]} (collect-errors)
        d (validate-element type-def data [] #(-> %& (nth 2)) metadata-handler)]
    (with-meta d (merge (get-metadata) {::valid nil}))))

(defn validate
  ([type-def]
   (fn [data]
     (let [{:keys [error-handler metadata-handler get-errors get-metadata]} (collect-errors)
           d (if (-> data meta ::valid) data (validate-element type-def data [] error-handler metadata-handler))]
       (if-let [errs (not-empty (get-errors))]
         (throw (ex-info (str "there were validation errors" errs " " data) {:message "there were validation errors" :errors errs :handle :validation-errors :data data}))
         (with-meta d (merge (get-metadata) {::valid true}))))))
  ([data type-def]
   (apply (validate type-def) [data])))

(defn merge-and-validate [new data type-def]
  (validate (merge data new) type-def))

(defn get-constructor [type-def]
  (let [ks (::map-keys (meta type-def))
        args (i/get-args type-def)
        arg-vals (i/get-arg-values type-def)
        ctr (comp #(validate % type-def)
                  (eval `(fn ~args
                           ~(zipmap ks arg-vals))))]
    ctr))

