(ns biomart-client.query  
  (:use [biomart-client.utils   :only (parse-tsv)]
        [clojure.contrib.string :only (as-str join split blank? lower-case replace-re trim)]
        [clojure.contrib.except :only (throwf)]
        [clojure.contrib.prxml  :only (prxml)])
  (:require [clj-http.client :as http]))

(def *default-query-opts* {:formatter            "TSV"
                           :header               true
                           :uniqueRows           true
                           :count                false
                           :datasetConfigVersion "0.6"
                           :virtualSchemaName    "default"})

(defn dataset
  "Specify the dataset, filter and attributes for the query."
  ([name filter attrs]
     (dataset name filter attrs "default"))
  ([name filter attrs interface]
     (letfn [(coll-to-csv [s] (if (coll? s) (join "," (map as-str (seq s))) (as-str s)))
             (attr-spec   [a] (vector :Attribute {:name (as-str a)}))
             (filter-spec [f] (vector :Filter {:name (as-str (key f)) :value (coll-to-csv (val f))}))]
       (vec (remove nil? (vector :Dataset {:name name :interface interface}
                                 (when attrs (map attr-spec attrs))
                                 (when filter (map filter-spec filter))))))))

(defn- build-query-xml
  "Build XML required to perform martservice query."
  [opts datasets]
  (let [boolean->str (fn [b] (cond
                             (identical? b true) "1"
                             (identical? b false) "0"
                             :else b))
        opts (zipmap (keys opts) (map boolean->str (vals opts)))]
    (with-out-str (prxml [:decl!] [:doctype! "Query"] [:Query opts datasets]))))

(defn- wrap-biomart-errors
  "Examine the response body and throw an exception if any BioMart errors
   are identified"
  [response]
  (let [body (apply str (:body response))]
    (if (re-find #"ERROR" body)
      (let [m (re-find #"(?:Filter|Attribute|Dataset) .+ NOT FOUND" body)]
        (if m (throwf "BioMart error: %s" m) (throwf "Biomart error: %s" body)))
      response)))

(defn- fetch-query-results
  "POST query to martservice-url and check for BioMart errors,
  throwing an exception if any occur."
  [martservice-url query-xml]
  (:body (wrap-biomart-errors (http/post martservice-url {:body (str "query=" query-xml)}))))

(defn- parse-query-results
  "Parse a string containing query results in TSV format. Returns a
  seq of maps keyed on column headers parsed from the first line of
  the response body."
  [res-body]
  (let [col-name->keyword (fn [c] (->> c trim (replace-re #"\s+" "_") lower-case keyword))
        rows (parse-tsv res-body)
        cols (map col-name->keyword (first rows))]
    (map #(zipmap cols %) (rest rows))))

(defn- parse-count
  "Parse a string containing count result returned by
  martservice. This should be one or more digits followed by a
  newline. Throw an error if body is not in expected format"
  [body]
  (if-let [r (re-seq #"^\d+$" body)]
    (Integer/parseInt (first r))
    (throwf "Biomart error: cannot parse count '%s'" body)))

(defn query
  "Query the specified datasets and return parsed results. For a
  normal query, a seq of maps keyed on column name is returned. When a
  count is requested (by passing opts {:count true}) the response will
  simply be an integer."
  [martservice-url opts & datasets]
  (when-not (or (= 1 (count datasets)) (= 2 (count datasets)))
    (throwf "query requires 1 or 2 datasets"))
  (let [opts      (merge *default-query-opts* opts)
        query-xml (build-query-xml opts datasets)
        res-body  (fetch-query-results martservice-url query-xml)]
    (if (opts :count)
      (parse-count res-body)
      (parse-query-results res-body))))
