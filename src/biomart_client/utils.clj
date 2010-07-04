(ns biomart-client.utils
  (:use [clojure.contrib.string :only (as-str split)]
	[clojure.contrib.except :only (throwf)]
	[clojure.contrib.prxml])
  (:require [clojure-http.resourcefully :as http]
	    [clojure.xml :as xml]
	    [clojure.zip :as zip])
  (:import (java.net URLEncoder)))

;; Names of fields returned by 'datasets' query
(def *dataset-fields* [:type :dataset :displayName :visible :version
		       :initialBatchSize :maxBatchSize :interfaces :modified])

;; Character encoding used by URLEncoder
(def *enc* "UTF-8")

;; Default options used by build-query-xml
(def *default-query-opts* {:formatter           "TSV"
			   :header               "0"
			   :uniqueRows           "1"
			   :count                "0"
			   :datasetConfigVersion "0.6" })

(defn- query-str
  "Take a map of query parameters and return a string of the form
  \"key1=val1&key2=val2&key3=val3[...]\" with the keys and values
  URL encoded"
  [params]
  (letfn [(encode [s] (URLEncoder/encode (as-str s) *enc*))
	  (encode-key-val [m] (str (encode (key m)) "=" (encode (val m))))]
    (apply str (interpose "&" (map encode-key-val params)))))

(defn- ensure-martservice
  "Ensure that s ends in /martservice (if not, append it)"
  [s]
  (if (re-find #"/martservice$" s) s (str s "/martservice")))

(defn martservice-url
  ([server]
     (ensure-martservice server))
  ([server query]
     (str (ensure-martservice server) "?" (query-str query))))

(defn- parse-xml
  "Parse a string of XML and return a tree of the xml/element struct map.
   See clojure.xml/parse for details. BioMart may return an error string in
   the response body: assume this is what's happened if we encounter a parse
   error and throw an exception"
  [s]
  (try
    (xml/parse (org.xml.sax.InputSource. (java.io.StringReader. s)))
    (catch org.xml.sax.SAXParseException _ (throwf (str "BioMart error: " s)))))

(defn- parse-tsv
  [lines]
  (when-not (re-find #"\t" (first lines)) (throwf (str "BioMart errer: " lines)))
  (map #(split #"\t" %) (filter #(re-find #"\S" %) lines)))

(defn- seq-to-hash
  [ks]
  (fn [vs] (apply hash-map (interleave ks vs))))

(defn- fetch-tsv
  [martservice params]
  (let [res-body (:body-seq (http/get (martservice-url martservice params)))]
    (parse-tsv res-body)))

(defn- build-query-xml
  ([ds args]
     (letfn [(coll-to-csv [s] (if (coll? s) (apply str (interpose "," (map as-str (seq s)))) (as-str s)))
	     (attr-spec   [a] (vector :Attribute {:name a}))
	     (filter-spec [f] (vector :Filter { :name (as-str (key f)) :value (coll-to-csv (val f))}))]
       (let [filter (get args :filter {})
	     attrs  (get args :attrs (:default-attrs ds))
	     opts   (assoc (get args :opts {}) :virtualSchemaName (:serverVirtualSchema (:mart (:dataset ds))))]
	 (with-out-str (prxml [:decl!]
			      [:doctype! "Query"]
			      [:Query (into *default-query-opts* opts)
			       [:Dataset {:name (:name ds) :interface "default"}
				(map attr-spec attrs)
				(map filter-spec filter)]]))))))


(defn- wrap-biomart-errors
  "Examine the response body and throw an exception if any BioMart errors
   are identified"
  [response]
  (let [body (apply str (:body-seq response))]
    (if (re-find #"ERROR" body)
      (let [m (re-find #"(?:Filter|Attribute|Dataset) .+ NOT FOUND" body)]
	(if m (throwf "BioMart error: %s" m) (throwf "Biomart error")))
      response)))

(defn fetch-meta-xml
  [martservice params]
  (let [res-body (apply str (:body-seq (http/get (martservice-url martservice params))))]
    (zip/xml-zip (parse-xml res-body))))

(defn fetch-datasets
  [martservice mart]
  (map #(assoc % :mart mart)
       (map (seq-to-hash *dataset-fields*) (fetch-tsv martservice {:type "datasets" :mart (:name mart)}))))

(defn fetch-query-results
  [ds query-spec]
  (let [query-xml (build-query-xml ds query-spec)
	res-body  (:body-seq (wrap-biomart-errors (http/put (:url (:martservice ds)) {} (str "query=" query-xml))))]
    (parse-tsv res-body)))

