(ns biomart-client.utils
  (:use [clojure.contrib.string :only (as-str split)]
	[clojure.contrib.except :only (throwf)])
  (:require [clojure-http.resourcefully :as http]
	    [clojure.xml :as xml]
	    [clojure.zip :as zip])
  (:import (java.net URLEncoder)))

;; Names of fields returned by 'datasets' query
(def *dataset-fields* [:type :dataset :displayName :visible :version
		       :initialBatchSize :maxBatchSize :interfaces :modified])

;; Character encoding used by URLEncoder
(def *enc* "UTF-8")

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
  (map #(split #"\t" %) lines))

(defn- seq-to-hash
  [ks]
  (fn [vs] (apply hash-map (interleave ks vs))))

(defn fetch-tsv
  [martservice params]
  (let [res-body (filter #(re-find #"\S" %) (:body-seq (http/get (martservice-url martservice params))))]
    (parse-tsv res-body)))

(defn fetch-meta-xml
  [martservice params]
  (let [res-body (apply str (:body-seq (http/get (martservice-url martservice params))))]
    (zip/xml-zip (parse-xml res-body))))

(defn fetch-datasets
  [martservice mart]
  (map (seq-to-hash *dataset-fields*) (fetch-tsv martservice {:type "datasets" :mart mart})))
  

;; XXX Incomplete! BioMart does not always put "ERROR" in the body; for example,
;; a request for <http://www.sanger.ac.uk/htgt/biomart/martservice?type=datasets&mart=kermits>
;; will return HTTP status 200 and body "Problem retrieving datasets for mart kermits, check your parameters"
;; (defn- wrap-biomart-errors
;;   "Examine the response body and throw an exception if any BioMart errors
;;    are identified"
;;   [response]
;;   (let [body (apply str (:body-seq response))]
;;     (if (re-find #"ERROR" body)
;;       (let [m (re-find #"(?:Filter|Attribute|Dataset) .+ NOT FOUND" body)]
;; 	(if m (throwf "Biomart error: %s" m) (throwf "Biomart error")))
;;       response)))
