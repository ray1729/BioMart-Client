(ns biomart-client.utils
  (:use [clojure.contrib.string :only (as-str split blank?)]
        [clojure.contrib.except :only (throwf)])
  (:require [clj-http.client :as http]
            [clojure.xml :as xml])
  (:import (java.net URLEncoder)))

;; Character encoding used by URLEncoder
(def *enc* "UTF-8")

(defn- query-str
  "Take a map of query parameters and return a string of the form
  \"key1=val1&key2=val2&key3=val3[...]\" with the keys and values
  URL encoded"
  [params]
  (letfn [(encode [s] (URLEncoder/encode (as-str s) *enc*))
          (encode-key-val [m] (str (encode (key m)) "=" (encode (val m))))]
    (apply str (interpose "&" (map encode-key-val (remove (comp nil? val) params))))))

(defn- ensure-martservice
  "Ensure that s ends in /martservice (if not, append it)"
  [s]
  (if (re-find #"/martservice$" s) s (str s "/martservice")))

(defn- check-redirects
  "Check for redirects, mainly on Ensembl, and reset the base URL"
  [url]
  (let [redir-loc (-> (http/post url) :headers (get "location"))]
    (if (nil? redir-loc)
      url
      (first (split #"\?" redir-loc)))))

(defn martservice-url
  [server]
  (-> server ensure-martservice check-redirects))

(defn query-url
  [martservice-url params]
  (str martservice-url "?" (query-str params)))

(defn parse-xml
  "Parse a string of XML and return a tree of the xml/element struct map.
   See clojure.xml/parse for details. BioMart may return an error string in
   the response body: assume this is what's happened if we encounter a parse
   error and throw an exception"
  [s]
  (try
    (xml/parse (org.xml.sax.InputSource. (java.io.StringReader. s)))
    (catch org.xml.sax.SAXParseException _ (throwf "BioMart error: %s" (str s)))))

(defn parse-tsv
  "Parse a string of tab-separated data, first splitting on newline
  into rows, then splitting each row on tab."
  [body]
  (when (.startsWith body "Problem retrieving")
    (throwf "Biomart error: %s" body))
  (let [rows (filter (comp not blank?) (split #"\n" body))]
    (map #(split #"\t" %) rows)))
