(ns biomart-client.metadata
  (:use [biomart-client.utils :only (query-url parse-tsv parse-xml)])  
  (:require [clj-http.client :as http]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.zip-filter.xml :as zf]))

(defn- fetch-xml
  [martservice-url params]
  (let [res-body (apply str (:body (http/get (query-url martservice-url params))))]
    (zip/xml-zip (parse-xml res-body))))

(defn- fetch-tsv
  [martservice-url params]
  (let [res-body (apply str (:body (http/get (query-url martservice-url params))))]
    (parse-tsv res-body)))

(defn registry
  [martservice-url]
  (fetch-xml martservice-url {:type "registry"}))

(defn marts
  [registry]
  (zf/xml-> registry
            (zf/tag= :MartURLLocation)
            (zf/attr= :visible "1")
            #(:attrs (zip/node %))))

(defn datasets
  [martservice-url mart & {:keys [virtualSchema] :as params}]  
  (fetch-tsv martservice-url (assoc params :type "datasets" :mart mart)))

(defn configuration
  [martservice-url dataset & {:keys [virtualSchema interface martUser] :as params}]
  (fetch-xml martservice-url (assoc params :type "configuration" :dataset dataset)))

(defn attributes
  [martservice-url dataset & {:keys [virtualSchema interface martUser] :as params}]
  (fetch-tsv martservice-url (assoc params :type "attributes" :dataset dataset)))

(defn filters
  [martservice-url dataset & {:keys [virtualSchema interface martUser] :as params}]
  (fetch-tsv martservice-url (assoc params :type "filters" :dataset dataset)))

(defn default-attributes
  [dataset-config]
  (zf/xml-> dataset-config
            (zf/tag=  :AttributePage)
            (zf/attr= :internalName "attributes")
            (zf/tag=  :AttributeGroup)
            (zf/tag=  :AttributeCollection)
            (zf/tag=  :AttributeDescription)
            (zf/attr= :default "true")
            (zf/attr  :internalName)))
