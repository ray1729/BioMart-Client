(ns biomart-client.core
  (:use [biomart-client.utils :only (martservice-url fetch-meta-xml fetch-datasets)])
  (:require [clojure-http.resourcefully :as http]
	    [clojure.zip :as zip]
	    [clojure.contrib.zip-filter.xml :as zf]))

(defstruct martservice-struct :url :registry)

(defn martservice
  [server]
  (let [url (martservice-url server)]
    (struct martservice-struct url (fetch-meta-xml url {:type "registry"}))))

(defn marts
  [ms]
  (zf/xml-> (:registry ms)
	    (zf/tag= :MartURLLocation) (zf/attr= :visible "1") #(:attrs (zip/node %))))

(defn datasets
  ([ms mart]
     (fetch-datasets (:url ms) mart))
  ([ms]
     (apply concat (map #(fetch-datasets (:url ms) (:name %)) (marts ms)))))


;; (defn visible-datasets
;;   [registry]
;;   (zf/xml-> registry (zf/tag= :MartURLLocation) (zf/attr= :visible "1") (zf/attr :name)))

;; (defn dataset
;;   [martservice dataset-name]
;;   (letfn [(attributes
;; 	   [config]
;; 	   (zf/xml-> config
;; 		     (zf/tag=  :AttributePage)
;; 		     (zf/attr= :internalName "attributes")
;; 		     (zf/tag=  :AttributeGroup)
;; 		     (zf/tag=  :AttributeCollection)
;; 		     (zf/tag=  :AttributeDescription)
;; 		     (zf/attr  :internalName)))
;; 	  (default-attributes
;; 	    [config]
;; 	    (zf/xml-> config
;; 		     (zf/tag=  :AttributePage)
;; 		     (zf/attr= :internalName "attributes")
;; 		     (zf/tag=  :AttributeGroup)
;; 		     (zf/tag=  :AttributeCollection)
;; 		     (zf/tag=  :AttributeDescription)
;; 		     (zf/attr= :default "true")
;; 		     (zf/attr  :internalName)))
;; 	  (software-version
;; 	   [config]
;; 	   (zf/xml-> config (zf/attr :softwareVersion)))]
;;     (let [config (fetch-metadata martservice {:type "configuration" :dataset dataset-name})]
;;       {:martservice        martservice
;;        :dataset            dataset-name
;;        :softwareVersion    (software-version config)
;;        :attributes         (attributes config)
;;        :default-attributes (default-attributes config)}
;;       )))

;; (defn registry
;;   [martservice]
;;   (let [registry (fetch-metadata martservice {:type "registry"})
;; 	datasets (zf/xml-> registry (zf/tag= :MartURLLocation) (zf/attr= :visible "1") #(:attrs (zip/node %)))]
;;     (apply hash-map (interleave (map :name datasets) datasets))))
	


;; get *all* attrs for a node
;; (zf/xml-> registry (zf/tag= :MartURLLocation) (zf/attr= :visible "1") #(:attrs (zip/node %)))

(comment

  (def ms (martservice "http://www.biomart.org/biomart"))

  (list-marts ms)

  (list-datasets ms)

  (query ms ds-name query-spec)
  )