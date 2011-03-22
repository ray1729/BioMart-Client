(ns biomart-client.core
  (:use [biomart-client.utils :only (martservice-url fetch-meta-xml
                                     fetch-datasets fetch-query-results)])
  (:require [clojure.zip :as zip]
            [clojure.contrib.zip-filter.xml :as zf]))

(defstruct martservice-struct :url :registry :marts :datasets)

(defn martservice
  [server]
  (let [url      (martservice-url server)
        registry (fetch-meta-xml url {:type "registry"})
        marts    (zf/xml-> registry
                           (zf/tag= :MartURLLocation)
                           (zf/attr= :visible "1")
                           #(:attrs (zip/node %)))
        datasets (apply concat (map #(fetch-datasets url %) marts))]    
    (struct martservice-struct url registry
            (apply hash-map (interleave (map :name marts) marts))
            (apply hash-map (interleave (map :dataset datasets) datasets)))))

(defn list-marts [ms] (doseq [m (keys (:marts ms))] (println m)))

(defn list-datasets [ms] (doseq [d (keys (:datasets ms))] (println d)))

(defstruct dataset-struct :martservice :virtualschema :dataset :config :attrs :default-attrs)

(defn dataset
  [ms dsname & [virtualschema]]
  (let [virtualschema (or virtualschema "default")
        dsconfig      (fetch-meta-xml (:url ms) {:type "configuration" :dataset dsname :virtualschema virtualschema})
        attrs         (zf/xml-> dsconfig
                                (zf/tag=  :AttributePage)
                                (zf/attr= :internalName "attributes")
                                (zf/tag=  :AttributeGroup)
                                (zf/tag=  :AttributeCollection)
                                (zf/tag=  :AttributeDescription)
                                (zf/attr  :internalName))
        default-attrs (zf/xml-> dsconfig
                                (zf/tag=  :AttributePage)
                                (zf/attr= :internalName "attributes")
                                (zf/tag=  :AttributeGroup)
                                (zf/tag=  :AttributeCollection)
                                (zf/tag=  :AttributeDescription)
                                (zf/attr= :default "true")
                                (zf/attr  :internalName))]
    (struct dataset-struct ms virtualschema dsname dsconfig attrs default-attrs)))

(defn query
  [ds & args]
  (let [query-spec (apply hash-map args)
        res (fetch-query-results ds query-spec)]
    res))
