(ns biomart-client.query-test
  (:use [biomart-client.utils :only (martservice-url)])
  (:use [biomart-client.query] :reload-all)
  (:use [clojure.test]))

(deftest ensembl-query-test
  (testing "Querying Ensembl martservice"
    (let [ms (martservice-url "http://www.ensembl.org/biomart")]
      (is ((comp not nil?) (re-find #"/martservice$" ms)))
      (let [rs (query ms {} (dataset "hsapiens_gene_ensembl"
                                     {:chromosome_name "6" :hgnc_symbol "HLA-A"}
                                     [:ensembl_gene_id :hgnc_symbol]))
            r (first rs)]
        (is (map? r))
        (is (:ensembl_gene_id r) "ENSG00000206503")
        (is (:hgnc_symbol r) "HLA-A")))))

(deftest idcc-query-test
  (testing "Querying IDCC martservice"
    (let [ms (martservice-url  "http://www.i-dcc.org/biomart")]
      (is ((comp not nil?) (re-find #"/martservice$" ms)))
      (let [rs (query ms {} (dataset "dcc"
                                     {:marker_symbol "Art4"}
                                     [:marker_symbol :mgi_accession_id]))
            r  (first rs)]
        (is (map? r))
        (is (:marker_symbol r) "Art4")
        (is (:mgi_accession_id r) "MGI:1202710"))
      (let [rs (query ms {:count true} (dataset "dcc"
                                                {:marker_symbol '(art4 abcd1 cbx1)}
                                                [:mgi_accession_id]))]
        (is (= rs 3))))))
