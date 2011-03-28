(ns biomart-client.query-test
  (:use [biomart-client.utils :only (martservice-url)])
  (:use [biomart-client.query] :reload-all)
  (:use [clojure.test]))

(deftest ensembl-query-test
  (testing "Querying Ensembl martservice"
    (let [ms (martservice-url "http://www.ensembl.org/biomart")]
      (is ((comp not nil?) (re-find #"/martservice$" ms)))
      (let [rs (query ms {:dataset "hsapiens_gene_ensembl"
                          :filter  {:chromosome_name "6" :hgnc_symbol "HLA-A"}
                          :attrs   [:ensembl_gene_id :hgnc_symbol]})
            r (first rs)]
        (is (map? r))
        (is (:ensembl_gene_id r) "ENSG00000206503")
        (is (:hgnc_symbol r) "HLA-A")))))

(deftest idcc-query-test
  (testing "Querying IDCC martservice"
    (let [ms (martservice-url  "http://www.i-dcc.org/biomart")]
      (is ((comp not nil?) (re-find #"/martservice$" ms)))
      (let [rs (query ms {:dataset "dcc"
                          :filter  {:marker_symbol "Art4"}
                          :attrs   [:marker_symbol :mgi_accession_id]})
            r  (first rs)]
        (is (map? r))
        (is (:marker_symbol r) "Art4")
        (is (:mgi_accession_id r) "MGI:1202710"))
      (let [rs (query ms {:count true} {:dataset "dcc"
                                        :filter  {:marker_symbol '(art4 abcd1 cbx1)}
                                        :attrs   [:mgi_accession_id]})]
        (is (= (:count (first rs)) 3))))))

(deftest federated-query-test
  (testing "Federated query via IDCC martservice"
    (let [ms       "http://www.i-dcc.org/biomart/martservice"
          targ_rep {:dataset "idcc_targ_rep"
                    :attrs   [:ikmc_project_id :mgi_accession_id :targeting_vector :escell_clone :pipeline]}
          kermits  {:dataset "kermits"
                    :filter  {:status [ "Genotype Confirmed" "Germline transmission achieved"]}
                    :attrs   [:status :emma :mi_centre :mi_date]}
          results  (query ms targ_rep kermits)]
      (is (seq? results))
      (is (map? (first results)))
      (is (:pipeline (first results)))
      (is (:ikmc_project_id (first results)))
      (is (:mi_centre (first results))))))
