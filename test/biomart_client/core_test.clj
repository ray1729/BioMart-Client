(ns biomart-client.core-test
  (:use [biomart-client.core] :reload-all)
  (:use [clojure.test]))

(deftest ensembl-test
  (let [ens-url "http://www.ensembl.org/biomart"]
    (testing "Conecting to Ensembl and retrieving marts"
      (let [ms (martservice ens-url)]
        (is (not (nil? ((:marts ms) "ENSEMBL_MART_ENSEMBL"))))
        (is (not (nil? ((:datasets ms) "hsapiens_gene_ensembl"))))
        (testing "Basic dataset queries"
          (let [ds (dataset ms "hsapiens_gene_ensembl")
                result (query ds :filter {:chromosome_name "6" :hgnc_symbol "HLA-A"}
                                 :attrs ["ensembl_gene_id" "hgnc_symbol"])]
            (is (= '(("ENSG00000206503" "HLA-A")) result))))))))
