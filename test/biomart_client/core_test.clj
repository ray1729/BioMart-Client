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

(deftest idcc-test
  (let [idcc-url "http://www.i-dcc.org/biomart"]
    (testing "Connecting to I-DCC and retrieving marts"
      (let [ms (martservice idcc-url)]
        (is (map? (:marts ms)))
        (is (not (nil? ((:marts ms) "ikmc"))))
        (is (not (nil? ((:datasets ms) "dcc"))))
        (testing "Query dcc mart"
          (let [ds (dataset ms "dcc")]
            (is (not (nil? ds)))
            (let [res (query ds :filter { :marker_symbol "Art4" } :attrs [ "marker_symbol" "mgi_accession_id" ])]
              (is (= '(("Art4" "MGI:1202710")) res)))))))))
