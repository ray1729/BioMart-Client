# biomart-client

Clojure library to query unRESTful BioMart web services.

## Usage

### Querying

    (use '[biomart-client.utils :only (martservice-url)]
         '[biomart-client.query])

    (def ms (martservice-url "http://www.i-dcc.org/biomart"))
    ;=> #'user/ms
    ;=> "http://www.i-dcc.org/biomart/martservice"

    (query ms {:dataset "dcc"
               :filter  {:marker_symbol ["Art4" "Cbx1"]}
               :attrs   [:marker_symbol :mgi_accession_id]}))
    ;=> ({:mgi_accession_id "MGI:1202710", :marker_symbol "Art4"}
         {:mgi_accession_id "MGI:105369", :marker_symbol "Cbx1"})

    (query ms {:count true} {:dataset "dcc"
                             :filter  {:marker_symbol '(art4 abcd1 cbx1)}
                             :attrs   [:mgi_accession_id]}))
    ;=> ({:count 3})

### BioMart metadata

    (use '[biomart-client.metadata])

    (def r (registry ms))
    (map :name (marts r))
    ;=> ("ikmc" "biomart" "htgt" "ikmc_unitrap" "emma_biomart"
         "Eurexpress Biomart" "europhenomeannotations" "ENSEMBL_MART_ENSEMBL"
         "ENSEMBL_MART_VEGA")

    (datasets ms "ikmc")
    ;=> (("TableSet" "kermits" "IKMC Mouse Production" "1" "" "200" "50000" "default" "2011-03-24 16:09:13") ("TableSet" "omim" "OMIM (with MGI Mouse Orthologue Mappings)" "1" "" "200" "50000" "default" "2011-03-24 16:09:14") ("TableSet" "idcc_targ_rep" "IKMC Targeted Products" "1" "" "200" "50000" "default" "2011-03-24 16:09:13") ("TableSet" "dcc" "IKMC Projects / Alleles" "1" "" "200" "50000" "default" "2011-03-24 16:09:12"))

    (map #(nth % 1) (datasets ms "ikmc"))
    ;=> ("kermits" "omim" "idcc_targ_rep" "dcc")

    (def c (configuration ms "dcc"))
    (default-attributes c)
    ;=> ("marker_symbol" "mgi_accession_id" "ikmc_project" "ikmc_project_id" "status" "mouse_available" "escell_available" "vector_available")

    (map first (attributes ms "dcc"))
    ;=> ("marker_symbol" "marker_name" "mgi_accession_id" "secondary_mgi_accession_id" "synonym" "ensembl_gene_id" "vega_gene_id" "ncbi_gene_id" "ccds_id" "omim_id" "omim_description" "chromosome" "strand" "start" "end" "ikmc_project" "ikmc_project_id" "status" "mouse_available" "escell_available" "vector_available" "mgi_gene_traps" "tigm_gene_traps" "igtc" "imsr" "targeted_mutations" "other_mutations")

    (def f (filters ms "dcc"))
    (map first f)
    ;=> ("marker_symbol" "mgi_accession_id" "secondary_mgi_accession_id" "ensembl_gene_id" "vega_gene_id" "ncbi_gene_id" "ccds_id" "omim_id" "chromosome" "start" "end" "strand" "project" "ikmc_project_id" "mouse_available" "escell_available" "vector_available" "status" "regeneron_status")

## Installation

Available from clojars.org; add:

    [biomart-client "1.0.0-SNAPSHOT"]

to your project.clj

## License

Copyright (C) 2010, Ray Miller <ray@1729.org.uk>.

THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE
PUBLIC LICENSE, <http://www.eclipse.org/org/documents/epl-v10.php>.

ANY USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES
RECIPIENT’S ACCEPTANCE OF THIS AGREEMENT.

## TODO

* Write more tests

* Deal with duplicate attrs in query parser? At the moment, for
  federated queries, an attribute in the second dataset will trample
  on data from the first dataset with the same attribute name.

* Implement work-around for boolean filters from Darren's Ruby library
  - see his dataset.rb class.  
