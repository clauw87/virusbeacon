# query_types 
User can select from three query types:

1. Query by variant> Query for a specific variant  (user enters a variant descriptor)

2. (discovery mode>) Query by annotation: genomic region (user selects from list one or more genomic regions, or full-length, to search for variants)
-Query based on annotation of genomic regions: a set of nucleotide positions, defined genomic regions including untranslated, intergenic and ORFs, custom regions, or the full-length sequence can be selected as search pool

3. (discovery mode>) Query by annotation: functional domain (user selects from list one or more functional regions/ domains to search for variants)
-Query based on annotation of functional domains: defined annotated functional domains including surface motifs, hyper variable regions, receptor binding sites and RNA modification sites can be selected as search pool


Each query type is associated to a response schema and a set of filters.
*Filters associated to each schema are in virus_beacon_filters.md
*Examples for queries and their responses are in queries_narratives.md