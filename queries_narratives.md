# queries\_narratives
1. **Query by variant** (VARIANT  DEFAULT)
U queries for a specific variant of interest by entering  variant descriptor (nt26141 A\>C) and wants to know whether this specific variant has been found elsewhere, and if so, in what studies, in what frequency and in which sample types (BALF vs Oro-pharyngeal swab) and in which host  (e.g human vs others, country, sex or clinical outcome fatal) . U may want to filter by any of this, for example, is interested only in results from human, from sample type BALF, country USA, a specific viral strain and missense variants

Response will include VARIANT IN SAMPLE, BIOSAMPLE and HOST endpoints, and will be limited by filters from these two and also from VARIANT ANNOTATION and VIRUS schema.

Filters: 
BIOSAMPLE:sample\_type= "Bronchoalveolar lavage fluid”
HOST:taxon\_id= "9606" (default)
HOST:geo\_origin= "USA"
VIRUS:strain\_name=”2019-nCoV/USA-WI1/2020" 
VARIANT\_ANNOTATION:variant\_type= “missense variant”

Response:
VARIANT IN SAMPLE\> variant\_frequency\_dataset, Info:study\_info:study\_id, 
BIOSAMPLE\> biosample\_type
HOST\> host\_age, host\_sex, geo\_origin, disease\_course, disease\_outcome
Handover\> VARIANT IN SAMPLE:Info:study\_info:study\_ref


2. **Query by region** (VARIANT ANNOTATION)

U queries for all variants in ORF8 by entering only a filter by VARIANT  ANNOTATION:region
and if so, in what frequency has each of them been found, in which sample types (BALF vs Oro-pharyngeal swab) and  associated to which clinical outcome. U may want to filter to select only missense variants

Response will include VARIANT IN SAMPLE, BIOSAMPLE and HOST endpoints, and will be limited by filter from Variant Annotation.

Filters: 
HOST:taxon\_id= "9606" (default)
VARIANT\_ANNOTATION:variant\_type= “missense variant”



Response:
VARIANT BASIC\> variant\_descriptor
VARIANT ANNOTATION\> variant\_type
VARIANT IN SAMPLE\> variant\_frequency\_dataset
BIOSAMPLE\> biosample\_type
HOST\> host\_age, host\_sex, geo\_origin, disease\_course, disease\_outcome
Handover\> BIOSAMPLE:study\_ref


3. **Query by functional regions/domains** (VIRUS)