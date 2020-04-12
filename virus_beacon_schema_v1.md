# virus_beacon_schema_v1
NOTE: (Metadata fields should be extracted from XML files. See metadata fields from Illumina and Nanopore XML files to feed virus beacon schema v1)


—VARIANT BASIC (basic beacon variant schema )
- [ ] ref_assembly_id
- [ ] start_nucleotide
- [ ] end_nucleotide
- [ ] ref
- [ ] alt
*descriptor: ref:start:ref:alt


—VARIANT  ANNOTATION  
- [ ] variant_id: (external ref if it exists)
- [ ] variant_type: e.g “missense variant”  (SO variant type ontology)
- [ ] genomic_region: categorical, from genomic annotation file for taxon_id (SARS-CoV2: 5UTR,ORF1ab, S, ORF3a, Intergenic, E,M, ORF6, ORF7a, ORF8, N, ORF10, 3UTR)
- [ ] functional_region: HVR1



—VARIANT IN SAMPLE
- [ ] biosample_id: (external ref ) e.g "SRS6007144"
- [ ] host_id: (external ref if it exists)
- [ ] variant_file_id: (external ref -or internal if we run pipeline)
- [ ] sequence_file_id:  (external ref) e.g "SRR10903401"
- [ ] variant_frequency_dataset (dataset):
- [x] variant_frequency_accross (all datasets 
- [ ] INFO
	- [ ] study_info: 
		- [ ] study_id: (study accession): e.g  "SRP242226"
		- [ ] study_ref: (article PUMED ID or URL)
	- [ ] experiment_info
		- [ ] exp_id (experiment accession): e.g  "SRX7571571"
		- [ ] exp_title: e.g ”Total RNA sequencing of BALF (human reads removed)”
		- [ ] exp_lib_strategy: (“RNA-Seq”, “WGS”, “AMPLICON”, “Targeted-Capture”) 
		- [ ] exp_lib_source: (“METATRANSCRIPTOMIC”, “METAGENOMIC”, “GENOMIC” , “VIRAL RNA”)
		- [ ] exp_lib_selection: ( “RANDOM”, “RT-PCR”, “RANDOM PCR”, “unspecified”, “PCR”, “cDNA”)
		- [ ] exp_lib_layout: (“PAIRED” “SINGLE”) 
		- [ ] exp_platform: (“Illumina , “Nanopore”)
		- [ ] exp_platform_model: (“Illumina MiSeq”, “Illumina MiniSeq” , ”Illumina HiSeq 2500” ,”NextSeq 500” , ”NextSeq 550”, “Illumina iSeq 100”, "GridION" ) 


—BIOSAMPLE 
- [ ] biosample_id: (external ref ) e.g "SRS6007144"
- [ ] biosample_alt_id: (external ref ) e.g "SAMN13872787" 
- [ ] biosample_type: (sample type/source) e.g "Bronchoalveolar lavage fluid”
- [ ] culture_cell: e.g: "Vero E6 cells (CRL-1586)" (NULL or none if not culture)
- [ ] culture_passage_history e.g "Original (not passaged)" (NULL or none if not culture)
- [ ] collection_date:  e.g  "2020-02-14" 
- [ ] study_ref: (article PUMED ID)


—HOST/INDIVIDUAL  (1 ENCOUNTER )
- [ ] host_taxon_id e.g "9606" (“Homo sapiens”)
- [ ] host_age: e.g “21”  (age in default schema)
- [ ] host_sex:  “female”, “male” (sex in default schema)
- [ ] geo_origin:  (different formats) e.g "USA:WI:Madison”/ "USA: CA, San Diego County”/ “30.52 N 114.31 E"  > harmonise, map to GAZ ontology (geographic origin in default schema)
- [ ] disease (virus-related disease e.g ("nCoV pneumonia”, "COVID-19" , "severe acute respiratory syndrome"))
- [ ] disease_stage: e.g “acute” (only one ca)
- [ ] comorbidities (individualDiseases from default schema): underlying chronic diseases 
- [ ] disease_course: e.g ”mild” 
- [ ] disease_outcome: e.g ”resolution/discharge” (harmonized from “Survived”)
- [ ] info 
	- [ ] study_ref (article PUMED ID)


— VIRUS
- [ ] taxon_id:  e.g ”433733”
- [ ] taxon_name: e.g “Severe acute 	 respiratory syndrome coronavirus 2”
- [ ] strain_id: 
- [ ] strain_name: e.g "2019-nCoV/USA-WI1/2020" 


