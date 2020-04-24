# virus_beacon_schema_v1_to_generic
—VARIANT BASIC (basic beacon variant schema: from VCF)

- [ ]  ref_assembly_id -> _Variant Basic refAssemblyId_ 

- [ ] start_nucleotide -> _Variant Basic startPos_ 

- [ ] end_nucleotide -> _Variant Basic  endPos_
- [ ] ref_sequence  -> _Variant Basic ref
- [ ] alt_sequence -> _Variant Basic  alt
- [ ] ::variantType: SNV, indel, CNV, structural variant:: -> Variant Basic  variantType

ORGANISM
	- [ ] taxon_id (taxon of sequenced species) -> Organism taxonId

—VARIANT  ANNOTATION  
- [ ] variant_id: -> Variant Annotation variantId
- [ ] variant effect: e.g “missense variant”  -> Variant Annotation molecularConsequence
- [ ] genomic_region: categorical, from virus genomic annotation in VIRUS: annotation (SARS-CoV2: 5UTR,ORF1ab, S, ORF3a, Intergenic, E,M, ORF6, ORF7a, ORF8, N, ORF10, 3UTR) ->   Variant Annotation genomicRegionClass and featureId
- [x] functional_region:  categorical, from functional annotation file VIRUS: annotation e.g “HVR”, “RBD”, “RNA modification site” or manual entering by user. REMOVED FOR NOW



—VARIANT IN SAMPLE
- [ ]  :::: biosample_id: (external ref ) e.g “SRS6007144” :::: -> Variant in Sample biosampleId 
- [ ]  :::: host_id: (external ref if it exists)  :::: -> Variant in Sample individualId
- [ ] variant_frequency_dataset: from vcf  ->  Variant in Sample variantFrequency
- [ ] info
	- [x] study_info: 
		- [x] study_id: (study accession): e.g  "SRP242226"
		- [x] study_ref: (article PUMED ID or URL)
	- [ ] experiment_info
		- [x] ::::variant_file_id: (accession if external ref -or internal if we run pipeline)- >  (to info?)::::
		- [ ] run_id:  (run accession) e.g "SRR10903401"  ->  Run runId
		- [ ] exp_id (experiment accession): e.g  "SRX7571571"  ->  Run info experimentId
		- [ ] exp_title: e.g ”Total RNA sequencing of BALF (human reads removed)”  ->  Run info experimentTitle
		- [ ] exp_lib_strategy: (“RNA-Seq”, “WGS”, “AMPLICON”, “Targeted-Capture”)  ->  Run libraryStrategy
		- [ ] exp_lib_source: ->  Run librarySource 
		- [ ] exp_lib_selection: -> Run librarySelection
		- [ ] exp_lib_layout: -> Run libraryLayout
		- [ ] exp_platform: -> Run platform
		- [ ] exp_platform_model: -> Run platformModel
		- [ ] variant caller: (from VCF or Galaxy or Master of Pores pipelines) ->  Variant in Sample variantCaller
	


—BIOSAMPLE  
- [ ] :::: biosample_id: (external ref ) e.g "SRS6007144":::: -> Biosample biosampleId 
- [ ] :::: biosample_alt_id: (external ref ) e.g "SAMN13872787"::::
- [ ] :::: individual_id :::: -> Biosample IndividualId 
- [ ] collection_date:  e.g  "2020-02-14"  -> Biosample collectionDate
- [ ] biosample_type: (sample type/source) e.g "Bronchoalveolar lavage fluid” or “Cellular passage” -> sampleOriginType and sampleOriginDetail
- [ ] procedure: -> obtentionProcedure
	- [x] culture_cell: e.g: "Vero E6 cells (CRL-1586)" (NULL or none if not culture)
	- [x] culture_passage_history e.g "Original (not passaged)" (NULL or none if not culture)
- [ ] info
	-> its out of info up there
	- [x] biosample_ref_material: e.g "BEI Resources catalog NR-52281 (lot 70033135)



— -HOST/- INDIVIDUAL  (ON THIS 1 ENCOUNTER )
- [ ] :::: individual_id: (external ref ) :::: 
- [ ] host_taxon_id: e.g "9606" (“Homo sapiens”) -> Organism taxonId
- [ ] host_age: e.g “21”  -> Variant in sample IndividualAgeAtCollection 
- [ ] host_sex:  “female”, “male” (sex in default schema) -> Individual sex
- [ ] geo_origin:  e.g "USA:WI:Madison” -> Individual geographicOrigin
- [ ] disease: (relevant virus-related diseases) e.g “COVID 19 pneumonia” -> Individual diseases diseaseId
- [ ] disease_stage: e.g “acute” -> Individual diseases disease stage
- [x] comorbidities: (underlying chronic diseases, format as individualDiseases from default schema): e.g ICD10 for “diabetes mellitus type II”  -> Individual diseasesDisease and diseasesStage where they are set stage: chronic NOT AVAILABLE AT THE MOMENT
- [x] disease_course: categorical “asymptomatic”, ”mild”, “severe”  
- [x] disease_outcome: e.g ”resolution/discharge” , “fatal” 
- [ ] info 
	- [ ] -> its out of info up there

	
REMOVED FROM HERE ON
-— VIRUS- _(Should there be one generic for organism/entity which sequence data belongs to to include their relevant phenotypic features - in the form of whatever phenotypic feature ontology for any species, or one specific to Genus , or being removed altogether?)_  
- [x] taxon_id:  e.g ”433733”  -> Variant Basic taxonId 
- [x] taxon_name: e.g “Severe acute respiratory syndrome coronavirus 2”
- [x] strain_id: 
- [x] strain_name: e.g "2019-nCoV/USA-WI1/2020" -> info?
- [x] annotation
	- [x] genomic annotation: file url
	- [x] functional annotation: file url


