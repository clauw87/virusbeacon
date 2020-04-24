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
- [ ] variant effect: -> Variant Annotation molecularConsequence
- [ ] genomic_region: ->   Variant Annotation genomicRegionClass and featureId



—VARIANT IN SAMPLE 
- [ ]  :::: variant_id  -> Variant in Sample variantId  :::: 
- [ ]  :::: biosample_id: (external ref ) e.g “SRS6007144” :::: -> Variant in Sample biosampleId 
- [ ]  :::: run_id:  :::: -> Variant in Sample runId 
- [ ]  :::: variant caller: ::::  ->  Variant in Sample variantCaller
- [ ]  :::: host_id: (external ref if it exists)  :::: -> Variant in Sample individualId
- [ ] variant_frequency_dataset:  ->  Variant in Sample variantFrequency
- [ ] info
	- [ ] experiment_info (ALL THE FOLLOWING  GOES TO ENDPOINT “RUN” in generic beacon v2 schema, see arrows)
		- [ ] run_id:  (run accession) e.g "SRR10903401"  ->  Run runId
		- [ ] exp_id (experiment accession): e.g  "SRX7571571"  ->  Run info experimentId
		- [ ] exp_title: e.g ”Total RNA sequencing of BALF (human reads removed)”  ->  Run info experimentTitle
		- [ ] exp_lib_strategy: (“RNA-Seq”, “WGS”, “AMPLICON”, “Targeted-Capture”)  ->  Run libraryStrategy
		- [ ] exp_lib_source: ->  Run librarySource 
		- [ ] exp_lib_selection: -> Run librarySelection
		- [ ] exp_lib_layout: -> Run libraryLayout
		- [ ] exp_platform: -> Run platform
		- [ ] exp_platform_model: -> Run platformModel
		 -> I put some fields out of info up there (highlighted ones)
	


—BIOSAMPLE  
- [ ] :::: biosample_id: :::: -> Biosample biosampleId 
- [ ] :::: individual_id :::: -> Biosample IndividualId 
- [ ] collection_date: -> Biosample collectionDate
- [ ] biosample_type: -> sampleOriginType and sampleOriginDetail
- [ ] procedure: -> obtentionProcedure
- [ ] info
	-> I put all out of info fields up there (highlighted ones)




— -HOST/- INDIVIDUAL  (ON THIS 1 ENCOUNTER )
- [ ] :::: individual_id:  ::::  -> Individual individualId
- [ ] host_taxon_id:  -> Organism taxonId
- [ ] host_age: e.g “21”  -> Variant in sample IndividualAgeAtCollection 
- [ ] host_sex:   -> Individual sex
- [ ] geo_origin:   -> Individual geographicOrigin
- [ ] disease: -> Individual diseases diseaseId
- [ ] disease_stage:  -> Individual diseases disease stage
- [ ] comorbidities:  -> Individual diseases diseaseId and disease stage (THESE TYPE DATA NOT AVAILABLE AT THE MOMENT SO YOU CAN FORGET THIS FOR NOW)
- [ ] disease_course: categorical “asymptomatic”, ”mild”, “severe”   -> Individual diseases disease level/severity 
- [ ] disease_outcome: -> Individual diseases disease outcome
- [ ] info 
	- [ ] -> I put all out of info fields up there (highlighted ones)

	
REMOVED FROM HERE ON


