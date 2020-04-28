# virus_beacon_schema_v1_to_generic
—VARIANT BASIC (basic beacon variant schema: from VCF)

- [ ]  ref_assembly_id -> Variant Basic refAssemblyId 
- [ ] start_nucleotide -> Variant Basic startPos 
- [ ] end_nucleotide -> Variant Basic  endPos
- [ ] ref_sequence  -> _Variant Basic ref
- [ ] alt_sequence -> _Variant Basic  alt
- [ ] variantType: SNV, indel, CNV, structural variant -> Variant Basic  variantType


—VARIANT  ANNOTATION   (highlighted are new fields that weren’t before :  Annotations to get from VCF Dietmar will send)
- [ ] variant_id: -> Variant Annotation variantId
- [ ] variant_effect: -> Variant Annotation molecularEffect
- [ ] variant_consequence: -> Variant Annotation molecularConsequence
- [ ] genomic_region: ->   Variant Annotation genomicRegion featureClass and Variant Annotation genomicRegion featureId
- [ ] ::annotation_tool_version::: ->   Variant Annotation annotationToolVersion
- [ ] ::molecular_effect::: ->   Variant Annotation molecularEffect
- [ ] ::aminoacid_change::: ->   Variant Annotation aminoacid change



— RUN (NOW TAKES FIELDS THAT WERE IN VARIANT IN SAMPLE, SEE VARIANT IN SAMPLE BELOW)
in generic beacon v2 schema, see arrows)
		- [ ] run_id:  (run accession)  ->  Run runId
		- [ ] exp_lib_source: ->  Run librarySource 
		- [ ] exp_lib_strategy: (“RNA-Seq”, “WGS”, “AMPLICON”, “Targeted-Capture”)  ->  Run libraryStrategy
		- [ ] exp_lib_selection: -> Run librarySelection
		- [ ] exp_lib_layout: -> Run libraryLayout
		- [ ] exp_platform: -> Run platform
		- [ ] exp_platform_model: -> Run platformModel
		- [ ] info 
			- [ ] experiment_info
				- [ ] exp_id (experiment accession):  ->  Run info experimentId
			- [ ] study_info
				- [ ] studyId ->  Run info studyId
				- [ ] study_title: e.g ”Total RNA sequencing of BALF (human reads removed)”  ->  Run info studyTitle
				- [ ] studyRef ->  Run info studyRef


—VARIANT IN SAMPLE 
- [ ]  :::: variant_id  :::: -> Variant in Sample variantId  
- [ ]  :::: biosample_id:  ::::  -> Variant in Sample biosampleId 
- [ ]  :::: run_id:  :::: -> Variant in Sample runId 
- [ ]  :::: variant_caller: ::::  ->  Variant in Sample variantCaller
- [ ]  :::: host_id:   :::: -> Variant in Sample individualId
- [ ] variant_frequency_dataset:  ->  Variant in Sample variantFrequency
- [x] info
 -> Some things from here and now out of info. Most that was here before now is on endpoint RUN 
	


—BIOSAMPLE  
- [ ] :::: biosample_id: :::: -> Biosample biosampleId 
- [ ] :::: individual_id :::: -> Biosample IndividualId 
- [ ] collection_date: -> Biosample collectionDate
- [ ] biosample_type: -> sampleOriginType and sampleOriginDetail
- [ ] procedure: -> obtentionProcedure
- [x] info
	-> I put all out of info fields up there (highlighted ones)




— -HOST/- INDIVIDUAL  -> INDIVIDUAL 
- [ ] :::: individual_id:  ::::  -> Individual individualId
- [ ] individual_taxon_id:  -> Organism taxonId
- [ ] age:  -> Biosample IndividualAgeAtCollection and  Individual diseases disease ageOfOnset
- [ ] sex:   -> Individual sex
- [ ] geo_origin:   -> Individual geographicOrigin
- [ ] disease: -> Individual diseases diseaseId
- [ ] disease_stage:  -> Individual diseases disease stage
- [ ] disease_outcome: -> Individual diseases disease outcome
- [ ] info 
	- [ ] -> I put all out of info fields up there (highlighted ones)

	

VIRUS -> ORGANISM
	- [ ] taxon_id (taxon of sequenced species) -> Organism taxonId
	- [ ] info
		- [ ] strain_name





