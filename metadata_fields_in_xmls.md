# metadata_fields_in_xmls

## Metadata fields from NCBI xml files to feed virus beacon schema v1 table

\*NOTE: some values need to be harmonized among datasets (see harmonization rules and mapping ontologies doc (harmonization_rules_mapping.md))



### RUN
* run_id:   `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID[[1]]`
* exp_lib_source: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE[[1]]`
* exp_lib_strategy: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY`
* exp_lib_selection:  `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION[[1]]`
* exp_lib_layout: `names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT)`
* exp_platform: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM`
* exp_platform_model: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]`
* INFO
	* experiment_info
		* exp_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID`
	* study_info: 
		* study_id: `(study accession): xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]`
		* study_title: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]`
		* study_ref: (article PUMED ID or URL): `STUDY/STUDY_LINKS/STUDY_LINK/XREF_LINK/ID&DB`

### VARIANT IN SAMPLE
* biosample_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]`

### BIOSAMPLE 
* biosample_id: 
	* `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]`
	* `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID[[1]]`
* collection_date: `attributes_values$collection_date`
* host_age \at \sampling: `sample_attributes_values$host_age, sample_attributes_values$age`
* biosample_type: 
	* `attributes_values$isolation_source`
	* `sample_attributes_values$tissue`
* procedure: 
	* `sample_attributes_values$Laboratory Host`
	* `attributes_values$passage_history` \> Map to CL ontology (NULL or none if not culture), 
	* `attributes_values$passage_history`
* biosample_description: 
	* `sample_attribute_values $isolate`
	* `sample_attribute_values$collected_by`
* info

### INDIVIDUAL 
* individual_taxon_id: `sample_attributes_values$host`
* sex:  
	* `sample_attributes_values$host_sex`
	* `sample_attributes_values$sex`
* age \of\onset: 
	* `sample_attributes_values$host_age`
	* `sample_attributes_values$age`
	(This is extracted to Biosample already)
* geo_origin: `sample_attributes_values$geo_loc_name`
* disease:  `sample_attributes_values$host_disease`
* disease_stage: `sample_attributes_values $host_disease_stage`
* host_disease_outcome:` sample_attributes_values$host_disease_outcome`
* info 

### ORGANISM
* taxon_id:    
* info
	* strain_name: `sample_attributes_values$strain`
