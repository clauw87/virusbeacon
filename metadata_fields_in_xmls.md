# metadata_fields_in_xmls

## Metadata fields from all sources’s xml files to feed into viral beacon table



### RUN
* run_id:  `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID`
* exp_lib_source: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE[[1]]` 
* exp_lib_strategy: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY`
* exp_lib_selection:  `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION[[1]]`
* exp_lib_layout: `names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT)`
(NOTE: ignore for GISAID and Genbank until we fix xml of those which are showing PAIRED by default)
* exp_platform: `names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM)` (NOTE: ignore for GISAID and Genbank until we fix xml of those which are showing ILLUMINA by default)
* exp_platform_model:
	* `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]`
	* `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$NANOPORE$INSTRUMENT_MODEL[[1]]`
	* `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ION_TORRENT$INSTRUMENT_MODEL[[1]]`

* pipeline: (only for GISAID and Genbank datasets, show in UI as column ASSEMBLY METHOD): `sample_attributes_values$assembly_method`

* INFO
	* experiment_info
		* exp_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID`








### VARIANT IN SAMPLE
* INFO
	* study_info: 
		* study_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]`
		* study_title: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]`
		* study_ref: 
			* `STUDY/STUDY_LINKS/STUDY_LINK/XREF_LINK/ID`
			* `STUDY/STUDY_LINKS/STUDY_LINK/XREF_LINK/DB`



### BIOSAMPLE 
* biosample_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]`
* biosample_model:
	* `sample_attributes_values$source_uvig`
	* `sample_attributes_values$BioSampleModel`
* collection_date: `sample_attributes_values$collection_date`
* biosample_type: 
	* `sample_attributes_values$isolation_source`
	* `sample_attributes_values$env_medium`
	* `sample_attributes_values$$host_tissue_sampled`
	* `sample_attributes_values$tissue`
	* `sample_attributes_values$isolation source host-associated`
	
*[x] procedure
	* culture_cell: 
		* `sample_attributes_values$Laboratory Host`
		* `sample_attributes_values$passage_history`
		* `sample_attributes_values$propagation`
		
	* procedure:
		* culture_passage_history: `sample_attributes_values$passage_history`
	

### HOST/INDIVIDUAL 
* host_taxon_id: 
	* `sample_attributes_values$host`
* host_age: 
	* `sample_attributes_values$host_age`
	* `sample_attributes_values$age`
	* `sample_attributes_values$host age`
	
* host_sex: 
	* `sample_attributes_values$host_sex`
	* `sample_attributes_values$sex`
	* `sample_attributes_values$host sex`
* geo_origin: `sample_attributes_values$geo_loc_name`
	* `sample_attributes_values$geo_loc_name`
	* `sample_attributes_values$Country`
	* `geographic location (country and/or sea)`:`geographic location (region and locality)`
* host_disease: `sample_attributes_values$host_disease`
* host_disease_stage: `sample_attributes_values $host_disease_stage` 
* host_disease_outcome: `sample_attributes_values$host_disease_outcome`
* info 
	* study_ref (article PUMED ID or URL): `sample_attributes_values$link_addit_analys`



### VIRUS
* taxon_id: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]`
* taxon_name: `xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]`
* strain: `sample_attributes_values$strain`,  
* isolate: `sample_attributes$isolate`  
