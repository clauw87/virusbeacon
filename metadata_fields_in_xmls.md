# metadata_fields_in_xmls

## Metadata fields from NCBI xml files to feed virus beacon schema v1 table

*NOTE: some values need to be harmonized among datasets (see harmonization rules and mapping ontologies doc (harmonization_rules_mapping.md))


—VARIANT IN SAMPLE
- [ ] biosample_id:   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]
- [ ] variant_file_id:   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID[[1]]
- INFO
- [ ] study_info: 
	* [ ] study_id: (study accession): xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
	* [ ] study_title: xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
	* [ ] study_ref: (article PUMED ID or URL): ~~sample_attributes_values$link_addit_analys~~ STUDY/STUDY_ATTRIBUTES/STUDY_ATTRIBUTE/TAG&VALUE
- [ ] experiment_info
	- [ ] exp_id: xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID
	- [ ] exp_lib_strategy: xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY
	- [ ] exp_lib_source: xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE[[1]]
	- [ ] exp_lib_selection:  xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION[[1]]
	- [ ] exp_lib_layout: names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT)
	- [ ] exp_platform: xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]


—BIOSAMPLE 
- [ ] collection_date: attributes_values$collection_date 
- [ ] biosample_type: attributes_values$isolation_source, sample_attributes_values$tissue
- [ ] procedure
	- [ ] culture_cell: sample_attributes_values$`Laboratory Host`, attributes_values$passage_history > Map to CL ontology (NULL or none if not culture)
	- [ ] procedure:culture_passage_history: attributes_values$passage_history e.g "Original (not passaged)" (NULL or none if not culture)
- [ ] info
	- [ ] biosample_id:  xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]
	- [ ] biosample_alt_id:  xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID[[1]]
	- [ ] biosample_ref_material: sample_attribute_values$ref_biomaterial


—HOST/INDIVIDUAL 
- [ ] host_taxon_id: sample_attributes_values$host, sample_attributes_values$env_broad_scale, sample_attributes_values$host_description, sample_attributes_values$country
- [ ] host_age: sample_attributes_values$host_age, sample_attributes_values$age
- [ ] host_sex:  sample_attributes_values$host_sex, sample_attributes_values$sex
- [ ] geo_origin: sample_attributes_values$geo_loc_name 
- [ ] host_disease:  sample_attributes_values$host_disease
- [ ] host_disease_stage: sample_attributes_values $host_disease_stage (e.g “acute” harmonization needed) 
- [ ] host_comorbidities (diseases in default schema): not data at present 
- [ ] host_disease_outcome: sample_attributes_values$host_disease_outcome
- [ ] info 
	- [ ] study_ref (article PUMED ID or URL): sample_attributes_values$link_addit_analys



— VIRUS
- [ ] taxon_id:    xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]
- [ ] taxon_name:   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]
- [ ] strain_id: 
- [ ] strain_name: sample_attributes_values$strain

