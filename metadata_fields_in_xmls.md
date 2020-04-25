# metadata\_fields\_in\_xmls

## Metadata fields from NCBI xml files to feed virus beacon schema v1 table

\*NOTE: some values need to be harmonized among datasets (see harmonization rules and mapping ontologies doc (harmonization\_rules\_mapping.md))



—RUN
- [ ] run\_id:   xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$RUN\_SET$RUN$IDENTIFIERS$PRIMARY\_ID[[1]]
-  [ ] exp\_lib\_source: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$DESIGN$LIBRARY\_DESCRIPTOR$LIBRARY\_SOURCE[[1]]
-  [ ] exp\_lib\_strategy: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$DESIGN$LIBRARY\_DESCRIPTOR$LIBRARY\_STRATEGY
-  [ ] exp\_lib\_selection:  xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$DESIGN$LIBRARY\_DESCRIPTOR$LIBRARY\_SELECTION[[1]]
- [ ] exp\_lib\_layout: names(xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$DESIGN$LIBRARY\_DESCRIPTOR$LIBRARY\_LAYOUT)
-  [ ] exp\_platform: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$PLATFORM
- [ ] exp\_platform\_model: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT\_MODEL[[1]]
- INFO
- [ ] experiment\_info
	- [ ] exp\_id: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY\_ID
- [ ] study\_info: 
	* [ ] study\_id: (study accession): xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$STUDY$IDENTIFIERS$PRIMARY\_ID[[1]]
	* [ ] study\_title: xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$STUDY$DESCRIPTOR$STUDY\_TITLE[[1]]
	* [ ] study\_ref: (article PUMED ID or URL): \~\~sample\_attributes\_values$link\_addit\_analys\~\~ \~\~STUDY/STUDY\_ATTRIBUTES/STUDY\_ATTRIBUTE/TAG&VALUE\~\~ STUDY/STUDY\_LINKS/STUDY\_LINK/XREF\_LINK/ID&DB




—VARIANT IN SAMPLE
- [ ] biosample\_id:   xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY\_ID[[1]]




—BIOSAMPLE 
- [ ] biosample\_id:  xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY\_ID[[1]], xml$EXPERIMENT\_PACKAGE\_SET$EXPERIMENT\_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL\_ID[[1]]
- [ ] collection\_date: attributes\_values$collection\_date 
-  [ ] host\_age \at \sampling: sample\_attributes\_values$host\_age, sample\_attributes\_values$age
- [ ] biosample\_type: attributes\_values$isolation\_source, sample\_attributes\_values$tissue
- [ ] procedure: sample\_attributes\_values$`Laboratory Host`, attributes\_values$passage\_history \> Map to CL ontology (NULL or none if not culture), attributes\_values$passage\_history 
-  [ ] biosample\_description: sample\_attribute\_values $isolate, sample\_attribute\_values$collected\_by
- [ ] info



—INDIVIDUAL 
- [ ] individual\_taxon\_id: sample\_attributes\_values$host, sample \_attributes\_values$env\_broad\_scale, sample\_attributes\_values$host\_description
- [ ] sex:  sample\_attributes\_values$host\_sex, sample\_attributes\_values$sex
-   [ ] age \of\onset: sample\_attributes\_values$host\_age, sample\_attributes\_values$age (This is extracted to Biosample already)
- [ ] geo\_origin: sample\_attributes\_values$geo\_loc\_name 
- [ ] disease:  sample\_attributes\_values$host\_disease
- [ ] disease\_stage: sample\_attributes\_values $host\_disease\_stage (e.g “acute” harmonization needed) 
- [ ] host\_disease\_outcome: sample\_attributes\_values$host\_disease\_outcome
- [ ] info 


— ORGANISM
- [ ] taxon\_id:    
- info
	- [ ] strain\_name: sample\_attributes\_values$strain
