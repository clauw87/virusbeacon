# harmonization_rules_mapping
Harmonize these values so they can be reached through queries and or filters

— BIOSAMPLE - most urgent one now, as it is showing unrelated stuff 
- [ ] biosmple_type: 
biosample_type tags  > tissue, env_medium, isolation_source
biosample_type values  > sample_attributes_values$tissue, sample_attributes_values$env_medium, sample_attributes_values

**Only good values**
"Human BALF sample", "Bronchoalveolar lavage fluid", "bronchoalveolar lavage fluid(BALF)",  "Oro-pharyngeal swab", "oropharyngeal swab",  "nasopharynx",  
"swab"
* rest of values in the column change to NULL,  or “unknown”

Groups to harmonize
- [ ] BALF values
"Bronchoalveolar lavage fluid", "bronchoalveolar lavage fluid(BALF)”, "Human BALF sample"
pattern: “broncho”| “alveolar”|“balf”
-> These values would be shown as “bronchoalveolar lavage fluid"  and be mapped to:
id: UBERON:0006524
name: alveolar system


- [ ] Oro-pharyngeal swab values
“Oro-pharyngeal swab", "oropharyngeal swab” 
pattern: (lowercase first) “oropharyn”| “oro-pharyn”
-> These values would be shown as “oro-pharyngeal swab” and mapped to:
id: UBERON:0001729
name: oropharynx


- [ ] naso-pharyngeal swab values
“nasopharynx" 
pattern: (lowercase first) “nasopharyn”| “naso-pharyn”
-> These values would be shown as “naso-pharyngeal swab” and mapped to:
id: UBERON:0001728
name: nasopharynx

- [ ] swab only values
“swab”  
-> These values would be shown as “pharyngeal swab” and mapped  to :
id: UBERON:0006562
name: pharynx


- [ ] collection_date: 
collection_date tags> attributes_values$collection_date 

collection_date values come in different formats e.g "02-Jan-2020”, "2020-02-14" , "2020”, "2020-03" -> homogenize to ISO 8601 international standard, i.e “%Y-%m-%d” 
formats currency present :
"%d-%B-%Y", 
“%Y-%m-%d”, 
“%Y-%m”,  “
%Y”
but only problematic one is :
"%d-%B-%Y"
-> check dates that are formatted "%d-%B-%Y" to “%Y-%m-%d” 
namely, values to change will be: 
"01-Mar-2020"
"02-Jan-2020"
"13-Jan-2020"
"27-Feb-2020"
"28-Feb-2020"
"29-Feb-2020"
t[which(t=="01-Mar-2020")] <- as.character(as.Date.character("01-Mar-2020", format=”%d-%B-%Y”)) 
to get:
“2020-03-01”
“2020-01-02”
“2020-01-13”
"2020-02-27"
"2020-02-28"
"2020-02-29"

- [ ] also, there is an error, date “2019-01-19” is not OK # 19 samples have it> it should be "2020-01-19" 
unique(t[which(str_detect(t, "2019"))])
t[which(t=="2019-01-19")] <- "2020-01-19" 


—HOST/INDIVIDUAL
- [ ] host_taxon_id tags> sample_attributes_values$host, sample_attributes_values$env_broad_scale, sample_attributes_values$host_description
host_species_name values> 

**Only good values**
”Homo sapiens”, 
”Human”, 
"Isolated from clinical sample from the first case of nCov in US from Washington State”)   -> convert to ”Homo sapiens”
* rest of values in the column change to NULL,  or “unknown”

- [ ] geo_origin:  
geo_origin tags > 
geo_loc_name,  country, at_lon 
geo_origin_values > sample_attributes_values$geo_loc_name, sample_attributes_values$country, sample_attributes_values$at_lon
(different formats) e.g "USA:WI:Madison”/ "USA: CA, San Diego County”/ “30.52 N 114.31 E"  ->  harmonise to format Country, State/Sub, City and map to GAZ ontology (geographic origin in default schema)
Group by countries by patterns, for filters only:
For now, country patterns would be:
"Australia|USA|China|Nepal"
But later on, GAZ geo loc ontology will have to be used to detect country/city names in values.


- [x] disease_stage: sample_attributes_values$host_disease_stage for now only “Acute” 

- [x] disease_outcome host_disease_outcome_values> e.g “Survived”

- [x] disease: sample_attributes_values$host_disease  
