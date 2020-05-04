# harmonization_rules_mapping_2
Harmonize these values so they can be reached through queries and or filters

— BIOSAMPLE 

1. biosample_type

**XML tags**
- [ ] tissue
- [ ] env_medium
- [ ] isolation

**Only good values**
"Human BALF sample", 
"Bronchoalveolar lavage fluid", 
"bronchoalveolar lavage fluid(BALF)", 
 "Oro-pharyngeal swab", 
"oropharyngeal swab",  
"nasopharynx",  
"swab"

* rest of values in the column change to NULL (leave empty)

**Groups to harmonize**

- [ ] BALF values 
(pattern: “broncho”| “alveolar”|“balf”):
"Bronchoalveolar lavage fluid", 
"bronchoalveolar lavage fluid(BALF)”, 
"Human BALF sample"

-> These values would be shown as “bronchoalveolar lavage fluid"  and be mapped to:
id: UBERON:0006524
name: alveolar system

- [ ] Oro-pharyngeal swab values 
(pattern: (lowercase first) “oropharyn”| “oro-pharyn”)
“Oro-pharyngeal swab", 
"oropharyngeal swab” 

-> These values would be shown as “oro-pharyngeal swab” and mapped to:
id: UBERON:0001729
name: oropharynx


- [ ] naso-pharyngeal swab values 
(pattern: (lowercase first) “nasopharyn”| “naso-pharyn”)
“nasopharynx" 

-> These values would be shown as “naso-pharyngeal swab” and mapped to:
id: UBERON:0001728
name: nasopharynx


- [ ] swab only values
“swab”  
-> These values would be shown as “pharyngeal swab” and mapped  to :
id: UBERON:0006562
name: pharynx


2. collection_date

**XML tags**
collection_date 

**Good values**
Note: All values are dates, but come in different formats e.g "02-Jan-2020”, "2020-02-14" , "2020”, "2020-03”. We want to homogenize to ISO 8601 international standard, i.e “%Y-%m-%d” 

**Groups of formats to harmonize**
Note:
”%d-%B-%Y” 
“%Y-%m” 
%Y”
-> harmonize all to “%Y-%m-%d”
(for “%Y-%m”, %Y”, not really only if there is a standard way of replacing missing month and day- I think usually they do replacement of missing month/date by “01”)

- [ ] “%d-%B-%Y” values 
"01-Mar-2020"
"02-Jan-2020"
"13-Jan-2020"
"27-Feb-2020"
"28-Feb-2020"
"29-Feb-2020"
-> convert to “%Y-%m-%d” to get:
“2020-03-01”
“2020-01-02”
“2020-01-13”
"2020-02-27"
"2020-02-28"
"2020-02-29"

**Value to fix**
“2019-01-19” ->  "2020-01-19" 


—HOST/INDIVIDUAL

1. host_taxon_id 
 
**XML tags**
- [ ] host
- [ ] env_broad_scale
- [ ] host_description

**Only good values**
”Homo sapiens”, 
”Human”, 
"Isolated from clinical sample from the first case of nCov in US from Washington State”)  
 -> convert all these to ”Homo sapiens” an map to 9606

* rest of values in the column change to NULL (leave empty)


1. geo_origin
**XML tags**
- [ ] geo_loc_name

**Only good values**
All are geo doc values, but different formats, e.g "USA:WI:Madison”, "USA: CA, San Diego County”
These are all values:
"China: Wuhan"                  
"China:Wuhan"   
"China: Hubei"                  
"USA:WI:Madison"                 
"Nepal: Kathmandu"              
"USA: Seattle, WA"              
"USA: WA"                       
"USA:WA"                         
"USA"                           
"Australia: Melbourne, Victoria"         
"USA: Washington"                
"Australia: Victoria"           
"Australia: Northern Territory" 

Group by countries by patterns, for filters only:

**Groups of formats to harmonize**
Note: It would be nice to harmonize to format Country, State_Sub, City and then map to GAZ ontology (geographic origin). To this GAZ ontology (and abbreviations for USA states) would have to be used to detect country_city names in values. But since now ontology structure is not working (so, searching for USA won’t return Madison), we better use grouping by countries for now.

- [ ] "Australia|USA|China|Nepal"
So, do:
everything containing string “Australia” ->  “Australia”
everything containing string “USA” ->  “United States of America”
everything containing string “China” ->  “China”
everything containing string “Nepal” ->  “Nepal”


