# harmonization_rules_mapping_2
Harmonize these values so they can be reached through queries and or filters

## RUN

1. run_platform

* **XML tags**
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM)

* **Valid values**
“ILLUMINA”
"OXFORD_NANOPORE"


2. run_platform_model 

* **XML tags**
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]

* **Valid values**
"Illumina MiSeq"        
"Illumina HiSeq 2500"  
"NextSeq 500"           
"NextSeq 550"          
"Illumina iSeq 100"     
"Illumina MiniSeq"     
"Illumina NovaSeq 6000"


* ** Values to show ** 
"MiSeq"        
"HiSeq 2500"  
"NextSeq 500"           
"NextSeq 550"          
“iSeq 100"     
"MiniSeq"     
"NovaSeq 6000"


* **Values to fix**
(in general, remove the string Illumina an the space afterwards from all values)
"Illumina MiSeq"  ->  "MiSeq"  
"Illumina HiSeq 2500"  ->  "HiSeq 2500"  
"Illumina iSeq 100"     ->  "iSeq 100"  
"Illumina MiniSeq"   ->     "MiniSeq"  
"Illumina NovaSeq 6000" -> "NovaSeq 6000"


## BIOSAMPLE 

1. biosample_type
* **XML tags**
  * tissue 
  * env_medium
  * isolation_source
  * host_tissue_sampled

* **Valid values**
  * "Human BALF sample", 
  * "Bronchoalveolar lavage fluid", 
  * "bronchoalveolar lavage fluid(BALF)", 
  * "Oro-pharyngeal swab", 
  * "oropharyngeal swab",  
  * "nasopharynx",  
  * "swab" 
  * "respiratory nasopharyngeal sample" 
  * "Nasopharyngeal/throat swab"
  * "Combined nasopharyngeal and oropharyngeal swab" 
  * "oral swab; nasal swab; tracheal wash" 
  * "Diagnostic Swab" 
  * "tracheal swab" 
  * "bronchoalveolar lavage"
  * "sputum"
  * "oralpharyngeal" 
  * "nasal swab"  
  * "oropharynx"
  * "Stool"


  Rest of values in the column change to NULL (leave empty)

NOTE: now there are samples that are mixes, e.g "oral swab; nasal swab; tracheal wash", we should label those as “mix” and also as their component sources (“oro-pharyngeal swab”/oropharynx, “naso-pharyngeal swab”/ nasopharynx, trachea) to be findable by filters but still present a warning that they are not purely from one source. That’s why they appear duplicted some times in more than one values to harmonize set.

* **Values to show** (these ones we’ll get after harmonization)
  * “bronchoalveolar lavage fluid" 
  * "oropharyngeal swab” 
  * “naso-pharyngeal swab”
  * “pharyngeal swab”
  * ”tracheal swab" 
  * ”sputum”
  * ”feces”
  * ~~“mix”~~ "oropharingeal swab; nasopharyngeal swab; traqueal whash" 

* **Values to harmonize**
  * BALF values (pattern: “broncho”| “alveolar”|“balf”):
    * "Bronchoalveolar lavage fluid", 
    * "bronchoalveolar lavage fluid(BALF)”, 
    * "Human BALF sample"
    * "bronchoalveolar lavage"

    These values would be shown as “bronchoalveolar lavage fluid"  and be mapped to:
    * id: UBERON:0006524
    * name: alveolar system

  * Oro-pharyngeal swab values (pattern: (lowercase first) “oropharyn”| “oro-pharyn”)
    * “Oro-pharyngeal swab", 
    * "oropharyngeal swab” 
    * "Combined nasopharyngeal and oropharyngeal swab"
    * "oral swab; nasal swab; tracheal wash" 
    * "oralpharyngeal"
    * "oropharynx"

    These values would be shown as “oro-pharyngeal swab” and mapped to:
    * id: UBERON:0001729
    * name: oropharynx

  * naso-pharyngeal swab values (pattern: (lowercase first) “nasopharynx”| “naso-pharynx”)
    * “nasopharynx" 
    * "respiratory nasopharyngeal sample"
    * "Nasopharyngeal/throat swab"
    * "Combined nasopharyngeal and oropharyngeal swab"
    * "oral swab; nasal swab; tracheal wash"  
    * "nasal swab"  

    These values would be shown as “naso-pharyngeal swab” and mapped to:
    * id: UBERON:0001728
    * name: nasopharynx


  * swab only values (general, mixed or unspecified pharyngeal swab)
    * “swab”
    * "Diagnostic Swab" 
    * "Combined nasopharyngeal and oropharyngeal swab"



  * tracheal swab values 
    * ”tracheal swab" 


    These values would be shown as “tracheal swab” and mapped to:
    * id: UBERON:0003126
    * name: trachea


  * tracheal wash values 
    * "oral swab; nasal swab; tracheal wash" 

    These values would be shown as “tracheal wash” and mapped to:
    * id: UBERON:0003126
    * name: trachea


    These values would be shown as “pharyngeal swab” and mapped  to :
    * id: UBERON:0006562
    * name: pharynx

  * sputum values 
    * “sputum”   

    These values would be shown as “sputum” and mapped to:
    * id: UBERON:0007311
    * name: sputum


  * feces values 
    * “Stool”   
    These values would be shown as “feces” and mapped to:
    * id: UBERON:0001988
    * name: feces

  * mix 
    * "oral swab; nasal swab; tracheal wash"   



2. collection_date

* **XML tags**
  * collection_date 

* **Valid values**
  All values are dates, but come in different formats e.g "02-Jan-2020”, "2020-02-14" , "2020”, "2020-03”. 

* **Values to show** (these ones we’ll get after harmonization)
  We want to homogenize to ISO 8601 international standard, i.e “%Y-%m-%d” 

* **Values to harmonize**
  * ”%d-%B-%Y” 
  * “%Y-%m” 
  * %Y”

  Harmonize all to “%Y-%m-%d”

  * “%Y-%m”, %Y”, not really necessary, only harmonize if there is a standard way of replacing missing month and day- I think usually they do replacement of missing month/date by “01”

  * “%d-%B-%Y” values 
    * "01-Mar-2020"
    * "19-MAR-2020"

    convert to “%Y-%m-%d” to get:
    * “2020-03-01”
    * "2020-03-19"

* **Value to fix**
  “2019-01-19” ->  "2020-01-19" 
  "missing"  -> NULL
  "not applicable"  -> NULL




## HOST/INDIVIDUAL

1. host_taxon_id 
 
* **XML tags**
  * host
  * env_broad_scale
  * host_description
  * host_taxid

* **Valid values**
  * ”Homo sapiens”, 
  * ”Human”, 
  * "Isolated from clinical sample from the first case of nCov in US from Washington State”)  
  * "9606"

  Convert all these to ”Homo sapiens” an map to “9606”.
  Rest of values in the column change to NULL (leave empty).

2. geo_origin

* **XML tags**
  * geo_loc_name
  * country
 (combined) * `geographic location (country and/or sea)`
 (combined) * `geographic location (region and locality)`  

(The last two are pairs country-region shown in different tags, so we could extract them in just one value as `geographic location (country and/or sea)`: `geographic location (region and locality)` to match the others i.e “United Kingdom: Scotland”)

* **Valid values**
   All are geo doc values, but different formats, e.g "USA:WI:Madison”, "USA: CA, San Diego County”.

   These are all values:
     * "China: Wuhan"                  
     * "China:Wuhan"   
     * "China: Hubei"                  
     * "USA:WI:Madison"                 
     * "Nepal: Kathmandu"              
     * "USA: Seattle, WA"              
     * "USA: WA"                       
     * "USA:WA"  
     * "USA: Washington"                       
     * "USA"  
     * "USA: CA, San Diego County"  
     * "USA:Virginia"
     * "USA: New York"          
     * "Australia: Melbourne, Victoria"         
     * "Australia: Victoria"           
     * "Australia: Northern Territory" 
     * "Peru: Lima" 
     * "Cambodia:Sihanoukville" 
     * "Malaysia: Kuala Lumpur"
     * "Malaysia"
     * "South Africa: KwaZulu-Natal"
     * "India: Rajkot"
     * "Israel"
     * "Germany: Dusseldorf"
     * "Germany: Heinsberg"
(after extracting “combined” tags ones):
     * “United Kingdom: England”
     * “United Kingdom: Scotland”

from ONT xmls:

     * "USA:WI,Madison" 
     * "China: Wuhan"   
     * "USA:Wisconsin"  
     * "USA: Wisconsin" 
     * "USA: Racine, Wisconsin"          
     * "USA: Milwaukee, Wisconsin"      
     * "USA: Port Washi, Wisconsin"      
     * "USA: Thiensvill, Wisconsin"     
     * "USA: Grafton, Wisconsin"         
     * "USA: Mequon, Wisconsin"         
     * "USA: Whitefish, Wisconsin"       
     * "USA: Pewaukee, Wisconsin"       
     * "USA: Cudahy, Wisconsin"          
     * "USA: Glendale, Wisconsin"       
     * "USA: Greenfield, Wisconsin"      
     * "USA: South Milwaukee, Wisconsin"
     * "USA: Elm Grove, Wisconsin"       
     * "USA: Franklin, Wisconsin"       
     * "USA: Brookfield, Wisconsin"      
     * "USA: Campbellsp, Wisconsin"     
     * "USA: Green Field, Wisconsin"     
     * "USA: Oak Creek, Wisconsin"      
     * "USA: Jackson, Wisconsin"         
     * "USA: Wauwatosa, Wisconsin"      
     * "USA: New Berlin, Wisconsin"      
     * "USA: California"                
     * "USA: Iowa"                       
     * "USA: Massachusetts"             
     * "USA: Virginia"                   
     * "USA: South Carolina"            
     * "USA: New Jersey"                 
     * "USA: New York"                  
     * "USA: Washington"



* **Values to fix**

"China:Wuhan" ->  "China: Hubei: Wuhan”
"China: Wuhan" ->  "China: Hubei: Wuhan”
"USA:WA" -> "USA: Washington" 
"USA: WA"  -> "USA: Washington" 
"USA:WI:Madison" -> "USA: Wisconsin: Madison"
"USA: Seattle, WA"  -> "USA: Washington: Seattle“ 
"USA: CA, San Diego County"  -> "USA: California: San Diego County”
"Australia: Melbourne, Victoria" -> "Australia: Victoria: Melbourne” 
"Cambodia:Sihanoukville" -> "Cambodia: Sihanoukville" 

from ont xmls:
"USA:WI,Madison" -> "USA: Wisconsin: Madison” 
"USA:Wisconsin" -> "USA: Wisconsin” 
"USA: Racine, Wisconsin" -> "USA: Wisconsin: Racine”
"USA: Milwaukee, Wisconsin"    -> "USA: Wisconsin: Milwaukee”   
"USA: Port Washi, Wisconsin"    -> "USA: Wisconsin: Racine”  
"USA: Thiensvill, Wisconsin"    -> "USA: Wisconsin: Port Washi” 
"USA: Grafton, Wisconsin"    -> "USA: Wisconsin: Grafton”     
"USA: Mequon, Wisconsin"      -> "USA: Wisconsin: Mequon”   
"USA: Whitefish, Wisconsin"    -> "USA: Wisconsin: Whitefish”   
"USA: Pewaukee, Wisconsin"     -> "USA: Wisconsin: Pewaukee”  
"USA: Cudahy, Wisconsin"      -> "USA: Wisconsin: Cudahy”    
"USA: Glendale, Wisconsin"       -> "USA: Wisconsin: Glendale”
"USA: Greenfield, Wisconsin"     -> "USA: Wisconsin: Greenfield” 
"USA: South Milwaukee, Wisconsin"  -> "USA: Wisconsin: South Milwaukee”
"USA: Elm Grove, Wisconsin"     -> "USA: Wisconsin: Elm Grove”  
"USA: Franklin, Wisconsin"       -> "USA: Wisconsin: Franklin”
"USA: Brookfield, Wisconsin"      -> "USA: Wisconsin: Brookfield”
"USA: Campbellsp, Wisconsin"     -> "USA: Wisconsin: Campbellsp”
"USA: Green Field, Wisconsin"     -> "USA: Wisconsin: Green Field”
"USA: Oak Creek, Wisconsin"      -> "USA: Wisconsin: Oak Creek”
"USA: Jackson, Wisconsin"         -> "USA: Wisconsin: Jackson”
"USA: Wauwatosa, Wisconsin"      -> "USA: Wisconsin: Wauwatosa”
"USA: New Berlin, Wisconsin"  -> "USA: Wisconsin: New Berlin” 
"USA: Wauwatosa, Wisconsin"  -> "USA: Wisconsin: Wauwatosa” 


* **Values to show**  (these ones we’ll get after harmonization )
  * "China: Hubei: Wuhan”
  * "USA: Washington" 
  * "USA: Wisconsin: Madison"
  * "Nepal: Kathmandu"  
  * "USA: Washington: Seattle“
  * "USA: Washington" 
  * "USA"
  * "USA: California: San Diego County”
  * "USA:Virginia"
  * "USA: New York" 
  * "Australia: Victoria" 
  * "Australia: Victoria: Melbourne” 
  * "Australia: Northern Territory" 
  * "Peru: Lima" 
  * "Cambodia: Sihanoukville" 
  * "Malaysia: Kuala Lumpur"
 "Malaysia"
  * "South Africa: KwaZulu-Natal"
  * "India: Rajkot"
  * "Israel"
  * "Germany: Dusseldorf"
  * "Germany: Heinsberg"


   Note: It would be nice to harmonize to format Country, State_Sub, City and then map to GAZ ontology (geographic origin). To this GAZ ontology (and abbreviations for USA states) would have to be used to detect country_city names in values. But since now ontology structure is not working (so, searching for USA won’t return Madison), we better use grouping by countries for now.

NOTE: Group by country: countries are in all at beginning, before “:”, so we can extract whatever is before “:” (“^(.*?):”) and use that country name to group. 

* **Values to show as groups** (for filters and statistics)
  * “Australia”
  * “USA”
  * “China”
  * “Nepal”
  * "Peru"
  * "Cambodia"
  * "Malaysia"
  * "South Africa"
  * "India"
  * "Germany"
  * "Israel" 
  * "United Kingdom"
  * "Egypt"



** Map values to show to Ontology
  * "China: Hubei: Wuhan” 
  * "USA: Washington" 
  * "USA: Wisconsin: Madison"
  * "Nepal: Kathmandu"  
  * "USA: Washington: Seattle“
  * "USA: Washington" 
  * "USA"
  * "USA: California: San Diego County”
  * "USA:Virginia"
  * "USA: New York" 
  * "Australia: Victoria" 
  * "Australia: Victoria: Melbourne” 
  * "Australia: Northern Territory" 
  * "Peru: Lima" 
  * "Cambodia: Sihanoukville" 
  * "Malaysia: Kuala Lumpur"
 "Malaysia"
  * "South Africa: KwaZulu-Natal"
  * "India: Rajkot"
  * "Israel"
  * "Germany: Dusseldorf"
  * "Germany: Heinsberg"




3. host_age

* **XML tags**
  * age
  * host_age

* **Values to fix** 
“missing” -> NULL
“4 years” -> 4 
“35 y” -> 35
“5 y” -> 5

4. host_disease

* **XML tags**
  * host_disease

* **Values to show**  (these ones we’ll get after harmonization)

  * "nCoV pneumonia"                                 
  * "COVID-19"                                                                                
  * "severe acute respiratory syndrome" (for graph abbreviate (SARS))  
  * "respiratory infection"           

  Rest of values in the column change to NULL (leave empty)

4. host_sex

* **XML tags**
  * host_sex

* **Values to show**

  * “female”                                 
  * “male” 

* **Values to harmonize/fix**
Female -> “female” 
Male -> “male” 

 Rest of values in the column change to NULL (leave empty)

## SPECIES (VIRUS)

1. taxon_id

* **XML tags**
  * EXPERIMENT_PACKAGE_SET> EXPERIMENT_PACKAGE> SAMPLE> SAMPLE_NAME> TAXON_ID[[1]]
    
* **Valid values**
  All values are taxon ids, only 3 need harmonization

* **Values to show** (these ones we’ll get after harmonization)
  
  “2697049” (“Severe acute respiratory syndrome coronavirus 2”) 436/439 xml have it  is what should appear here for all samples.
  
  NOTE: In title and abstract they all described that they attempted sequencing SARS-CoV2, so, despite the taxon they applied they only used novel reads (not mapped to human, so we could convert all values to “2697049” for the sake of taxon\_id as filtering)

* **Values to harmonize/fix**
  * "9606"  (Homo sapiens) - 2 xml have it
  * "433733” ("human lung metagenome" ) - 1 xml have it

  Change all to “2697049”
