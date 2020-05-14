# harmonization_rules_mapping_2
Harmonize these values so they can be reached through queries and or filters

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


  Rest of values in the column change to NULL (leave empty)

NOTE: now there are samples that are mixes, e.g "oral swab; nasal swab; tracheal wash", we should label those as “mix” and also as their component sources (“oro-pharyngeal swab”/oropharynx, “naso-pharyngeal swab”/ nasopharynx, trachea) to be findable by filters but still present a warning that they are not purely from one source. That’s why they appear duplicted some times in more than one values to harmonize set.

* **Values to show** (these ones we’ll get after harmonization)
  * “bronchoalveolar lavage fluid" 
  * "oropharyngeal swab” 
  * “naso-pharyngeal swab”
  * “pharyngeal swab”
  * ”tracheal swab" 
  * “mix” 


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

    These values would be shown as “oro-pharyngeal swab” and mapped to:
    * id: UBERON:0001729
    * name: oropharynx

  * naso-pharyngeal swab values (pattern: (lowercase first) “nasopharynx”| “naso-pharynx”)
    * “nasopharynx" 
    * "respiratory nasopharyngeal sample"
    * "Nasopharyngeal/throat swab"
    * "Combined nasopharyngeal and oropharyngeal swab"
    * "oral swab; nasal swab; tracheal wash"  

    These values would be shown as “naso-pharyngeal swab” and mapped to:
    * id: UBERON:0001728
    * name: nasopharynx

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

  * swab only values (general, mixed or unespecified pharyngeal swab)
    * “swab”
    * "Diagnostic Swab" 
    * "Combined nasopharyngeal and oropharyngeal swab"

    These values would be shown as “pharyngeal swab” and mapped  to :
    * id: UBERON:0006562
    * name: pharynx

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
    * "02-Jan-2020"
    * "13-Jan-2020"
    * "27-Feb-2020"
    * "28-Feb-2020"
    * "29-Feb-2020"

   * "02-Apr-2020"
   * ”10-Mar-2020"
   * "18-Mar-2020"
   * "20-Mar-2020"
   * "22-Mar-2020" 
   * "14-Mar-2020" 
   * "05-Apr-2020"

    convert to “%Y-%m-%d” to get:
    * “2020-03-01”
    * “2020-01-02”
    * “2020-01-13”
    * "2020-02-27"
    * "2020-02-28"
    * "2020-02-29"

    * "2020-04-02"
    * "2020-03-10"
    * "2020-03-18"
    * "2020-03-20"
    * "2020-03-22"
    * "2020-03-14"
    * "2020-04-05"

* **Value to fix**
  “2019-01-19” ->  "2020-01-19" 



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


   Note: It would be nice to harmonize to format Country, State_Sub, City and then map to GAZ ontology (geographic origin). To this GAZ ontology (and abbreviations for USA states) would have to be used to detect country_city names in values. But since now ontology structure is not working (so, searching for USA won’t return Madison), we better use grouping by countries for now.

NOTE: Group by country: countries are in all at beginning, before “:”, so we can extract whatever is before “:” (“^(.*?):”) and use that country name to group. 

* **Values to show**  (these ones we’ll get after harmonization)
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

* **Values to harmonize** 
  * all values 

  So, do:
  * everything containing string in Values to Show: -> Value to Show


3. host_age

* **XML tags**
  * age
  * host_age

* **Values to fix** 
“missing” -> NULL
“4 years” -> 4 


4. host_disease

* **XML tags**
  * host_disease

* **Values to show**  (these ones we’ll get after harmonization)

  * "nCoV pneumonia"                                 
  * "COVID-19"                                                                                
  * "severe acute respiratory syndrome" (for graph abbreviate (SARS))             

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
