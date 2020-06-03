library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#folder <-  "~/repolab/work/virusbeacon/6_05_2020_release"

#folder <- "~/repolab/work/virusbeacon/nanopore_ones_xml"
#ont_folder <- "~/repolab/work/virusbeacon/21_05_2020_ont_metadata"
ont_folder <- "~/repolab/work/virusbeacon/24_05_ONT_metadata"



length(list.files(ont_folder)) # 2118 ..3960 .. 3959 bc of empty one
length(unique(list.files(ont_folder))) # 2118 .. 3960 3959 bc of empty one


all_ont_files <- paste(ont_folder, list.files(ont_folder), sep = "/")

ont_xml_list <-  lapply(all_ont_files, function(f){
        xml2::as_list(xml2::read_xml(f)) 
})

length(ont_xml_list)  #    2118 ..3960
length(unique(ont_xml_list)) #  2118 ..3960


length(unique(ont_xml_list)) # 2112 # 6 xmls are repeated (identical to other) in folder (it's bc they yielded errors!)
sum(duplicated(ont_xml_list)) # 6 xmls are repeated in folder
duplicated <- all_ont_files[which(duplicated(ont_xml_list))]
duplicated  


ont_xml <- ont_xml_list[[1]]


# EXPERIMENT 

# EXPERIMENT > IDENTIFIERS > PRIMARY_ID
exp_primary_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID[[1]]
})
#)
)
length(unique(exp_primary_id)) # 3959


# EXPERIMENT > DESIGN > LIBRARY_DESCRIPTORS > LIBRARY NAME 
exp_library_name <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_NAME
})
#)
)
length(exp_library_name) # 1608
length(unique(exp_library_name)) #1351

# EXPERIMENT > DESIGN > LIBRARY STRATEGY
exp_library_strategy <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY

                })
        #)
)

# (“RNA-Seq”, "WGS”, "AMPLICON”, "Targeted-Capture”)
length(unique(exp_library_strategy )) # 2
unique(exp_library_strategy) #  "WGA"      "AMPLICON" "WGS"      "RNA-Seq"   


# EXPERIMENT > DESIGN > LIBRARY SOURCE
exp_library_source <- 
        unlist(
                #unique(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
})
#)
)
length(unique(exp_library_source )) # 3
unique(exp_library_source ) 
# "GENOMIC"            "VIRAL RNA"         
# "METAGENOMIC"        "SYNTHETIC"         
# "OTHER"              "METATRANSCRIPTOMIC"

# EXPERIMENT > DESIGN > LIBRARY SELECTION
exp_library_selection <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
})
#)
)
length(unique(exp_library_selection )) # 3
unique(exp_library_selection )
#"RT-PCR"     "PCR"        "RANDOM"     "RANDOM PCR"
# "Oligo-dT"   "cDNA"    
        

# EXPERIMENT > DESIGN > LIBRARY LAYOUT
exp_library_layout <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
#)
) # "PAIRED" "SINGLE"
length(unique(exp_library_layout)) #2
unique(exp_library_layout)
#  "SINGLE"
 
# EXPERIMENT PLATFORM or TECHNOLOGY
exp_platform <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM)
        })
        #)
) 
length(unique(exp_platform)) 
unique(exp_platform)
# "OXFORD_NANOPORE" "ION_TORRENT"  


# EXPERIMENT PLATFORM > NANOPORE MODEL
exp_platform_model <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$OXFORD_NANOPORE$INSTRUMENT_MODEL[[1]]
        })
#)
) 

length(unique(exp_platform_model)) # 1
unique(exp_platform_model)
# "MinION"  "GridION"


exp_platform_model2 <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ION_TORRENT$INSTRUMENT_MODEL[[1]]
        })
        #)
) 

length(unique(exp_platform_model2)) # 1
unique(exp_platform_model2)
#  "Ion Torrent S5"

##################    "SAMPLE"    ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)
# names , alias, accession
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)
# "IDENTIFIERS"       "SAMPLE_NAME"      
# "SAMPLE_LINKS"      "SAMPLE_ATTRIBUTES"

xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]

# "IDENTIFIERS" 
sample_primary_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]
        })
#)
) 
head(sample_primary_id) #  "ERS4399630"

sample_external_id <- #unlist(unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 

head(sample_external_id) # "SAMEA6638373"

# alias
sample_alias <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$alias
        })
#)
) 
head(sample_alias) # "SARS-CoV-2/Valencia003/human/2020/ES"

# accesion
sample_accession <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$accession
        })
#)
)  
head(sample_accession ) #  "ERS4399630" 
identical(sample_accession, sample_primary_id) # TRUE

# "SAMPLE_NAME"  
# TAXON_ID
sample_taxon_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]
        })
#)
) 
unique(sample_taxon_id )

# SCIENTIFIC_NAME /SPECIES NAME
sample_scientific_name <-unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]
        })
#)
) 
unique(sample_scientific_name) # "Severe acute respiratory syndrome coronavirus 2", "Wuhan seafood market pneumonia virus"

# "SAMPLE_LINKS"      
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]] # "bioproject"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]] # "601736""
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"

sample_links_db <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]]
        })
#)
) 
head(unique(sample_links_db)) #  "bioproject"

sample_links_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]]
        })
#)
) 
head(unique(sample_links_id )) # "601630"

sample_links_label <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"
                
})
#)
) 
head(unique(sample_links_label)) #  "PRJNA601630" 



# -------------------------------------------------------------------------------------------
#############################################################################################


# "SAMPLE_ATTRIBUTES" 

sample_attributes_tags <- 
        lapply(ont_xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG
                        
                })    
                
                
        })


length(sample_attributes_tags)  #   3959


length(unlist(sample_attributes_tags, recursive = F)) #   33808

sample_attributes_values <- 
        lapply(ont_xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$VALUE
                        
                })    
                
                
        })

length(unlist(sample_attributes_values, recursive = F)) #  3277033808
length(sample_attributes_values)  #  3959
length(unique(unlist(sample_attributes_values))) #   2571 unique values


run_id <- #unique(
        #unlist(
        lapply(ont_xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID
                        
                })    
                
                
        })
# )
#)

length(unlist(run_id, recursive = F)) #  33808

length(run_id)  #   3959
length(unique(unlist(run_id))) #   3959

run <-  unlist(run_id, recursive = F)

tag  <- unlist(sample_attributes_tags, recursive = F)

value <- unlist(sample_attributes_values, recursive = F)


pair_table <- data_frame(run, tag, value)
dim(pair_table)     # 33808     2
colnames(pair_table) <- c("run","tag", "value")


# unique(pair_table$tag) 
# unique(pair_table$value)

unique(unlist(filter(pair_table)$tag)) # 46


##### -----------#####--------------#####----------------#####--------------#####
# Values  


# Sample ID
unique(unlist(filter(pair_table, tag=="gisaid id")$value)) # 253 identifiers like "EPI_ISL_422810"
unique(unlist(filter(pair_table, tag=="barcode_identifiers")$value)) # "artic_barcode02"
unique(unlist(filter(pair_table, tag=="barcode_identifiers")$value)) # "artic_barcode02"
unique(unlist(filter(pair_table, tag=="ARTIC barcode identifiers")$value)) # "NB12"

# ----------- -----------  ----------- ----------- ----------- ----------- -----------  


##### Geo
unique(unlist(unique(filter(pair_table, tag=="geo_loc_name")$value))) # 67 values
unique(unlist(unique(filter(pair_table, tag=="country")$value))) #   "USA: CA, San Diego County"
unique(unlist(unique(filter(pair_table, tag=="geographic location (country and/or sea)")$value))) #"Denmark"        "United Kingdom" "Netherlands"         "United Kingdom" "Netherlands" 
unique(unlist(unique(filter(pair_table, tag=="geographic location (region and locality)")$value))) #   
# "not provided"  "England"       "Scotland"     
# "Delft"         "Nieuwendijk"   "Diemen"       
# "Oss"           "Houten"        "Helmond"      
# "Utrecht"       "Loon"          "Tilburg"      
# "Coevorden"     "Rotterdam"     "Eindhoven"    
#  "Haarlem"       "Dalen"         "Andel"        
# "Berlicum"      "Hardinxveld"   "Oisterwijk"   
# "Blaricum"      "Naarden"       "Zeewolde"     
# "Nootdorp"      "NoordBrabant"  "Limburg"      
# "Overijssel"    "ZuidHolland"   "Gelderland"   
# "NoordHolland"  "Flevoland"     "not collected"
# "Wales"   

# with geo contry
length(unlist(unique(filter(pair_table, tag=="geographic location (country and/or sea)")$run))) # 2289
length(unlist(unique(filter(pair_table, tag=="geographic location (region and locality)")$run))) # 1994

# with geo contry only
country_only <- setdiff(unlist(unique(filter(pair_table, tag=="geographic location (country and/or sea)")$run)),
        unlist(unique(filter(pair_table, tag=="geographic location (region and locality)")$run)))
# 293

country_only_val <- unique(unlist(filter(pair_table, run %in% country_only,
                           tag=="geographic location (country and/or sea)")$value))

# # with geo region -NONE
region_only <- setdiff(unlist(unique(filter(pair_table, tag=="geographic location (region and locality)")$run)),
                       unlist(unique(filter(pair_table, tag=="geographic location (country and/or sea)")$run)))
# 0


# combis
both <- intersect(unlist(unique(filter(pair_table, tag=="geographic location (country and/or sea)")$run)),
                unlist(unique(filter(pair_table, tag=="geographic location (region and locality)")$run)))
        # 1994
co <- filter(pair_table, run %in% both,
             tag=="geographic location (country and/or sea)")$value

reg <- filter(pair_table, run %in% both,
              tag=="geographic location (region and locality)")$value


combi <- paste(unlist(co), unlist(reg), sep=":")


geo_ont  <- unique(c(
        unique(unlist(unique(filter(pair_table, tag=="geo_loc_name")$value))),
        unique(unlist(unique(filter(pair_table, tag=="country")$value))),
       country_only_val,
        combi

))


write.table(geo_ont, "/Users/claudiavasallovega/repolab/work/virusbeacon/ont_geo.txt", row.names = F, col.names = F)

# Harmonize
remove <- c(
)
source[which(geo_ont %in% remove )] <- NA


# ----------- -----------  ----------- ----------- ----------- ----------- -----------                         
#####  Virus
unique(unlist(filter(pair_table, tag=="strain")$value)) # # empty
unique(unlist(filter(pair_table, tag=="isolate")$value)) # # empty
unique(unlist(filter(pair_table, tag=="Isolate" )$value)) # ISOLATE
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Collection lab
unique(unlist(filter(pair_table, tag=="collected_by")$value)) # empty
unique(unlist(filter(pair_table, tag=="collecting institution")$value)) # empty
# "collector name" 
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Collection date
unlist(unique(filter(pair_table, tag=="collection_date")$value)) # OK except "NA"
unlist(unique(filter(pair_table, tag=="collection date")$value)) # OK except "NA"
# "receipt date"
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Source
unlist(unique(filter(pair_table, tag=="host_tissue_sampled")$value)) # "nasopharynx"      "Oro-pharyngeal swab"
unlist(unique(filter(pair_table, tag=="isolation_source")$value)) # some new
unlist(unique(filter(pair_table, tag=="isolation source host-associated")$value)) #  "not provided"  "not collected"

# "sample capture status"
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Host
# taxon
unique(unlist(filter(pair_table, tag=="host")$value)) # "Homo sapiens"
unique(unlist(filter(pair_table, tag=="host_taxid")$value)) # "9606"
# age
unique(unlist(filter(pair_table, tag=="host_age")$value)) # 
unique(unlist(filter(pair_table, tag=="host age")$value)) # "1337"
# sex
unique(unlist(filter(pair_table, tag=="host_sex")$value)) #  "male"   "female"
unique(unlist(filter(pair_table, tag=="host sex")$value)) # "not provided"


# lab
#"Laboratory Host"
# "passage_history"
# "source_uvig"
# "env_medium"
#"env_local_scale"
#"env_broad_scale"
# "isolation source non-host-associated" 
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Host disease
unique(unlist(filter(pair_table, tag=="host_disease")$value)) # 
unique(unlist(filter(pair_table, tag=="host_disease_stage")$value)) # 
unique(unlist(filter(pair_table, tag=="host_disease_outcome")$value)) #  "recovery"
unique(unlist(filter(pair_table, tag=="host health state")$value)) # "not provided"  "not collected"
unique(unlist(filter(pair_table, tag=="host behaviour" )$value)) #  "other"
unlist(unique(filter(pair_table, tag=="sample capture status" )$value)) # "active surveillance in response to outbreak"
unlist(unique(filter(pair_table, tag=="serotype (required for a seropositive sample)")$value)) # "not provided"


# ----------- -----------  ----------- ----------- ----------- ----------- -----------   
# Lab host
unique(unlist(filter(pair_table, tag=="propagation")$value)) # "lytic"
unique(unlist(filter(pair_table, tag=="lab_host")$value)) # "Vero E6 cells"          "Homo Sapiens HAE cells"
#  "virus_enrich_appr"
# "stock_production_date"
# "Titer (Ct value)"
#"isol_growth_condt"
# ----------- -----------  ----------- ----------- ----------- ----------- -----------   




##### -----------#####--------------#####----------------#####--------------#####





# -------------------------------------------------------------------------------------------
#############################################################################################

################## SUBMISSION  ##########################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession

#  "IDENTIFIERS"
sub_primary_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID[[1]]
        })
        #)
) # same info as in ex primary id
head(unique(sub_primary_id)) # "ERA2420837"

#  accession attribute
sub_accession <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession
        })
        #)
) # same info as in ex primary id



################## "Organization" ####################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization)
# "Name"    "Address" "Contact"
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Contact)
#"Address" "Name"

# "Organization" > "Address" 
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Name[[1]] # "Wuhan University"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]] # China
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]] # "Hubei"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$City[[1]] # "Wuhan"

org_country <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]]
        })
        #)
)  #  "USA"   , "United States of America" homogenise
unique(org_country)

org_sub<- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]]
        })
        #)
)  #  "WA"  "wa" homogenise
unique(org_sub)

org_city <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$City[[1]]
        })
        #)
)  # "Seattle" ,  "seattle" homogenise
unique(org_city)



################## STUDY ####################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY) #  "IDENTIFIERS" "DESCRIPTOR"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY) # names "IDENTIFIERS" "DESCRIPTOR", center_name, alias, accesion 
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$center_name # "BioProject"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$alias # "PRJNA601736" (same as external id)
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession # "SRP242226" (same as primaryl id)

# "IDENTIFIERS"

# STUDY PRIMARY ID / ACCESSION
study_primary_id <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
        })
        #)
) 
study_accession <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession
        })
        #)
)  
head(study_accession)
identical(study_accession, study_primary_id) # TRUE

# STUDY EXTERNAL ID / ALIAS
study_external_id <- #unlist(unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 
unique(study_external_id)

study_alias <- unlist(
        #        unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$alias
        })
        #)
)  

unique(study_alias)
identical(study_external_id, study_alias) # FALSE 
# "Rapid metagenomic Sequencing" not identical



# "DESCRIPTOR"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
# STUDY_TITLE, STUDY_TYPE, STUDY_ABSTRACT, CENTER_PROJECT_NAME

# STUDY_TITLE
study_title <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
        })
        #)
)
length(unique(study_title)) # 19

# STUDY_TYPE
study_type <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TYPE)
        })
        #)
) 
length(unique(study_type)) # 2
unique(study_type)
#"Other"                   "Whole Genome Sequencing"

# STUDY_ABSTRACT
study_abstract <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_ABSTRACT[[1]]
        })
        #)
) 
length(unique(study_abstract)) # 21

# CENTER_PROJECT_NAME
study_center <- unlist(
        #unique(
        sapply(ont_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$CENTER_PROJECT_NAME[[1]]
        })
        #)
)
length(unique(study_center)) # 6




##################    Pool    ##########################################################################################
names(xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool) # "Member"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member$IDENTIFIERS # same info as in SAMPLE ID
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name # "nCov1" same info as sample alias
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots # "66043074"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases # "8728641275"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id # "2697049"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism # "Severe acute respiratory syndrome coronavirus 2"

pool_sample_name <-  unlist(unique(
       sapply(ont_xml_list, function(xml){
               attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name
                }) )) 

identical(pool_sample_name , sample_alias)

pool_spots <-  unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots
        }))) 

pool_bases <-  unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases
        }))) 

pool_tax_id <-  unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id
        }))) 

pool_organism <-  unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism
        }))) 

##################   "RUN_SET"   ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # names "RUN"
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # "RUN"

# "RUN"

run_id <-  unlist(
        sapply(ont_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID[[1]] # "SRR10903401"
})
)
length(run_id) # 2649
length(unique(run_id)) # 2649



# names
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN) 
# "IDENTIFIERS"    "EXPERIMENT_REF"
# "Pool"           "SRAFiles"      
# "CloudFiles"     "Statistics"    
# "Databases"      "Bases"       

xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID[[1]] # "SRR10903401"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$EXPERIMENT_REF$IDENTIFIERS
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$EXPERIMENT_REF)
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$EXPERIMENT_REF)$accession # "SRX7571571"

xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool)
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool) # "Member"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool$Member) # names, member_name, 
# accession, sample_name, spots, bases

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool$Member)$accession # (same as sample descriptot)
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool$Member)$sample_name # "WHU02" (same as sample alias)
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool$Member)$spots # "476632" (same as total spots)
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Pool$Member)$bases #  "143565674" (same as total bases)



attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN) #$names , accesion, alias, 
# total_spots, total_base, size, published, is_public, cluster_name, static_data_available, filtered_data_available

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$accession # "SRR10903401"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$alias # "wuhan2_1.fq.gz"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_spots # "476632"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_bases # "143565674"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$published # "2020-01-18 10:56:12"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$is_public # "true"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$cluster_name # "public"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$static_data_available # "1"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$filtered_data_available # "true"


run_spots <- unlist(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_spots # "476632"
        })) 

run_bases <- unlist(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_bases # "143565674"
        })) 

run_size <- unlist(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        })) 

run_sra_files <- unlist(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        }))

# CloudFiles filetype
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$CloudFiles$CloudFile)$filetype # fastq


# Statistics

# n reads
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads

run_nreads <- unlist(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads # "2"        "variable"
        })
        ) 

# nspots
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)

run_nspots <- unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)
        })
)) 

## read
# index
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0

run_read_index <- unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0
        })
)) 
#count
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
run_read_count <- unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
        })
)) 
#average
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average # "150.55"
run_read_ave <- unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average
        })
)) 
#stdev
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev # "0.74"
run_read_st <- unlist(unique(
        sapply(ont_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev
        }))) 
