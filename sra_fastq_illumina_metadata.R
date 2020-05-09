library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)


folder <-  "~/repolab/work/virusbeacon/439_xmls"
length(list.files(folder))
all_files <- paste(folder, list.files(folder), sep = "/")
xml_list <-  lapply(all_files, function(e){
        xml2::as_list(xml2::read_xml(e)) 
})
length(xml_list)                   
xml <- xml_list[[439]]

# EXPERIMENT 

# EXPERIMENT > IDENTIFIERS > PRIMARY_ID
exp_primary_id <- #unlist(unique(
        sapply(xml_list, function(xml){
   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID[[1]]
})
#))
     


# EXPERIMENT > DESIGN > LIBRARY_DESCRIPTORS > LIBRARY NAME 
exp_library_name <- #unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_NAME
})
#))


# EXPERIMENT > DESIGN > LIBRARY STRATEGY
exp_library_strategy <- #unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY
})
#))
# (“RNA-Seq”, "WGS”, "AMPLICON”, "Targeted-Capture”)


# EXPERIMENT > DESIGN > LIBRARY SOURCE
exp_library_source <- #unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
})
#))


# EXPERIMENT > DESIGN > LIBRARY SELECTION
exp_library_selection <- #unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
})
#))


        
# EXPERIMENT > DESIGN > LIBRARY LAYOUT
exp_library_layout <- #unlist(unique(
        sapply(xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
#)) # "PAIRED" "SINGLE"


 

# EXPERIMENT PLATFORM > ILLUMINA MODEL
exp_platform <- #unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]
        })
#)) 


################## SUBMISSION  ##########################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID
xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession

#  "IDENTIFIERS"
sub_primary_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID[[1]]
        })
#)) # same info as in ex primary id

#  accession attribute
sub_accession <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession
        })
#)) # same info as in ex primary id



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

org_country <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]]
        })
#))  #  "USA"   , "United States of America" homogenise

org_sub<- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]]
        })
#))  #  "WA"  "wa" homogenise

org_city <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$City[[1]]
        })
#))  # "Seattle" ,  "seattle" homogenise




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
study_primary_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
        })
#)) 
study_accession <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession
        })
#))  

identical(study_accession, study_primary_id) # TRUE

# STUDY EXTERNAL ID / ALIAS
study_external_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 

study_alias <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$alias
        })
#))  

identical(study_external_id, study_alias) # TRUE



# "DESCRIPTOR"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
# STUDY_TITLE, STUDY_TYPE, STUDY_ABSTRACT, CENTER_PROJECT_NAME

# STUDY_TITLE
study_title <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
        })
#))
# STUDY_TYPE
study_type <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TYPE)
        })
#)) 
# STUDY_ABSTRACT
study_abstract <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_ABSTRACT[[1]]
        })
#)) 
# CENTER_PROJECT_NAME
study_center <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$CENTER_PROJECT_NAME[[1]]
        })
)) 


##################    "SAMPLE"    ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)
# names , alias, accession
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)
# "IDENTIFIERS"       "SAMPLE_NAME"      
# "SAMPLE_LINKS"      "SAMPLE_ATTRIBUTES"

xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]

# "IDENTIFIERS" 
sample_primary_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]
        })
#)) 

sample_external_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 

# alias
sample_alias <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$alias
        })
#)) 

# accesion
sample_accession <- #unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$accession
        })
#)) 
identical(sample_accession, sample_primary_id) # TRUE

# "SAMPLE_NAME"  
# TAXON_ID
sample_taxon_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]
        })
#)) 

# SCIENTIFIC_NAME /SPECIES NAME
sample_scientific_name <-# unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]
        })
#)) 


# "SAMPLE_LINKS"      
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]] # "bioproject"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]] # "601736""
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"

sample_links_db <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]]
        })
#)) 

sample_links_id <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]]
        })
#)) 

sample_links_label <- #unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"
                
})
#)) 




# -------------------------------------------------------------------------------------------
#############################################################################################


# "SAMPLE_ATTRIBUTES" 

sample_attributes_tags <- unique(
        unlist(
        sapply(xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG[[1]]
                        
                })    
                
                
        })
        )
)



# all unique tags unlisted
all_all <- lapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES
})
unlist(all_all)
all_tags <- unlist(all_all)[which(names(unlist(all_all))=="SAMPLE_ATTRIBUTE.TAG")]
u_tags <- unique(all_tags)
all_values <- unlist(all_all)[which(names(unlist(all_all))=="SAMPLE_ATTRIBUTE.VALUE")]


# all tags per xml
all_tags_per_xml <- lapply(all_all, function(x){
        unique(
         unlist(x)[which(names(unlist(x))=="SAMPLE_ATTRIBUTE.TAG")]
                )
        })


# all values per tags per xml
all_values_per_tag_per_xml <- list()
length(all_values_per_tag_per_xml) <- length(all_all)
      for (i in 1:length(all_all)) {
              
       dtable <- c("tag", "value")
              #colnames(dtable) <- c("tag", "value") # for each tag-value par
       for (j in  1:length(all_all[[i]])) {
                     
       dtable <- data.frame(rbind(dtable, as.character(c(all_all[[i]][j]$SAMPLE_ATTRIBUTE$TAG[[1]] , 
                                 all_all[[i]][j]$SAMPLE_ATTRIBUTE$VALUE[[1]]))), 
                            stringsAsFactors = F)
       
       
       }  
      dtable <- dtable[-1,]
      colnames(dtable) <- c("tag", "value")
      all_values_per_tag_per_xml[[i]] <- dtable  
       
      }
               
        


# Which file has each tag
which(str_detect(all_tags_per_xml,"host_disease_outcome"))


which(str_detect(all_tags_per_xml,"ref_biomaterial"))


# Values per TAG (31 tags)

sample_attributes_values <- lapply(u_tags, function(t){
        unique(all_values[which(all_tags==t)])
})
names(sample_attributes_values) <- u_tags


sample_attributes_values$host
sample_attributes_values$host_disease_outcome # "Survived"
sample_attributes_values$host_disease
sample_attributes_values$host_disease_stage # "Acute"
sample_attributes_values$passage_history
sample_attributes_values$sex
sample_attributes_values$host_sex

# How many xml has each tag
# Plot

## Duplicated info in tags?
#### SAMPLE ORIGIN
sample_attributes_values$culture_collection
sample_attributes_values$tissue    # good one
sample_attributes_values$isolation_source # good one   
sample_attributes_values$host_tissue_sampled  # good one 
sample_attributes_values$env_medium  # good one 
sample_attributes_values$geo_loc_name



which(str_detect(all_tags_per_xml,"tissue"))
# Biosample sample type tags> Non/redundant
length(which(str_detect(all_tags_per_xml,"tissue"))) # 20
length(which(str_detect(all_tags_per_xml,"isolation_source"))) # 436
length(which(str_detect(all_tags_per_xml,"host_tissue_sampled"))) # 18


length(intersect(which(str_detect(all_tags_per_xml,"tissue")),  # 
                 which(str_detect(all_tags_per_xml,"isolation_source")))) # 18 everlap (18/22 ovelap)

setdiff(which(str_detect(all_tags_per_xml,"tissue")), 
        which(str_detect(all_tags_per_xml,"isolation_source"))) # 2, xml 1 and 2 (4: 1,2,3,4)

setdiff(which(str_detect(all_tags_per_xml,"isolation_source" )), 
        which(str_detect(all_tags_per_xml,"tissue"))) # 2, xml 1 and 2 (4: 1,2,3,4)

length(intersect(which(str_detect(all_tags_per_xml,"tissue")),  # 18
                 which(str_detect(all_tags_per_xml,"host_tissue_sampled")))) # (18 overlap)

setdiff(which(str_detect(all_tags_per_xml,"tissue")), 
        which(str_detect(all_tags_per_xml,"host_tissue_sampled"))) # 2, xml 1 , 2  

setdiff(which(str_detect(all_tags_per_xml,"host_tissue_sampled" )), 
        which(str_detect(all_tags_per_xml,"tissue"))) # 0

setdiff(which(str_detect(all_tags_per_xml,"host_tissue_sampled" )), 
        which(str_detect(all_tags_per_xml,"isolation_source"))) # 0


# Geo location tags > geo_loc_name, country and la_lon > country and lan_lon are redundant with geo_loc_name
length(intersect(which(str_detect(all_tags_per_xml,"geo_loc_name")),  # 47 
                 which(str_detect(all_tags_per_xml,"country")))) # 0 no everlap (only 1 xml #30)



length(intersect(which(str_detect(all_tags_per_xml,"geo_loc_name")), # 47
                 which(str_detect(all_tags_per_xml,"lat_lon")))) # all 47 common, almost full overlap

setdiff(which(str_detect(all_tags_per_xml,"geo_loc_name")), 
                  which(str_detect(all_tags_per_xml,"lat_lon"))) # 0


length(intersect(which(str_detect(all_tags_per_xml,"country")), # 1 xml # 30
                 which(str_detect(all_tags_per_xml,"lat_lon")))) # 1 (1, common to the only one with country tag)


# The two studies using ref matierial instead of new isolates> # sample type,ref_biomaterial, host_description, passage_history, Extraction Method
length(intersect(which(str_detect(all_tags_per_xml,"ref_biomaterial")), # 2 xml #
                 which(str_detect(all_tags_per_xml,"host_description")))) # 2 (2 common)

length(intersect(which(str_detect(all_tags_per_xml,"ref_biomaterial")), # 2 xml #
                 which(str_detect(all_tags_per_xml,"sample type")))) # 2 (2 common)

length(intersect(which(str_detect(all_tags_per_xml,"ref_biomaterial")), # 2 xml #
                 which(str_detect(all_tags_per_xml,"passage_history")))) # 2 (2 common)


# Statistics on tags

# Geo_location, sex, disease


# Age
sample_attributes_values$age
sample_attributes_values$host_age
length(intersect(which(str_detect(all_tags_per_xml,"age")),  # 
                 which(str_detect(all_tags_per_xml,"host_age")))) # 329 

length(setdiff(which(str_detect(all_tags_per_xml,"age")),  # 
                 which(str_detect(all_tags_per_xml,"host_age")))) # 22
# -------------------------------------------------------------------------------------------
#############################################################################################




##################    Pool    ##########################################################################################
names(xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool) # "Member"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member$IDENTIFIERS # same info as in SAMPLE ID
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name # "nCov1" same info as sample alias
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots # "66043074"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases # "8728641275"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id # "2697049"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism # "Severe acute respiratory syndrome coronavirus 2"

pool_sample_name <-  unlist(unique(
       sapply(xml_list, function(xml){
               attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name
                }) )) 

identical(pool_sample_name , sample_alias)

pool_spots <-  unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots
        }))) 

pool_bases <-  unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases
        }))) 

pool_tax_id <-  unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id
        }))) 

pool_organism <-  unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism
        }))) 

##################   "RUN_SET"   ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # names "RUN"
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # "RUN"

# "RUN"

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


run_spots <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_spots # "476632"
        }))) 

run_bases <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_bases # "143565674"
        }))) 

run_size <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        }))) 

run_sra_files <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        }))) 

# CloudFiles filetype
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$CloudFiles$CloudFile)$filetype # fastq


# Statistics

# n reads
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads

run_nreads <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads # "2"        "variable"
        })
        )) 

# nspots
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)

run_nspots <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)
        })
)) 

## read
# index
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0

run_read_index <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0
        })
)) 
#count
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
run_read_count <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
        })
)) 
#average
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average # "150.55"
run_read_ave <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average
        })
)) 
#stdev
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev # "0.74"
run_read_st <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev
        }))) 
