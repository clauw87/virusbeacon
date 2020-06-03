library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#folder <-  "~/repolab/work/virusbeacon/6_05_2020_release"
#sra_xml_folder <- "~/repolab/work/virusbeacon/sra_metadata_frezee_24_05"
sra_xml_folder <- sra_xml_folder <- "~/repolab/work/virusbeacon/sra_metadata_30_05"
 
length(list.files(sra_xml_folder)) 
length(unique(list.files(sra_xml_folder))) # 2514 .. 3069

all_sra_files <- paste(sra_xml_folder, list.files(sra_xml_folder), sep = "/")

sra_xml_list <-  lapply(all_sra_files, function(f){
        xml2::as_list(xml2::read_xml(f)) 
})

length(sra_xml_list)  #   2514 .. 3069
length(unique(sra_xml_list)) # 2514.. 3069

sra_xml <- sra_xml_list[[1]]

xml <- sra_xml
# EXPERIMENT 

# EXPERIMENT > IDENTIFIERS > PRIMARY_ID
exp_primary_id <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID
})
#)
)
length(unique(exp_primary_id)) # 2513 .. 3069


# EXPERIMENT > DESIGN > LIBRARY_DESCRIPTORS > LIBRARY NAME 
exp_library_name <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_NAME
})
#)
)
length(exp_library_name) # 0
length(unique(exp_library_name)) #  0

# EXPERIMENT > DESIGN > LIBRARY STRATEGY
exp_library_strategy <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY

                })
        #)
)

# (“RNA-Seq”, "WGS”, "AMPLICON”, "Targeted-Capture”)
length(unique(exp_library_strategy )) # 
unique(exp_library_strategy)


# EXPERIMENT > DESIGN > LIBRARY SOURCE
exp_library_source <- 
        unlist(
                #unique(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
})
#)
) # "VIRAL RNA"   "METAGENOMIC" "OTHER" 
length(unique(exp_library_source )) # 5
unique(exp_library_source ) # "genomic RNA"

# EXPERIMENT > DESIGN > LIBRARY SELECTION
exp_library_selection <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
})
#)
)
length(unique(exp_library_selection )) # 7
unique(exp_library_selection )
# "RANDOM" "RT-PCR"  "RANDOM PCR"  "unspecified" "PCR" "cDNA" "Hybrid Selection"
        
# EXPERIMENT > DESIGN > LIBRARY LAYOUT
exp_library_layout <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
#)
) # "PAIRED" "SINGLE"
length(unique(exp_library_layout)) #2
unique(exp_library_layout) #  "PAIRED"

 
# EXPERIMENT PLATFORM or TECHNOLOGY
exp_platform <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM)
        })
        #)
) 
length(unique(exp_platform)) #  "ILLUMINA"        "OXFORD_NANOPORE" (error, some ont files are misplaced here!)
unique(exp_platform) #  "ILLUMINA"

run_id[which(exp_platform== "OXFORD_NANOPORE")]
# "SRR11140745" "SRR11140747" "SRR11140749" "SRR11140751"
# "ERR4085074" "ERR4085076" "ERR4085078" "ERR4085080"


# EXPERIMENT PLATFORM > ILLUMINA MODEL
exp_platform_model <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]
        })
#)
) 
length(unique(exp_platform_model)) # 
unique(exp_platform_model)



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
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID
        })
#)
) 

sample_external_id <- #unlist(unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID
        })
#)) 

# alias
sample_alias <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$alias
        })
#)
) 

# accesion
sample_accession <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$accession
        })
#)
) 
identical(sample_accession, sample_primary_id) # TRUE

# "SAMPLE_NAME"  
# TAXON_ID
sample_taxon_id <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]
        })
#)
) 

# SCIENTIFIC_NAME /SPECIES NAME
sample_scientific_name <-unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]
        })
#)
) 


# "SAMPLE_LINKS"      
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]] # "bioproject"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]] # "601736""
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"

sample_links_db <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]]
        })
#)
) 

sample_links_id <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]]
        })
#)
) 

sample_links_label <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"
                
})
#)
) 




# -------------------------------------------------------------------------------------------
#############################################################################################


# "SAMPLE_ATTRIBUTES" 

sample_attributes_tags <- #unique(
        #unlist(
        sapply(sra_xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG
                        
                })    
                
                
        })
       # )
#)


length(sample_attributes_tags)  # 25140 ..30690
length(unique(unlist(sample_attributes_tags))) # 10

sample_attributes_values <- #unique(
        #unlist(
                sapply(sra_xml_list, function(xml){
                        sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$VALUE
                                
                        })    
                        
                        
                })
       # )
#)


length(sample_attributes_values)  # 25140
length(unique(unlist(sample_attributes_values))) #  2761

tag <- sapply(sample_attributes_tags, function(l){
       unlist(l) 
})
length(tag)     

value <- sapply(sample_attributes_values, function(l){
        unlist(l) 
})
length(value)

pair_table <- data_frame(tag, value)
dim(pair_table)    
colnames(pair_table) <- c("tag", "value")

# for (l in 1:length(pair_table$value)) {
#         pair_table$value[l] <- unlist(pair_table$value[l])
# }
# pair_table$value <- sapply(pair_table$value, function(l){
# unlist(l)
# })

unique(pair_table$tag)
unique(pair_table$value)

# Values
unique(unlist(filter(pair_table, tag=="strain")$value)) # # empty
unique(unlist(filter(pair_table, tag=="collected_by")$value)) # empty
unique(unlist(filter(pair_table, tag=="collection_date")$value)) # OK
unique(unlist(filter(pair_table, tag=="geo_loc_name")$value)) 
unique(unlist(filter(pair_table, tag=="host")$value)) # host, sex, age
unique(unlist(filter(pair_table, tag=="host_disease")$value)) # empty
unique(unlist(filter(pair_table, tag=="host_age")$value)) # empty
unique(unlist(filter(pair_table, tag=="host_sex")$value)) # empty
unique(unlist(filter(pair_table, tag=="isolate" )$value)) # ISOLATE
unique(unlist(filter(pair_table, tag=="assembly_method")$value)) # PIPELINE


geo <- unique(unlist(filter(pair_table, tag=="geo_loc_name")$value))
sex <- unique(unlist(filter(pair_table, tag=="host")$value))
age <- unique(unlist(filter(pair_table, tag=="host")$value))
isolate <- unique(unlist(filter(pair_table, tag=="isolate" )$value))
assembly_method <- unique(unlist(filter(pair_table, tag=="assembly_method")$value)) 

#############################################################################################

write.table(geo, "/Users/claudiavasallovega/repolab/work/virusbeacon/sra_geo.txt", row.names = F, col.names = F)
write.table(sex, "/Users/claudiavasallovega/repolab/work/virusbeacon/sra_sex.txt", row.names = F, col.names = F)
write.table(age, "/Users/claudiavasallovega/repolab/work/virusbeacon/sra_age.txt", row.names = F, col.names = F)



geo <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_geo.txt")$V1)
length(geo) #128

geo_fixed <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_geo_fixed.txt")$V1)
length(geo_fixed)  






################## SUBMISSION  ##########################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession

#  "IDENTIFIERS"
sub_primary_id <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID[[1]]
        })
        #)
) # same info as in ex primary id

#  accession attribute
sub_accession <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
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
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]]
        })
        #)
)  #  "USA"   , "United States of America" homogenise
unique(org_country)

org_sub<- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]]
        })
        #)
)  #  "WA"  "wa" homogenise
unique(org_sub)

org_city <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
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
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
        })
        #)
) 
study_accession <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession
        })
        #)
)  

identical(study_accession, study_primary_id) # TRUE

# STUDY EXTERNAL ID / ALIAS
study_external_id <- #unlist(unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 
unique(study_external_id)
study_alias <- unlist(
        #        unique(
        sapply(sra_xml_list, function(xml){
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
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
        })
        #)
)
length(unique(study_title)) # 24

# STUDY_TYPE
study_type <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TYPE)
        })
        #)
) 
length(unique(study_type)) # 3
#  "Metagenomics"  "Whole Genome Sequencing"  "Other"

# STUDY_ABSTRACT
study_abstract <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_ABSTRACT[[1]]
        })
        #)
) 
length(unique(study_abstract)) # 26

# CENTER_PROJECT_NAME
study_project_name <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$CENTER_PROJECT_NAME[[1]]
        })
        #)
)
length(unique(study_project_name)) # 6


# CENTER

center_name <- unlist(
        #unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$center_name
        })
        #)
)

unique(center_name)




##################    Pool    ##########################################################################################
names(xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool) # "Member"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member$IDENTIFIERS # same info as in SAMPLE ID
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name # "nCov1" same info as sample alias
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots # "66043074"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases # "8728641275"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id # "2697049"
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism # "Severe acute respiratory syndrome coronavirus 2"

pool_sample_name <-  unlist(unique(
       sapply(sra_xml_list, function(xml){
               attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name
                }) )) 

identical(pool_sample_name , sample_alias)

pool_spots <-  unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots
        }))) 

pool_bases <-  unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases
        }))) 

pool_tax_id <-  unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id
        }))) 

pool_organism <-  unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism
        }))) 

##################   "RUN_SET"   ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # names "RUN"
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # "RUN"

# "RUN"

run_id <-  unlist(
        sapply(sra_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$IDENTIFIERS$PRIMARY_ID # "SRR10903401"
})
)
length(run_id) # ]  2513
length(unique(run_id)) # ]  2513



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
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_spots # "476632"
        })) 

run_bases <- unlist(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_bases # "143565674"
        })) 

run_size <- unlist(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        })) 

run_sra_files <- unlist(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        }))

# CloudFiles filetype
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$CloudFiles$CloudFile)$filetype # fastq


# Statistics

# n reads
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads

run_nreads <- unlist(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads # "2"        "variable"
        })
        ) 

# nspots
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)

run_nspots <- unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)
        })
)) 

## read
# index
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0

run_read_index <- unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0
        })
)) 
#count
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
run_read_count <- unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
        })
)) 
#average
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average # "150.55"
run_read_ave <- unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average
        })
)) 
#stdev
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev # "0.74"
run_read_st <- unlist(unique(
        sapply(sra_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev
        }))) 
