library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)


folder <- "~/Downloads/gatk_ones_xml"
list.files(folder)
all_files <- paste(folder, list.files(folder), sep = "/")
xml_list <-  lapply(all_files, function(e){
        xml2::as_list(xml2::read_xml(e)) 
})
                    

# EXPERIMENT 

# EXPERIMENT > IDENTIFIERS > PRIMARY_ID
exp_primary_id <-unlist(unique(sapply(xml_list, function(xml){
   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID
})))
     


# EXPERIMENT > DESIGN > LIBRARY_DESCRIPTORS > LIBRARY NAME 
exp_library_name <- unlist(unique(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_NAME
})))


# EXPERIMENT > DESIGN > LIBRARY STRATEGY
exp_library_strategy <- unlist(unique(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY
})))
# (“RNA-Seq”, "WGS”, "AMPLICON”, "Targeted-Capture”)

# Graph EXPERIMENT > DESIGN > LIBRARY STRATEGY -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY
}))
data <- as.data.frame(table(t))

ggplot(data = data, 
       aes(Freq, t))

edlst <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY STRATEGY") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edlst
-------------------------------------------------------------------------       
        


# EXPERIMENT > DESIGN > LIBRARY SOURCE
exp_library_source <- unlist(unique(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
})))

# Graph EXPERIMENT > DESIGN > LIBRARY SOURCE -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
}))
data <- as.data.frame(table(t))

ggplot(data = data, 
       aes(Freq, t))

edlso <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY SOURCE") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edlso
-------------------------------------------------------------------------       
        
        

# EXPERIMENT > DESIGN > LIBRARY SELECTION
exp_library_selection <- unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
})
))

# Graph EXPERIMENT > DESIGN > LIBRARY SELECTION -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
}))
data <- as.data.frame(table(t))

ggplot(data = data, 
       aes(Freq, t))

edls <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY SELECTION") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edls
-------------------------------------------------------------------------       
        
        
# EXPERIMENT > DESIGN > LIBRARY LAYOUT
exp_library_layout <- unlist(unique(
        sapply(xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
)) # "PAIRED" "SINGLE"


# Graph EXPERIMENT > DESIGN > LIBRARY LAYOUT -------------------------------------------------------------------------
t <- sapply(xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
data <- as.data.frame(table(t))

ggplot(data = data, 
       aes(Freq, t))

edll<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY LAYOUT") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edll
-------------------------------------------------------------------------       
        


# EXPERIMENT PLATFORM > ILLUMINA MODEL
exp_platform <- unlist(unique(
        sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]
        })
)) 

# Graph PLATFORM > ILLUMINA MODEL -------------------------------------------------------------------------
t <- sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]})
data <- as.data.frame(table(t))

ggplot(data = data, 
        aes(Freq, t))

ep<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("PLATFORM") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
ep       
-------------------------------------------------------------------------       
# EXPERIMENT GRID
pdf("experiment.grid.pdf")
#grid.arrange(ep, nrow=2, ncol=3 ) # 
grid.arrange(ep, edlst , edlso, edls, edll,  nrow=2, ncol=3 ) # 
dev.off()




################## SUBMISSION  ##########################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID
xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession

#  "IDENTIFIERS"
sub_primary_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID[[1]]
        })
)) # same info as in ex primary id

#  accession attribute
sub_accession <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession
        })
)) # same info as in ex primary id



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

org_country <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]]
        })
))  #  "USA"   , "United States of America" homogenise

org_sub<- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]]
        })
))  #  "WA"  "wa" homogenise

org_city <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$City[[1]]
        })
))  # "Seattle" ,  "seattle" homogenise




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
study_primary_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID[[1]]
        })
)) 
study_accession <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession
        })
))  

identical(study_accession, study_primary_id) # TRUE

# STUDY EXTERNAL ID / ALIAS
study_external_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
)) 
study_alias <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$alias
        })
))  

identical(study_external_id, study_alias) # TRUE



# "DESCRIPTOR"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR)
# STUDY_TITLE, STUDY_TYPE, STUDY_ABSTRACT, CENTER_PROJECT_NAME

# STUDY_TITLE
study_title <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
        })
))
# STUDY_TYPE
study_type <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TYPE)
        })
)) 
# STUDY_ABSTRACT
study_abstract <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_ABSTRACT[[1]]
        })
)) 
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
sample_primary_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID[[1]]
        })
)) 

sample_external_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
)) 
# alias
sample_alias <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$alias
        })
)) 

# accesion
sample_accession <- unlist(unique(
        sapply(xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$accession
        })
)) 
identical(sample_accession, sample_primary_id) # TRUE

# "SAMPLE_NAME"  
# TAXON_ID
sample_taxon_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$TAXON_ID[[1]]
        })
)) 

# SCIENTIFIC_NAME /SPECIES NAME
sample_scientific_name <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_NAME$SCIENTIFIC_NAME[[1]]
        })
)) 


# "SAMPLE_LINKS"      
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]] # "bioproject"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]] # "601736""
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"

sample_links_db <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]]
        })
)) 

sample_links_id <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]]
        })
)) 

sample_links_label <- unlist(unique(
        sapply(xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"
                
        })
)) 


# "SAMPLE_ATTRIBUTES" 

# length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES) # 6
# sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
#         xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG[[1]]
#         
# })
# 
# length(xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES) # 11
# sapply(1:length(xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
#         xml2$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG[[1]]
#         
# })


sample_attributes <- unique(
        unlist(
        sapply(xml_list, function(xml){
                sapply(1:length(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES), function(a){
                        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG[[1]]
                        
                })    
                
                
        })
        )
)

all_all <- sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES})
all_all <- unlist(all_all)
all_tags <- all_all[which(names(all_all)=="SAMPLE_ATTRIBUTE.TAG")]
u_tags <- unique(all_tags)
all_values <- all_all[which(names(all_all)=="SAMPLE_ATTRIBUTE.VALUE")]

# Values per TAG (31 tags)

values <- lapply(u_tags, function(t){
        unique(all_values[which(all_tags==t)])
})

names(values) <- u_tags

values$BioSampleModel

# How many xml has each tag
u_tags

# Graph PLATFORM > ILLUMINA MODEL -------------------------------------------------------------------------
t <- all_tags
data <- as.data.frame(table(t))

tags<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("Sample attributes") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
tags      
-------------------------------------------------------------------------       
        



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
