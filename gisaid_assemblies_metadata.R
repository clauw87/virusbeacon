library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#folder <-  "~/repolab/work/virusbeacon/6_05_2020_release"
#gisaid_xml_folder <- "~/repolab/work/virusbeacon/gisaid_metadata"
gisaid_xml_folder <- "~/repolab/work/virusbeacon/gisaid_metadata_epi_accession"


length(list.files(gisaid_xml_folder)) # 34083 .. 34083 OK
length(unique(list.files(gisaid_xml_folder))) # 34083.. 34083 OK


all_gisaid_files <- paste(gisaid_xml_folder, list.files(gisaid_xml_folder), sep = "/")

# gisaid_xml_list <-  lapply(all_gisaid_files, function(f){
#         xml2::as_list(xml2::read_xml(f)) 
# })
# 
gisaid_xml_list <-  lapply(all_gisaid_files, function(f){
        xml2::as_list(xml2::read_xml(f,as_html=TRUE) )
})




# 
# 
# gisaid_xml_list1 <-  lapply(all_gisaid_files[1:1000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list2 <-  lapply(all_gisaid_files[1001:2000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list3 <-  lapply(all_gisaid_files[2001:3000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# # 
# gisaid_xml_list4 <-  lapply(all_gisaid_files[3001:4000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list5 <-  lapply(all_gisaid_files[4001:5000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list6 <-  lapply(all_gisaid_files[5001:6000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# # gisaid_xml_list7 <-  lapply(all_gisaid_files[6001:7000], function(f){
# #         xml2::as_list(xml2::read_xml(f))
# # })
# 
# gisaid_xml_list8 <-  lapply(all_gisaid_files[7001:8000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list9 <-  lapply(all_gisaid_files[8001:9000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })
# 
# gisaid_xml_list10 <-  lapply(all_gisaid_files[8001:9000], function(f){
#         xml2::as_list(xml2::read_xml(f))
# })


length(gisaid_xml_list)  #   34082
length(unique(gisaid_xml_list)) # 34082

gisaid_xml <- gisaid_xml_list[[1]]

xml <- gisaid_xml
# EXPERIMENT 

# EXPERIMENT > IDENTIFIERS > PRIMARY_ID
exp_primary_id <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
   xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$IDENTIFIERS$PRIMARY_ID
})
#)
)
length(unique(exp_primary_id)) # 2513


# EXPERIMENT > DESIGN > LIBRARY_DESCRIPTORS > LIBRARY NAME 
exp_library_name <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_NAME
})
#)
)
length(exp_library_name) # 0
length(unique(exp_library_name)) #  0

# EXPERIMENT > DESIGN > LIBRARY STRATEGY
exp_library_strategy <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY

                })
        #)
)

# (“RNA-Seq”, "WGS”, "AMPLICON”, "Targeted-Capture”)
length(unique(exp_library_strategy )) # 4
unique(exp_library_strategy)


# EXPERIMENT > DESIGN > LIBRARY SOURCE
exp_library_source <- 
        unlist(
                #unique(
        sapply(gisaid_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
})
#)
) # "VIRAL RNA"   "METAGENOMIC" "OTHER" 
length(unique(exp_library_source )) # 5
unique(exp_library_source ) # "genomic RNA"

# EXPERIMENT > DESIGN > LIBRARY SELECTION
exp_library_selection <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
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
        sapply(gisaid_xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
#)
) # "PAIRED" "SINGLE"
length(unique(exp_library_layout)) #2
unique(exp_library_layout) #  "PAIRED"

 
# EXPERIMENT PLATFORM or TECHNOLOGY
exp_platform <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
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
        sapply(gisaid_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL
        })
#)
) 
length(unique(exp_platform_model)) # 
unique(exp_platform_model)

################## SUBMISSION  ##########################################################################################
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)

attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION)$accession

#  "IDENTIFIERS"
sub_primary_id <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SUBMISSION$IDENTIFIERS$PRIMARY_ID[[1]]
        })
#)
) # same info as in ex primary id

#  accession attribute
sub_accession <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
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
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Country[[1]]
        })
#)
)  #  "USA"   , "United States of America" homogenise
unique(org_country)

org_sub<- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Organization$Address$Sub[[1]]
        })
#)
)  #  "WA"  "wa" homogenise
unique(org_sub)

org_city <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
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
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$PRIMARY_ID
        })
#)
) 
study_accession <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$accession
        })
#)
)  

identical(study_accession, study_primary_id) # TRUE

# STUDY EXTERNAL ID / ALIAS
study_external_id <- #unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$IDENTIFIERS$EXTERNAL_ID[[1]]
        })
#)) 
unique(study_external_id)
study_alias <- unlist(
#        unique(
        sapply(gisaid_xml_list, function(xml){
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
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TITLE[[1]]
        })
#)
)
length(unique(study_title)) # 24

# STUDY_TYPE
study_type <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_TYPE)
        })
#)
) 
length(unique(study_type)) # 3
#  "Metagenomics"  "Whole Genome Sequencing"  "Other"

# STUDY_ABSTRACT
study_abstract <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$STUDY_ABSTRACT[[1]]
        })
#)
) 
length(unique(study_abstract)) # 26

# CENTER_PROJECT_NAME
study_project_name <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY$DESCRIPTOR$CENTER_PROJECT_NAME[[1]]
        })
#)
)
length(unique(study_project_name)) # 6


# CENTER

center_name <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$STUDY)$center_name
        })
        #)
)

unique(center_name)




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
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$PRIMARY_ID
        })
#)
) 

sample_external_id <- #unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$IDENTIFIERS$EXTERNAL_ID
        })
#)) 

# alias
sample_alias <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$alias
        })
#)
) 

# accesion
sample_accession <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE)$accession
        })
#)
) 
identical(sample_accession, sample_primary_id) # TRUE

# "SAMPLE_NAME"  
# TAXON_ID
sample_taxon_id <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                gisaid_xml_list[[1]]$html$body$experiment_package_set$experiment_package$sample$sample_name$taxon_id[[1]]
        })
#)
) 
unique(sample_taxon_id) # "2697049"

# SCIENTIFIC_NAME /SPECIES NAME
sample_scientific_name <-unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                gisaid_xml_list[[1]]$html$body$experiment_package_set$experiment_package$sample$sample_name$scientific_name[[1]]
        })
#)
) 
unique(sample_scientific_name)

# "SAMPLE_LINKS"      
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]] # "bioproject"
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID[[1]] # "601736""
xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL[[1]] # "PRJNA601736"

sample_links_db <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$DB[[1]]
        })
#)
) 

sample_links_id <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$ID
        })
#)
) 

sample_links_label <- unlist(
        #unique(
        sapply(gisaid_xml_list, function(xml){
                xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_LINKS$SAMPLE_LINK$XREF_LINK$LABEL # "PRJNA601736"
                
})
#)
) 




# -------------------------------------------------------------------------------------------
#############################################################################################


# "SAMPLE_ATTRIBUTES" 

sample_attributes_tags <- #unique(
        #unlist(
        sapply(gisaid_xml_list, function(xml){
                sapply(1:length(xml$html$body$experiment_package_set$experiment_package$sample$sample_attributes), function(a){
                       # xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$SAMPLE$SAMPLE_ATTRIBUTES[[a]]$TAG
                        xml$html$body$experiment_package_set$experiment_package$sample$sample_attributes[[a]]$tag
                })    
                
                
        })
       # )
#)


length(sample_attributes_tags)  #  306738
length(unique(unlist(sample_attributes_tags))) # 9

sample_attributes_values <- #unique(
        #unlist(
                sapply(gisaid_xml_list, function(xml){
                        sapply(1:length(xml$html$body$experiment_package_set$experiment_package$sample$sample_attributes), function(a){
                                xml$html$body$experiment_package_set$experiment_package$sample$sample_attributes[[a]]$value
                                
                        })    
                        
                        
                })
       # )
#)


length(sample_attributes_values)  # 306738
length(unique(sample_attributes_values)) #  306738

tag <- sapply(sample_attributes_tags, function(l){
       unlist(l) 
})
       
value <- sapply(sample_attributes_values, function(l){
        unlist(l) 
})

pair_table <- data_frame(tag, value)
dim(pair_table)    
colnames(pair_table) <- c("tag", "value")


# for (l in 1:length(pair_table$value)) {
#         pair_table$value[l] <- unlist(pair_table$value[l])
# }

unique(pair_table$tag)
unique(pair_table$value)

# Values
unique(filter(pair_table, tag=="strain")$value) # strain
unique(filter(pair_table, tag=="collected_by")$value) # lab
unlist(unique(filter(pair_table, tag=="collection_date")$value))
unique(unlist(unique(filter(pair_table, tag=="geo_loc_name")$value)))
unique(unlist(filter(pair_table, tag=="host")$value)) # host
unique(unlist(filter(pair_table, tag=="host_disease")$value)) # empty
unique(unlist(filter(pair_table, tag=="host_age")$value)) # age
unique(unlist(filter(pair_table, tag=="host_sex")$value)) # empty
unique(filter(pair_table, tag=="isolate" )$value) # # empty
unique(filter(pair_table, tag=="assembly_method")$value) # PIPELINE


gisaid_geo <- unique(unlist(filter(pair_table, tag=="geo_loc_name")$value))
sex <- unique(unlist(filter(pair_table, tag=="host_sex")$value))
isolate <- unique(unlist(filter(pair_table, tag=="isolate" )$value))
assembly_method <- unique(unlist(filter(pair_table, tag=="assembly_method")$value)) 


write.table(gisaid_geo, "/Users/claudiavasallovega/repolab/work/virusbeacon/gisaid_geo.txt", row.names = F, col.names = F)

#gisaid_geo <- read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/gisaid_geo.txt")
gisaid_geo_fixed <- gsub(x=gisaid_geo, pattern = ":$", ":-")


# fix these values
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Germany:Duesseldorf:-")] <- "Europe:Germany:Dusseldorf:-" 
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Germany:North Rhine Westphalia:Duesseldorf")] <- "Europe:Germany:North Rhine Westphalia:Dusseldorf"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:Washington:Franklin County WA")] <- "North America:USA:Washington:Franklin County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:Washington:Adams County WA")] <- "North America:USA:Washington:Adams County" 
gisaid_geo_fixed[which(gisaid_geo_fixed=="South America:Colombia:Cundinamarca:Madrid CO")] <- "South America:Colombia:Cundinamarca:Madrid"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:New York:Orange County NY")] <- "North America:USA:New York:Orange County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:New York:Jefferson County NY")] <- "North America:USA:New York:Jefferson County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:New York:Franklin County NY")] <- "North America:USA:New York:Franklin County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:New York:Washington County NY")] <- "North America:USA:New York:Washington County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:Oregon:Washington County OR")] <- "North America:USA:Oregon:Washington County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Italy:Veneto:Verona IT")] <- "Europe:Italy:Veneto:Verona"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:Washington:Jefferson County WA")] <- "North America:USA:Washington:Jefferson County"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Croatia:Varazdin:-")] <- "Europe:Croatia:Varazdin County:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Finland:Finland:-")] <- "Europe:Finland:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:France:-")] <- "Europe:France:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Oceania:Guam:Guam:-")] <- "Oceania:Guam:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Hong Kong:Hong Kong:-")] <- "Asia:Hong Kong:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Hong Kong:Hong Kong:Hong Kong")] <- "Asia:Hong Kong:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Oceania:Australia:Australia:-")] <- "Oceania:Australia:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Austria:Austria:")] <- "Europe:Austria:-:"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:India:India:-")] <- "Asia:India:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Israel:Israel:-")] <- "Asia:Israel:-:"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:Jamaica:Jamaica:")] <- "North America:Jamaica:-:"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Japan:Aichi:Aichi")] <- "Asia:Japan:Aichi:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Japan:Kyoto:Kyoto")] <- "Asia:Japan:Kyoto:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Japan:Tokyo:Tokyo")] <- "Asia:Japan:Tokyo:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Japan:Aichi:Aichi")] <- "Asia:Japan:Aichi:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Luxembourg:Luxembourg:Luxembourg")] <- "Europe:Luxembourg:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Bangladesh:Bangladesh:-")] <- "Asia:Bangladesh:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Philippines:Philippines:-")] <- "Asia:Philippines:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Singapore:Singapore:Singapore")] <- "Asia:Singapore:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Turkey:Turkey:-")] <- "Europe:Turkey:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:Belgium:-")] <- "Europe:Belgium:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:South Korea:Gyeonggi:Gyeonggi")] <- "Asia:South Korea:Gyeonggi:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:South Korea:Seoul:Seoul")] <- "Asia:South Korea:Seoul:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:South Korea:Chungcheongnam:Chungcheongnam")] <- "Asia:South Korea:Chungcheongnam:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Brunei:Brunei:-")] <- "Asia:Brunei:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="South America:Brazil:Brazil:-")] <- "South America:Brazil:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Africa:South Africa:South Africa:-")] <- "Africa:South Africa:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Croatia:VaraÅ¾din County:-")] <- "Europe:Croatia:Varazdin County:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Switzerland:ZÃ¼rich:-")] <- "Europe:Switzerland:Zurich:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:LiÃ¨ge:Wandre")] <- "Europe:Belgium:Liege:Wandre"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:LiÃ¨ge:-")] <- "Europe:Belgium:Liege:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:LiÃ¨ge:Yernee-fraineux")] <- "Europe:Belgium:Liege:Yernee-fraineux"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:LiÃ¨ge:Bierset")] <- "Europe:Belgium:Liege:Bierset"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Privas")] <- "Europe:France:Auvergne-Rhone-Alpes:Privas"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Macon")] <- "Europe:France:Auvergne-Rhone-Alpes:Macon"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Valence")] <- "Europe:France:Auvergne-Rhone-Alpes:Valence"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Venissieux")] <- "Europe:France:Auvergne-Rhone-Alpes:Venissieux"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Lyon")] <- "Europe:France:Auvergne-Rhone-Alpes:Lyon"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Bourg-en-Bresse")] <- "Europe:France:Auvergne-Rhone-Alpes:Bourg-en-Bresse"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Saint-Priest")] <- "Europe:France:Auvergne-Rhone-Alpes:Saint-Priest"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:Vienne")] <- "Europe:France:Auvergne-Rhone-Alpes:Vienne"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne-RhÃ´ne-Alpes:-")] <- "Europe:France:Auvergne-Rhone-Alpes:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Auvergne Rhone Alpes:Contamines")] <- "Europe:France:Auvergne-Rhone-Alpes:Contamines"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Hauts de France:CrÃ©py en Valois")] <- "Europe:France:Hauts de France:Crepy en Valois"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Poland:Malopolskie:KrakÃ³w")] <- "Europe:Poland:Malopolskie:Krakow"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Belgium:Braine-l alleud:-")] <- "Europe:Belgium:Braine-l’Alleud:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Switzerland:GraubÃ¼nden:-")] <- "Europe:Switzerland:Graubunden:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Turkey:AdÄ±yaman:-")] <-"Europe:Turkey:Adiyaman:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Bourgogne-France-ComtÃ©:Montreux-Chateau")] <- "Europe:France:Bourgogne-Franche-Comte:Montreux-Chateau"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Bourgogne-France-ComtÃ©:Thise")] <- "Europe:France:Bourgogne-Franche-Comte:Thise"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Bourgogne-France-ComtÃ©:Chalon sur Saone")] <- "Europe:France:Bourgogne-Franche-Comte:Chalon sur Saone"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Hauts de France:CompiÃ¨gne")] <- "Europe:France:Hauts de France:Compiegne"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Ile de France:Meudon la ForÃªt")] <- "Europe:France:Ile de France:Meudon la Foret"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:France:Ile de France:AsniÃ¨res-sur-Seine")] <- "Europe:France:Ile de France:Asnieres-sur-Seine"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Sweden:SmÃ¥land:JÃ¶nkÃ¶ping")] <- "Europe:Sweden:Smaland:Jonkoping"
gisaid_geo_fixed[which(gisaid_geo_fixed=="South America:Colombia:Norte de santander:CÃºcuta")] <- "South America:Colombia:Norte de santander:Cucuta"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Africa:Egypt:Egypt:-")] <- "Africa:Egypt:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Africa:Uganda:Uganda:-")] <- "Africa:Uganda:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Africa:Democratic Republic of the Congo:Democratic Republic of the Congo:-")] <- "Africa:Democratic Republic of the Congo:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="South America:Argentina:Argentina:-")] <- "South America:Argentina:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="South America:Colombia:Colombia:-")] <- "South America:Colombia:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:China:China:-")] <- "Asia:China:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Japan:Japan:-")] <-"Asia:Japan:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Luxembourg:Luxembourg:-")] <- "Europe:Luxembourg:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Latvia:Latvia:-")] <- "Europe:Latvia:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Nepal:Kathmandu:Kathmandu")] <- "Asia:Nepal:Kathmandu:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Oceania:Australia:Northern territory:-")] <-"Oceania:Australia:Northern Territory:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Malaysia:Malaysia:-")] <-"Asia:Malaysia:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Portugal:Portugal:-")] <-"Europe:Portugal:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:South Korea:South Korea:-")] <- "Asia:South Korea:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Italy:Italy:-")] <- "Europe:Italy:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Taiwan:Taiwan:-")] <- "Asia:Taiwan:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Germany:Germany:-")] <- "Europe:Germany:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Thailand:Thailand:-")] <- "Asia:Thailand:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:Iran:Iran:-")] <- "Asia:Iran:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Netherlands:Netherlands:-")] <- "Europe:Netherlands:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="North America:USA:Ohio:Columbus OH")] <- "North America:USA:Ohio:Columbus"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Asia:China:Beijing:Beijing")] <- "Asia:China:Beijing:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Denmark:Denmark:-")] <- "Europe:Denmark:-:-"
gisaid_geo_fixed[which(gisaid_geo_fixed=="Europe:Netherlands:Utrecht:Utrecht")] <- "Europe:Netherlands:Utrecht:-"


#
#


write.table(gisaid_geo_fixed, "/Users/claudiavasallovega/repolab/work/virusbeacon/gisaid_geo_fixed.txt", row.names = F, col.names = F)



# illumina
il <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/illum_geo.txt")$V1)
ilf <-  as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/illum_geo_fixed.txt")$V1)
length(il) #  40
length(ilf) #  40


# ont
o <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/ont_geo.txt")$V1)
of <-  as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/ont_geo_fixed.txt")$V1)
length(o) #   103 .. 104 counting Denkmarkt as possibility for Sabela extraction
length(of) #   103.. 104


# sra
s <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_geo.txt")$V1)
sf <-  as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_geo_fixed.txt")$V1)
length(s) #  128
length(sf) #  128


# gisaid
g <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/gisaid_geo.txt")$V1)
gf <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/gisaid_geo_fixed.txt")$V1)
length(g) # 1067
length(gf) # 1067


geo <- as.character(c(il, o, s, g))
write.table(geo, "/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/geo_loc_name.txt", row.names = F, col.names = F)
geof <- as.character(c(ilf, of, sf, gf))
write.table(geof, "/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/geo_loc_name_fixed.txt", row.names = F, col.names = F)

geo <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/geo_loc_name.txt")$V1)
geof <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/geo_loc_name_fixed.txt")$V1)
 
length(geo) # 1338 ..1339
length(geof) # 1338..1339




# Biosample type
b <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/biosample_type.txt")$V1)
bf <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/biosample_type_fixed.txt")$V1)
length(b ) # 24
length(bf) # 24


# Host age
a <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_age.txt")$V1)
af <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_age_fixed.txt")$V1)
length(a ) # 4
length(af) # 4


# Host sex
se <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_sex.txt")$V1)
sef <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_sex_fixed.txt")$V1)
length(se) # 6
length(sef) # 6

# Host sex
se <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_sex.txt")$V1)
sef <- as.character(read.table("/Users/claudiavasallovega/repolab/work/virusbeacon/harmonization/host_sex_fixed.txt")$V1)
length(se) # 6
length(sef) # 6



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
       sapply(gisaid_xml_list, function(xml){
               attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$sample_name
                }) )) 

identical(pool_sample_name , sample_alias)

pool_spots <-  unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$spots
        }))) 

pool_bases <-  unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$bases
        }))) 

pool_tax_id <-  unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$tax_id
        }))) 

pool_organism <-  unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$Pool$Member)$organism
        }))) 

##################   "RUN_SET"   ##########################################################################################
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # names "RUN"
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET) # "RUN"

# "RUN"

run_id <-  unlist(
        sapply(gisaid_xml_list, function(xml){
                xml$html$body$experiment_package_set$experiment_package$run_set$run$identifiers$primary_id[[1]]
})
)
length(run_id) # ]  34082
length(unique(run_id)) # ]  34082



# names
names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN) 
# "IDENTIFIERS"    "EXPERIMENT_REF"
# "Pool"           "gisaidFiles"      
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
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_spots # "476632"
        })) 

run_bases <- unlist(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$total_bases # "143565674"
        })) 

run_size <- unlist(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        })) 

run_gisaid_files <- unlist(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN)$size # "72426963"
        }))

# CloudFiles filetype
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$CloudFiles$CloudFile)$filetype # fastq


# Statistics

# n reads
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads

run_nreads <- unlist(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nreads # "2"        "variable"
        })
        ) 

# nspots
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)

run_nspots <- unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics)$nspots # (same as spots)
        })
)) 

## read
# index
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0

run_read_index <- unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$index # 0
        })
)) 
#count
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
run_read_count <- unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$count #  # (same as spots)
        })
)) 
#average
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average # "150.55"
run_read_ave <- unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$average
        })
)) 
#stdev
attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev # "0.74"
run_read_st <- unlist(unique(
        sapply(gisaid_xml_list, function(xml){
                attributes(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$RUN_SET$RUN$Statistics$Read)$stdev
        }))) 
