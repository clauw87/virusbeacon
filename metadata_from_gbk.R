# Getting metadata from GBK file
library(readr)
library(dplyr)
library(stringr)
library(Hmisc)


# Genebank GBK files

#gbk <- read_file("~/repolab/work/virusbeacon/genbank_fasta_xmls/MT019529.1.gbk")

# gbk <- read_delim("~/repolab/work/virusbeacon/genbank_fasta_xmls/gbk/MT019529.1.gbk",
#                   delim= "\n", col_names = F)$X1
# gbk <- read_delim("~/repolab/work/virusbeacon/genbank_fasta_xmls/gbk/MT293164.1.gbk",
#                   delim= "\n", col_names = F)$X1

folder <-  "~/repolab/work/virusbeacon/genbank_fasta_xmls/gbk"
length(list.files(folder))
all_files_names <- paste(folder, list.files(folder), sep = "/")

gbk_list <-  lapply(all_files_names, function(e){
        read_delim(e, delim= "\n", col_names = F)$X1 
})

length(gbk_list)                   

##### METADATA TO EXTRACT

#/organism="Severe acute respiratory syndrome coronavirus 2"
#/mol_type="genomic RNA"
#/isolate="BetaCoV/Wuhan/IPBCAMS-WH-01/2019"
#/isolation_source="bronchoalveolar lavage fluid"
#/host="Homo sapiens; male; age 65"
#/db_xref="taxon:2697049"
#/country="China: Hubei, Wuhan"
#/lat_lon="30.59 N 114.3 E"
#/collection_date="23-Dec-2019"
#/collected_by="Institute of Pathogen Biology, Chinese
#Academy of Medical Sciences & Peking Union Medical
#College"
#/note="host_disease: nCov Pneumonia"

#f <- gbk_list[[1]]

mol_type <- sapply(gbk_list, function(f){
        gsub("\"", "", str_match(f[str_detect(f, "mol_type")], "mol_type=(.*?)$")[2])
}) # for librarySource

length <- sapply(gbk_list, function(f){
        str_split(f[str_detect(f, "LOCUS")], pattern = " ")[[1]][23]
}) # for completeness for librarySource

accession <- sapply(gbk_list, function(f){
        str_split(f[str_detect(f, "ACCESSION")], pattern = " ")[[1]][4]
}) # for "run primary id"

version <- sapply(gbk_list, function(f){
        str_split(f[str_detect(f, "VERSION")], pattern = " ")[[1]][6]
})

#organism <- gsub("\"", "", x = str_match(gbk[str_detect(gbk, "organism")], "organism=(.*?)$")[2]) # names doesn't get the 2
organism <- sapply(gbk_list, function(f){
        gsub("\"", "", x = str_match(f[str_detect(f, "SOURCE")], "SOURCE      (.*?)$")[2])
}) # "Severe acute respiratory syndrome coronavirus 2 (SARS-CoV2)"

country <- sapply(gbk_list, function(f){
        gsub("\"", "", x=str_match(f[str_detect(f, "country")], "country=(.*?)$")[2])
})

assembly_method <- sapply(gbk_list, function(f){
        str_match(f[str_detect(f, "Assembly Method")], ":: (.*?)$")[2] # "Megahit v. v.1.2.8"
})

sequencing_platform <- sapply(gbk_list, function(f){
        str_match(f[str_detect(f, "Sequencing Technology")],":: (.*?)$")[2] #  "Illumian NextSeq 500"
})

collection_date <- sapply(gbk_list, function(f){
        gsub("\"", "", str_match(f[str_detect(f, "collection")], "collection_date=(.*?)$")[2]) #collection-date
})

taxon_id <- sapply(gbk_list, function(f){
        gsub("taxon:", "", gsub("\"", "", str_match(f[str_detect(f, "taxon")], "db_xref=(.*?)$")[2])) # taxon
})

isolate <- sapply(gbk_list, function(f){
        gsub("\"", "", str_match(f[str_detect(f, "isolate=")], "isolate=(.*?)$")[2]) # isolate
})

isolation_source <- sapply(gbk_list, function(f){
        gsub("\"", "", str_match(f[str_detect(f, "isolation")], "isolation_source=(.*?)$")[2]) # isolate
})

host_species <- sapply(gbk_list, function(f){
        str_split(gsub("\"", "", str_match(f[str_detect(f, "host=")], "host=(.*?)$")[2]), pattern = ";")[[1]][1]
})

host_sex <- sapply(gbk_list, function(f){
        gsub(" ", "", str_split(gsub("\"", "", str_match(f[str_detect(f, "host=")], "host=(.*?)$")[2]), pattern = ";")[[1]][2])
})

host_age <- sapply(gbk_list, function(f){
        gsub(" age ", "", str_split(gsub("\"", "", str_match(f[str_detect(f, "host=")], "host=(.*?)$")[2]), pattern = ";")[[1]][3])
})

host_disease <-  sapply(gbk_list, function(f){
        gsub("^ ", "", gsub("\"", "", 
                str_match(f[str_detect(f, "host_disease")], "host_disease:(.*?)$")[2]))
})




