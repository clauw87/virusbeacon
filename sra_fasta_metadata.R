library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)


folder <-  "~/repolab/work/virusbeacon/sra_fasta_xmls"
length(list.files(folder))
all_files <- paste(folder, list.files(folder), sep = "/")
xml_list <-  lapply(all_files, function(e){
        xml2::as_list(xml2::read_xml(e)) 
})
length(xml_list)                   
xml <- xml_list[[1]]

# structure xml
attributes(names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`)) 
#NULL
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`)
# [1] "Bioseq-set_class"  
# [2] "Bioseq-set_descr"  
# [3] "Bioseq-set_seq-set"
# [4] "Bioseq-set_annot"


# [1] "Bioseq-set_class"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_class`
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_class`)[1] # nuc-prot
#"nuc-prot"


# [2] "Bioseq-set_descr"  
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`) # $names "Seq-descr"
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`) # "Seq-descr"

# Descr "Bioseq-set_descr"  `Seq-descr`$Seqdesc$Seqdesc_source$BioSource
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource)
# $names
# [1] "BioSource_genome" 
# [2] "BioSource_org"    
# [3] "BioSource_subtype"

# BioSource_genome
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_genome
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_genome)
# value  "genomic"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_genome[[1]]
# 1
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_genome)$value
# "genomic" > LIBRARY SOURCE

# "BioSource_org"  
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org)
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org)# "Org-ref"
# Org-ref
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`
)
# [1] "Org-ref_taxname"
# [2] "Org-ref_db"     
# [3] "Org-ref_orgname"

# "Org-ref_taxname"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_taxname`[[1]]
# "Severe acute respiratory syndrome coronavirus 2"

#  "Org-ref_db"  TAXON ID
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_db`$Dbtag$Dbtag_tag$`Object-id`$`Object-id_id`[[1]]
# "2697049"

#  "Org-ref_orgname"
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`)
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`)
# OrgName
length(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_mod)
# for each
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName)
# $names
# [1] "OrgName_name"   
# [2] "OrgName_attrib" 
# [3] "OrgName_mod"    
# [4] "OrgName_lineage"
# [5] "OrgName_gcode"  
# [6] "OrgName_div"

# "OrgName_name" 
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_name$OrgName_name_virus[[1]]
# "Severe acute respiratory syndrome coronavirus 2" # same info as in "Org-ref_taxname"

# "OrgName_attrib"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_attrib[[1]]
#  "specified" 

#  "OrgName_mod" > $OrgMod_subtype
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_mod$OrgMod$OrgMod_subtype)
#  "isolate"

#  "OrgName_mod" > $OrgMod_subname > ISOLATE NAME
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_mod$OrgMod$OrgMod_subname[[1]]
# "SARS-CoV-2/mink/NLD/1/2020"

# "OrgName_lineage"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_lineage[[1]]
#  "Viruses; Riboviria; Nidovirales; Cornidovirineae; Coronaviridae; Orthocoronavirinae; Betacoronavirus; Sarbecovirus"

# "OrgName_gcode" 
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_gcode[[1]]
# "1"

# "OrgName_div"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_org$`Org-ref`$`Org-ref_orgname`$OrgName$OrgName_div[[1]]
#  "VRL"


# "BioSource_subtype" SAMPLE ATTRIBUTES
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype)
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype
sample_attributes_tags <- #unique(unlist(
                sapply(xml_list, function(xml){
                        sapply(1:length(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype), function(a){
                                attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype[[a]]$SubSource_subtype)
                                
                        })    
                        
                        
                })
        #))
sample_attributes_tags_values <- #unique(unlist(
        sapply(xml_list, function(xml){
                sapply(1:length(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype), function(a){
                        xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_descr`$`Seq-descr`$Seqdesc$Seqdesc_source$BioSource$BioSource_subtype[[a]]$SubSource_name
                        
                })    
                
                
        })
#))
unique(sample_attributes_tags_values)



# [3] "Bioseq-set_seq-set"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_seq`$Bioseq)
#  "Bioseq_id"    "Bioseq_descr" "Bioseq_inst"  "Bioseq_annot"

# Bioseq_id
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_seq`$Bioseq$Bioseq_id

# Textseq id accession
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_seq`$Bioseq$Bioseq_id$`Seq-id`$`Seq-id_genbank`$`Textseq-id`$`Textseq-id_accession`[[1]]
#  "MT396266"
# Textseq id version
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_seq`$Bioseq$Bioseq_id$`Seq-id`$`Seq-id_genbank`$`Textseq-id`$`Textseq-id_version`[[1]]
# 1
# Seq id gi
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_seq`$Bioseq$Bioseq_id[2]$`Seq-id`$`Seq-id_gi`[[1]]
# "1835438410"

###### ANNOTATION ####
# [4] "Bioseq-set_annot"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`)
# Seq feat
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`
names(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`)
# [1] "Seq-feat_id"          "Seq-feat_data"        "Seq-feat_except"     
# [4] "Seq-feat_product"     "Seq-feat_location"    "Seq-feat_except-text"

# "Seq-feat_id" 
# Object id
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_id`$`Feat-id`$`Feat-id_local`$`Object-id`$`Object-id_id`[[1]]


# "Seq-feat_data" 
# cd region
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_data`$SeqFeatData$SeqFeatData_cdregion$Cdregion
# cd region frame
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_data`$SeqFeatData$SeqFeatData_cdregion$Cdregion$Cdregion_frame)
# "one"

# cd region code
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_data`$SeqFeatData$SeqFeatData_cdregion$Cdregion$Cdregion_code)
# "Genetic-code"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_data`$SeqFeatData$SeqFeatData_cdregion$Cdregion$Cdregion_code$`Genetic-code`$`Genetic-code_E`$`Genetic-code_E_id`[[1]]
# "1"

# "Seq-feat_except"   
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_data`$SeqFeatData$SeqFeatData_cdregion$Cdregion
# true

# "Seq-feat_product" 
# seq-id_gi
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_product`$`Seq-loc`$`Seq-loc_whole`$`Seq-id`$`Seq-id_gi`[[1]]

#   "Seq-feat_location"  
# interval from
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_location`$`Seq-loc`$`Seq-loc_mix`$`Seq-loc-mix`$`Seq-loc`$`Seq-loc_int`$`Seq-interval`$`Seq-interval_from`[[1]]
# 255
# interval from
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_location`$`Seq-loc`$`Seq-loc_mix`$`Seq-loc-mix`$`Seq-loc`$`Seq-loc_int`$`Seq-interval`$`Seq-interval_to`[[1]]
# 13457
# strand
attributes(xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_location`$`Seq-loc`$`Seq-loc_mix`$`Seq-loc-mix`$`Seq-loc`$`Seq-loc_int`$`Seq-interval`$`Seq-interval_strand`$`Na-strand`)
#plus
# seq interval id
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_location`$`Seq-loc`$`Seq-loc_mix`$`Seq-loc-mix`$`Seq-loc`$`Seq-loc_int`$`Seq-interval`$`Seq-interval_id`$`Seq-id`$`Seq-id_gi`[[1]]
# "1835438410"


# "Seq-feat_except-text"
xml$`Bioseq-set`$`Bioseq-set_seq-set`$`Seq-entry`$`Seq-entry_set`$`Bioseq-set`$`Bioseq-set_annot`$`Seq-annot`$`Seq-annot_data`$`Seq-annot_data_ftable`$`Seq-feat`$`Seq-feat_except-text`[[1]]
# ribosomal slippage




