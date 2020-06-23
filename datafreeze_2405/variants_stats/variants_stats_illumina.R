rm(list=ls())

library(data.table)
library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)
library(Hmisc)
library(magrittr)
library(cowplot)
library(ggbubr)



source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/ggplot2_charts.R")
source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/plot_pies.R")
source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_query.R")


setwd( "/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/data")


## All variants
# resources_folders <-list.files("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/data")
# SRA Illumina- Galaxy
# f <- "sra_illumina"
# variant_file <- paste0(f, "/1497.annot.variants.galaxy_freq.data")
# var <- fread(input = variant_file, sep = ";")
# var$variant <- sapply(1:nrow(var), function(v){
#         paste(as.character(var$position[v]),var$reference[v],  var$alternate[v], sep = ">")
# })
# 
# var$VarCount <- unlist(lapply(var$variant, function(v){
#         sum(filter(var, variant==v)$sampleMatchingCount)
# }))
# 
# var$PosCount <- unlist(lapply(var$position, function(p){
#         sum(filter(var, position==p)$sampleMatchingCount)
# }))
# 
# var$resource <- rep(f, nrow(var))
# 
# # Not anymore, frequency is already calculated by unique variant
# # var$MyFequency <- unlist(lapply(unique(var$variant), function(v){
# #         sum(filter(var, variant==v)$frequency)
# # }))
# 
# assign(value=var, x=paste0(as.character(f), "_var"))
# 
# save(sra_illumina_var, file = "sra_illumina_freq_var.rda")
# 


# #### Load variants file
# 
# load("sra_illumina_freq_var.rda")
# var <- sra_illumina_var
# 
# ###
# length(var$variant) # 51006
# length(unique(var$variant)) # 46359
# 
# 
# duvar <- var[which(duplicated(var$variant)),]
# length(unique(duvar$variant)) # 4533
# 
# 
# uvar <- var[which(!(duplicated(var$variant))),]
# length(unique(uvar$variant)) # 46359
# 
# 
# uvar$class <- rep(NA, nrow(uvar))
# uvar$class <- ifelse (uvar$Functional_Class=="MISSENSE"|uvar$Functional_Class=="NONSENSE", "NS", uvar$class)
# uvar$class <- ifelse (uvar$Functional_Class=="SILENT", "S", uvar$class)
# uvar$class <- ifelse (uvar$Functional_Class=="", "NC", uvar$class)
# 
# save(uvar, file="sra_illumina_uvar.rda")
# ######## Load af
# 
# af <- fread("af_galaxy")
# nrow(af) # 693706
# 
# af$variant <- sapply(1:nrow(af), function(v){
#         paste(as.character(af$Pos[v]),af$REF[v], af$ALT[v], 
#               sep = ">")
# })
# af$sampleMatchingCount <- sapply(1:nrow(af), function(v){
#         filter(uvar, variant==af$variant[v])$VarCount
# })
# af$class <- sapply(1:nrow(af), function(v){
#         filter(uvar, variant==af$variant[v])$class
# })
# ###################
# 
# length(unique(af$variant)) # 46359
# 
# save(af, file = "sra_illumina_af.rda")
# 
# 



# ############## STATS ########################################################

load("sra_illumina_uvar.rda")
load("sra_illumina_af.rda")



####### Positions --------------------------------------------------------------

# # Positions with variants
length(unique(uvar$position))
# 26366        

#29906- 26366 = 3540 positions without variants


# # Proportion of # Positions with variants
length(unique(uvar$position))/29906 *100
# # 88.16291




# snp alternates per position
illum_alt_per_pos <- sapply(unique(uvar$position), function(p) {
        unique(filter(uvar, position==p)$alternate)
})
length(illum_alt_per_pos) # 26366

num_alt_per_pos <- sapply(illum_alt_per_pos, length)

   3540   /29906*100
sum(num_alt_per_pos==1)/29906*100 # 11135
sum(num_alt_per_pos==2)#/29906*100 # 10469
sum(num_alt_per_pos==3)/29906*100 # 4762


###### Variants ###############################################################

# Number of variants
# Unique variants
length(var$variant) # 46359
length(unique(var$variant)) # 46459


nrow(var)
# 51006

nrow(uvar)
# 46359

# Variant Type
unique(var$svType) # "SNP"   



#----------#----------#----------#----------#----------#----------#----------#----------#----------#
## PER POSITION VARIABILITY
# Total unique variants per position
var_data <- filter(uvar, svType=="SNP")
title <- "NUM VARIANTS (ALT) PER POSITION"
var_data$numvar <- sapply(var_data$position, function(p) {
        length(unique(filter(var_data, position==p, alternate %in% c("A", "T", "C", "G"))$alternate))
})

vpp <- ggplot(var_data,
              #aes(x=Var1, ymax=Freq, ymin=0)) + 
              aes(x=position, ymax=numvar, ymin=0, color=class)) + 
        # geom_linerange(color="orangered2") +
        geom_linerange() +
        scale_color_manual(values = c("green","orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.position = "top") +
        theme(legend.title = element_blank()) +
        
        labs(x="Genomic Position") +
        labs(y="Number of variants") 
vpp
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/needle_vpp_ill.pdf",  height =5.83,  width =8.27)
vpp
dev.off()


#----------#----------#----------#----------#----------#----------#----------#----------#----------#
## SAMPLE MATCHING COUNT

# Frequency
min(uvar$sampleMatchingCount) # 1
min(uvar$PosCount) # 1
min(uvar$VarCount) # 1
max(uvar$sampleMatchingCount) # 795
max(uvar$PosCount) # 801
max(uvar$VarCount) # 795


title <- "RUNS WITH VARIANTS PER POSITION"
# dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
#dat <- data.frame(pos=var_data$position, freq=var_data$dbfreq, class=var_data$Functional_Class, stringsAsFactors = F)
#dat <- data.frame(pos=uvar$position, freq=uvar$PosCount/1497, 
#                  class=uvar$Functional_Class, stringsAsFactors = F)
# dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
# dat$class[which(dat$class=="SILENT")] <- "S"
# dat$class[which(dat$class=="")] <- "NC"
dat <- data.frame(pos=uvar$position, freq=uvar$PosCount/1497, 
                  class=uvar$class, stringsAsFactors = F)


smc_needle <- ggplot(dat, 
                 aes(x=pos, ymax=freq, ymin=0, color=class)) +
        #geom_linerange() + #color="orangered2"
        #aes(x=pos, ymax=freq, ymin=0, ) +
        #geom_linerange(position = position_dodge()) +
        geom_linerange(stat="identity", position = "dodge") +
        scale_color_manual(values = c("green","orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        #theme(title = element_text(size = 3)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Genomic Position") +
        theme(legend.title = element_blank())+
        theme(legend.position = "top") +
        labs(y="Freq") 
smc_needle

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_smc_needle.pdf",
    height =5.83,  width =8.27)
smc_needle
dev.off()



############################################################################


# General statistics graphs


#############################################################################


#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# Total unique variants per frequency group
title <- "VARIANTS PER FREQUENCY GROUP"
# FREQUENCY OF MATCHING RUNS 
# Var1 <- uvar$MyFrequency
# hist(uvar$MyFrequency, breaks=c(0, 0.001,0.01,0.1, 0.6))
# hist( uvar$MyFrequency,breaks=1000 )
# unique(Var1)
uvar$FreqGroup <- rep(NA, nrow(uvar))
uvar$FreqGroup <- ifelse(uvar$VarCount/1497 <=0.001, "<0.001", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$VarCount/1497 >0.001 & uvar$VarCount/1497 <=0.01, "0.001-0.01", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$VarCount/1497 >0.01 & uvar$VarCount/1497 <=0.1, "0.01-0.1", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$VarCount/1497 >0.1 & uvar$VarCount/1497 <=0.5, "0.1-0.5", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$VarCount/1497 >0.5 ,">0.5", uvar$FreqGroup) 
# Num variants per frequency group
# uvar$class <- rep(NA, nrow(uvar))
# uvar$class <- ifelse (uvar$Functional_Class=="MISSENSE"|uvar$Functional_Class=="NONSENSE", "NS", uvar$class)
# uvar$class <- ifelse (uvar$Functional_Class=="SILENT", "S", uvar$class)
# uvar$class <- ifelse (uvar$Functional_Class=="", "NC", uvar$class)
uvar_ct <- uvar %>% 
        group_by(class, FreqGroup) %>%
        summarise(count=n())
vf <- uvar_ct
vf$FreqGroup <- as.factor(vf$FreqGroup)
vf <- mutate(vf, FreqGroup=fct_relevel(FreqGroup,
                                       "<0.001", "0.001-0.01", "0.01-0.1","0.1-0.5", ">0.5"
))
sumlabel <- vf %>%
        group_by(FreqGroup) %>%
        summarise(total=sum(count))

colors <- c("green", "orangered2", "blue")
vfp  <- ggplot(vf, aes(x=FreqGroup, y=count, fill=class)) +
        geom_bar(position='stack', stat='identity') +
        #geom_bar(position='stack', stat='identity') +
        ggtitle(title) +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Count of variants in dataset") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=colors) +
        geom_text(aes(FreqGroup, total+500, label=total, fill=NULL), data=sumlabel, size=3) +
                  #position = position_dodge(vjus, size=3) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
vfp
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/vars_per_fg_illumina.pdf",  
    height =4,  width =4)
vfp
dev.off()

#----------#----------#----------#----------#----------#----------#----------#----------#----------#
library(ggbubr)
source("plot_pies.R")
#plotie()

# Create a grid for all pies of for each category for 4 different resources
ggpubr::ggarrange(fcp_bp, 
                  fcp_bp,
                  fcp_bp,
                  fcp_bp,
                  labels="AUTO",
                  common.legend=T,
                  align="hv"
                  )



############################################################################


# Variants annotation


#############################################################################

source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_query.R")


########

# Region class
unique(uvar$Gene_Coding) # "" (this is noncoding)      "CODING"   

# Functional class
unique(uvar$Functional_Class) # ""  (this is noncoding)       "MISSENSE" "NONSENSE" "SILENT"  

# "Amino_Acid_Change"  
length(unique(uvar$Amino_Acid_Change)) # 27556 distinct aminoacid changes across samples


#"Gene_Name" 
unique(uvar$Gene_Name) # ""  (this is noncoding) : all genes     "orf1ab" "S"      "ORF3a"  "E"      "M"      "ORF6"   "ORF7a"  "ORF7b"  "ORF8"   "N"      "ORF10" 

# unique(uvar$Genotype)


#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# Graph Number of mutations per genomic region  -------------------------------------------------------------------------

# Genes

# Filter on the fly

genomic_regions <-  genes_table$gene_name
# number of different variants
# genomic_regions <- c(utrs_table$utr_name, 
#                      #stem_loops_table$stem_loop_name,
#                      genes_table$gene_name
#                      #unique(proteins_table$protein_name)
# )

n <- sapply(genomic_regions, function(q) {
        #nrow(filter(uvar, position %in% region_filter(region_id = q)))
length(unique(filter(uvar, position %in% region_filter(region_id = q))$variant))
})
# FIXME: making stack bars for S vs NS
s <-  sapply(genomic_regions, function(q) {
        length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="SILENT")$variant))
})

ns <-  sapply(genomic_regions, function(q) {
        length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE")$variant))
})

g_dat <- data.frame(reg=genomic_regions, count=n, s_count =s, ns_count=ns)

g_dat <-  mutate(g_dat, reg=fct_relevel(reg,   
                                    #"5'UTR", "ORF1ab" , 
                                    #"fsSE SL1" ,  "fsSE SL2" , 
                                    "ORF1ab",
                                    "S", "ORF3a", "E", "M", "ORF6", "ORF7a", 
                                    "ORF7b", "ORF8", "N", "ORF10"  
                                    #"3utr ps SL1", "3utr ps SL2", "s2m" ,
                                    #"3'UTR" 
                                    ))


vgenes <- ggplot(g_dat, aes(reg, count)) +
        #geom_col(fill=class) +
        geom_col(aes(reg, count, fill="blue")) +
        geom_col(aes(reg, s_count, fill="orangered2")) + # "honeydew3"
        theme_minimal() +
        ggtitle("VARIANTS PER GENE") +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=c("orangered2", "blue"), labels=c("NS", "S")) +
        geom_text(label=g_dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vgenes
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_gene_illumina.pdf",  
    height =5.83,  width =8.27)
vgenes
dev.off()


#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# Graph Number of mutations per genomic region  -------------------------------------------------------------------------
# Proteins - CDS + mat proteins

# Filter on the fly

genomic_regions <-  as.character(mature_proteins_table$protein_name)
genomic_regions <- genomic_regions[-c(1,2)] # remove polyproteins

# Number of variants per region - Codig region : CDS+ mature proteins
n <- sapply(genomic_regions, function(q) {
        #nrow(filter(uvar, position %in% region_filter(region_id = q)))
        length(unique(filter(uvar, position %in% region_filter(region_id = q))$variant))
})

# FIXME: making stack bars for S vs NS
s <-  sapply(genomic_regions, function(q) {
   length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="SILENT")$variant))
})
 
ns <-  sapply(genomic_regions, function(q) {
        length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE")$variant))
})

aa <-   sapply(genomic_regions, function(q) {
        length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE", Amino_Acid_Change!="")$Amino_Acid_Change
          ))
})

# Skip for stacked chart
dat <- data.frame(reg=genomic_regions, count=n, ns_count=ns, s_count=s, aa_changes=aa)
# change names for abbrev
dat$reg <- as.character(dat$reg)
dat$reg[1] <- "leader"
dat$reg[5] <- "3C-like"
dat$reg[12] <- "RdRp"
dat$reg[14] <- "3'-5' exoN"
dat$reg[15] <- "endoRNAse"
dat$reg[16] <- "2'-o-MT"
dat$reg[17] <- "Spike"
dat$reg[18] <- "ORF3a protein"
dat$reg[19] <- "envelop"
dat$reg[20] <- "membrane"
dat$reg[21] <- "ORF6 protein"
dat$reg[22] <- "ORF7a protein"
dat$reg[23] <- "ORF7b"
dat$reg[24] <- "ORF8 protein"
dat$reg[25] <- "nucleocapsid"
dat$reg[26] <- "ORF10 protein"
#rownames(dat) <- names(t)
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "leader", 
                                    #"pp1a" , "pp1ab",
                                    "nsp2", "nsp3", "nsp4", "3C-like", 
                                    "nsp6", "nsp7","nsp8", "nsp9", "nsp10",
                                    "nsp11", "RdRp",  "helicase",
                                    "3'-5' exoN", "endoRNAse", "2'-o-MT",
                                    "Spike", "ORF3a protein", "envelop", "membrane",
                                    "ORF6 protein", "ORF7a protein", "ORF7b", "ORF8 protein",
                                    "nucleocapsid", "ORF10 protein"
))

dat 
vprot <- ggplot(dat, aes(reg, count)) +
        geom_col(aes(x=reg, y=count, fill="blue")) +
        geom_col(aes(x=reg, y=s_count, fill="orangered2")) +
        #geom_col(aes(reg, aa_changes), fill="dodgerblue4") +
        theme_minimal() +
        ggtitle("VARIANTS PER MATURE PROTEINS") +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=c("orangered2", "blue"), labels=c("NS", "S")) +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vprot
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein_illumina.pdf",  height =5.83,  width =8.27)
vprot
dev.off()


# Total unique aminoacid changes per region
aap <- ggplot(dat, aes(reg, aa_changes)) +
        geom_col(aes(reg, aa_changes), fill="dodgerblue") +
        theme_minimal() +
        ggtitle("AA CHANGES PER MATURE PROTEINS") +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique aa changes") +
        theme(legend.title = element_blank()) +
        geom_text(label=dat$aa_changes, vjust=-0.1, size=2.5, colour = "red") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
aap
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/aa_per_protein_illumina.pdf",  height =5.83,  width =8.27)
aap
dev.off()



#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# FIXED
# S, NS stacked with legend

dat <- data.frame(reg=genomic_regions, NS=ns, S=s)


# change names for abbrev
dat$reg <- as.character(dat$reg)
dat$reg[1] <- "leader"
dat$reg[5] <- "3C-like"
dat$reg[12] <- "RdRp"
dat$reg[14] <- "3'-5' exoN"
dat$reg[15] <- "endoRNAse"
dat$reg[16] <- "2'-o-MT"
dat$reg[17] <- "Spike"
dat$reg[18] <- "ORF3a protein"
dat$reg[19] <- "envelop"
dat$reg[20] <- "membrane"
dat$reg[21] <- "ORF6 protein"
dat$reg[22] <- "ORF7a protein"
dat$reg[23] <- "ORF7b"
dat$reg[24] <- "ORF8 protein"
dat$reg[25] <- "nucleocapsid"
dat$reg[26] <- "ORF10 protein"
#rownames(dat) <- names(t)
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "leader", 
                                    #"pp1a" , "pp1ab",
                                    "nsp2", "nsp3", "nsp4", "3C-like", 
                                    "nsp6", "nsp7","nsp8", "nsp9", "nsp10",
                                    "nsp11", "RdRp",  "helicase",
                                    "3'-5' exoN", "endoRNAse", "2'-o-MT",
                                    "Spike", "ORF3a protein", "envelop", "membrane",
                                    "ORF6 protein", "ORF7a protein", "ORF7b", "ORF8 protein",
                                    "nucleocapsid", "ORF10 protein"
))

# dat <- as_tibble(dat)
dat <-gather(dat, "class", "count", -reg)

vprot_stacked <- ggplot(dat, aes(fill=class, y=count, x=reg))+
        geom_bar(position = "stack", stat = "identity" ) +
        theme_minimal() +
        ggtitle("VARIANTS PER MATURE PROTEINS") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
   
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = c("orangered2", "blue")) +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))

vprot_stacked
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein.pdf",  height =5.83,  width =8.27)
vprot_stacked
dev.off()





#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# Non-coding regions------------------------------------------------------------------------------

# CODING
c_positions <- unique(uvar$position[which(uvar$Gene_Coding=="CODING")])
length(c_positions) # 25877 positions

# NON CODING
nc_positions <- unique(uvar$position[which(uvar$Gene_Coding=="")])
length(nc_positions) # 489

genomic_regions <-  utrs_table$utr_name
utr_positions <- sapply(genomic_regions, function(q) {
        unique(filter(uvar, position %in% region_filter(region_id = q))$position)
})
length(unlist(utr_positions)) #  350

sum(unlist(utr_positions) %in% nc_positions) #350

intergenic <- setdiff(nc_positions, unlist(utr_positions))
length(intergenic) # 139

# utrs
genomic_regions <-  utrs_table$utr_name
n1 <- sapply(genomic_regions, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})
# intergenic
genomic_regions2 <-  intergenic 
n2 <- sum(sapply(genomic_regions2, function(q) {
        nrow(filter(uvar, position %in% q))
}))
# coding
genomic_regions3 <-  c_positions
n3 <- sum(sapply(genomic_regions3, function(q) {
        nrow(filter(uvar, position %in% q))
}))

dat1 <- data.frame(reg=genomic_regions, count=n1)
dat2 <- data.frame(reg="intergenic", count=n2)
dat3 <- data.frame(reg="coding", count=n3)

nc_dat<-   rbind(dat1 , dat2)
nc_dat <-  mutate(nc_dat, reg=fct_relevel(reg,   
                                    "5'UTR",
                                    #"coding",
                                    "intergenic",
                                    "3'UTR" 
))
vnc <- ggplot(nc_dat, aes(reg, count)) +
        geom_col(fill="green", position = "dodge") +
        theme_minimal() +
        ggtitle("VARIANTS PER NCR") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=nc_dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vnc


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_nc_illumina.pdf",  
    height =3.83,  width =2.83)
vnc
dev.off()


#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# All genomic regions------------------------------------------------------------------------------
all_dat<-   rbind(dat1 , dat2, dat3)
all_dat <-  mutate(all_dat, reg=fct_relevel(reg,   
                                    "5'UTR",
                                    "coding",
                                    "intergenic",
                                    "3'UTR" 
))

#pie
title <- "VARIANTS PER GENOMIC REGION" 
va_bp <- ggplot(all_dat, aes(reg, count)) +
        aes(x="", y=count, fill=reg) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
va_bp 
va_pie <- va_bp  + 
        coord_polar("y", start=0) +
        ggtitle(title) +
        blank_theme +
        theme(plot.title = element_text(hjust = 0.5, face = "plain", size = 24)) +
        theme(legend.title  = element_blank()) + 
        theme(legend.text = element_text(size = 12)) + 
        scale_fill_manual(values=c("chartreuse", "dodgerblue", "green", "greenyellow")) +
        theme(axis.text.x = element_blank()) 
va_pie 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/var_region_illumina.pdf",  
    height =5.83,  width =8.27)
va_pie 

dev.off()

#----------#----------#----------#----------#----------#----------#----------#----------#----------#
# Functional /STEM LOOPS
stem_loops_table$stem_loop_name <- c("fsSE SL1", "fsSE SL2", "3utr ps SL1", "3utr ps SL2", "s2m")

genomic_regions <- stem_loops_table$stem_loop_name

n <- sapply(genomic_regions, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})
dat<- data.frame(reg=genomic_regions, count=n)

 

dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    #"5'UTR", 
                                    "fsSE SL1" ,  "fsSE SL2" , 
                                    "3utr ps SL1", "3utr ps SL2", "s2m" 
                                    #"3'UTR"
))
vsl <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3") +
        ggtitle("Variants per stem loops") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vsl
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_sl.pdf",  height =5.83,  width =8.27)
vsl
dev.off()







#####################################################################################


# Polymorphic e intrahost


##################################################################

# AF
plot(var$position, var$frequency)
# samples with spm polynorphic
# samples with spm monomorphic

# ## AF IN DATASET -more than one frequency per position if many alternates
title <- "RANGE OF AF OF VARIANTS PER POSITION"
#dat <- data.frame(pos=unique(var$position), meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)
dat <- data.frame(pos=af$Pos, #meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)
                  freq= af$AF, class=af$class)

dat <- mutate(dat, class= fct_relevel(class,   
                                      "NS","S", "NC"))

afg <- ggplot(dat,
              aes(x=pos, y=freq, color=class)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        #geom_linerange(ymin=minfreq, ymax=maxfreq, size=0.1) +
        geom_point(size=0.1)+
        scale_color_manual(values = c("orangered2", "blue", "green")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Genomic Position") +
        labs(y="Allele Frequency")
#         theme(legend.title = element_blank())+
#         labs(y="Freq")
afg
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_afg.pdf",
    height =5.83,  width =8.27)
afg
plot_grid(af, labels=c("A"), label_size = 18)
dev.off()





# MIN VS MAX


# Max and Min AF per position
minfreq <- unlist(lapply(unique(af$Pos), function(p) {
        min(filter(af, Pos==p)$AF)      
}))
#meanfreq <-  unlist(lapply(unique(af$Pos), function(p) {
 #       mean(filter(af, Pos==p)$AF)      
#}))
maxfreq <- unlist(lapply(unique(af$Pos), function(p) {
        max(filter(af, Pos==p)$AF)      
}))

title <- "MAX VS MIN AF IN DATASET PER POSITION"
plot(minfreq, maxfreq)

dat <- data.frame(pos=unique(af$Pos),minfreq=minfreq, maxfreq=maxfreq) 
#  meanfreq=meanfreq, 
mm <- ggplot(dat,
             aes(x=minfreq, y=maxfreq)) +
        geom_point(size=1, color="dodgerblue") +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Min AF in position") +
        labs(y="Max AF in position") +
        theme(legend.title = element_blank())
#mm
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_mm.pdf",
    height =5.83,  width =8.27)
#mm
plot_grid(mm, labels=c("B"), label_size = 18)
dev.off()
length(filter(dat, maxfreq==minfreq)$pos) # 11139
length(filter(dat, maxfreq!=minfreq)$pos) # 15227 
# these ones are positions with more than one variant, 
# i.e, polymorphic in dataset
sum(filter(dat, maxfreq!=minfreq)$pos %in% illum_ppm_sites) # 15227 





# Samples with Variants vs Samples with Variant in Position
# ie not everyone with position has same variant


plot(var$PosCount, var$VarCount)

# VAR VS POS
title <- "VARIANT COUNTS VS POSITION COUNTS PER POSITION"
dat <- data.frame(pos=var$position, run_var=var$VarCount, run_pos=var$PosCount, class=var$Functional_Class, stringsAsFactors = F)# dat <- data.frame(pos=var_data$position, freq=var_data$frequency, class=var_data$Functional_Class, stringsAsFactors = F)
dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
dat$class[which(dat$class=="SILENT")] <- "S"
dat$class[which(dat$class=="")] <- "NC"


mm2 <- ggplot(dat,
              aes(x=run_pos, y=run_var, color=class)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_point(size=1) +  #, color="dodgerblue
        scale_color_manual(values = c("green", "orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Runs matching position") +
        labs(y="Runs matching variant") +
        theme(legend.position = "top") +
        theme(legend.title = element_blank())
mm2
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_mm2.pdf",
    height =5.83,  width =8.27)
#mm2
plot_grid(mm2, labels=c("C"), label_size = 18)
dev.off()
length(filter(var, VarCount!=PosCount)$pos) # 39871
length(unique(filter(var, VarCount!=PosCount)$pos)) # 15231



plot(var$position, var$PosCount)
plot(var$position, var$VarCount)




plot(uvar$VarCount/1497, uvar$frequency)
# where a few samples bear a variant and it is nevertheless  high freq in population..
# wait..what?

# Posible intra hosts, 
# where a lot of samples bear a variant that is nevertheless low freq in dataset
title <- "RUN MATHCING COUNTS VS AF IN RUN"
dat <- data.frame(pos=var$position, run_var=var$VarCount, run_pos=var$PosCount, 
                  freq=var$frequency, class=var$class, stringsAsFactors = F)

mm3 <- ggplot(dat,
              aes(x=run_var/1497, y=freq, color=class)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_point(size=1) +  #, color="dodgerblue
        scale_color_manual(values = c("green", "orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y="AF in run") +
        labs(x="Frequency in dataset (mathing runs)") +
        theme(legend.position = "top") +
        theme(legend.title = element_blank())
#mm3
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_mm3.pdf",
    height =5.83,  width =8.27)
#mm3
plot_grid(mm3, labels=c("D"), label_size = 18)
dev.off()
length(filter(var, VarCount!=PosCount)$pos) # 39871
length(unique(filter(var, VarCount!=PosCount)$pos)) # 15231


######################
######################
#####################


# FIXED ONE: WITH TRUE AF IN RUN
# Posible intra hosts, 
# where a lot of samples bear a variant that is nevertheless low freq in dataset
title <- "AF VS RUN COUNTS IN DATASET"
#dat <- data.frame(pos=var$position, run_var=var$VarCount, run_pos=var$PosCount, af=var$frequency, class=var$Functional_Class, stringsAsFactors = F)# dat <- data.frame(pos=var_data$position, freq=var_data$frequency, class=var_data$Functional_Class, stringsAsFactors = F)
dat <- data.frame(pos=af$Pos, run_var=af$sampleMatchingCount, #run_pos=af$PosCount, 
                  freq=af$AF, class=af$class, stringsAsFactors = F)
# dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
# dat$class[which(dat$class=="SILENT")] <- "S"
# dat$class[which(dat$class=="")] <- "NC"
mm4 <- ggplot(dat,
              aes(x=run_var, y=freq, color=class)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_point(size=0.1) +  #, color="dodgerblue
        scale_color_manual(values = c("green", "orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y="AF in run") +
        labs(x="Number of runs with variant") +
        theme(legend.position = "top") +
        theme(legend.title = element_blank())
mm4

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_mm4.pdf",
    height =5.83,  width =8.27)
plot_grid(mm4, labels=c("D"), label_size = 18)
dev.off()


################################################################################################


################################################################################################


################################################################################################

var <- merge(af, uvar, all = TRUE)



length(unique(filter(var, VarCount<PosCount)$Pos)) # 15077
length(unique(filter(var, VarCount<PosCount)$position)) # 15231

identical(var$position, var$Pos)
setdiff(var$position, var$Pos)


# # Polymorphic sites: sites with multile alleles > 1 alternate position in a sample
# polymorphic in sample:  > 1 alternate position

# polymorphic in dataset / population > Positions have more than one different alternates in dataset
iupac <- setdiff(unique(uvar$alternate), c("A", "T", "C", "G"))

illum_alt_per_pos <- sapply(unique(uvar$position), function(p) {
        unique(filter(uvar, position==p)$alternate)
})
length(illum_alt_per_pos) # 26366


illum_ppm_sites <- unique(filter(var, position %in% which(sapply(
        illum_alt_per_pos, function(a){
        length(a)>1| sum(iupac %in% a)>0
})))$position)

length(illum_ppm_sites) # 13412 

head(illum_ppm_sites) # 100 10000 10001 10003


# Variants in polymorphic sites
length(unique(filter(var, VarCount<PosCount)$variant)) # 35224




#  Polymorphic in samples > positions are polymorphic in at least 1 sample
pmss <- lapply(var$Run, function(r){
        
        unique(var$position)[sapply(unique(var$position), function(p) {
                length(unique(filter(var, Run==r, position==p)$alternate))>1
        })]
})

pmss <- lapply(var$Run[1], function(r){
        
        unique(var$position)[sapply(unique(var$position), function(p) {
                length(unique(filter(var, Run==r, position==p)$alternate))>1
        })]
})


runs <- unique(var$Run)
positions <- illum_ppm_sites
pmss <- lapply(runs, function(r){
        positions[sapply(positions, function(p) {
        length(unique(filter(var, Run==r, position==p)$alternate))>1})]
})
length(unlist(pmss)) 


# Shared polymprhic sites > in more than one sample 
length(duplicated(unlist(pmss)))
spmss <- pmss[duplicated(unlist(pmss))]


# Runs with polymorphic sites polymorphic vs monomorphic









# Shared polymorphic positions: polymorphic in > 1 sample
# FIXME: pending of sample matching count for minor

# head(filter(var, position %in% pm)$sampleMatchingCount)
# 
# spm <- filter(var, position %in% pm, sampleMatchingCount>1)$position
# length(unique(spm)) # 16339 shared polymorphic positions


gisaid_spm_sites <- read_delim("gisaid_spm_sites.txt", delim="\n")

# Shared Polymorphic positions in gisaid consensus
length(unique(intersect(illum_ppm_sites, gisaid_spm_sites)))

# samples with spm polynorphic


# samples with spm monomorphic






















# Runs with variants in Polymorphic sites 

# Intrahost in polymorphic sites

title <- "RUNS WITH VARIANTS IN PMS PER POSITION"
# dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
pvar <- filter(var, position %in% illum_ppm_sites)
dat <- data.frame(pos=pvar$position, freq=pvar$PosCount/1497, class=pvar$class, stringsAsFactors = F)

pms_needle <- ggplot(dat, 
                     aes(x=pos, ymax=freq, ymin=0)) +
        geom_linerange() + #color="orangered2"
        aes(x=pos, ymax=freq, ymin=0, color=class) +
        geom_linerange() +
        scale_color_manual(values = c("green","orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        #theme(title = element_text(size = 3)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Genomic Position") +
        theme(legend.title = element_blank())+
        labs(y="Freq") 
pms_needle 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_pms_needle.pdf",  
    height =5.83,  width =8.27)
pms_needle 
dev.off()



# # Runs with non polymorphic sites
# 
# title <- "RUNS WITH VARIANTS IN non-PMS PER POSITION"
# # dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
# non_pm_sites <- var$position[which(!(var$position %in% illum_pm_sites))]
# 
# npvar <- filter(var, position %in% non_pm_sites )
# dat <- data.frame(pos=npvar$position, freq=npvar$PosCount/1497, class=npvar$Functional_Class, stringsAsFactors = F)
# 
# 
# npms_needle <- ggplot(dat, 
#                      aes(x=pos, ymax=freq, ymin=0)) +
#         geom_linerange() + #color="orangered2"
#         aes(x=pos, ymax=freq, ymin=0, color=class) +
#         geom_linerange() +
#         scale_color_manual(values = c("green","orangered2", "blue")) +
#         theme_minimal() +
#         ggtitle(title) +
#         #theme(title = element_text(size = 3)) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         labs(x="Genomic Position") +
#         theme(legend.title = element_blank())+
#         labs(y="Freq") 
# npms_needle 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_npms_needle.pdf",  
#     height =5.83,  width =8.27)
# npms_needle 
# dev.off()
# 





# AF in polymorphic sites
# AF
plot(pvar$position, pvar$frequency)


# Max and Min AF per position
minfreq <- unlist(lapply(unique(pvar$position), function(p) {
        min(filter(pvar, position==p)$frequency)      
}))
meanfreq <-  unlist(lapply(unique(pvar$position), function(p) {
        mean(filter(pvar, position==p)$frequency)      
}))
maxfreq <- unlist(lapply(unique(pvar$position), function(p) {
        max(filter(pvar, position==p)$frequency)      
}))


# samples with spm polynorphic
# samples with spm monomorphic

# ## ?AF IN DATASET -NOT POSIBBLE TO PLOT- more than one frequency per position if many alternates
title <- "RANGE OF AF OF VARIANTS PER POSITION IN PMS"
dat <- data.frame(pos=unique(pvar$position), meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)
paf <- ggplot(dat,
             aes(x=pos, y=maxfreq)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_linerange(ymin=minfreq, ymax=maxfreq, size=0.1) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Genomic Position") +
        labs(y="Allele Frequency range")
#         theme(legend.title = element_blank())+
#         labs(y="Freq")
paf
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_pms_af.pdf",
    height =5.83,  width =8.27)
paf
dev.off()




# AF in non-polymorphic sites
# AF
plot(npvar$position, npvar$frequency)


# Max and Min AF per position
minfreq <- unlist(lapply(unique(npvar$position), function(p) {
        min(filter(npvar, position==p)$frequency)      
}))
meanfreq <-  unlist(lapply(unique(npvar$position), function(p) {
        mean(filter(npvar, position==p)$frequency)      
}))
maxfreq <- unlist(lapply(unique(npvar$position), function(p) {
        max(filter(npvar, position==p)$frequency)      
}))


# samples with spm polynorphic

# ## ?AF IN DATASET -NOT POSIBBLE TO PLOT- more than one frequency per position if many alternates
title <- "RANGE OF AF OF VARIANTS PER POSITION IN PMS"
dat <- data.frame(pos=unique(npvar$position), meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)
npaf <- ggplot(dat,
              aes(x=pos, y=maxfreq)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_linerange(ymin=minfreq, ymax=maxfreq, size=0.1) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Genomic Position") +
        labs(y="Allele Frequency range")
#         theme(legend.title = element_blank())+
#         labs(y="Freq")
npaf
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_npms_af.pdf",
    height =5.83,  width =8.27)
npaf
dev.off()








# Cowplot grid
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/cowplot_grid.pdf",
#     height =5.83,  width =8.27)
# plot_grid(af, mm, mm2, mm3, labels=c("A", "B", "C", "D"), align = "h")
# dev.off()
# 
# 
# 
# plot_grid(af, mm, mm2, mm3, labels=c("A", "B", "C", "D"), align="h")
# 
# 
# plot_grid(af, labels=c("A"))
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_af2.pdf",
#     height =5.83,  width =8.27)
# plot_grid(af, labels=c("A"))
# dev.off()





