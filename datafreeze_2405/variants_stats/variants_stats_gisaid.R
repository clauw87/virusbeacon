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


source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/ggplot2_charts.R")


setwd( "/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/data")


## All variants
resources_folders <-list.files("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/data")
# GISAID
f <- "gisaid"
variant_file <- paste0(f, "/29629.annot.variants.data")
var <- fread(input = variant_file, sep = ";")
var$variant <- sapply(1:nrow(var), function(v){
        paste(as.character(var$position[v]),var$reference[v],  var$alternate[v], sep = ">")
})

var$VarCount <- unlist(lapply(var$variant, function(v){
        sum(filter(var, variant==v)$sampleMatchingCount)
}))

var$PosCount <- unlist(lapply(var$position, function(p){
        sum(filter(var, position==p)$sampleMatchingCount)
}))

var$resource <- rep(f, nrow(var))

assign(value=var, x=paste0(as.character(f), "_var"))

save(gisaid_var, file = "gisaid_var.rda")



#### Load 

load("gisaid_var.rda")
var <- gisaid_var


duvar <- var[which(duplicated(var$variant)),]
length(unique(duvar$variant)) # 0


uvar <- var[which(!(duplicated(var$variant))),]
length(unique(uvar$variant)) # 20288
####################################################################################




# ############## STATS ########################################################

####### Positions --------------------------------------------------------------

# # Positions with variants
length(unique(uvar$position))
# 13987      

29906- length(unique(uvar$position)) 
        # 15919 positions without variants


# # Proportion of # Positions with variants
length(unique(uvar$position))/29906 *100
# # 46.76988





# snp alternates per position
gis_alt_per_pos <- sapply(unique(uvar$position), function(p) {
        unique(filter(uvar, position==p)$alternate)
})
length(gis_alt_per_pos) # 13987

num_alt_per_pos <- sapply(gis_alt_per_pos, length)

(29906-length(unique(uvar$position))  )  /29906*100
sum(num_alt_per_pos==1)/29906*100 # 11135
sum(num_alt_per_pos==2)/29906*100 # 10469
sum(num_alt_per_pos==3)/29906*100 # 4762









###### Variants ###############################################################

# Number of variants
# Unique variants
length(var$variant) # 20288
length(unique(var$variant)) # 20288


nrow(var)
# 20288

nrow(uvar)
# 20288

# Variant Type
unique(var$svType) # "SNP"   


uvar$Class <- rep(NA, nrow(uvar))
uvar$Class <- ifelse (uvar$Functional_Class=="MISSENSE"|uvar$Functional_Class=="NONSENSE", "NS", uvar$Class)
uvar$Class <- ifelse (uvar$Functional_Class=="SILENT", "S", uvar$Class)
uvar$Class <- ifelse (uvar$Functional_Class=="", "NC", uvar$Class)


## PER POSITION VARIABILITY
# Total unique variants per position
var_data <- filter(uvar, svType=="SNP")
title <- "NUM VARIANTS (NON-AMBIBOUS ALT) PER POSITION"
var_data$numvar <- sapply(var_data$position, function(p) {
        length(unique(filter(var_data, position==p, alternate %in% c("A", "T", "C", "G"))$alternate))
})
vpp <- ggplot(var_data,
              #aes(x=Var1, ymax=Freq, ymin=0)) + 
              aes(x=position, ymax=numvar, ymin=0, color=Class)) + 
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/needle_vpp_gis.pdf",  height =5.83,  width =8.27)
vpp
dev.off()


## SAMPLE MATCHING COUNT

# Frequency
min(uvar$sampleMatchingCount) # 1
min(uvar$PosCount) # 1
min(uvar$VarCount) # 1
max(uvar$sampleMatchingCount) # 21083
max(uvar$PosCount) # 21140
max(uvar$VarCount) # 21083


title <- "RUNS WITH VARIANTS PER POSITION"
dat <- data.frame(pos=uvar$position, freq=uvar$PosCount/1497, class=uvar$Functional_Class, stringsAsFactors = F)
dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
dat$class[which(dat$class=="SILENT")] <- "S"
dat$class[which(dat$class=="")] <- "NC"

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

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_smc_needle.pdf",
    height =5.83,  width =8.27)
smc_needle
dev.off()






###########################################################################################


########
#########   # Variants annotation
##########
# Region Class
unique(uvar$Gene_Coding) # "" (this is noncoding)      "CODING"   

# Functional Class
unique(uvar$Functional_Class) # ""  (this is noncoding)       "MISSENSE" "NONSENSE" "SILENT"  

# "Amino_Acid_Change"  
length(unique(uvar$Amino_Acid_Change)) # 27556 distinct aminoacid changes across samples


#"Gene_Name" 
unique(uvar$Gene_Name) # ""  (this is noncoding) : all genes     "orf1ab" "S"      "ORF3a"  "E"      "M"      "ORF6"   "ORF7a"  "ORF7b"  "ORF8"   "N"      "ORF10" 

# unique(uvar$Genotype)



############################################################################

# General statistics graphs

# Total unique variants per frequency group
title <- "VARIANTS PER FREQ GROUP"
uvar$FreqGroup <- rep(NA, nrow(uvar))
uvar$FreqGroup <- ifelse(uvar$frequency <=0.001, "<0.001", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$frequency >0.001 & uvar$frequency <=0.01, "0.001-0.01", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$frequency >0.01 & uvar$frequency <=0.1, "0.01-0.1", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$frequency >0.1 & uvar$frequency <=0.5, "0.1-0.5", uvar$FreqGroup) 
uvar$FreqGroup <- ifelse(uvar$frequency >0.5 ,">0.5", uvar$FreqGroup) 

# Num variants per frequency group

uvar$Class <- rep(NA, nrow(uvar))
uvar$Class <- ifelse (uvar$Functional_Class=="MISSENSE"|uvar$Functional_Class=="NONSENSE", "NS", uvar$Class)
uvar$Class <- ifelse (uvar$Functional_Class=="SILENT", "S", uvar$Class)
uvar$Class <- ifelse (uvar$Functional_Class=="", "NC", uvar$Class)

uvar_ct <- uvar %>% 
        group_by(Class, FreqGroup) %>%
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
vfp  <- ggplot(vf, aes(x=FreqGroup, y=count, fill=Class)) +
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

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/vars_per_fg_gisaid.pdf",  
    height =4,  width =4)
vfp
dev.off()



# Total unique variants per variant type
vt <- data.frame(Var1=c("SNP","INDEL" ), Freq=c(sum(uvar$svType=="SNP")
                                                , sum(uvar$svType=="INDEL")
))
vt <-  mutate(vt, Var1=fct_relevel(Var1,   
                                   "SNP","INDEL"))
vtp <- ggplot(vt) +
        geom_col(aes(Var1, Freq)) +
        theme_minimal() +
        ggtitle("VARIANT TYPE") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vtp

#pie
my_colors <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", "#666666",
               "#F69F00", "#56B4E9", "#F89F10", "#999999")

blank_theme <-theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background  = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face="bold"))

vtp_bp <- ggplot(vt, aes(Var1, Freq)) +
        aes(x="", y=Freq, fill=Var1) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
vtp_bp 
vtp_pie <- vtp_bp + 
        coord_polar("y", start=0) +
        ggtitle("VARIANT TYPE") +
        theme(plot.title = element_text(hjust = 0.5)) +
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
vtp_pie 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variant_type_gisaid.pdf",  
    height =5.83,  width =8.27)
vtp_pie
dev.off()


# Total unique variants per Region class
title <- "REGION CLASS"
Var1 <- uvar$Gene_Coding
Var1[which(Var1=="")] <- "NON-CODING"
unique(Var1)
# "NON-CODING" "CODING"  
rc <- as.data.frame(table(Var1))
rcp <- ggplot(rc) +
        geom_col(aes(Var1, Freq)) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
rcp

#pie
rcp_bp <- ggplot(rc, aes(Var1, Freq)) +
        aes(x="", y=Freq, fill=Var1) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
rcp_bp 
rcp_pie <- rcp_bp + 
        coord_polar("y", start=0) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
rcp_pie 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_class_gisaid.pdf",  
    height =5.83,  width =8.27)
rcp_pie 

dev.off()



# Total unique variants per variant effect
title <- "VARIANT EFFECT"
Var1 <- uvar$Effect
unique(Var1)
Var1[which(Var1==".")] <- "NON-CODING"
unique(Var1)
ve <- as.data.frame(table(Var1))
ve <- mutate(ve, Var1=fct_relevel(Var1,
                                  "NON-CODING", 
                                  "NON_SYNONYMOUS_START" ,              
                                  "START_LOST" ,                          "STOP_GAINED"  ,                       
                                  "NON_SYNONYMOUS_CODING" ,               "SYNONYMOUS_CODING"  ,                 
                                  "SPLICE_SITE_REGION+SYNONYMOUS_CODING" ,"STOP_LOST+SPLICE_SITE_REGION" ,       
                                  "SPLICE_SITE_REGION+SYNONYMOUS_STOP"  
))

vep <- ggplot(ve) +
        geom_col(aes(Var1, Freq)) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vep

#pie
vep_bp <- ggplot(ve, aes(Var1, Freq)) +
        aes(x="", y=Freq, fill=Var1) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
vep_bp 
vep_pie <- vep_bp  + 
        coord_polar("y", start=0) +
        ggtitle(title) +
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
        #theme(legend.position = "none")
vep_pie 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/var_effect_gisaid.pdf",  
    height =5.83,  width =8.27)
vep_pie 

dev.off()



# Total unique variants per functional class
title <- "FUNCTIONAL CLASS"
Var1 <- uvar$Functional_Class
unique(Var1)
Var1[which(Var1=="")] <- "NON-CODING"
unique(Var1)
fc <- as.data.frame(table(Var1))
fc <- mutate(fc, Var1=fct_relevel(Var1,
                                  "NON-CODING", "NONSENSE" , "MISSENSE", "SILENT"
))
fcp <- ggplot(fc) +
        geom_col(aes(Var1, Freq)) +
        theme_minimal() +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
fcp

#pie
fcp_bp <- ggplot(fc, aes(Var1, Freq)) +
        aes(x="", y=Freq, fill=Var1) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
fcp_bp
fcp_pie <- fcp_bp  + 
        coord_polar("y", start=0) +
        ggtitle(title) +
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
fcp_pie

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/fun_class_gisaid.pdf",  
    height =5.83,  width =8.27)
fcp_pie 

dev.off()


# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_stats_all_pies.pdf", 
#     width = 26, height = 8)
# #grid.arrange(ep, nrow=2, ncol=3 ) #
# # using cowplot this would look better
# grid.arrange(vtp_pie, rcp_pie,
#              vep_pie,fcp_pie, 
#              ncol=3, nrow=3,
#              layout_matrix=rbind(
#                      c(1,2),
#                      c(3,4)
#                    )) 
# dev.off()
# 

####################### Region query 
utrs_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/utr_coordinates.txt")
genes_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/gene_coordinates.txt")
cds_table  <- read_csv("~/repolab/work/virusbeacon/genome_annotation/cds_coordinates.txt")
mature_proteins_table  <- read.table("~/repolab/work/virusbeacon/genome_annotation/protein_coordinates.txt")
colnames(mature_proteins_table)[5] <- "protein_name"
#mature_proteins_table  <- read.table("mature_peptide_annot.txt")
#colnames(mature_proteins_table)[3] <- "protein_name"
# mature_proteins_table <- read_csv("~/repolab/work/virusbeacon/mature_proteins.csv",
#                                   col_names = T)
stem_loops_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/sl_coordinates.txt")

region_filter <- function(region_id) { # region_type=utr, 
        # if (region_type=="utr")   { 
        if (region_id %in% utrs_table$utr_name)   { 
                table_type <- utrs_table
                region_name <- "utr_name"
        }
        
        #if (region_type=="gene")   { 
        if (region_id %in% genes_table$gene_name)   { 
                table_type <- genes_table
                region_name <- "gene_name"
        }
        
        #if (region_type=="cds")   { 
        #if (region_id %in% proteins_table$protein_name)   { 
        if (region_id %in% mature_proteins_table$protein_name)   { 
                table_type <- mature_proteins_table
                region_name <- "protein_name"
        }
        
        #if (region_type=="sl")   { 
        if (region_id %in% stem_loops_table$stem_loop_name)   { 
                table_type <- stem_loops_table
                region_name <- "stem_loop_name"
        }
        range <- seq(filter(table_type, eval(as.name( region_name))==region_id)$start, 
                     filter(table_type, eval(as.name( region_name))==region_id)$end)
        
        range
}

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
        #geom_col(fill=Class) +
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_gene_gisaid.pdf",  
    height =5.83,  width =8.27)
vgenes
dev.off()



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

# skip for stacked chart
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein_gisaid.pdf",  height =5.83,  width =8.27)
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/aa_per_protein_gisaid.pdf",  height =5.83,  width =8.27)
aap
dev.off()




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


stacked <- ggplot(dat, aes(fill=class, y=count, x=reg))+
        geom_bar(position = "stack", stat = "identity" ) +
        theme_minimal() +
        ggtitle("VARIANTS PER MATURE PROTEINS") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = c("honeydew4", "honeydew3")) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))

stacked
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein.pdf",  height =5.83,  width =8.27)
stacked
dev.off()



# # eX
# library(viridis)
# library(hrbrthemes)
# 
# specie <- c(rep("sorgho", 3), rep("poacee", 3), rep("banana", 3), rep("triticum", 3) )
# condition <- rep(c("normal", "stress", "N"), 4)
# value <- abs(rnorm(12,0,15))
# data <- data.frame(specie, condition, value)
# 
# specie <- dat$reg
# condition <- c("S", "NS")
# value <- dat$ns_count
# 
# ggplot(data, aes(fill=condition, y=value, x=specie))+
#         geom_bar(position = "stack", stat = "identity" )# 
# 
# 







reg <- table(uvar$region, useNA = "ifany")
dat <- data.frame(reg)

reg <- ggplot(dat, aes(Var1, Freq)) +
        geom_col(fill="hotpink4", position = "identity") +
        theme_minimal() +
        ggtitle("Unique variants per region class") +
        theme(plot.title = element_text(hjust = 0.5)) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$Freq, vjust=-0.1, size=2.5, colour = "red") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
reg
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_class.pdf",  height =5.83,  width =8.27)
reg
dev.off()








# CODING positions
c_positions <- unique(uvar$position[which(uvar$Gene_Coding=="CODING")])
length(c_positions) # 13494

# NON CODING
nc_positions <- unique(uvar$position[which(uvar$Gene_Coding=="")])
length(nc_positions) # 493

genomic_regions <-  utrs_table$utr_name
utr_positions <- sapply(genomic_regions, function(q) {
        unique(filter(uvar, position %in% region_filter(region_id = q))$position)
})
length(unlist(utr_positions)) #  417

sum(unlist(utr_positions) %in% nc_positions) #417

intergenic <- setdiff(nc_positions, unlist(utr_positions))
length(intergenic) # 76

# # coding
# genomic_regions <-  utrs_table$utr_name
# n1 <- sapply(genomic_regions, function(q) {
#         nrow(filter(uvar, position %in% region_filter(region_id = q)))
# })

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

# number of different variants
# genomic_regions <- c(utrs_table$utr_name, 
#                      #stem_loops_table$stem_loop_name,
#                      genes_table$gene_name
#                      #unique(proteins_table$protein_name)
# )


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


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_nc_gisaid.pdf",  
    height =3.83,  width =2.83)
vnc
dev.off()


all_dat<-   rbind(dat1 , dat2, dat3)

all_dat <-  mutate(all_dat, reg=fct_relevel(reg,   
                                            "5'UTR",
                                            "coding",
                                            "intergenic",
                                            "3'UTR" 
))

#pie
title <- "VARIANTS PER GENOMIC REGION" 
vnc_bp <- ggplot(all_dat, aes(reg, count)) +
        aes(x="", y=count, fill=reg) +
        theme(legend.title = element_blank()) +
        geom_bar(width = 1, stat = "identity") 
vnc_bp 
vnc_pie <- vnc_bp  + 
        coord_polar("y", start=0) +
        ggtitle(title) +
        blank_theme +
        theme(plot.title = element_text(hjust = 0.5, face = "plain", size = 24)) +
        theme(legend.title  = element_blank()) + 
        theme(legend.text = element_text(size = 12)) + 
        scale_fill_manual(values=c("chartreuse", "dodgerblue", "green", "greenyellow")) +
        theme(axis.text.x = element_blank()) 
vnc_pie 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/var_region_gisaid.pdf",  
    height =5.83,  width =8.27)
vnc_pie 

dev.off()

# Functional /STEM LOOPS

stem_loops_table$stem_loop_name <- c("fsSE SL1", "fsSE SL2", "3utr ps SL1", "3utr ps SL2", "s2m")

genomic_regions <- stem_loops_table$stem_loop_name

n <- sapply(genomic_regions, function(q) {
        nrow(filter(var, position %in% region_filter(region_id = q)))
})


dat<- data.frame(reg=genomic_regions, count=n)
#rownames(dat) <- names(t)
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_sl_gis.pdf",  height =5.83,  width =8.27)
vsl
dev.off()


# # Both as a grid
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_nc.pdf",  height =5.83,  width =8.27)
# vnc
# dev.off()
# 
# 
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/nc_sl.pdf",  height =5.83,  width =3.5)
# 
# #grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(vnc, vsl,
#              ncol=1, nrow=2,
#              layout_matrix=rbind(
#                      c(1),
#                      c(2)
#                      # c(4,5,6),
#                      # c(7,8,9)
#              )) #
# dev.off()
























###################################################################################
# Polymorphic e intrahost


############################################################

# # Polymorphic sites: sites with multile alleles > 1 alternate position in a sample
# polymorphic in sample:  > 1 alternate position
#  positions are polymorphic in at least 1 sample

# polymorphic in dataset / population
iupac <- setdiff(unique(var$alternate), c("A", "T", "C", "G"))


gis_ppm_sites <- unique(uvar$position)[sapply(gis_alt_per_pos, function(a){
        length(a)>1| sum(iupac %in% a)>0
})]
length(gis_ppm_sites) # 8139
head(gis_ppm_sites) # 1 2 3..


# polymorphic in sample is IUPAC only here (polymorphic in at least 1 sample)
gis_pm_sites <- unique(uvar$position)[sapply(gis_alt_per_pos, function(a){
        #length(filter(uvar, position %in% a & alternate %in% iupac)$alternate)>0
        sum(iupac %in% a)>0
})]
length(gis_pm_sites) # 7362

# shared polymorphic, polymorphic in more than 1 sample
gis_spm_sites <- unique(filter(uvar, position %in% gis_pm_sites, 
                               alternate %in% iupac,
                               VarCount>1)$position)
length(gis_spm_sites) # 1627
gis_spm_sites
write(gis_spm_sites, "gisaid_spm_sites.txt")









# Freq
plot(var$position, var$frequency)


# Max and Min AF per position
minfreq <- unlist(lapply(unique(var$position), function(p) {
        min(filter(var, position==p)$frequency)      
}))
meanfreq <-  unlist(lapply(unique(var$position), function(p) {
        mean(filter(var, position==p)$frequency)      
}))
maxfreq <- unlist(lapply(unique(var$position), function(p) {
        max(filter(var, position==p)$frequency)      
}))


# samples with spm polynorphic
# samples with spm monomorphic





# Intrahost_


# ## ?AF IN DATASET -NOT POSIBBLE TO PLOT- more than one frequency per position if many alternates
title <- "RANGE OF DATASET FREQ OF VARIANTS PER POSITION"
dat <- data.frame(pos=unique(var$position), meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)
freq <- ggplot(dat,
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
freq
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_freq.pdf",
    height =5.83,  width =8.27)
#af
plot_grid(freq, labels=c("A"), label_size = 18)
dev.off()





# MIN VS MAX
title <- "MAX VS MIN FREQ IN DATASET PER POSITION"
plot(minfreq, maxfreq)

dat <- data.frame(pos=unique(var$position), meanfreq=meanfreq, minfreq=minfreq, maxfreq=maxfreq)

# dat <- data.frame(pos=var_data$position, freq=var_data$frequency, class=var_data$Functional_Class, stringsAsFactors = F)
# dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
# dat$class[which(dat$class!="NS")] <- "S"
mm <- ggplot(dat,
             aes(x=minfreq, y=maxfreq)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_point(size=1, color="dodgerblue") +
        #scale_color_manual(values = c("orangered2", "blue", "green")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Min AF in position") +
        labs(y="Max AF in position") +
        theme(legend.title = element_blank())
mm
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_mm.pdf",
    height =5.83,  width =8.27)
#mm
plot_grid(mm, labels=c("B"), label_size = 18)
dev.off()
length(unique(filter(dat, maxfreq==minfreq)$pos)) # 10750
length(filter(dat, maxfreq!=minfreq)$pos) # 3237 
# these ones are positions with more than one variant, 
# i.e, polymorphic in dataset
sum(filter(dat, maxfreq!=minfreq)$pos %in% gis_ppm_sites) # 3237 





# Samples with Variants vs Samples with Variant in Position
# ie not everyone with position has same variant


plot(var$PosCount, var$VarCount)

# MIN VS MAX
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
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_mm2.pdf",
    height =5.83,  width =8.27)
#mm2
plot_grid(mm2, labels=c("C"), label_size = 18)
dev.off()
length(filter(var, VarCount!=PosCount)$pos) # 11143
length(unique(filter(var, VarCount!=PosCount)$pos)) # 4842
length(unique(filter(var, VarCount==PosCount)$pos)) # 9145



plot(var$position, var$PosCount)
plot(var$position, var$VarCount)




plot(uvar$VarCount/29629, uvar$frequency)
# where a few samples bear a variant and it is nevertheless  high freq in population..
# wait..what?

# Posible intra hosts, 
# where a lot of samples bear a variant that is nevertheless low freq in dataset
title <- "RUN COUNTS VS POSITION COUNTS IN DATASET"
dat <- data.frame(pos=var$position, run_var=var$VarCount, run_pos=var$PosCount, af=var$frequency, class=var$Functional_Class, stringsAsFactors = F)# dat <- data.frame(pos=var_data$position, freq=var_data$frequency, class=var_data$Functional_Class, stringsAsFactors = F)
dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
dat$class[which(dat$class=="SILENT")] <- "S"
dat$class[which(dat$class=="")] <- "NC"


mm3 <- ggplot(dat,
              aes(x=run_var/29629, y=af, color=class)) +
        #ggplot(data=dat, aes(x=pos, y=meanfreq, ymin=minfreq, ymax=maxfreq)) +
        geom_point(size=1) +  #, color="dodgerblue
        scale_color_manual(values = c("green", "orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        theme(title = element_text(size = 10)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y="Frquency in dataset") +
        labs(x="Number of runs with variant") +
        theme(legend.position = "top") +
        theme(legend.title = element_blank())
mm3
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_mm3.pdf",
    height =5.83,  width =8.27)
#mm3
plot_grid(mm3, labels=c("D"), label_size = 18)
dev.off()
length(filter(var, VarCount!=PosCount)$pos) # 11143
length(unique(filter(var, VarCount!=PosCount)$pos)) # 4842
length(unique(filter(var, VarCount==PosCount)$pos)) # 9145


























##### Compare with other datasets

# Position common to datasets
length(intersect(unique(genbank_var$position), unique(gisaid_var$position)))
# 4209

length(intersect(unique(sra_illumina_var$position), unique(sra_ont_var$position)))
# 14349



# Consensus variants in gisaid 
illum_ont <- intersect(unique(sra_gisaid_var$variant), unique(sra_ont_var$variant))
# 5802
illum_genbank <- intersect(unique(sra_illumina_var$variant), unique(genbank_var$variant))
# 1928
illum_gisaid <- intersect(unique(sra_illumina_var$variant), unique(gisaid_var$variant))
# 8655
consensus <- c(illum_genbank, illum_gisaid)
length(consensus)
# 10583












###OLD


# 
# 
# # ############## STATS ########################################################
# 
# ####### Positions --------------------------------------------------------------
# 
# # # Positions with variants
# length(unique(uvar$position))
# #  13987       
# 
# 29906-  13987 
#         # 15919 positions without variants
# 
# 
# # # Proportion of # Positions with variants
# length(unique(uvar$position))/29906 *100
# # # 46.77
# 
# 
# 
# 
# # snp alternates per position
# gis_alt_per_pos <- sapply(unique(uvar$position), function(p) {
#         unique(filter(uvar, position==p)$alternate)
# })
# length(gis_alt_per_pos) # 13987
# 
# num_alt_per_pos <- sapply(gis_alt_per_pos, length)
# 
# 15919/29906*100
# sum(num_alt_per_pos==1)/29906*100 # 11135
# sum(num_alt_per_pos==2)/29906*100 # 10469
# sum(num_alt_per_pos==3)/29906*100 # 4762
# 
# 
# 
# 
# title <- "RUNS WITH VARIANTS IN PMS PER POSITION"
# # dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
# pvar <- filter(var, position %in% gis_pm_sites)
# dat <- data.frame(pos=pvar$position, freq=pvar$PosCount/29629, class=pvar$Functional_Class, stringsAsFactors = F)
# #dat <- data.frame(pos=var_data$position, freq=var_data$dbfreq, class=var_data$Functional_Class, stringsAsFactors = F)
# dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
# dat$class[which(dat$class=="SILENT")] <- "S"
# dat$class[which(dat$class=="")] <- "NC"
# 
# 
# pms_needle <- ggplot(dat, 
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
# pms_needle 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/illumina_pms_needle.pdf",  
#     height =5.83,  width =8.27)
# pms_needle 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# ###### Variants ###############################################################
# 
# # Number of variants
# # Unique variants
# length(var$variant) # 46359
# length(unique(var$variant)) # 46459
# 
# 
# nrow(var)
# # 51006
# 
# nrow(uvar)
# # 46359
# 
# # Variant Type
# unique(var$svType) # "SNP"   
# 
# ## SAMPLE MATCHING COUNT
# 
# # Frequency
# min(uvar$sampleMatchingCount) # 1
# min(uvar$PosCount) # 1
# min(uvar$VarCount) # 1
# max(uvar$sampleMatchingCount) # 795
# max(uvar$PosCount) # 801
# max(uvar$VarCount) # 795
# 
# 
# 
# title <- "RUNS WITH VARIANTS PER POSITION"
# # dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
# dat <- data.frame(pos=uvar$position, freq=uvar$PosCount/29629, class=uvar$Functional_Class, stringsAsFactors = F)
# #dat <- data.frame(pos=var_data$position, freq=var_data$dbfreq, class=var_data$Functional_Class, stringsAsFactors = F)
# dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
# dat$class[which(dat$class=="SILENT")] <- "S"
# dat$class[which(dat$class=="")] <- "NC"
# 
# smc_needle <- ggplot(dat, 
#                  aes(x=pos, ymax=freq, ymin=0, color=class)) +
#         #geom_linerange() + #color="orangered2"
#         #aes(x=pos, ymax=freq, ymin=0, ) +
#         #geom_linerange(position = position_dodge()) +
#         geom_linerange(stat="identity", position = "dodge") +
#         scale_color_manual(values = c("green","orangered2", "blue")) +
#         theme_minimal() +
#         ggtitle(title) +
#         #theme(title = element_text(size = 3)) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         labs(x="Genomic Position") +
#         theme(legend.title = element_blank())+
#         labs(y="Freq") 
# smc_needle 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/gisaid_smc_needle.pdf",  
#     height =5.83,  width =8.27)
# smc_needle 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# ## PER POSITION VARIABILITY
# # Total unique variants per position
# var_data <- filter(var, svType=="SNP")
# title <- "NUM VARIANTS (ALT) PER POSITION"
# #Var1 <- unique(var_data$position)
# #Var2 <- sapply(Var1, function(p) {
# #        length(unique(filter(var_data, position==p)$alternate))
# #})
# # Var3 <- sapply(Var1, function(p) {
# #         length(unique(filter(var_data, position==p, alternate %in% c("A", "T", "C", "G"))$alternate))
# # })
# 
# var_data$numvar <- sapply(var_data$position, function(p) {
#         length(unique(filter(var_data, position==p, alternate %in% c("A", "T", "C", "G"))$alternate))
# })
# 
# 
# #vp <-  as.data.frame(table(Var1))
# 
# #vp <-  data.frame(Var1, Var3)
# 
# 
# # vp$Var1 <- as.numeric(vp$Var1)
# # vp$Var2 <- as.numeric(vp$Var2)
# # vp$Var3 <- as.numeric(vp$Var3)
# 
# 
# #vp$Freq <- as.numeric(vp$Freq)
# 
# vpp <- ggplot(var_data,
#               #aes(x=Var1, ymax=Freq, ymin=0)) + 
#               aes(x=position, ymax=numvar, ymin=0, color=Class)) + 
#         # geom_linerange(color="orangered2") +
#         geom_linerange() +
#         scale_color_manual(values = c("green","orangered2", "blue")) +
#         theme_minimal() +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(legend.position = "top") +
#         theme(legend.title = element_blank()) +
#         
#         labs(x="Genomic Position") +
#         labs(y="Number of variants") 
# vpp
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/needle_vpp_ill.pdf",  height =5.83,  width =8.27)
# vpp
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# ###########################################################################################
# 
# 
# ########
# #########   # Variants annotation
# ##########
# # Region Class
# unique(uvar$Gene_Coding) # "" (this is noncoding)      "CODING"   
# 
# # Functional Class
# unique(uvar$Functional_Class) # ""  (this is noncoding)       "MISSENSE" "NONSENSE" "SILENT"  
# 
# # "Amino_Acid_Change"  
# length(unique(uvar$Amino_Acid_Change)) # 13198 distinct aminoacid changes across samples
# 
# 
# #"Gene_Name" 
# unique(uvar$Gene_Name) # ""  (this is noncoding) : all genes     "orf1ab" "S"      "ORF3a"  "E"      "M"      "ORF6"   "ORF7a"  "ORF7b"  "ORF8"   "N"      "ORF10" 
# 
# # unique(uvar$Genotype)
# 
# 
# 
# ############################################################################
# 
# # General statistics graphs
# 
# # Total unique variants per frequency group
# title <- "SAMPLE MATCHING FREQUENCY"
# # Var1 <- uvar$MyFrequency
# # hist(uvar$MyFrequency, breaks=c(0, 0.001,0.01,0.1, 0.6))
# # hist( uvar$MyFrequency,breaks=1000 )
# # unique(Var1)
# uvar$FreqGroup <- rep(NA, nrow(uvar))
# uvar$FreqGroup <- ifelse(uvar$frequency <=0.001, "<0.001", uvar$FreqGroup) 
# uvar$FreqGroup <- ifelse(uvar$frequency >0.001 & uvar$frequency <=0.01, "0.001-0.01", uvar$FreqGroup) 
# uvar$FreqGroup <- ifelse(uvar$frequency >0.01 & uvar$frequency <=0.1, "0.01-0.1", uvar$FreqGroup) 
# uvar$FreqGroup <- ifelse(uvar$frequency >0.1 & uvar$frequency <=0.5, "0.1-0.5", uvar$FreqGroup) 
# uvar$FreqGroup <- ifelse(uvar$frequency >0.5 ,">0.5", uvar$FreqGroup) 
# 
# uvar$FreqGroup <- as.factor(uvar$FreqGroup)
# # Num variants per frequency group
# 
# uvar$Class <- rep(NA, nrow(uvar))
# uvar$Class <- ifelse (uvar$Functional_Class=="MISSENSE"|uvar$Functional_Class=="NONSENSE", "NS", uvar$Class)
# uvar$Class <- ifelse (uvar$Functional_Class=="SILENT", "S", uvar$Class)
# uvar$Class <- ifelse (uvar$Functional_Class=="", "NC", uvar$Class)
# 
# uvar$Class <- as.factor(uvar$Class)
# uvar_ct <- uvar %>% 
#         group_by(Class, FreqGroup) %>%
#         summarise(count=n())
# 
# 
# vf <- uvar_ct
# 
# vf <- mutate(vf, FreqGroup=fct_relevel(FreqGroup,
#                                        "<0.001", "0.001-0.01", "0.01-0.1","0.1-0.5", ">0.5"
# ))
# 
# sumlabel <- vf %>%
#         group_by(FreqGroup) %>%
#         summarise(total=sum(count))
# 
# colors <- c("green", "orangered2", "blue")
# vfp  <- ggplot(vf, aes(x=FreqGroup, y=count, fill=Class)) +
#         geom_bar(position='stack', stat='identity') +
#         #geom_bar(position='stack', stat='identity') +
#         ggtitle("NUM VARIANTS PER FREQ GROUP") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Count of variants in dataset") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(legend.title = element_blank()) +
#         scale_fill_manual(values=colors) +
#         geom_text(aes(FreqGroup, total+500, label=total, fill=NULL), data=sumlabel, size=3) +
#                   #position = position_dodge(vjus, size=3) + # 
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
# vfp
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/vars_per_fg_gisaid.pdf",  
#     height =4,  width =4)
# vfp
# dev.off()
# 
# 
# 
# # Total unique variants per variant type
# vt <- data.frame(Var1=c("SNP","INDEL" ), Freq=c(sum(uvar$svType=="SNP")
# , sum(uvar$svType=="INDEL")
# ))
# vt <-  mutate(vt, Var1=fct_relevel(Var1,   
#                                      "SNP","INDEL"))
# vtp <- ggplot(vt) +
#         geom_col(aes(Var1, Freq)) +
#         theme_minimal() +
#         ggtitle("VARIANT TYPE") +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of variants") +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vtp
# 
# #pie
# my_colors <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", "#666666",
#                "#F69F00", "#56B4E9", "#F89F10", "#999999")
# 
# blank_theme <-theme(
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         panel.background  = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(size = 14, face="bold"))
# 
# vtp_bp <- ggplot(vt, aes(Var1, Freq)) +
#          aes(x="", y=Freq, fill=Var1) +
#         theme(legend.title = element_blank()) +
#         geom_bar(width = 1, stat = "identity") 
# vtp_bp 
# vtp_pie <- vtp_bp + 
#         coord_polar("y", start=0) +
#         ggtitle("VARIANT TYPE") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         blank_theme + 
#         scale_fill_manual(values=my_colors) +
#         theme(axis.text.x = element_blank()) 
# vtp_pie 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variant_type_gisaid.pdf",  
#     height =5.83,  width =8.27)
# vtp_pie
# dev.off()
# 
# 
# # Total unique variants per Region class
# title <- "REGION CLASS"
# Var1 <- uvar$Gene_Coding
# Var1[which(Var1=="")] <- "NON-CODING"
# unique(Var1)
# # "NON-CODING" "CODING"  
# rc <- as.data.frame(table(Var1))
# rcp <- ggplot(rc) +
#         geom_col(aes(Var1, Freq)) +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of variants") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# rcp
# 
# #pie
# rcp_bp <- ggplot(rc, aes(Var1, Freq)) +
#         aes(x="", y=Freq, fill=Var1) +
#         theme(legend.title = element_blank()) +
#         geom_bar(width = 1, stat = "identity") 
# rcp_bp 
# rcp_pie <- rcp_bp + 
#         coord_polar("y", start=0) +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         blank_theme + 
#         scale_fill_manual(values=my_colors) +
#         theme(axis.text.x = element_blank()) 
# rcp_pie 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_class_gisaid.pdf",  
#     height =5.83,  width =8.27)
# rcp_pie 
# 
# dev.off()
# 
# 
# 
# # Total unique variants per variant effect
# title <- "VARIANT EFFECT"
# Var1 <- uvar$Effect
# unique(Var1)
# Var1[which(Var1==".")] <- "NON-CODING"
# unique(Var1)
# ve <- as.data.frame(table(Var1))
# ve <- mutate(ve, Var1=fct_relevel(Var1,
#                                   "NON-CODING", 
#                                   "NON_SYNONYMOUS_START" ,              
#                                   "START_LOST" ,                          "STOP_GAINED"  ,                       
#                                   "NON_SYNONYMOUS_CODING" ,               "SYNONYMOUS_CODING"  ,                 
#                                   "SPLICE_SITE_REGION+SYNONYMOUS_CODING" ,"STOP_LOST+SPLICE_SITE_REGION" ,       
#                                   "SPLICE_SITE_REGION+SYNONYMOUS_STOP"  
# ))
# 
# vep <- ggplot(ve) +
#         geom_col(aes(Var1, Freq)) +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of variants") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vep
# 
# #pie
# vep_bp <- ggplot(ve, aes(Var1, Freq)) +
#         aes(x="", y=Freq, fill=Var1) +
#         theme(legend.title = element_blank()) +
#         geom_bar(width = 1, stat = "identity") 
# vep_bp 
# vep_pie <- vep_bp  + 
#         coord_polar("y", start=0) +
#         ggtitle(title) +
#         blank_theme + 
#         scale_fill_manual(values=my_colors) +
#         theme(axis.text.x = element_blank()) 
# vep_pie 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/var_effect_gisaid.pdf",  
#     height =5.83,  width =8.27)
# vep_pie 
# 
# dev.off()
# 
# 
# 
# # Total unique variants per functional class
# title <- "FUNCTIONAL CLASS"
# Var1 <- uvar$Functional_Class
# unique(Var1)
# Var1[which(Var1=="")] <- "NON-CODING"
# unique(Var1)
# fc <- as.data.frame(table(Var1))
# fc <- mutate(fc, Var1=fct_relevel(Var1,
#                                    "NON-CODING", "NONSENSE" , "MISSENSE", "SILENT"
# ))
# fcp <- ggplot(fc) +
#         geom_col(aes(Var1, Freq)) +
#         theme_minimal() +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of variants") +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# fcp
# 
# #pie
# fcp_bp <- ggplot(fc, aes(Var1, Freq)) +
#         aes(x="", y=Freq, fill=Var1) +
#         theme(legend.title = element_blank()) +
#         geom_bar(width = 1, stat = "identity") 
# fcp_bp
# fcp_pie <- fcp_bp  + 
#         coord_polar("y", start=0) +
#         ggtitle(title) +
#         blank_theme + 
#         scale_fill_manual(values=my_colors) +
#         theme(axis.text.x = element_blank()) 
# fcp_pie
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/fun_class_gisaid.pdf",  
#     height =5.83,  width =8.27)
# fcp_pie 
# 
# dev.off()
# 
# 
# # 
# # pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_stats_all_pies.pdf", 
# #     width = 26, height = 8)
# # #grid.arrange(ep, nrow=2, ncol=3 ) #
# # # using cowplot this would look better
# # grid.arrange(vtp_pie, rcp_pie,
# #              vep_pie,fcp_pie, 
# #              ncol=3, nrow=3,
# #              layout_matrix=rbind(
# #                      c(1,2),
# #                      c(3,4)
# #                    )) 
# # dev.off()
# # 
# 
# ####################### Region query 
# utrs_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/utr_coordinates.txt")
# genes_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/gene_coordinates.txt")
# cds_table  <- read_csv("~/repolab/work/virusbeacon/genome_annotation/cds_coordinates.txt")
# mature_proteins_table  <- read.table("~/repolab/work/virusbeacon/genome_annotation/protein_coordinates.txt")
# colnames(mature_proteins_table)[5] <- "protein_name"
# #mature_proteins_table  <- read.table("mature_peptide_annot.txt")
# #colnames(mature_proteins_table)[3] <- "protein_name"
# # mature_proteins_table <- read_csv("~/repolab/work/virusbeacon/mature_proteins.csv",
# #                                   col_names = T)
# stem_loops_table <- read_csv("~/repolab/work/virusbeacon/genome_annotation/sl_coordinates.txt")
# 
# region_filter <- function(region_id) { # region_type=utr, 
#         # if (region_type=="utr")   { 
#         if (region_id %in% utrs_table$utr_name)   { 
#                 table_type <- utrs_table
#                 region_name <- "utr_name"
#         }
#         
#         #if (region_type=="gene")   { 
#         if (region_id %in% genes_table$gene_name)   { 
#                 table_type <- genes_table
#                 region_name <- "gene_name"
#         }
#         
#         #if (region_type=="cds")   { 
#         #if (region_id %in% proteins_table$protein_name)   { 
#         if (region_id %in% mature_proteins_table$protein_name)   { 
#                 table_type <- mature_proteins_table
#                 region_name <- "protein_name"
#         }
#         
#         #if (region_type=="sl")   { 
#         if (region_id %in% stem_loops_table$stem_loop_name)   { 
#                 table_type <- stem_loops_table
#                 region_name <- "stem_loop_name"
#         }
#         range <- seq(filter(table_type, eval(as.name( region_name))==region_id)$start, 
#                      filter(table_type, eval(as.name( region_name))==region_id)$end)
#         
#         range
# }
# 
# # Graph Number of mutations per genomic region  -------------------------------------------------------------------------
# # Genes
# 
# # Filter on the fly
# 
# genomic_regions <-  genes_table$gene_name
# # number of different variants
# # genomic_regions <- c(utrs_table$utr_name, 
# #                      #stem_loops_table$stem_loop_name,
# #                      genes_table$gene_name
# #                      #unique(proteins_table$protein_name)
# # )
# 
# n <- sapply(genomic_regions, function(q) {
#         #nrow(filter(uvar, position %in% region_filter(region_id = q)))
# length(unique(filter(uvar, position %in% region_filter(region_id = q))$variant))
# })
# # FIXME: making stack bars for S vs NS
# s <-  sapply(genomic_regions, function(q) {
#         length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="SILENT")$variant))
# })
# 
# ns <-  sapply(genomic_regions, function(q) {
#         length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE")$variant))
# })
# 
# dat <- data.frame(reg=genomic_regions, count=n, s_count =s, ns_count=ns)
# 
# dat <-  mutate(dat, reg=fct_relevel(reg,   
#                                     #"5'UTR", "ORF1ab" , 
#                                     #"fsSE SL1" ,  "fsSE SL2" , 
#                                     "ORF1ab",
#                                     "S", "ORF3a", "E", "M", "ORF6", "ORF7a", 
#                                     "ORF7b", "ORF8", "N", "ORF10"  
#                                     #"3utr ps SL1", "3utr ps SL2", "s2m" ,
#                                     #"3'UTR" 
#                                     ))
# 
# 
# vgenes <- ggplot(dat, aes(reg, count)) +
#         #geom_col(fill=Class) +
#         geom_col(aes(reg, count, fill="blue")) +
#         geom_col(aes(reg, s_count, fill="orangered2")) + # "honeydew3"
#         theme_minimal() +
#         ggtitle("Variants per gene") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         theme(legend.title = element_blank()) +
#         scale_fill_manual(values=c("orangered2", "blue"), labels=c("NS", "S")) +
#         geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vgenes
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_gene_gisaid.pdf",  
#     height =5.83,  width =8.27)
# vgenes
# dev.off()
# 
# 
# 
# # Graph Number of mutations per genomic region  -------------------------------------------------------------------------
# # Proteins - CDS + mat proteins
# 
# # Filter on the fly
# 
# genomic_regions <-  as.character(mature_proteins_table$protein_name)
# genomic_regions <- genomic_regions[-c(1,2)] # remove polyproteins
# 
# # Number of variants per region - Codig region : CDS+ mature proteins
# n <- sapply(genomic_regions, function(q) {
#         #nrow(filter(uvar, position %in% region_filter(region_id = q)))
#         length(unique(filter(uvar, position %in% region_filter(region_id = q))$variant))
# })
# 
# # FIXME: making stack bars for S vs NS
# s <-  sapply(genomic_regions, function(q) {
#    length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="SILENT")$variant))
# })
#  
# ns <-  sapply(genomic_regions, function(q) {
#         length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE")$variant))
# })
# 
# aa <-   sapply(genomic_regions, function(q) {
#         length(unique(filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE", Amino_Acid_Change!="")$Amino_Acid_Change
#           ))
# })
# 
# # skip for stacked chart
# dat <- data.frame(reg=genomic_regions, count=n, ns_count=ns, s_count=s, aa_changes=aa)
# 
# # change names for abbrev
# dat$reg <- as.character(dat$reg)
# dat$reg[1] <- "leader"
# dat$reg[5] <- "3C-like"
# dat$reg[12] <- "RdRp"
# dat$reg[14] <- "3'-5' exoN"
# dat$reg[15] <- "endoRNAse"
# dat$reg[16] <- "2'-o-MT"
# dat$reg[17] <- "Spike"
# dat$reg[18] <- "ORF3a protein"
# dat$reg[19] <- "envelop"
# dat$reg[20] <- "membrane"
# dat$reg[21] <- "ORF6 protein"
# dat$reg[22] <- "ORF7a protein"
# dat$reg[23] <- "ORF7b"
# dat$reg[24] <- "ORF8 protein"
# dat$reg[25] <- "nucleocapsid"
# dat$reg[26] <- "ORF10 protein"
# #rownames(dat) <- names(t)
# dat <-  mutate(dat, reg=fct_relevel(reg,   
#                                     "leader", 
#                                     #"pp1a" , "pp1ab",
#                                     "nsp2", "nsp3", "nsp4", "3C-like", 
#                                     "nsp6", "nsp7","nsp8", "nsp9", "nsp10",
#                                     "nsp11", "RdRp",  "helicase",
#                                     "3'-5' exoN", "endoRNAse", "2'-o-MT",
#                                     "Spike", "ORF3a protein", "envelop", "membrane",
#                                     "ORF6 protein", "ORF7a protein", "ORF7b", "ORF8 protein",
#                                     "nucleocapsid", "ORF10 protein"
# ))
# 
# dat 
# 
# vprot <- ggplot(dat, aes(reg, count)) +
#         geom_col(aes(x=reg, y=count, fill="blue")) +
#         geom_col(aes(x=reg, y=s_count, fill="orangered2")) +
#         #geom_col(aes(reg, aa_changes), fill="dodgerblue4") +
#         theme_minimal() +
#         ggtitle("Variants per mature protein") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         theme(legend.title = element_blank()) +
#         scale_fill_manual(values=c("orangered2", "blue"), labels=c("NS", "S")) +
#         #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vprot
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein_gisaid.pdf",  height =5.83,  width =8.27)
# vprot
# dev.off()
# 
# 
# # Total unique aminoacid changes per region
# aa <- ggplot(dat, aes(reg, aa_changes)) +
#         geom_col(aes(reg, aa_changes), fill="dodgerblue4") +
#         theme_minimal() +
#         ggtitle("AA changes per mature protein") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         #theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Unique aa changes") +
#         theme(legend.title = element_blank()) +
#         geom_text(label=dat$aa_changes, vjust=-0.1, size=2.5, colour = "red") +
#         #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# aa
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/aa_per_protein_gisaid.pdf",  height =5.83,  width =8.27)
# aa
# dev.off()
# 
# 
# 
# 
# # FIXED
# # S, NS stacked with legend
# dat <- data.frame(reg=genomic_regions, NS=ns, S=s)
# 
# 
# # change names for abbrev
# dat$reg <- as.character(dat$reg)
# dat$reg[1] <- "leader"
# dat$reg[5] <- "3C-like"
# dat$reg[12] <- "RdRp"
# dat$reg[14] <- "3'-5' exoN"
# dat$reg[15] <- "endoRNAse"
# dat$reg[16] <- "2'-o-MT"
# dat$reg[17] <- "Spike"
# dat$reg[18] <- "ORF3a protein"
# dat$reg[19] <- "envelop"
# dat$reg[20] <- "membrane"
# dat$reg[21] <- "ORF6 protein"
# dat$reg[22] <- "ORF7a protein"
# dat$reg[23] <- "ORF7b"
# dat$reg[24] <- "ORF8 protein"
# dat$reg[25] <- "nucleocapsid"
# dat$reg[26] <- "ORF10 protein"
# #rownames(dat) <- names(t)
# dat <-  mutate(dat, reg=fct_relevel(reg,   
#                                     "leader", 
#                                     #"pp1a" , "pp1ab",
#                                     "nsp2", "nsp3", "nsp4", "3C-like", 
#                                     "nsp6", "nsp7","nsp8", "nsp9", "nsp10",
#                                     "nsp11", "RdRp",  "helicase",
#                                     "3'-5' exoN", "endoRNAse", "2'-o-MT",
#                                     "Spike", "ORF3a protein", "envelop", "membrane",
#                                     "ORF6 protein", "ORF7a protein", "ORF7b", "ORF8 protein",
#                                     "nucleocapsid", "ORF10 protein"
# ))
# 
# # dat <- as_tibble(dat)
# dat <-gather(dat, "class", "count", -reg)
# 
# 
# stacked <- ggplot(dat, aes(fill=class, y=count, x=reg))+
#         geom_bar(position = "stack", stat = "identity" ) +
#         theme_minimal() +
#         ggtitle("Variants per mature protein") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#    
#         theme(legend.title = element_blank()) +
#         scale_fill_manual(values = c("honeydew4", "honeydew3")) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# 
# stacked
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_protein_gisaid2.pdf",  height =5.83,  width =8.27)
# stacked
# dev.off()
# 
# 
# 
# # # eX
# # library(viridis)
# # library(hrbrthemes)
# # 
# # specie <- c(rep("sorgho", 3), rep("poacee", 3), rep("banana", 3), rep("triticum", 3) )
# # condition <- rep(c("normal", "stress", "N"), 4)
# # value <- abs(rnorm(12,0,15))
# # data <- data.frame(specie, condition, value)
# # 
# # specie <- dat$reg
# # condition <- c("S", "NS")
# # value <- dat$ns_count
# # 
# # ggplot(data, aes(fill=condition, y=value, x=specie))+
# #         geom_bar(position = "stack", stat = "identity" )# 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# reg <- table(uvar$region, useNA = "ifany")
# dat <- data.frame(reg)
# 
# reg <- ggplot(dat, aes(Var1, Freq)) +
#         geom_col(fill="hotpink4", position = "identity") +
#         theme_minimal() +
#         ggtitle("Unique variants per region class") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         #geom_text(label=dat$Freq, vjust=-0.1, size=2.5, colour = "red") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# reg
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/region_class_gisaid.pdf",  height =5.83,  width =8.27)
# reg
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# # CODING
# c_positions <- unique(uvar$position[which(uvar$Gene_Coding=="CODING")])
# length(c_positions) # 13494
# 
# # NON CODING
# nc_positions <- unique(uvar$position[which(uvar$Gene_Coding=="")])
# length(nc_positions) # 493
# 
# genomic_regions <- utrs_table$utr_name
# utr_positions <- sapply(genomic_regions, function(q) {
#         unique(filter(uvar, position %in% region_filter(region_id = q))$position)
# })
# length(unlist(utr_positions)) #  417
# 
# sum(unlist(utr_positions) %in% nc_positions) # 417
# 
# intergenic <- setdiff(nc_positions, unlist(utr_positions))
# length(intergenic) # 76
# 
# # utrs
# genomic_regions1 <-  utrs_table$utr_name
# n1 <- sapply(genomic_regions, function(q) {
#         nrow(filter(uvar, position %in% region_filter(region_id = q)))
# })
# # intergenic
# genomic_regions2 <-  intergenic 
# n2 <- sum(sapply(genomic_regions2, function(q) {
#         nrow(filter(uvar, position %in% q))
# }))
# # coding
# genomic_regions3 <-  c_positions
# n3 <- sum(sapply(genomic_regions3, function(q) {
#         nrow(filter(uvar, position %in% q))
# }))
# 
# # number of different variants
# # genomic_regions <- c(utrs_table$utr_name, 
# #                      #stem_loops_table$stem_loop_name,
# #                      genes_table$gene_name
# #                      #unique(proteins_table$protein_name)
# # )
# 
# 
# dat1 <- data.frame(reg=genomic_regions, count=n)
# dat2 <- data.frame(reg="intergenic", count=n2)
# dat3 <- data.frame(reg="coding", count=n3)
# 
# 
# dat<-   rbind(dat1 , dat2)
# 
# 
# dat <-  mutate(dat, reg=fct_relevel(reg,   
#                                     "5'UTR",
#                                     #"coding",
#                                     "intergenic",
#                                     "3'UTR" 
# ))
# 
# 
# vnc <- ggplot(dat, aes(reg, count)) +
#         geom_col(fill="green", position = "dodge") +
#         theme_minimal() +
#         ggtitle("Variants per genomic region") +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vnc
# 
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/variants_per_nc_gisaid.pdf",  
#     height =5.83,  width =5.83)
# vnc
# dev.off()
# 
# 
# dat <-   rbind(dat1 , dat2, dat3)
# #pie
# title <- "VARIANTS PER GENOMIC REGION" 
# vnc_bp <- ggplot(dat, aes(reg, count)) +
#         aes(x="", y=count, fill=reg) +
#         theme(legend.title = element_blank()) +
#         geom_bar(width = 1, stat = "identity") 
# vnc_bp 
# vnc_pie <- vnc_bp  + 
#         coord_polar("y", start=0) +
#         ggtitle(title) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         blank_theme + 
#         scale_fill_manual(values=my_colors) +
#         theme(axis.text.x = element_blank()) 
# vnc_pie 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/variants_stats/var_region_gisaid.pdf",  
#     height =5.83,  width =8.27)
# vnc_pie 
# 
# dev.off()
# 
# 
# 
# # Functional /STEM LOOPS
# 
# stem_loops_table$stem_loop_name <- c("fsSE SL1", "fsSE SL2", "3utr ps SL1", "3utr ps SL2", "s2m")
# 
# genomic_regions <- stem_loops_table$stem_loop_name
# 
# n <- sapply(genomic_regions, function(q) {
#         nrow(filter(var, position %in% region_filter(region_id = q)))
# })
# 
# 
# dat<- data.frame(reg=genomic_regions, count=n)
# #rownames(dat) <- names(t)
# dat <-  mutate(dat, reg=fct_relevel(reg,   
#                                     #"5'UTR", 
#                                     "fsSE SL1" ,  "fsSE SL2" , 
#                                     "3utr ps SL1", "3utr ps SL2", "s2m" 
#                                     #"3'UTR"
# ))
# 
# 
# vsl <- ggplot(dat, aes(reg, count)) +
#         geom_col(fill="honeydew3") +
#         ggtitle("Variants per stem loops") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         #theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Unique variant counts") +
#         geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# vsl
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_sl.pdf",  height =5.83,  width =8.27)
# vsl
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 










