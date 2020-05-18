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

# All variants in 1094 first samples in viral beacon 
# var <-  read_delim("~/repolab/work/virusbeacon/439_genomes_claudia.txt.csv",
#                    delim = ";",
#                    col_names = T)
setwd( "/Users/claudiavasallovega/repolab/work/virusbeacon")
variant_file <- system("ls *.annot.variants.data | awk '{print $1}'", intern = TRUE)
var <- fread(input = variant_file, sep = ";") 

# to get the number of samples of the run (less 1 bc of header)
number_samples <- as.numeric(system("wc -l 1094.annot.variants.data | awk '{print $1}'", intern = TRUE))-1
number_samples
#  55911

colnames(var)
# [1] "datasetId"           "Refseq"              "position"           
# [4] "variantId"           "reference"           "alternate"          
# [7] "end"                 "svType"              "svLength"           
# [10] "variantCount"        "callCount"           "sampleCount"        
# [13] "frequency"           "Effect"              "Effect_Impact"      
# [16] "Functional_Class"    "Codon_Change"        "Amino_Acid_Change"  
# [19] "Amino_Acid_length"   "Gene_Name"           "Transcript_BioType" 
# [22] "Gene_Coding"         "Transcript_ID"       "Exon_Rank"          
# [25] "Genotype"            "sampleMatchingCount"
################################################################################################################
# "datasetId"
head(unique(var$datasetId)) # 1 (not using)
#  "Refseq" 
head(unique(var$Refseq)) # 1 (only one refseq: ] "NC_045512")
# "position"   
head(unique(var$position)) #  28754 #  27 34 39 40 44 45
# "variantId" 
head(unique(var$variantId)) # "." (not using)
#  "reference"  
head(unique(var$reference)) # 4 "A" "C" "G" "T"
#  "alternate" 
head(unique(var$alternate)) # 4 "A" "C" "G" "T"
#  "end" 
head(unique(var$end)) # 28754 # 27 34 39 40 44 45
# "svType"
head(unique(var$svType))  # "SNP"
# "svLength"  
head(unique(var$svLength))  # NA (shouldn't it be 1)
# "variantCount"  
head(unique(var$variantCount)) #  2  4 26 38  6 30 # ?
max(unique(var$variantCount)) # 1266
#  "callCount"
head(unique(var$callCount)) #  2  4 26 40  6 30 # 
max((unique(var$callCount))) # 1276
# "sampleCount" 
head(unique(var$sampleCount)) # 1  2 13 20  3 15 
max(unique(var$sampleCount)) # 638
# "sampleMatchingCount"
head(unique(var$sampleMatchingCount)) # 1  2 13 19  3 15  # in how many samples appear the variant?
max(unique(var$sampleMatchingCount)) # 633
############## STATS #######################################
# Positions with variants
length(unique(var$position)) #  28754

# Proportion of # Positions with variants
28754/29903 *100


# Variants 


# Unique variants
var$variant <- sapply(1:nrow(var), function(v){
        paste(as.character(var$position[v]), var$alternate[v], sep = ">")
})
length(var$variant) # 55911
length(unique(var$variant)) # 51313
sum(duplicated(var$variant))  # 4598 *(These appear in separate original vcfs )

# See duplicated frequency
example61 <- read(file = "/Users/claudiavasallovega/repolab/work/virusbeacon/example61.rows.txt", stringsAsFactors = FALSE)


example5387 <- read(file = "/Users/claudiavasallovega/repolab/work/virusbeacon/example5387.rows.txt", stringsAsFactors = FALSE)








var$MyCount <- unlist(lapply(var$variant, function(v){
        sum(var$sampleMatchingCount[which(var$variant==v)]) 
}))


var$MyFrequency <- unlist(lapply(var$variant, function(v){
        sum(var$frequency[which(var$variant==v)]) 
}))

# See duplicated
head(var[duplicated(var$variant)]$position) # 61

dups <-  var[duplicated(var$variant)]$position
trips <- var[duplicated(var$variant)]$position

dup_trips <- dups[dups%in% trips]

        vp$Var1[which(vp$Freq==3)]
filter(var, position==61)

filter(var, position==5387)
# What happens with variants when more than one is found in a sample?????


# Unique variants
uvar <- var[which(!(duplicated(var$variant))),]

# Number of unique variant found
nrow(uvar) #  51313






# Variant Type
unique(uvar$svType) # "SNP"


# Frequency
title <- "FREQUENCY VARIANTS PER POSITION"
# dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency)
dat <- data.frame(pos=uvar$position, freq=uvar$MyFrequency, class=uvar$Functional_Class, stringsAsFactors = F)
dat$class[which(dat$class=="MISSENSE"|dat$class=="NONSENSE")] <- "NS"
dat$class[which(dat$class!="NS")] <- "S"
#dat_sil <- filter(dat, class=="SILENT")

# Plot frequency per position
# recontrabasic needle plot
#plot(uvar$position, uvar$frequency, type = "h") 

needle <- ggplot(dat, 
                 aes(x=pos, ymax=freq, ymin=0)) +
        geom_linerange(color="orangered2") +
        #aes(x=pos, ymax=freq, ymin=0, color=class) +
        #geom_linerange() +
        #scale_color_manual(values = c("orangered2", "blue")) +
        theme_minimal() +
        ggtitle(title) +
        labs(x="Genomic Position") +
        theme(legend.title = element_blank())+
        labs(y="Freq") 
needle 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/needle.pdf",  height =5.83,  width =8.27)
needle 
dev.off()



# Total unique variants per position
title <- "VARIANTS PER POSITION"
Var1 <- uvar$position
unique(Var1)
vp <-  as.data.frame(table(Var1))
vp$Var1 <- as.numeric(vp$Var1)
vp$Freq <- as.numeric(vp$Freq)

vpp <- ggplot(vp,
        aes(x=Var1, ymax=Freq, ymin=0)) + 
        geom_linerange(color="orangered2") +
        theme_minimal() +
        ggtitle(title) +
        labs(x="Genomic Position") +
        labs(y="Number of variants") 
vpp
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/needle_vpp.pdf",  height =5.83,  width =8.27)
vpp
dev.off()





########
#########   # Variants annotation
##########
# Region Class
unique(uvar$Gene_Coding) # "" (this is noncoding)      "CODING"   

# Functional Class
unique(uvar$Functional_Class) # ""  (this is noncoding)       "MISSENSE" "NONSENSE" "SILENT"  

# "Amino_Acid_Change"  
length(unique(uvar$Amino_Acid_Change)) # 28916 distinct aminoacid changes across 

#"Gene_Name" 
unique(uvar$Gene_Name) # ""  (this is noncoding) : all genes     "orf1ab" "S"      "ORF3a"  "E"      "M"      "ORF6"   "ORF7a"  "ORF7b"  "ORF8"   "N"      "ORF10" 

# unique(uvar$Genotype)



############################################################################

# General statistics graphs

# Total unique variants per frequency
plot(vf$Freq, vf$Var1)
title <- "FREQUENCY"
Var1 <- uvar$frequency
hist(uvar$frequency, breaks=c(0, 0.001,0.01,0.1, 0.6))
hist( uvar$frequency,breaks=1000 )
unique(Var1)
#quantiles <- quantile(uvar$frequency, prob=seq(0,1, length = 5), type = 5)
# Var1 <- cut2(Var1, cuts = as.numeric(quantiles))
vf <- as.data.frame(table(Var1), stringsAsFactors = F)
vf$group <- rep(NA, length(vf$Var1))
vf$group <- ifelse(vf$Var1<=0.001, "<0.001",vf$group) 
vf$group <- ifelse(vf$Var1>0.001&vf$Var1<=0.01, "0.001-0.01", vf$group) 
vf$group <- ifelse(vf$Var1>0.01&vf$Var1<=0.1, "0.01-0.1", vf$group) 
vf$group <- ifelse(vf$Var1>0.1, ">0.1", vf$group) 


# vf <- vf  %>% mutate(quantile=ntile(Var1,10))
# vf$Freq <- as.numeric(vf$Freq)
vf$Var1 <- as.numeric(vf$Var1)
vf$group <- as.character(vf$group)

vf <- mutate(vf, group=fct_relevel(group,
                                   "<0.001", "0.001-0.01", "0.01-0.1",">0.1"
))

vfg <- vf %>% group_by(group) %>% summarise_at(vars(Freq), funs(sum(., na.rm=TRUE)))

vfg$group <- as.character(vfg$group)

vfg <- mutate(vfg, group=fct_relevel(group,
                                     "<0.001", "0.001-0.01", "0.01-0.1",">0.1"
))

vfp <- ggplot(vfg, aes(x=group, y=Freq)) +
        #geom_col(aes(x=quantile, y=Freq)) +
        geom_col(fill="dodgerblue4") +
        ggtitle(title) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme_minimal() +
        geom_text(aes(label=Freq, vjust=-0.1), size=2.5) +
        theme(axis.text.x = element_text(angle = 0, hjust=1, vjust = 1))
vfp




# Total unique variants per variant type
vt <- data.frame(Var1=c("SNP","INDEL" ), Freq=c(34951, 0))
vt <-  mutate(vt, Var1=fct_relevel(Var1,   
                                     "SNP","INDEL"))
vtp <- ggplot(vt) +
        geom_col(aes(Var1, Freq)) +
        ggtitle("VARIANT TYPE") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vtp

#pie
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
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
vtp_pie 


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
        blank_theme + 
        scale_fill_manual(values=my_colors) +
        theme(axis.text.x = element_blank()) 
rcp_pie 



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
vep_pie 




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
        ggtitle(title) +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of variants") +
        theme_minimal() +
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




pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_stats_all_pies.pdf", width = 16, height = 8)
#grid.arrange(ep, nrow=2, ncol=3 ) #
# using cowplot this would look better
grid.arrange(vtp_pie, rcp_pie,
             vep_pie,fcp_pie, 
             ncol=3, nrow=3,
             layout_matrix=rbind(
                     c(1,2),
                     c(3,4)
                   ), left) 
dev.off()


####################### Region query 
utrs_table <- read_csv("utr_coordinates.txt")
genes_table <- read_csv("gene_coordinates.txt")
cds_table  <- read_csv("cds_coordinates.txt")
mature_proteins_table  <- read.table("protein_coordinates.txt")
colnames(mature_proteins_table)[5] <- "protein_name"
#mature_proteins_table  <- read.table("mature_peptide_annot.txt")
#colnames(mature_proteins_table)[3] <- "protein_name"
# mature_proteins_table <- read_csv("~/repolab/work/virusbeacon/mature_proteins.csv",
#                                   col_names = T)
stem_loops_table <- read_csv("sl_coordinates.txt")

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
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})

dat <- data.frame(reg=genomic_regions, count=n)

dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    #"5'UTR", "ORF1ab" , 
                                    #"fsSE SL1" ,  "fsSE SL2" , 
                                    "ORF1ab",
                                    "S", "ORF3a", "E", "M", "ORF6", "ORF7a", 
                                    "ORF7b", "ORF8", "N", "ORF10"  
                                    #"3utr ps SL1", "3utr ps SL2", "s2m" ,
                                    #"3'UTR" 
                                    ))


vgenes <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3") +
        ggtitle("Variants per gene") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vgenes
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_gene.pdf",  height =5.83,  width =8.27)
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
        length(unique(
       filter(uvar, position %in% region_filter(region_id = q), Functional_Class=="MISSENSE"|Functional_Class=="NONSENSE", Amino_Acid_Change!="")$Amino_Acid_Change
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

vprot <- ggplot(dat ) +
        geom_col(aes(reg, count), fill="honeydew4") +
        geom_col(aes(reg, s_count), fill="honeydew3") +
        #geom_col(aes(reg, aa_changes), fill="dodgerblue4") +
        ggtitle("Variants per mature protein") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vprot
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_protein.pdf",  height =5.83,  width =8.27)
vprot
dev.off()


# Total unique aminoacid changes per region
aa <- ggplot(dat) +
        geom_col(aes(reg, aa_changes), fill="dodgerblue4") +
        ggtitle("AA changes per mature protein") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique aa changes") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
aa
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/aa_per_protein.pdf",  height =5.83,  width =8.27)
aa
dev.off()


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
        ggtitle("Variants per mature protein") +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = c("honeydew4", "honeydew3")) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))

stacked
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_protein.pdf",  height =5.83,  width =8.27)
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
        ggtitle("Unique variants per region class") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        #geom_text(label=dat$Freq, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
reg
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/region_class.pdf",  height =5.83,  width =8.27)
reg
dev.off()


# NON CODING

nc_positions <- unique(uvar$position[which(uvar$Gene_Coding=="")])
length(nc_positions) # 504

utr_positions <- sapply(genomic_regions, function(q) {
        unique(filter(uvar, position %in% region_filter(region_id = q))$position)
})
length(unlist(utr_positions)) #  355

sum(unlist(utr_positions) %in% nc_positions) #355

intergenic <- setdiff(nc_positions, unlist(utr_positions))
length(intergenic) # 149

# utrs
genomic_regions <-  utrs_table$utr_name
n <- sapply(genomic_regions, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})
# intergenic
genomic_regions2 <-  intergenic 
n2 <- sum(sapply(genomic_regions2, function(q) {
        nrow(filter(uvar, position %in% q))
}))


# number of different variants
# genomic_regions <- c(utrs_table$utr_name, 
#                      #stem_loops_table$stem_loop_name,
#                      genes_table$gene_name
#                      #unique(proteins_table$protein_name)
# )


dat <- data.frame(reg=genomic_regions, count=n)
dat2 <- data.frame(reg="intergenic", count=n2)
 
dat<-   rbind(dat , dat2)


dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "5'UTR", 
                                    "intergenic",
                                    "3'UTR" 
))


vnc <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3", position = "dodge") +
        ggtitle("Variants per non-coding region") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vnc
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_nc.pdf",  height =5.83,  width =8.27)
vnc
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
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
vsl
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_sl.pdf",  height =5.83,  width =8.27)
vsl
dev.off()


# # Both as a grid
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_nc.pdf",  height =5.83,  width =8.27)
# vnc
# dev.off()
# 
# 
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/nc_sl.pdf",  height =5.83,  width =3.5)
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






