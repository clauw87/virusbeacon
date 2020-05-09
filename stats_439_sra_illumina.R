library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)

# All variants in 439 first samples in viral beacon 
var <-  read_delim("~/repolab/work/virusbeacon/439_genomes_claudia.txt.csv",
                   delim = ";",
                 col_names = T)

nrow(var) # 36514


var$variant <- sapply(1:nrow(var), function(v){
        paste(as.character(rep$position[v]), rep$alternate[v], sep = ">")
})
length(var$variant) # 36514
length(unique(var$variant)) # 34951
sum(duplicated(var$variant))  # 1563



var$samplecount <- unlist(lapply(var$variant, function(v){
        sum(var$sampleMatchingCount[which(var$variant==v)]) 
}))



uvar <- var[which(!(duplicated(var$variant))),]
# Number of unique variant found
nrow(uvar) # 34951

# Number of samples with variants
# will never now

# Graph Number of mutations per genomic region  -------------------------------------------------------------------------
# range
# range <- seq(filter(utrs_table, utr_name==region)$start, 
#              filter(utrs_table, utr_name==region)$end)
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


# Filter on the fly
# number of different variants
genomic_regions <- c(utrs_table$utr_name, 
                     #stem_loops_table$stem_loop_name,
                     genes_table$gene_name
                     #unique(proteins_table$protein_name)
                     )

# t <- data.frame(reg=genomic_regions, uvar=rep(NA, length(genomic_regions)))
# for (i in genomic_regions){
# query_range <- region_filter(region_type = "utr", region_id = region_id)
# t <- rbind(t, )
# # 166
# }

n <- sapply(genomic_regions, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
        })


dat<- data.frame(reg=genomic_regions, count=n)
#rownames(dat) <- names(t)
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                           "5'UTR", "ORF1ab" , 
                                    #"fsSE SL1" ,  "fsSE SL2" , 
                                            "S", "ORF3a", "E", "M", "ORF6", "ORF7a", 
                                           "ORF7b", "ORF8", "N", "ORF10",  
                                    #"3utr ps SL1", "3utr ps SL2", "s2m" ,
                                    "3'UTR" ))


nuvar <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3") +
        ggtitle("Variants per genomic region") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
nuvar
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_region.pdf",  height =5.83,  width =8.27)
nuvar
dev.off()



# Sequence with variant counts
f <- sapply(genomic_regions, function(q) {
        sum(filter(uvar, position %in% region_filter(region_id = q))$samplecount)
})

dat<- data.frame(reg=genomic_regions, freq=f)

# Graph Number of sequences with aa changes per protein  -------------------------------------------------------------------------
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "5'UTR", "ORF1ab" , 
                                    #"fsSE SL1" ,  "fsSE SL2" , 
                                    "S", "ORF3a", "E", "M", "ORF6", "ORF7a", 
                                    "ORF7b", "ORF8", "N", "ORF10",  
                                    #"3utr ps SL1", "3utr ps SL2", "s2m" ,
                                    "3'UTR" ))

freq <- ggplot(dat, aes(reg, freq)) +
        geom_col( fill="honeydew3") +
        ggtitle("Freq variants per region") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Sequence count") +
        theme_minimal() +
        geom_text(label=dat$freq, vjust=-0.1, size=2.5, colour = "red") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
freq 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_region_freq.pdf",  height =5.83,  width =8.27)
freq
dev.off()


#############################################################################
# Proteins
mature_proteins_table <- read_csv("~/repolab/work/virusbeacon/mature_proteins.csv",
                                    col_names = T)
colnames(mature_proteins_table)[5] <- "protein_name"
proteins <- mature_proteins_table$protein_name
proteins <- proteins[-c(1,2)]
n <- sapply(proteins, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})


dat<- data.frame(reg=proteins, count=n)
#rownames(dat) <- names(t)
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "leader", 
                                    #"pp1a" , "pp1ab",
                                    "nsp2", "nsp3", "nsp4", "3C-like", 
                                    "nsp6", "nsp7","nsp8", "nsp9", "nsp10",
                                    "nsp11", "RdRp",  "helicase",
                                    "3'-5' exoN", "enoRNAse", "2'-o-MT",
                                    "Spike", "ORF3ap", "envelop", "membrane",
                                    "ORF6p", "ORF7ap", "ORF7bp", "ORF8p",
                                    "nucleocapsid", "ORF10p"
                                    ))


pnuvar <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3") +
        ggtitle("Variants per mature protein") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
pnuvar
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_protein.pdf",  height =5.83,  width =8.27)
pnuvar
dev.off()




#############################################################################
# Non-coding regions / intergenic are missing
#region_id= "5'UTR"
#aliases for stem loops
stem_loops_table$stem_loop_name <- c("fsSE SL1", "fsSE SL2", "3utr ps SL1", "3utr ps SL2", "s2m")


non_coding_regions <- c(utrs_table$utr_name, stem_loops_table$stem_loop_name)

n <- sapply(non_coding_regions, function(q) {
        nrow(filter(uvar, position %in% region_filter(region_id = q)))
})


dat<- data.frame(reg=non_coding_regions, count=n)
#rownames(dat) <- names(t)
dat <-  mutate(dat, reg=fct_relevel(reg,   
                                    "5'UTR", 
                                    "fsSE SL1" ,  "fsSE SL2" , 
                                    "3utr ps SL1", "3utr ps SL2", "s2m" ,
                                    "3'UTR"
))


ncuvar <- ggplot(dat, aes(reg, count)) +
        geom_col(fill="honeydew3") +
        ggtitle("Variants per non-coding region") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        #theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Unique variant counts") +
        geom_text(label=dat$count, vjust=-0.1, size=2.5, colour = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
ncuvar
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/variants_per_ncregion.pdf",  height =5.83,  width =8.27)
ncuvar
dev.off()

