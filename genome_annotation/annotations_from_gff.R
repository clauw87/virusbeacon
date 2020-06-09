 # Getting table of region-coordinates - annotation from GFF file
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(Hmisc)
#library(ggbio)

# Genebank GFF file
gff <- read_delim("~/repolab/work/virusbeacon/GCF_009858895.2_ASM985889v3_genomic.gff", delim = "\t", skip = 7, col_names = F)
colnames(gff) <-c("id", "type", "region", "start", "end", "X7", "strand", "X8","annot" )

        
nrow(gff) # 33 last row empty

# strands
unique(gff$strand) # "+" NA
sum(is.na(gff$strand)) # 1
gff[which(is.na(gff$strand)),]
which(is.na(gff$strand)) # 33


# features
unique(gff$region) # "five_prime_UTR"  "gene"      "CDS"    "stem_loop"    "three_prime_UTR" NA 
sum(is.na(gff$region)) # 1


# UTRs
utrs <-filter(gff, region=="five_prime_UTR"| region=="three_prime_UTR")
utr_name <- unlist(lapply(utrs$annot, function(l){
      str_match(l, "gbkey=(.*?)$")[,2]    
}))

utr_id <- unlist(lapply(utrs$annot, function(l){
        str_match(l, "ID=id-(.*?);")[,2]    
}))

utrs_table <- data.frame(start=as.numeric(utrs$start), 
                         end=as.numeric(utrs$end), 
                         utr_name=utr_name,
                         utr_id = utr_id,
                         stringsAsFactors = F)
        
        
# five_utr <-filter(gff, region=="five_prime_UTR")
# five_utr_id <- str_match(five_utr$annot, "gbkey=(.*?)$")[,2]
# 
# three_utr <-filter(gff, region=="three_prime_UTR")
# three_utr_id <- str_match(three_utr$annot, "gbkey=(.*?)$")[,2]


# stem_loop
stem_loops <-filter(gff, region=="stem_loop")

stem_loop_id <- unlist(lapply(stem_loops$annot, function(l){
         str_match(l, "ID=id-(.*?);")[,2]    
}))
        
stem_loop_name  <- unlist(lapply(stem_loops$annot, function(l){
        str_match(l, "function=(.*?);")[,2]    
}))

stem_loop_locus_id <- unlist(lapply(stem_loops$annot, function(l){
        str_match(l, "ID=id-(.*?);")[,2]    
}))
stem_loop_gene_id <- unlist(lapply(stem_loops$annot, function(l){
        str_match(l, "GeneID:(.*?);")[,2]    
}))

stem_loop_gene_name <- unlist(lapply(stem_loops$annot, function(l){
        str_match(l, "gene=(.*?);")[,2]    
}))


stem_loops_table <- data.frame(start=as.numeric(stem_loops$start), 
                               end=as.numeric(stem_loops$end), 
                               stem_loop_name=stem_loop_name,
                               #stem_loop_id=stem_loop_id, 
                              stem_loop_locus_id=stem_loop_locus_id, 
                              stem_loop_gene_id=stem_loop_gene_id, 
                              stem_loop_gene_name=stem_loop_gene_name,
                               stringsAsFactors = F)


# genes
genes <-filter(gff, region=="gene")
length(unique(filter(gff, region=="gene")$annot)) # 11 genes
# get info from annot      
locus_id <- unlist(lapply(genes$annot, function(l){
        str_match(l, "ID=gene-(.*?);")[,2]    
}))
gene_id <- unlist(lapply(genes$annot, function(l){
        str_match(l, "GeneID:(.*?);")[,2]    
}))
gene_name <- unlist(lapply(genes$annot, function(l){
        str_match(l, "gene=(.*?);")[,2]    
}))

gene_synonym <- unlist(lapply(genes$annot, function(l){
        str_match(l, "gene_synonym=(.*?);")[,2]
}))

gene_biotype <- unlist(lapply(genes$annot, function(l){
        str_match(l, "gene_biotype=(.*?);")[,2]    
}))

genes_table <- data.frame(start=as.numeric(genes$start), 
                            end=as.numeric(genes$end), 
                            locus_id=locus_id, 
                            gene_id=gene_id, 
                            gene_name=gene_name, 
                            gene_synonym=gene_synonym, 
                            gene_biotype=gene_biotype, 
                          stringsAsFactors = F)



# CDS /proteins
cdss <-filter(gff, region=="CDS")
length(unique(cdss$annot)) # 12 proteins

# get info from annot
cdss_id <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "ID=cds-(.*?);")[,2]    
}))

protein_id <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "protein_id=(.*?)$")[,2]    
}))

protein_name <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "product=(.*?);")[,2]    
}))
protein_name[10] <- "ORF7b protein"
        
parent_locus_id <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "Parent=gene-(.*?);")[,2]    
}))

parent_gene_id <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "GeneID:(.*?);")[,2]    
}))

parent_gene_name <- unlist(lapply(cdss$annot, function(l){
        str_match(l, "gene=(.*?);")[,2]    
}))


proteins_table <- data.frame(start=as.numeric(cdss$start), 
                          end=as.numeric(cdss$end), 
                          cdss_id=cdss_id, 
                          protein_id=protein_id, 
                          protein_name=protein_name,
                          parent_locus_id=parent_locus_id,
                          parent_gene_id=parent_gene_id,
                          parent_gene_name=parent_gene_name,
                          stringsAsFactors = F)



genomic <- filter(gff, region %in% c(utrs$region, genes$region, stem_loops$region))


# # # #
# GEOM_BAR VIRAL GENOME
# coordinates
min(gff$start, na.rm = T) # 1
max(gff$end, na.rm = T) # 29903

#29903- 1 #= 29902

diff <- unlist(lapply(1:nrow(genomic), function(l){
        genomic$end[l] -  genomic$start[l]
}))
diff_scaled <- diff/29902*1000
coors<- data.frame(start=genomic$start, end=genomic$end, region_type=genomic$region, diff, diff_scaled, stringsAsFactors = F)
# # # #

# coordinates table for genomic regions

latex(genes_table, file="~/repolab/work/virusbeacon/orf_coords")     
write.csv(genes_table, "~/repolab/work/virusbeacon/gene_coordinates.txt", row.names = F)


# coordinates table for utr regions

latex(utrs_table, file="~/repolab/work/virusbeacon/utr_coords")     
write.csv(utrs_table, "~/repolab/work/virusbeacon/utr_coordinates.txt", row.names = F)

# coordinates table for stem loops
latex(stem_loops_table, file="~/repolab/work/virusbeacon/stemloops_coords")     
write.csv(stem_loops_table, "~/repolab/work/virusbeacon/sl_coordinates.txt", row.names = F)


# coordinates table for proteins
latex(proteins_table[c(1,2,5,4,8)], file="~/repolab/work/virusbeacon/proteins_coords")     
write.csv(proteins_table, "~/repolab/work/virusbeacon/cds_coordinates.txt", row.names = F)

###########################
# One tble with all genomic regions
genomic_short <- genomic[,3:5]

# genes data
genomic_short$name <- rep("NA", length(nrow(genomic_short)))
for (i in 1:nrow(genomic_short)) {
        
if (genomic_short$start[i] %in% genes_table$start) {
        genomic_short$name[i] <-  genes_table[which(genes_table$start==genomic_short$start[i]),]$gene_name
                                 
}
}

genomic_short$syn <- rep("NA", length(nrow(genomic_short)))
for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% genes_table$start) {
                genomic_short$syn[i] <-  genes_table[which(genes_table$start==genomic_short$start[i]),]$gene_synonym
            
        }
}

genomic_short$locus_id <- rep("NA", length(nrow(genomic_short)))
for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% genes_table$start) {
                genomic_short$locus_id[i] <-  genes_table[which(genes_table$start==genomic_short$start[i]),]$locus_id
                
        }
}

genomic_short$gene_id <- rep("NA", length(nrow(genomic_short)))
for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% genes_table$start) {
                genomic_short$gene_id[i] <-  genes_table[which(genes_table$start==genomic_short$start[i]),]$gene_id
                
        }
}

# utrs data
for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% utrs_table$start) {
                genomic_short$name[i] <-  utrs_table[which(utrs_table$start==genomic_short$start[i]),]$utr_name
                
        }
}

for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% utrs_table$start) {
                genomic_short$locus_id[i] <-  utrs_table[which(utrs_table$start==genomic_short$start[i]),]$utr_id
                
        }
}

# stem loops data
for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% stem_loops_table$start) {
                genomic_short$name[i] <-  stem_loops_table[which(stem_loops_table$start==genomic_short$start[i]),]$stem_loop_name
                
        }
}

for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% stem_loops_table$start) {
                genomic_short$locus_id[i] <-  stem_loops_table[which(stem_loops_table$start==genomic_short$start[i]),]$stem_loop_locus_id       
                
        }
}

for (i in 1:nrow(genomic_short)) {
        
        if (genomic_short$start[i] %in% stem_loops_table$start) {
                genomic_short$gene_id[i] <-  stem_loops_table[which(stem_loops_table$start==genomic_short$start[i]),]$stem_loop_gene_id       
                
        }
}

genomic_short_ok <- genomic_short
genomic_short_ok$region[which(genomic_short_ok$region %in% c("five_prime_UTR", "three_prime_UTR"))] <- "utr"
genomic_short_ok$region[1] <- "utr"
latex(genomic_short_ok[,c(1,2,3,4,6)], file="~/repolab/work/virusbeacon/genomic_coords", rowname = NULL)     


# All in one table better
# class | type | start | end | name | syn_alias | locus_tag | id | accession
#genes
g_table <- genes_table[,c(7,7,1,2,5,6, 3,4)]
g_table$gene_biotype <- rep("gene",11)
g_table$gene_biotype.1 <- rep("coding",11)
g_table$accession <- rep(NA,11)
colnames(g_table) <- c("class" , "type" ,  "start" ,"end", "name" ,"syn_alias", "locus_tag" , "id" , "accession")

#cds
c_table <- mature_proteins_table[,c(1,1,2,3,4,5,6, 6,11)]
colnames(c_table) <- c("class" , "type" ,  "start" ,"end", "name" ,"syn_alias", "locus_tag" , "id" , "accession")
c_table$class <- rep("cds",28)
#c_table$type <- c("joint CDS", "CDS", rep("mature peptide",16), rep("CDS",10))
c_table$type <- c("product", "product", rep("mature peptide",16), rep("product",10))
c_table$id <- rep(NA,28)
c_table$locus_tag <- rep(NA,28)
c_table$syn_alias <- c(rep(NA,28))
c_table$syn_alias[which(c_table$syn_alias=="enoRNAse")] <- "nsp15"
                  
# ut
u_table <- utrs_table[,c(3,3,1,2,3,3,4,4,4)]
colnames(u_table) <- c("class" , "type" ,  "start" ,"end", "name" ,"syn_alias", "locus_tag" , "id" , "accession")
u_table$class <- rep("non-coding",2)
u_table$type <- rep("utr",2)
u_table$syn_alias <- rep(NA,2)
u_table$id <- rep(NA,2)
u_table$accession <- rep(NA,2)
u_table$locus_tag <- rep(NA,2)

#functional_structural
s_table <- stem_loops_table[,c(3,3,1,2,3,3,4,5,6)]
colnames(s_table) <- c("class" , "type" ,  "start" ,"end", "name" ,"syn_alias", "locus_tag" , "id" , "accession")
s_table$class <- rep("functional",5)
s_table$type <- rep("stem loop",5)
slnames <- unlist(lapply(stem_loops$annot, function(l){
             str_match(l, "function=(.*?);")[,2]    
         }))
s_table$name <- slnames 
s_table$syn_alias[3] <- "3utr pk SL1"
s_table$syn_alias[4] <- "3utr pk SL2"
s_table$id <- rep(NA,5)
s_table$accession <- rep(NA,5)
s_table$locus_tag <- rep(NA,5)

full_table <- rbind(g_table, c_table, u_table, s_table)


filter(full_table, class=="non-coding")
filter(full_table, class=="gene")
filter(full_table, !(is.na(locus_tag)))
filter(full_table, class=="cds")
filter(full_table, class=="cds" & type=="product")
filter(full_table, class=="cds" & type=="mature peptide")
filter(full_table, class=="cds" & type=="non-structural")


nrow(full_table)
full_table$annot <- rep(NA, nrow(full_table))
full_table$annot[30] <- "structural protein"
full_table$annot[32] <- "structural protein"
full_table$annot[33] <- "structural protein"
full_table$annot[38] <- "structural protein"
full_table$annot[which(full_table$name %in% filter(full_table, class=="cds" & type=="mature peptide")$name)] <- "non-structural protein"

# annotate mature peptides as functional instead of csd
full_table$class[which(full_table$name %in% filter(full_table, class=="cds" & type=="mature peptide")$name)] <- "functional"



# latex coordinate table
latex(full_table, file="~/repolab/work/virusbeacon/annot_coord_table", 
      rowname = NULL, caption = "Genomic regions coordinates and aliases", 
      label = "table:1", longtable = FALSE, table.env = FALSE)  # na.blank = T  #longtable = T extracolsize=FALSE, ,landscape = FALSE , longtable = F
write.csv(full_table, "~/repolab/work/virusbeacon/annot_coord_table.csv", row.names = F)
write.csv(full_table, "~/repolab/work/virusbeacon/annot_coord_table.txt", row.names = F)



