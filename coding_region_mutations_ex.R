library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats) # relevel

# AA replacements
rep <-  read_csv("~/repolab/work/virusbeacon/amino_acid_replacements.csv",
                          col_names = T)
# rep$grantham_distance1 <- as.numeric(rep$grantham_distance1)
# rep$grantham_distance1 <- as.numeric(rep$miyata_distance2)

nrow(rep) # 5033 aa replacement variants described

# Proteins with aminoacid changes 
length(unique(rep$ptotein_id)) # 26 out of 30 proteins

# Unique replacements or unique variants?
r <- rep[which(duplicated(rep$replacement)),]
sapply(unique(rep$protein_id), function(pt){
 sum(duplicated(filter(r, protein_id == pt )$replacement))  
}) # yeap nope, variants were grouped by aminoacid change



# Graph Number of aa changes per protein  -------------------------------------------------------------------------
t <- table(rep$protein_id)
dat<- as.data.frame(t)
rownames(dat) <- names(t)
colnames(dat) <- c("protein_id", "counts")

dat <-  mutate(dat, protein_id=fct_relevel(protein_id, 
                "nsp1", "nsp2", "nsp3", "nsp4", "nsp5", "nsp6", "nsp7", 
                "nsp8", "nsp9", "nsp10", "nsp11", "nsp12", "nsp13", "nsp14",
                "nsp15", "nsp16", "S", "ORF 3a", "E", "M", "ORF 6", "ORF 7a", 
                "ORF 7b", "ORF 8", "N", "ORF 10"))


num_change <- ggplot(dat, aes(protein_id, counts)) +
        geom_col(fill="firebrick2") +
        ggtitle("Variants per protein") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Variant count") +
        theme_minimal() +
        geom_text(label=dat$counts, vjust=-0.1, size=2.5, colour = "darkgreen") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
num_change
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/aa_replacement.pdf",  height =5.83,  width =8.27)
num_change
dev.off()

# Num seq anylyzed with changes- ie. most frequent variants..
# it would be interesting to find co/evolving variants among those 
# with similar frequency.

dat <- aggregate(num_seq~ protein_id, data=rep, sum)

# # check
# sum(filter(rep, protein_id=="E")$num_seq)
# sum(filter(rep, protein_id=="E")$num_seq)

# Graph Number of sequences with aa changes per protein  -------------------------------------------------------------------------
dat <-  mutate(dat, protein_id=fct_relevel(protein_id, 
                                           "nsp1", "nsp2", "nsp3", "nsp4", "nsp5", "nsp6", "nsp7", 
                                           "nsp8", "nsp9", "nsp10", "nsp11", "nsp12", "nsp13", "nsp14",
                                           "nsp15", "nsp16", "S", "ORF 3a", "E", "M", "ORF 6", "ORF 7a", 
                                           "ORF 7b", "ORF 8", "N", "ORF 10"))


freq_change <- ggplot(dat) +
        geom_col(aes(protein_id, num_seq), fill="firebrick2") +
        ggtitle("Sequences with variant per protein") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Sequence count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
freq_change 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/freq_aa_replacement.pdf",  height =5.83,  width =8.27)
freq_change
dev.off()

