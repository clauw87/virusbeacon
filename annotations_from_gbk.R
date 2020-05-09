# Getting and region coordinates-annotations from gbk file of reference sequence
library(readr)
library(dplyr)
library(stringr)
library(Hmisc)


###################################################################################################################
# NCBI web refseq annotation 
#rsannot <- read_delim("~/repolab/work/virusbeacon/annot_file.txt", delim = "/", skip = 1, col_names = F)

rsannot <- read_delim("~/repolab/work/virusbeacon/annot_file.txt",  delim= "\n", skip = 1, col_names = F)$X1

# genes
# gbk[str_detect(gbk, "gene=")]
rsannot[which(str_detect(rsannot, "gene "))] # 11

# cds
rsannot[which(str_detect(rsannot, "CDS "))] # 12

# mat
rsannot[which(str_detect(rsannot, "mat_peptide"))] # 26

# utr
rsannot[which(str_detect(rsannot, "'UTR "))] # 2

# stem loop
rsannot[which(str_detect(rsannot, "stem_loop"))] # 5



# Extract info
# genes
g <- rsannot[which(str_detect(rsannot, "gene "))] # 11
coordinates <- unlist(
        lapply(g, function(r){
                str_match(r, "^gene (.*?) /")[,2]    
        }
        )
)  
#start split in ..
#end

# cdss
c <- rsannot[which(str_detect(rsannot, "CDS "))] # 12
coordinates <- unlist(
        lapply(c, function(r){
                str_match(r, "^CDS (.*?) /")[,2]    
        }
        )
)  


# mat pept
p <- rsannot[which(str_detect(rsannot, "mat_peptide"))]
coordinates <- unlist(
        lapply(p, function(r){
                str_match(r, "^mat_peptide (.*?) /")[,2]    
        }
        )
)  