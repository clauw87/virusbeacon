# Getting and region coordinates-annotations from gbk file of reference sequence
library(readr)
library(dplyr)
library(stringr)
library(Hmisc)


###################################################################################################################
##### Genbank Full file
gannot <- read_delim("~/repolab/work/virusbeacon/NC_045512.2.genbank.txt",  delim= "\n", skip = 1, col_names = F)$X1

# Features
# genes
gannot[which(str_detect(gannot, "gene "))] # 11
# cds
gannot[which(str_detect(gannot, "CDS "))] # 12
# utr
gannot[which(str_detect(gannot, "'UTR "))] # 2
# mat
gannot[which(str_detect(gannot, "mat_peptide"))] # 26
# stem loop
gannot[which(str_detect(gannot, "stem_loop"))] # 5


# Extract info


########### GENES ##################
# coordinates
# genes
g <- gannot[which(str_detect(gannot, "gene "))] # 11
g_coords <- unlist(
        lapply(g, function(r){
                gsub(" ", "", str_match(r, "gene (.*?)$")[2])
        }
        )
)  
# coordinates split into start and end
start <- unlist(lapply(g_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][1]
}))
end <- unlist(lapply(g_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][2]
}))

# gene annot
g_annot <- str_split(paste0(gannot, collapse = ""), pattern = " gene ")[[1]][-1]

gene <- unlist(lapply(g_annot, function(g){
        str_match(g, "gene=\"(.*?)\" ")[2]
}))
syn <- unlist(lapply(g_annot, function(g){
        str_match(g, "gene_synonym=\"(.*?)\" ")[2]
}))
locus_tag <- unlist(lapply(g_annot, function(g){
        str_match(g, "locus_tag=\"(.*?)\" ")[2]
}))
gene_id <- unlist(lapply(g_annot, function(g){
        gsub("GeneID:", "", str_match(g, "db_xref=\"(.*?)\" ")[2])
}))


g_table <- data.frame(start=start, end=end, name=gene, synonym=syn,
                      locus_tag=locus_tag, gene_id=gene_id)
write.table(g_table, "/Users/claudiavasallovega/repolab/work/virusbeacon/gene_annot.txt")




#########  CDSs ########################
# coordinates
c <- gannot[which(str_detect(gannot, "CDS "))] # 12
c_coords <- unlist(
        lapply(c, function(r){
                gsub(" ", "", str_match(r, "CDS(.*?)$")[2]) 
        }
        )
)  
# coordinates split into start and end
# coordinates of join cds as full stretch
c_coords[1] <- "266..21555"
start <- unlist(lapply(c_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][1]
}))
end <- unlist(lapply(c_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][2]
}))

# cds annot 
c_annot <- str_split(paste0(gannot, collapse = ""), pattern = " CDS ")[[1]][-1]
note <- unlist(lapply(c_annot, function(c){
        str_match(c, "note=\"(.*?)\" ")[2]
}))
gene <- unlist(lapply(c_annot, function(c){
        str_match(c, "gene=\"(.*?)\" ")[2]
}))
locus_tag <- unlist(lapply(c_annot, function(c){
        str_match(c, "locus_tag=\"(.*?)\" ")[2]
}))
gene_id <- unlist(lapply(c_annot, function(c){
        gsub("GeneID:", "", str_match(c, "db_xref=\"(.*?)\" ")[2])
}))
product <- unlist(lapply(c_annot, function(c){
str_match(c, "product=\"(.*?)\" ")[2]
}))

protein_id <- unlist(lapply(c_annot, function(c){
        str_match(c, "protein_id=\"(.*?)\" ")[2]
}))

# table
c_table <- data.frame(start=start, end=end, product=product,
                      note=note, protein_id=protein_id,
                      gene=gene, locus_tag=locus_tag)
write.table(c_table, "/Users/claudiavasallovega/repolab/work/virusbeacon/cds_annot.txt")


#########  UTRs ########################
# coordinates
u <- gannot[which(str_detect(gannot, "'UTR "))] 
u_coords <- unlist(
        lapply(u, function(r){
                gsub(" ", "", str_match(r, "UTR(.*?)$")[2]) 
        }
        )
)  
# coordinates split into start and end
start <- unlist(lapply(u_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][1]
}))
end <- unlist(lapply(u_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][2]
}))

# utr annot  -none. names only
name <- unlist(
        lapply(u, function(r){
                str_split(r, " ")[[1]][6]
        }))  

# table
u_table <- data.frame(start=start, end=end, name=name)
write.table(u_table, "/Users/claudiavasallovega/repolab/work/virusbeacon/utr_annot.txt")



#########  MATURE PEPTIDES ########################
# coordinates
p <- gannot[which(str_detect(gannot, "mat_peptide"))]
p_coords <- unlist(
        lapply(p, function(r){
                gsub(" ", "", str_match(r, "mat_peptide(.*?)$")[2])  
        }
        )
)  
# coordinates split into start and end
# coordinates of join cds as full stretch
p_coords[11] <- "13442..16236"
start <- unlist(lapply(p_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][1]
}))
end <- unlist(lapply(p_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][2]
}))

# mat peptide annot 
p_annot <- str_split(paste0(gannot, collapse = ""), pattern = " mat_peptide ")[[1]][-1]

gene <- unlist(lapply(p_annot, function(p){
                str_match(p, "gene=\"(.*?)\" ")[2]
        }))
locus_tag <- unlist(lapply(p_annot, function(p){
        str_match(p, "locus_tag=\"(.*?)\" ")[2]
        }))

product <- unlist(lapply(p_annot, function(p){
        str_match(p, "product=\"(.*?)\" ")[2]
}))

note <- unlist(lapply(p_annot, function(p){
        str_match(p, "note=\"(.*?)\" ")[2]
}))
protein_id <- unlist(lapply(p_annot, function(p){
        str_match(p, "protein_id=\"(.*?)\" ")[2]
}))

# table
p_table <- data.frame(start=start, end=end, name=product,
                      aliases=note, protein_id=protein_id,
                      gene=gene, locus_tag=locus_tag)
write.table(p_table, "/Users/claudiavasallovega/repolab/work/virusbeacon/mature_peptide_annot.txt")



#########  Stem Loops ########################
# coordinates
s <- gannot[which(str_detect(gannot, "stem_loop"))] 
s_coords <- unlist(
        lapply(s, function(r){
                gsub(" ", "", str_match(r, "stem_loop(.*?)$")[2]) 
        }
        )
)  
# coordinates split into start and end
start <- unlist(lapply(s_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][1]
}))
end <- unlist(lapply(s_coords, function(r){
        strsplit(x = r, split = "..", fixed = T)[[1]][2]
}))

# SL 
s_annot <- str_split(paste0(gannot, collapse = ""), pattern = " stem_loop ")[[1]][-1]

gene <- unlist(lapply(s_annot, function(r){
        str_match(r, "gene=\"(.*?)\" ")[2]
}))
locus_tag <- unlist(lapply(s_annot, function(r){
        str_match(r, "locus_tag=\"(.*?)\" ")[2]
}))

# note <- unlist(lapply(s_annot, function(r){
#         str_match(r, "note=\"(.*?)\" ")[2]
# }))

funct <- unlist(lapply(s_annot, function(r){
        paste0(str_split(str_match(r, "function=\"(.*?)\"")[2], " ")[[1]][nchar(str_split(str_match(r, "function=\"(.*?)\"")[2], " ")[[1]])>0], collapse = " ")
}))

# table
s_table <- data.frame(start=start, end=end, 
                      name=funct, gene=gene,
                      locus_tag=locus_tag)
write.table(s_table, "/Users/claudiavasallovega/repolab/work/virusbeacon/sl_annot.txt")



#####
# # NCBI web refseq annotation 
# #rsannot <- read_delim("~/repolab/work/virusbeacon/annot_file.txt", delim = "/", skip = 1, col_names = F)
# rsannot <- read_delim("~/repolab/work/virusbeacon/annot_file.txt",  delim= "\n", skip = 1, col_names = F)$X1
# 
# # Features
# # genes
# rsannot[which(str_detect(rsannot, "gene "))] # 11
# # cds
# rsannot[which(str_detect(rsannot, "CDS "))] # 12
# # mat_peptide
# rsannot[which(str_detect(rsannot, "mat_peptide"))] # 26
# # utr
# rsannot[which(str_detect(rsannot, "'UTR "))] # 2
# # stem loop
# rsannot[which(str_detect(rsannot, "stem_loop"))] # 5
# 
# 
# # Extract info
# # genes
# g <- rsannot[which(str_detect(rsannot, "gene "))] # 11
# g_coords1 <- unlist(
#         lapply(g, function(r){
#                 str_match(r, "^gene (.*?) /")[,2]    
#         }
#         )
# )  
# #start split in ..
# #end
# 
# # cdss
# c <- rsannot[which(str_detect(rsannot, "CDS "))] # 12
# c_coords1 <- unlist(
#         lapply(c, function(r){
#                 str_match(r, "^CDS (.*?) /")[,2]    
#         }
#         )
# )  
# 
# # mat pept
# p <- rsannot[which(str_detect(rsannot, "mat_peptide"))]
# p_coords1 <- unlist(
#         lapply(p, function(r){
#                 str_match(r, "^mat_peptide (.*?) /")[,2]    
#         }
#         )
# )  
# 



