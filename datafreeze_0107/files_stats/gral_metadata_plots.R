rm(list = ls())


source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/ggplot2_charts.R")
source("/Users/claudiavasallovega/repolab/work/virusbeacon/metadata_funs.R")

library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(gridExtra)

setwd( "~/repolab/work/virusbeacon/datafreeze_0107")

resources <- c("ENA-Illumina", "ENA-ONT", "ENA-consensus", "GISAID")

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
## GENERAL METATADA STATS (whole database)

# 
# # Resource
# 
# # SRA: 4650 (raw sequence files) \\
# # Genebank=3053 (consensus sequence files)\\
# # GISIAD=29629 (consensus sequence files)
# 
# 
# # Manual version
data <- data.frame(1:4, 1:4)
colnames(data) <- c("reso", "Freq")
data$reso <- as.character(data$reso)
data$reso[1] <- "ENA-Illumina" # - 1497
data$Freq[1] <- 3378
data$reso[2] <- "ENA-ONT" # - 1497
data$Freq[2] <- 3539 
data$reso[3] <- "ENA-consensus" # - 3830
data$Freq[3] <- 5430
data$reso[4] <- "GISAID" # - 3830
data$Freq[4] <- 58246

# plot
# colnames(data) <- c("technology", "Freq")
#
resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
reso <- ggplot(data) +
        geom_col(aes(reso, Freq)) +
        ggtitle("VARIANT FILE reso") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank(), legend.text = element_text(size=15)) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
reso
reso_bp <- ggplot(data, aes(reso, Freq)) +
        aes(x="", y=Freq, fill=reso) +
        geom_bar(width = 1, stat = "identity")
reso_pie <- reso_bp +
        coord_polar("y", start=0)
reso_pie <-  reso_pie + scale_fill_manual(values=resource_colors) +
        blank_theme +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank(), legend.text = element_text(size=15)) +
        ggtitle("RESOURCE") +
        theme(axis.text.x = element_blank())
reso_pie

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_reso_pie.pdf")
grid.arrange(reso_pie,
             nrow=1, ncol=1 ) #
dev.off()

# 
# 
# 
# Type
# # Manual version
data <- data.frame(1:2, 1:2)
colnames(data) <- c("type", "Freq")
data$type <- as.character(data$type)
data$type[1] <- "raw" # - 1497
data$Freq[1] <- 6917 #length(exp_platform)
data$type[2] <- "consensus" # - 3830
data$Freq[2] <-  63676

# plot
# colnames(data) <- c("technology", "Freq")
#
#type_colors <- c("royalblue", "red")
type_colors <- c("blue", "firebrick1")
type <- ggplot(data) +
        geom_col(aes(type, Freq)) +
        ggtitle("VARIANT FILE TYPE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
type
type_bp <- ggplot(data, aes(type, Freq)) +
        aes(x="", y=Freq, fill=type) +
        geom_bar(width = 1, stat = "identity")
type_pie <- type_bp +
        coord_polar("y", start=0)
type_pie <-  type_pie + scale_fill_manual(values=type_colors) +
        blank_theme +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank(), legend.text = element_text(size=15)) +
        ggtitle("VARIANT FILE TYPE") +
        theme(axis.text.x = element_blank())
type_pie

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_type_pie.pdf")
grid.arrange(type_pie,
             nrow=1, ncol=1 ) #
dev.off()

# 
# 


# # -------------------------------------------------------------------------------------
# # Seq technology
# # Automatic version
# 
# tec <- c()
# load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/sra_illumina_metadata.rda")
# metadata_df <- as_tibble(metadata_df)
# tec <- c(tec, metadata_df$platform)
# unique(metadata_df$platform)
# load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/sra_ont_metadata.rda")
# metadata_df <- as_tibble(metadata_df)
# tec <- c(tec, metadata_df$platform)
# unique(metadata_df$platform)
# load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/ena_consensus_metadata.rda")
# metadata_df <- as.data.frame(metadata_df)
# tec <- c(tec, metadata_df$platform)
# unique(metadata_df$platform)
# load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/gisaid_metadata.rda")
# metadata_df <- as.data.frame(metadata_df)
# tec <- c(tec, metadata_df$platform)
# tec <- unlist(tec)
# # remove error from illumina in Gisaid
# tec[which(tec=="illumina")] <- NA
# 
# data <- as.data.frame(table(tec))
# 
# # Harmonize
# #tec[which(tec=="illumina")] <- "Illumina Technologies" 
# #tec[which(tec=="OXFORD_NANOPORE")] <- "Oxford Nanopore Technologies" 
# 
# colnames(data) <- c("technology", "Freq")
# # 
# # aspect_ratio <- 2.5
# # height <- 7
# 
# tec <- ggplot(data) +
#         geom_col(aes(technology, Freq),na.rm = FALSE) +
#         #geom_bar(aes(technology, Freq), stat="identity", width = 0.3) +
#         ggtitle("SEQ TECHNOLOGY") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(legend.title = element_blank()) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# tec

#ggsave(file="tec", plot=tec, height=5, width = 5, device="pdf")

# tec_bp <- ggplot(data, aes(technology, Freq)) +
#         aes(x="", y=Freq, fill=technology) +
#         geom_bar(width = 1, stat = "identity") 
# tec_pie <- tec_bp + 
#         coord_polar("y", start=0) 
# tec_pie <-  tec_pie + scale_fill_manual(values=my_colors_more) + 
#         blank_theme +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(legend.title = element_blank()) +
#         ggtitle("SEQUENCING TECHNOLOGY") +
#         theme(axis.text.x = element_blank()) 
# tec_pie

# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_tec.pdf")
# grid.arrange(tec,
#              nrow=1, ncol=1 ) #
# dev.off()

# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_tec_pie.pdf")
# grid.arrange(tec_pie,
#              nrow=1, ncol=1 ) #
# dev.off()
# 

# -------------------------------------------------------------------------------------
#  Geo loc
 # for (r in resources) {
 # }   


load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/sra_illumina_metadata.rda")
big_meta <- metadata_df # 3378   13
load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/sra_ont_metadata.rda")
big_meta <- rbind(big_meta, metadata_df) # 3539   13
load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/ena_consensus_metadata.rda")
big_meta <- rbind(big_meta, metadata_df) # 5430   13
load("~/repolab/work/virusbeacon/datafreeze_0107/metadata/gisaid_metadata.rda")
big_meta <- rbind(big_meta, metadata_df) # 58246    13

dim(big_meta) # 70593    13


loc_res <- select(big_meta, "geo_loc", "resource" )

# harmonize in situ 
loc_res$geo_loc[which(loc_res$geo_loc== "Netherlands:-")] <- "Europe:Netherlands:-:-"
loc_res$geo_loc[which(loc_res$geo_loc== "China:-:-:-")] <- "Asia:China:-:-"
loc_res$geo_loc[which(loc_res$geo_loc== "Denmark:-")] <- "Europe:Denmark:-:-"

#plot

continents <- unique(
        sapply(loc_res$geo_loc, function(l) {
                str_split(
                        l, ":")[[1]][1] }
        )
)
continents
# 6 continents

countries <- unique(
        sapply(loc_res$geo_loc, function(l) {
                str_split(
                        l, ":")[[1]][2] }
        )
)

length(countries) #  85

for (c in countries) {
        loc[str_detect(loc, c)] <- as.character(c)
        
}

loc_res$country <- sapply(loc_res$geo_loc, country_from_geo)


data <- data.frame(table(loc_res$country, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   70593

locp <- ggplot(data, aes(Var1, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("GEO LOCATION") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
locp



# All loc split by resource

# loc_res$group <- ifelse(
#         loc_res$res=="sra_illumina"|loc_res$res=="sra_ont"|loc_res$res=="sra_ion",
#         "SRA",
#         loc_res$res
# )
#  


loc_res_ct <-loc_res %>% 
        group_by(country, resource) %>%
        summarise(count=n())


dat <- loc_res_ct
#data$res <- as.character(data$res)
# #data <-  mutate(data, group=fct_relevel(group,
#                                       c("SRA", "ENA", "GISAID")
# )) 
# data <-  mutate(data, res=fct_relevel(res,
#                                     c("sra_illumina", "sra_ont", "genbank", "gisaid")
#                                     )) 

resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
loc_resp  <- ggplot(dat, aes(x=country, y=count, fill=resource)) +  # fill=res
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) +
        ggtitle("GEO LOCATION") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=5)) +
        theme(legend.position = "top") 
        
loc_resp 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_loc_res_facet.pdf")
grid.arrange(loc_resp ,
             nrow=1, ncol=1 ) #
dev.off()


# -------------------------------------------------------------------------------------
# Collection date 

date_res <- select(big_meta, "collection_date", "resource" )

date_res$collection_date <- unlist(date_res$collection_date)

#date_res$month <- sapply(date_res$collection_date, month_from_date)



# Remove "2020" and wrong data

date_res$collection_date[str_detect(date_res$collection_date, "1905-07")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2019-01")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-07")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-08")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-09")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-10")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-11")] <- NA
date_res$collection_date[str_detect(date_res$collection_date, "2020-12")] <-  NA
date_res$collection_date[str_detect(date_res$collection_date, "2021-")] <- NA


# date_res$month[which(date_res$month=="1905-07")] <- NA
# date_res$month[which(date_res$month=="2019-01")] <- NA
# date_res$month[which(date_res$month=="2019-01")] <- "2020-01"
# date_res$month[which(date_res$month=="2020-07")] <-  NA
# date_res$month[which(date_res$month=="2020-08")] <- NA
# date_res$month[which(date_res$month=="2020-09")] <- NA
# date_res$month[which(date_res$month=="2020-10")] <- NA
# date_res$month[which(date_res$month=="2020-11")] <- NA
# date_res$month[which(date_res$month=="2020-12")] <- NA
# date_res$month[which(date_res$month=="2021-01")] <- NA
# date_res$month[which(date_res$month=="2021-02")] <- NA
# 


# # All date
#data <- data.frame(table(date, useNA = "ifany")) # , stringsAsFactors = F
# 
# 
# sum(data$Freq) #   
# 
# #colnames(data) <- c("date", "Freq")
# 
# datep <- ggplot(data, aes(date, Freq)) +
#         geom_col() +
#         #geom_bar(stat = 'identity') +
#         #coord_flip() +
#         ggtitle("COLLECTION DATE") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
# datep
# 



# # All collection date split by resource

date_res_ct <- date_res %>% 
        #group_by(month, resource) %>%
        group_by(collection_date, resource) %>%
        summarise(count=n())

dat <- date_res_ct


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
date_resp  <- ggplot(dat, aes(x=as.Date.character(collection_date), y=count, fill=resource)) + # month
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) +
        ggtitle("COLLECTION DATE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        #theme(axis.ticks.x = axis.Date(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))) +
        #xlim(as.Date.character("2019-12-01"), as.Date.character("2020-06-31")) +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=7))  + # size=5, 
        theme(legend.position = "top")
date_resp 
#pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_month_res_facet.pdf")
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_date_res_facet.pdf")
grid.arrange(date_resp,
             nrow=1, ncol=1 ) #
dev.off()












# -------------------------------------------------------------------------------------
# Age groups



#plot
# Grpup per age group


# # Age groups > NCIT
# age[which(age<=1)] <- "<1"
# age[which(age>1 & age<=12)] <- "1-12"
# age[which(age>12 & age<=45)] <- "12-45"
# age[which(age>45 & age<=65)] <- "45-65"
# age[which(age>65)] <- ">65"
# 
# 
# 
# age_res <- data.frame(age=age, res=res, stringsAsFactors = F)
# age_res <-  mutate(age_res, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))

# # All age
# data <- data.frame(table(age, useNA = "ifany")) # , stringsAsFactors = F
# 
# 
# sum(data$Freq) #   
# 
# 
# data <-  mutate(data, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))
# 
# 
# 
# agep <- ggplot(data, aes(age, Freq)) +
#         geom_col() +
#         #geom_bar(stat = 'identity') +
#         #coord_flip() +
#         ggtitle("AGE") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
# agep

# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_age.pdf")
# grid.arrange(age,
#              nrow=1, ncol=1 ) #
# dev.off()

age_res <- select(big_meta, "host_age", "resource")



# All collection age split by resource
age_res_ct <- age_res %>% 
        group_by(host_age, resource) %>%
        summarise(count=n())

age_res_ct$host_age <- as.numeric(age_res_ct$host_age)

dat <- age_res_ct
#data$group <- as.character(data$group)
#data$age <- as.character(data$age)
# data <-  mutate(data, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))
# 


# data <-  mutate(data, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))
# 


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
age_res  <- ggplot(dat, aes(x=host_age, y=count, fill=resource)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) +
        ggtitle("AGE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=dat$count, vjust=-0.1, size=2) + #  
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=7)) + # size=5, 
        theme(legend.position = "top")
age_res 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_age_res_facet.pdf")
grid.arrange(age_res,
             nrow=1, ncol=1 ) #
dev.off()







# -------------------------------------------------------------------------------------
# Sex 

sex_res <- select(big_meta, "host_sex", "resource" )

sex_res$host_sex <- unlist(sex_res$host_sex)



#date_res$month <- sapply(date_res$collection_date, month_from_date)

#plot
# Grpup per sex group



# # All sex
# data <- data.frame(table(hossex, useNA = "ifany")) # , stringsAsFactors = F
# 
# 
# sum(data$Freq) #   
# 
# 
# sex <- ggplot(data, aes(sex, Freq)) +
#         geom_col() +
#         #geom_bar(stat = 'identity') +
#         #coord_flip() +
#         ggtitle("SEX") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
# sex
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sex_test.pdf",
#     height = 5, width = 2.5)
# grid.arrange(sex,
#              nrow=1, ncol=1 ) #
# dev.off()
# 


# All collection sex split by resource

sex_res_ct <- sex_res %>% 
        group_by(host_sex, resource) %>%
        summarise(count=n())


dat <- sex_res_ct


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
sex_resp  <- ggplot(dat, aes(x=host_sex, y=count, fill=resource)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) + # 
        ggtitle("SEX") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        geom_text(label=dat$count, vjust=-0.1, size=2) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=7)) + # size=5, 
        theme(legend.position = "top")

sex_resp 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_sex_res_facet.pdf") #, height = 5.5, width = 3)
grid.arrange(sex_resp,
             nrow=1, ncol=1 ) #
dev.off()








# -------------------------------------------------------------------------------------
# Sample source

source_res <- select(big_meta, "sample_source", "resource" )

source_res$sample_source <- unlist(source_res$sample_source)




# fix for plot
source_res$sample_source[which(source_res$sample_source=="bronchoalveolar lavage fluid")] <- "BALF"
source_res$sample_source[which(source_res$sample_source=="oro/naso-pharyngeal swab")] <- "OPS/NPS"
source_res$sample_source[which(source_res$sample_source=="tracheal swab/wash")] <- "TS/TW"
source_res$sample_source[which(source_res$sample_source=="oro/naso-pharyngeal swab, tracheal swab/wash")] <- "OPS/NPS, TS/TW"

# #plot
# # Grpup per sample group
# 
# 
# sample_res <- data.frame(sample=sample, res=res, stringsAsFactors = F)
# 
# # All sample
# data <- data.frame(table(sample, useNA = "ifany")) # , stringsAsFactors = F
# 
# 
# sum(data$Freq) #   
# 
# 
# sample <- ggplot(data, aes(sample, Freq)) +
#         geom_col() +
#         #geom_bar(stat = 'identity') +
#         #coord_flip() +
#         ggtitle("SAMPLE SOURCE") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
# sample
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sample.pdf")
# grid.arrange(sample,
#              nrow=1, ncol=1 ) #
# dev.off()


# All collection sex split by resource

source_res_ct <- source_res %>% 
        group_by(sample_source, resource) %>%
        summarise(count=n())


dat <- source_res_ct
# All collection sample split by resource



resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
sample_resp  <- ggplot(dat, aes(x=sample_source, y=count, fill=resource)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) + # 
        ggtitle("SAMPLE SOURCE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        geom_text(label=dat$count, vjust=-0.1, size=1.5) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=7)) + # size=5, 
        theme(legend.position = "top")
sample_resp 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_sample_res_facet.pdf")
grid.arrange(sample_resp,
             nrow=1, ncol=1 ) #
dev.off()








# -------------------------------------------------------------------------------------
# Seq technology

plat_res <- select(big_meta, "platform", "resource" )

plat_res$platform <- unlist(lapply(plat_res$platform, function(l) {
        ifelse(length(unlist(l))>0, 
               unlist(l),
               NA)}))

#plot
# Grpup per plat group

# 
# plat_res <- data.frame(plat=plat, res=res, stringsAsFactors = F)
# 
# # All plat
# data <- data.frame(table(plat, useNA = "ifany")) # , stringsAsFactors = F
# 
# 
# sum(data$Freq) #   
# 
# 
# plat <- ggplot(data, aes(plat, Freq)) +
#         geom_col() +
#         #geom_bar(stat = 'identity') +
#         #coord_flip() +
#         ggtitle("plat SOURCE") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
# plat
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_plat.pdf")
# grid.arrange(plat,
#              nrow=1, ncol=1 ) #
# dev.off()
# 


# All collection plat split by resource


plat_res_ct <- plat_res %>% 
        group_by(platform, resource) %>%
        summarise(count=n())


dat <- plat_res_ct

# data <-  mutate(data, group=fct_relevel(group,
#                                         c("SRA", "ENA", "GISAID")
# )) 
# 
# sum(data$count) #   






resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
plat_resp  <- ggplot(dat, aes(x=platform, y=count, fill=resource)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        facet_grid(facets = vars(resource), scales = "free_y", margins = FALSE) + # 
        ggtitle("SEQUENCING TECHNOLOGY") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        geom_text(label=dat$count, vjust=-1.1, size=1.5) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size=7))  + # size=5, 
        theme(legend.position = "top")
plat_resp 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_0107/files_stats/all_plat_res_facet.pdf")
grid.arrange(plat_resp,
             nrow=1, ncol=1 ) #
dev.off()















######################################################################################
# Grid
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_grid.pdf")
grid.arrange(date_res, age_res, sex_res, sample_res,
             nrow=2, ncol=2 ) #
dev.off()





#  Sample id



### Duplicated samples
# Illumina
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
ill_sample_id <- sample_id
length(ill_sample_id)
sum(duplicated(ill_sample_id))


# ONT
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
ont_sample_id <- sample_id
length(ont_sample_id)
sum(duplicated(ont_sample_id))

# commmon Illumina ONT
length(intersect(ill_sample_id, ont_sample_id))
# 26


# Samples common to ONT and GISAD
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_sample_attributes.rda")

unique(unlist(filter(pair_table)$tag))

ont_gisaid_id <- unique(unlist(filter(pair_table, tag=="gisaid id")$value)) 

load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
gisaid_id <-run_id


length(intersect(ont_gisaid_id, gisaid_id)) # 246  
