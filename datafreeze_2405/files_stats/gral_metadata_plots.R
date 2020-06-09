rm(list = ls())


source("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/ggplot2_charts.R")

library(dplyr)
library(stringr)
library(forcats)


setwd( "~/repolab/work/virusbeacon/datafreeze_2405")

resources <- c("sra_illumina", "sra_ont", "genbank", "gisaid")

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
## GENERAL METATADA STATS (whole database)


# Resource

# SRA: 4650 (raw sequence files) \\
# Genebank=3053 (consensus sequence files)\\
# GISIAD=29629 (consensus sequence files)


# # Manual version
data <- data.frame(1:3, 1:3)
colnames(data) <- c("reso", "Freq")
data$reso <- as.character(data$reso)
data$reso[1] <- "SRA" # - 1497
data$Freq[1] <- 4650
data$reso[2] <- "ENA" # - 3830
data$Freq[2] <- 3053
data$reso[3] <- "GISAID" # - 3830
data$Freq[3] <- 29629

# plot
# colnames(data) <- c("technology", "Freq")
#
resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
reso <- ggplot(data) +
        geom_col(aes(reso, Freq)) +
        ggtitle("VARIANT FILE reso") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
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

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_reso_pie.pdf")
grid.arrange(reso_pie,
             nrow=1, ncol=1 ) #
dev.off()




# Type
# # Manual version
data <- data.frame(1:2, 1:2)
colnames(data) <- c("type", "Freq")
data$type <- as.character(data$type)
data$type[1] <- "raw" # - 1497
data$Freq[1] <- 1497 #length(exp_platform)
data$type[2] <- "consensus" # - 3830
data$Freq[2] <- 36512

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
        labs(y="Number of runs") +
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

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_type_pie.pdf")
grid.arrange(type_pie,
             nrow=1, ncol=1 ) #
dev.off()




# DATA FREEZE stats for raw sequences

#Graph EXPERIMENT > DESIGN > exp_platform -------------------------------------------------------------------------
# # Seq technology
# 
# # Manual version
# data <- data.frame(1:3, 1:3)
# colnames(data) <- c("technology", "Freq")
# data$technology <- as.character(data$technology)
# data$technology[1] <- "Illumina" # - 1497
# data$Freq[1] <- 1497 #length(exp_platform)
# data$technology[2] <- "Oxford Nanopore" # - 3830
# data$Freq[2] <- 3830 #length(exp_platform)
# data$Freq[3] <- 95 #length(exp_platform)
# data$technology[3] <- "IonTorrent" # - 95
# 
# # plot
# colnames(data) <- c("technology", "Freq")
# 
# tec <- ggplot(data) +
#         geom_col(aes(technology, Freq)) +
#         ggtitle("SEQ TECHNOLOGY") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         theme(legend.title = element_blank()) +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# tec
# tec_bp <- ggplot(data, aes(technology, Freq)) +
#         aes(x="", y=Freq, fill=technology) +
#         geom_bar(width = 1, stat = "identity") 
# tec_pie <- tec_bp + 
#         coord_polar("y", start=0) 
# tec_pie <-  tec_pie + scale_fill_manual(values=my_colors) + 
#         blank_theme +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         ggtitle("SEQUENCING TECHNOLOGY") +
#         theme(axis.text.x = element_blank()) 
# tec_pie
# 
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_tec_pie.pdf")
# grid.arrange(tec_pie,
#              nrow=1, ncol=1 ) #
# dev.off()

# -------------------------------------------------------------------------------------
# Seq technology
# Automatic version

tec <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
tec <- c(tec, platform)
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
tec <- c(tec, platform)
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
tec <- c(tec, platform)
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
tec <- c(tec, platform)

tec <- unlist(tec)

# remove error from illumina in Gisaid
tec[which(tec=="illumina")] <- NA

data <- as.data.frame(table(tec))

# Harmonize
#tec[which(tec=="illumina")] <- "Illumina Technologies" 
#tec[which(tec=="OXFORD_NANOPORE")] <- "Oxford Nanopore Technologies" 

colnames(data) <- c("technology", "Freq")
# 
# aspect_ratio <- 2.5
# height <- 7

tec <- ggplot(data) +
        geom_col(aes(technology, Freq)) +
        #geom_bar(aes(technology, Freq), stat="identity", width = 0.3) +
        ggtitle("SEQ TECHNOLOGY") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
tec

#ggsave(file="tec", plot=tec, height=5, width = 5, device="pdf")

tec_bp <- ggplot(data, aes(technology, Freq)) +
        aes(x="", y=Freq, fill=technology) +
        geom_bar(width = 1, stat = "identity") 
tec_pie <- tec_bp + 
        coord_polar("y", start=0) 
tec_pie <-  tec_pie + scale_fill_manual(values=my_colors_more) + 
        blank_theme +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        ggtitle("SEQUENCING TECHNOLOGY") +
        theme(axis.text.x = element_blank()) 
tec_pie

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_tec.pdf")
grid.arrange(tec,
             nrow=1, ncol=1 ) #
dev.off()


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_tec_pie.pdf")
grid.arrange(tec_pie,
             nrow=1, ncol=1 ) #
dev.off()


# -------------------------------------------------------------------------------------
#  Geo loc
 # for (r in resources) {
 # }     
run <- c()
loc <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(unique(run_id)))
loc <- c(loc, geo_loc)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(unique(run_id)))
loc <- c(loc, geo_loc)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(unique(run_id)))
loc <- c(loc, geo_loc)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
run <- c(run, length(unique(run_id)))
loc <- c(loc, geo_loc)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(loc)
length(res)


loc_res <- data.frame(loc=loc, res=res, stringsAsFactors = F)

#plot

continents <- unique(
        sapply(loc, function(l) {
                str_split(
                        l, ":")[[1]][1] }
        )
)
# 6 continents

countries <- unique(
        sapply(loc, function(l) {
                str_split(
                        l, ":")[[1]][2] }
        )
)

length(countries) #  85

for (c in countries) {
        loc[str_detect(loc, c)] <- as.character(c)
        
}




data <- data.frame(table(loc, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   

loc <- ggplot(data, aes(loc, Freq)) +
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
loc
#ggsave(file="loc", plot=tec, height=5, width = 5, device="pdf")
#ggsave(file="/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/loc.pdf", 
 #      plot=loc, height=10, width = 10, device="pdf")

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_loc.pdf")
grid.arrange(loc,
             nrow=1, ncol=1 ) #
dev.off()



# All loc split by resource

loc_res$group <- ifelse(
        loc_res$res=="sra_illumina"|loc_res$res=="sra_ont"|loc_res$res=="sra_ion",
        "SRA",
        loc_res$res
)
 


loc_res_ct <-loc_res %>% 
        group_by(loc, group) %>%
        summarise(count=n())



countries <- unique(
        sapply(loc_res_ct$loc, function(l) {
                str_split(
                        l, ":")[[1]][2] }
        )
)

length(countries) #  85

for (c in countries) {
        loc_res_ct$loc[str_detect(as.character(loc_res_ct$loc), c)] <- as.character(c)
        
}

data <- loc_res_ct
#data$res <- as.character(data$res)
data <-  mutate(data, group=fct_relevel(group,
                                      c("SRA", "ENA", "GISAID")
)) 
# data <-  mutate(data, res=fct_relevel(res,
#                                     c("sra_illumina", "sra_ont", "genbank", "gisaid")
#                                     )) 

resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
loc_res  <- ggplot(data, aes(x=loc, y=count, fill=group)) +  # fill=res
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("GEO LOCATION") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
loc_res 
#ggsave(file="/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/loc_res2.pdf", 
 #      plot=loc_res, height=10, width = 10, device="pdf")



pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_loc_res.pdf")
grid.arrange(loc_res,
             nrow=1, ncol=1 ) #
dev.off()


# -------------------------------------------------------------------------------------
# Collection date 

# for (r in resources) {
# }     
run <- c()
date <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
date <- c(date, collection_date)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
date <- c(date, collection_date)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
date <- c(date, collection_date)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
run <- c(run, length(run_id))
date <- c(date, collection_date)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(date)
length(res)


#plot
# Grpup per month
# date[str_detect(date, "2019-12")] <- "Dec 2019"
# date[str_detect(date, "2020-01")] <- "Jan 2020"
# date[str_detect(date, "2020-02")] <- "Feb 2020"
# date[str_detect(date, "2020-03")] <- "Mar 2020"
# date[str_detect(date, "2020-04")] <- "Apr 2020"
# date[str_detect(date, "2020-05")] <- "May 2020"

date[str_detect(date, "2019-12")] <- "2019-12"
date[str_detect(date, "2020-01")] <- "2020-01"
date[str_detect(date, "2020-02")] <- "2020-02"
date[str_detect(date, "2020-03")] <- "2020-03"
date[str_detect(date, "2020-04")] <- "2020-04"
date[str_detect(date, "2020-05")] <-  "2020-05"


# Remove "2020"
date[date=="2020"] <- NA
date[date=="missing"] <- NA
date[date=="Missing"] <- NA


date_res <- data.frame(date=date, res=res, stringsAsFactors = F)


# All date
data <- data.frame(table(date, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   

#colnames(data) <- c("date", "Freq")

date <- ggplot(data, aes(date, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("COLLECTION DATE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
date


# # Tryin
# 
# date <- ggplot(data, aes(date, Freq)) +
#         geom_col(width = 0.9) +
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
# date


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats//all_date.pdf")
grid.arrange(date,
             nrow=1, ncol=1 ) #
dev.off()



# All collection date split by resource
date_res$group <- ifelse(
        date_res$res=="sra_illumina"|date_res$res=="sra_ont"|date_res$res=="sra_ion",
        "SRA",
        date_res$res
)




date_res_ct <- date_res %>% 
        group_by(date,group) %>%
        summarise(count=n())


data <- date_res_ct
data$group <- as.character(data$group)
data <-  mutate(data, group=fct_relevel(group,
                                      c("SRA", "ENA", "GISAID")
)) 
# data <-  mutate(data, date=fct_relevel(date, 
#                                        "Dec 2019", 
#                                        "Jan 2020", "Feb 2020",     
#                                        "Mar 2020", "Apr 2020", 
#                                        "May 2020"#, "Jun 2020", "Jul 2020",
#                                        #"Aug 2020", "Sep 2020", "Oct 2020",
#                                        #"Nov 2020", "Dec 2020",
#                                        #"Jan 2021", "Feb 2021" #"Mar 2021", "Apr 2021", "May 2021"
# ))


# data <-  mutate(data, res=fct_relevel(res,
#                                       c("sra_illumina", "sra_ont", "genbank", "gisaid")
# )) 


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
date_res  <- ggplot(data, aes(x=date, y=count, fill=group)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("COLLECTION DATE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
date_res 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_date_res.pdf")
grid.arrange(date_res,
             nrow=1, ncol=1 ) #
dev.off()












# -------------------------------------------------------------------------------------
# Age groups

# for (r in resources) {
# }     
run <- c()
age <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
age <- c(age, host_age)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
age <- c(age, host_age)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
age <- c(age, host_age)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
run <- c(run, length(run_id))
age <- c(age, host_age)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(age)
length(res)


#plot
# Grpup per age group


# Age groups > NCIT
age[which(age<=1)] <- "<1"
age[which(age>1 & age<=12)] <- "1-12"
age[which(age>12 & age<=45)] <- "12-45"
age[which(age>45 & age<=65)] <- "45-65"
age[which(age>65)] <- ">65"



age_res <- data.frame(age=age, res=res, stringsAsFactors = F)
age_res <-  mutate(age_res, age=fct_relevel(age,
                                      c("<1", 
                                        "1-12", "12-45", "45-65", ">65")))

# All age
data <- data.frame(table(age, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   


data <-  mutate(data, age=fct_relevel(age,
                                      c("<1", 
                                        "1-12", "12-45", "45-65", ">65")))



age <- ggplot(data, aes(age, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("AGE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
age

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_age.pdf")
grid.arrange(age,
             nrow=1, ncol=1 ) #
dev.off()



# All collection age split by resource
age_res$group <- ifelse(
        age_res$res=="sra_illumina"|age_res$res=="sra_ont"|age_res$res=="sra_ion",
        "SRA",
        age_res$res
)




age_res_ct <- age_res %>% 
        group_by(age,group) %>%
        summarise(count=n())


data <- age_res_ct
#data$group <- as.character(data$group)
#data$age <- as.character(data$age)
# data <-  mutate(data, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))
# 


data <-  mutate(data, group=fct_relevel(group,
                                        c("SRA", "ENA", "GISAID")
)) 

sum(data$count) #   


# data <-  mutate(data, age=fct_relevel(age,
#                                       c("<1", 
#                                         "1-12", "12-45", "45-65", ">65")))
# 


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
age_res  <- ggplot(data, aes(x=age, y=count, fill=group)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("AGE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
age_res 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_age_res.pdf")
grid.arrange(age_res,
             nrow=1, ncol=1 ) #
dev.off()







# -------------------------------------------------------------------------------------
# Sex 

# for (r in resources) {
# }     
run <- c()
sex <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
sex <- c(sex, host_sex)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
sex <- c(sex, host_sex)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
sex <- c(sex, host_sex)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
run <- c(run, length(run_id))
sex <- c(sex, host_sex)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(sex)
length(res)


#plot
# Grpup per sex group





sex_res <- data.frame(sex=sex, res=res, stringsAsFactors = F)

# All sex
data <- data.frame(table(sex, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   


sex <- ggplot(data, aes(sex, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("SEX") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
sex

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sex_test.pdf",
    height = 5, width = 2.5)
grid.arrange(sex,
             nrow=1, ncol=1 ) #
dev.off()



# All collection sex split by resource
sex_res$group <- ifelse(
        sex_res$res=="sra_illumina"|sex_res$res=="sra_ont"|sex_res$res=="sra_ion",
        "SRA",
        sex_res$res
)




sex_res_ct <- sex_res %>% 
        group_by(sex,group) %>%
        summarise(count=n())


data <- sex_res_ct

data <-  mutate(data, group=fct_relevel(group,
                                        c("SRA", "ENA", "GISAID")
)) 

sum(data$count) #   


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
sex_res  <- ggplot(data, aes(x=sex, y=count, fill=group)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("SEX") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
sex_res 
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sex_res_test.pdf",
    height = 5, width = 2.5)
grid.arrange(sex_res,
             nrow=1, ncol=1 ) #
dev.off()

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sex_res.pdf")
grid.arrange(sex_res,
             nrow=1, ncol=1 ) #
dev.off()









# -------------------------------------------------------------------------------------
# Sample source

# for (r in resources) {
# }     
run <- c()
sample <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
sample <- c(sample, sample_source)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
sample <- c(sample, sample_source)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
sample <- c(sample, sample_source)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
sample_source <- rep(NA, length(run_id))
run <- c(run, length(run_id))
sample <- c(sample, sample_source)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(sample)
length(res)

unique(sample)

# fix for plot
sample[which(sample=="bronchoalveolar lavage fluid")] <- "BALF"
sample[which(sample=="oro/naso-pharyngeal swab")] <- "OPS/NPS"
sample[which(sample=="tracheal swab/wash")] <- "TS/TW"
sample[which(sample=="oro/naso-pharyngeal swab, tracheal swab/wash")] <- "OPS/NPS, TS/TW"

#plot
# Grpup per sample group


sample_res <- data.frame(sample=sample, res=res, stringsAsFactors = F)

# All sample
data <- data.frame(table(sample, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   


sample <- ggplot(data, aes(sample, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("SAMPLE SOURCE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
sample

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sample.pdf")
grid.arrange(sample,
             nrow=1, ncol=1 ) #
dev.off()



# All collection sample split by resource
sample_res$group <- ifelse(
        sample_res$res=="sra_illumina"|sample_res$res=="sra_ont"|sample_res$res=="sra_ion",
        "SRA",
        sample_res$res
)




sample_res_ct <- sample_res %>% 
        group_by(sample,group) %>%
        summarise(count=n())


data <- sample_res_ct

data <-  mutate(data, group=fct_relevel(group,
                                        c("SRA", "ENA", "GISAID")
)) 

sum(data$count) #   


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
sample_res  <- ggplot(data, aes(x=sample, y=count, fill=group)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("SAMPLE SOURCE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
sample_res 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_sample_res.pdf")
grid.arrange(sample_res,
             nrow=1, ncol=1 ) #
dev.off()








# -------------------------------------------------------------------------------------
# Seq technology

# for (r in resources) {
# }     
run <- c()
plat <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
plat <- c(plat, platform)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
plat <- c(plat, platform)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
plat <- c(plat, platform)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
platform <- rep(NA, length(run_id))
run <- c(run, length(run_id))
plat <- c(plat, platform)
res <- c(res, rep("GISAID", length(run_id)))


plat[which(sapply(plat, length)==0)] <- NA


plat <- unlist(plat)




run_total <- sum(run) # 38009
run_total
length(plat)
length(res)

unique(plat)
unique(res)

#plot
# Grpup per plat group


plat_res <- data.frame(plat=plat, res=res, stringsAsFactors = F)

# All plat
data <- data.frame(table(plat, useNA = "ifany")) # , stringsAsFactors = F


sum(data$Freq) #   


plat <- ggplot(data, aes(plat, Freq)) +
        geom_col() +
        #geom_bar(stat = 'identity') +
        #coord_flip() +
        ggtitle("plat SOURCE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(label=data$Freq, vjust=-0.1, size=1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=5)) # size=5, 
plat

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_plat.pdf")
grid.arrange(plat,
             nrow=1, ncol=1 ) #
dev.off()



# All collection plat split by resource
plat_res$group <- ifelse(
        plat_res$res=="sra_illumina"|plat_res$res=="sra_ont"|plat_res$res=="sra_ion",
        "SRA",
        plat_res$res
)




plat_res_ct <- plat_res %>% 
        group_by(plat,group) %>%
        summarise(count=n())


data <- plat_res_ct

data <-  mutate(data, group=fct_relevel(group,
                                        c("SRA", "ENA", "GISAID")
)) 

sum(data$count) #   


resource_colors <- c("red", "olivedrab1", "royalblue", "orange")
plat_res  <- ggplot(data, aes(x=plat, y=count, fill=group)) +
        #geom_bar(position='dodge', stat='identity') +
        geom_bar(position='stack', stat='identity') +
        ggtitle("SEQUENCING TECHNOLOGY") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of variant files") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values=resource_colors) +
        #geom_text(label=data$count, vjust=-0.1, size=1) + # 
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1, size=7)) # size=5, 
plat_res 

pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_plat_res.pdf")
grid.arrange(plat_res,
             nrow=1, ncol=1 ) #
dev.off()


######################################################################################
# Grid
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405/files_stats/all_grid.pdf")
grid.arrange(date_res, age_res, sex_res, sample_res,
             nrow=2, ncol=2 ) #
dev.off()





#  Sample id


# for (r in resources) {
# }     
run <- c()
sam <- c()
res <- c()
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_illumina/sra_illumina_metadata.rda")
run <- c(run, length(run_id))
sam <- c(sam, sample_id)
res <- c(res, rep("sra_illumina", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/sra_ont/sra_ont_metadata.rda")
run <- c(run, length(run_id))
sam <- c(sam, sample_id)
res <- c(res, rep("sra_ont", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/genbank/genbank_metadata.rda")
run <- c(run, length(run_id))
sam <- c(sam, sample_id)
res <- c(res, rep("ENA", length(run_id)))
load("~/repolab/work/virusbeacon/datafreeze_2405/gisaid/gisaid_metadata.rda")
run <- c(run, length(run_id))
sam <- c(sam, sample_id)
res <- c(res, rep("GISAID", length(run_id)))



run_total <- sum(run) # 38009
run_total
length(sam[which(!(is.na(sam)))])
length(unique(sam[which(!(is.na(sam)))]))


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
