rm(list = ls())


# METADATA BY RESOURCES

resource <- "sra_illumina"

# for (r in resources) {
#         if r == "sra-"
# }




library(ggplot2)
#library(cowplot)
library(forcats)
library(gridExtra)
library(stringr)

source("/Users/claudiavasallovega/repolab/work/virusbeacon/ggplot2_charts.R")


setwd("/Users/claudiavasallovega/repolab/work/virusbeacon/datafreeze_2405")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

r  <- resource
path = paste0(paste(getwd(),eval(r), sep="/"), "/")   

load(file = paste0(path , eval(r), "_metadata", ".rda")) 
# load(paste0(eval(resource),"_metadata.rda"))






#Graph PLATFORM (SEQ TECHNOLOGY) -------------------------------------------------------------------------

# tec <- platform

tec <- sapply(platform, function(p) {
        ifelse(length(unlist(p))>0,
               unlist(p),
        NA)
})
       
        
        
data <- as.data.frame(table(tec, useNA = "ifany"))

# missing_count <- length(run_id) - sum(data$Freq)
# 
# 
# data <- rbind(data , c( "<NA>", missing_count))
# 
# data$Freq <- as.numeric(data$Freq)
sum(data$Freq) #   3053

colnames(data) <- c("technology", "Freq")

tec <- ggplot(data) +
        geom_col(aes(technology, Freq)) +
        ggtitle("SEQ TECHNOLOGY") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
tec
tec_bp <- ggplot(data, aes(technology, Freq)) +
        aes(x="", y=Freq, fill=technology) +
        geom_bar(width = 1, stat = "identity") 
tec_pie <- tec_bp + 
        coord_polar("y", start=0) 

tec_pie <-  tec_pie + scale_fill_manual(values=my_colors_more ) + 
        blank_theme +
        ggtitle("SEQUENCING TECHNOLOGY") +
        theme(axis.text.x = element_blank()) 
tec_pie
save(tec , file=paste0(eval(resource), "_", "tec", ".rda"))
save(tec_pie , file=paste0(eval(resource), "_", "tec_pie", ".rda"))



#Graph PLATFORM MODEL -------------------------------------------------------------------------
# ep <- unlist(sapply(xml_list, function(xml){
# #         xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]}))


# harmonize
platform_model[which(platform_model=="Illumina MiSeq" )] <- "MiSeq"
platform_model[which(platform_model=="Illumina MiniSeq" )] <- "MiniSeq"
platform_model[which(platform_model=="Illumina HiSeq 2500")] <- "HiSeq 2500"
platform_model[which(platform_model=="Illumina iSeq 100"   )] <- "iSeq 100"


#ep <- platform_model

ep <- sapply(platform_model, function(p) {
        ifelse(length(unlist(p))>0,
               unlist(p),
               NA)
})
ep[which(ep=="NULL")] <- NA 


data <- as.data.frame(table(ep,useNA = "ifany"))


# missing_count <- length(run_id) - sum(data$Freq)
# 
# 
# data <- rbind(data , c( "<NA>", missing_count))
# 
# data$Freq <- as.numeric(data$Freq)
sum(data$Freq) #   3053



colnames(data) <- c("model", "Freq")
ep <- ggplot(data) +
        geom_col(aes(model, Freq)) +
        ggtitle("PLATFORM MODEL") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
ep
ep_bp <- ggplot(data, aes(model, Freq)) +
        aes(x="", y=Freq, fill=model) +
        geom_bar(width = 1, stat = "identity") 
ep_pie <- ep_bp + 
        coord_polar("y", start=0) 

ep_pie <- ep_pie  + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("PLATFORM MODEL") +
        theme(axis.text.x = element_blank()) 
ep_pie 
save(ep_pie , file=paste0(eval(resource), "_", "ep_pie", ".rda"))
save(ep , file=paste0(eval(resource), "_", "ep", ".rda"))


# For each platform stats

#Graph EXPERIMENT > DESIGN > LIBRARY STRATEGY -------------------------------------------------------------------------
# edlst <-     unlist(sapply(illum_xml_list, function(xml){
#         xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY[[1]]
# }))
# 
# edlst <- exp_library_strategy
# 
# data <- as.data.frame(table(edlst, useNA = "ifany"))
# 
# ggplot(data = data,
#        aes(Freq, edlst))
# 
# edlst <- ggplot(data) +
#         geom_col(aes(edlst, Freq)) +
#         ggtitle("LIBRARY STRATEGY") +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# edlst



# Graph EXPERIMENT > DESIGN > LIBRARY SOURCE -------------------------------------------------------------------------
# t <-     unlist(sapply(xml_list, function(xml){
#         xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
# }))
# 
# edlso <- exp_library_source
# 
# data <- as.data.frame(table(edlso, useNA = "ifany"))
# 
# ggplot(data = data,
#        aes(Freq, edlso))
# 
# edlso <- ggplot(data) +
#         geom_col(aes(edlso, Freq)) +
#         ggtitle("LIBRARY SOURCE") +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# edlso



# Graph EXPERIMENT > DESIGN > LIBRARY SELECTION -------------------------------------------------------------------------
# edls  <-     unlist(sapply(xml_list, function(xml){
#         xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
# }))
# 
# edls  <- exp_library_selection
# 
# data <- as.data.frame(table(edls, useNA = "ifany"))
# 
# ggplot(data = data,
#        aes(Freq, edls ))
# 
# edls <- ggplot(data) +
#         geom_col(aes(edls , Freq)) +
#         ggtitle("LIBRARY SELECTION") +
#         #theme(title = element_text(vjust = 0.5, colour = "red")) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# edls



# Graph EXPERIMENT > DESIGN > LIBRARY LAYOUT -------------------------------------------------------------------------
# edll <- sapply(xml_list, function(xml){
#         names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
# })

edll <- library_layout

data <- as.data.frame(table(edll, useNA = "ifany"))
colnames(data) <- c("layout", "Freq")

edll<- ggplot(data) +
        geom_col(aes(layout, Freq)) +
        ggtitle("LIBRARY LAYOUT") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edll


edll_bp <- ggplot(data, aes(layout, Freq)) +
        aes(x="", y=Freq, fill=layout) +
        geom_bar(width = 1, stat = "identity") 
edll_pie <- edll_bp + 
        coord_polar("y", start=0) 

edll_pie <- edll_pie + scale_fill_manual(values=my_colors) +
        ggtitle("LIBRARY LAYOUT") +
        blank_theme +
        theme(axis.text.x = element_blank()) 
edll_pie
save(edll_pie , file=paste0(eval(resource), "_", "edll_pie", ".rda"))


# Graph GEO LOC  -------------------------------------------------------------------------

loc <- geo_loc


loc <- sapply(geo_loc, function(p) {
        ifelse(length(unlist(p))>0,
               unlist(p),
               NA)
})

length(loc) #  3053


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

length(countries) #  14

for (c in countries) {
        loc[str_detect(loc, c)] <- as.character(c)
        
}

data <- data.frame(table(loc, useNA = "ifany")) # , stringsAsFactors = F
#data$loc <- as.character(data$loc)
sum(data$Freq) #   1497
# 
# # missing 
# missing_count <- length(run_id) - sum(data$Freq)
# 
# data <- rbind(data , c( "<NA>", missing_count))
# 
# data$Freq <- as.numeric(data$Freq)
# sum(data$Freq) #   1497

# lets keep it alphabetical order
# data <-  mutate(data, loc=fct_relevel(loc, 
#                                     "United Kingdom", "China", "USA", 
#                                     #"Nepal",
#                                     "Australia", 
#                                     #"Peru", "Cambodia",  
#                                     "Malaysia" , 
#                                     #"South Africa", 
#                                     "India", 
#                                     #"Israel",
#                                      "Germany" , 
#                                     "Brazil", "Colombia", "Denmark", "Hong Kong", "Netherlands"
#                                     #"Egypt"
#                                     )) # NA 

colnames(data) <- c("country", "Freq")

loc <- ggplot(data, aes(country, Freq)) +
        geom_col() +
        ggtitle("GEO LOCATION") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1) + # , size=1
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1)) # size=5, 
loc
save(loc , file=paste0(eval(resource), "_", "loc", ".rda"))

# pie not good with so many values

# my_colors_more <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", 
#                "#666666", "limegreen", "#56B4E9", "#F89F10", "lightcoral" , "olivedrab1",  "honeydew2" )
# # "#F69F00"
# # ugly gray "#999999"
# blank_theme <- theme_minimal() +
#         theme(
#                 axis.title.x = element_blank(),
#                 axis.title.y = element_blank(),
#                 panel.border = element_blank(),
#                 panel.grid = element_blank(),
#                 axis.ticks = element_blank(),
#                 plot.title = element_text(size = 14, face="bold"))
# loc_bp <- ggplot(data, aes(country, Freq)) +
#         aes(x="", y=Freq, fill=country) +
#         geom_bar(width = 1, stat = "identity") 
# loc_bp 
# loc_pie <- loc_bp + 
#         coord_polar("y", start=0) 
# loc_pie
# loc_pie <- loc_pie + 
#         #scale_fill_manual(values=my_colors_more) + 
#         blank_theme +
#         ggtitle("GEO LOCATION") +
#         theme(axis.text.x = element_blank()) 
# loc_pie



# Graph Sex -------------------------------------------------------------------------

sex <- host_sex
sex<- sapply(host_sex, function(p) {
        ifelse(length(unlist(p))>0,
               unlist(p),
               NA)
})

data <- as.data.frame(table(sex,  useNA = "ifany"))


# # missing 
# missing_count <- length(run_id) - sum(data$Freq) # 3851
# 
# data <- rbind(data , c( "<NA>", missing_count))

data$Freq <- as.numeric(data$Freq)
sum(data$Freq) # 29629


colnames(data) <- c("sex", "Freq")

sex <- ggplot(data, aes(sex, Freq)) +
        geom_col() +
        ggtitle("SEX") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
sex
sex_bp <- ggplot(data, aes(sex, Freq)) +
        aes(x="", y=Freq, fill=sex) +
        geom_bar(width = 1, stat = "identity") 
sex_pie <- sex_bp + 
        coord_polar("y", start=0) 

sex_pie <- sex_pie + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("SEX") +
        theme(axis.text.x = element_blank()) 
sex_pie
save(sex_pie , file=paste0(eval(resource), "_", "sex_pie", ".rda"))
save(sex , file=paste0(eval(resource), "_", "sex", ".rda"))


# Graph Age -------------------------------------------------------------------------
age <- host_age


# Age groups > NCIT
age[which(age<=1)] <- "<1"
age[which(age>1 & age<=12)] <- "1-12"
age[which(age>12 & age<=45)] <- "12-45"
age[which(age>45 & age<=65)] <- "45-65"
age[which(age>65)] <- ">65"


data <- as.data.frame(table(age, useNA = "ifany")) # , stringsAsFactors = FALSE

# data$t <- as.numeric(data$t)
# order <- as.character(data$t)

sum(data$Freq) # 

#missing_count <- length(run_id)   - sum(data$Freq) # 19218

# missing
#data$t <- as.character(data$t)
#data <- rbind(data, c("<NA>", missing_count))

data$Freq <- as.numeric(data$Freq)
sum(data$Freq)  #3053

# data <-  mutate(data, age=fct_relevel(age, 
#                              sort(age[!(is.na(age))]))) # NA

data <-  mutate(data, age=fct_relevel(age,
                      c("<1", 
                              "1-12", "12-45", "45-65", ">65")))

colnames(data) <-c("age", "Freq")
#data$age <- as.character(data$age)
data$Freq <- as.numeric(data$Freq)

age <- ggplot(data, aes(age, Freq)) +
        geom_col() +
        ggtitle("AGE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        #geom_text(aes(label=Freq, vjust=-0.1), size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
age


age_bp <- ggplot(data, aes(age, Freq)) +
        aes(x="", y=Freq, fill=age) +
        geom_bar(width = 1, stat = "identity") 
        #geom_text(aes(label=Freq),  position = position_stack(vjust=0.5)) # size=2.5,
        #geom_text(aes(label=Freq, x = 1.3),  position = position_stack(vjust=0.5))
        
age_bp 
age_pie <- age_bp + 
        labs(fill="age")+
        coord_polar("y", start=0) + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("AGE") +
        theme(axis.text.x = element_blank()) 
age_pie
save(age , file=paste0(eval(resource), "_", "age", ".rda"))
save(age_pie , file=paste0(eval(resource), "_", "age_pie", ".rda"))


# Graph Collection Date -------------------------------------------------------------------------
date <- collection_date


# Group per month
# date[str_detect(date, "2019-12")] <- "2019-12"
# date[str_detect(date, "2020-01")] <- "2020-01"
# date[str_detect(date, "2020-02")] <- "2020-02"
# date[str_detect(date, "2020-03")] <- "2020-03"
# date[str_detect(date, "2020-04")] <- "2020-04"

# Grpup per month
date[str_detect(date, "2019-12")] <- "Dec 2019"
date[str_detect(date, "2020-01")] <- "Jan 2020"
date[str_detect(date, "2020-02")] <- "Feb 2020"
date[str_detect(date, "2020-03")] <- "Mar 2020"
date[str_detect(date, "2020-04")] <- "Apr 2020"
date[str_detect(date, "2020-05")] <- "May 2020"

# date[str_detect(date, "2020-06")] <- "Jun 2020"
# date[str_detect(date, "2020-07")] <- "Jul 2020"
# date[str_detect(date, "2020-08")] <- "Aug 2020"
# date[str_detect(date, "2020-09")] <- "Sep 2020"
# date[str_detect(date, "2020-10")] <- "Oct 2020"
# date[str_detect(date, "2020-11")] <- "Nov 2020"
# date[str_detect(date, "2020-12")] <- "Dec 2020"
# 
# date[str_detect(date, "2021-01")] <- "Jan 2021"
# date[str_detect(date, "2021-02")] <- "Feb 2021"
#date[str_detect(date, "2021-03")] <- "Mar 2021"
#date[str_detect(date, "2021-04")] <- "Apr 2021"
#date[str_detect(date, "2021-05")] <- "May 2021"

# Remove "2020"
date[date=="2020"] <- NA
date[date=="missing"] <- NA
date[date=="Missing"] <- NA


data <- as.data.frame(table(date, useNA = "ifany")) # # , stringsAsFactors = FALSE
sum(data$Freq) # 3053

#missing_count <- length(run_id) - sum(data$Freq) #  374

#data$t <- as.character(data$t)
#data <- rbind(data, c("<NA>", missing_count))


data$Freq <- as.numeric(data$Freq)
data <-  mutate(data, date=fct_relevel(date, 
                                    "Dec 2019", 
                                    "Jan 2020", "Feb 2020",     
                                    "Mar 2020", "Apr 2020", 
                                    "May 2020"#, "Jun 2020", "Jul 2020",
                                    #"Aug 2020", "Sep 2020", "Oct 2020",
                                    #"Nov 2020", "Dec 2020",
                                    #"Jan 2021", "Feb 2021" #"Mar 2021", "Apr 2021", "May 2021"
                                    ))


colnames(data) <- c("date", "Freq")
date <- ggplot(data, aes(date, Freq)) +
        geom_col() +
        ggtitle("COLLECTION DATE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.5 ) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
date
save(date , file=paste0(eval(resource), "_", "date",".rda"))


date_bp <- ggplot(data, aes(date, Freq)) +
        aes(x="", y=Freq, fill=date) +
        geom_bar(width = 1, stat = "identity") 
date_pie <- date_bp + 
        #geom_text(aes(label=Freq, x = 1.3),  position = position_stack(vjust=0.5)) +
        coord_polar("y", start=0) 

date_pie <- date_pie + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("COLLECTION DATE") +
        #geom_text(aes(label=Freq, x = 1.3),  position = position_stack(vjust=0.5)) + # ,
        theme(axis.text.x = element_blank()) 
date_pie
save(date_pie , file=paste0(eval(resource), "_", "date_pie", ".rda"))
save(date , file=paste0(eval(resource), "_", "date", ".rda"))


# Graph Isolation Source -------------------------------------------------------------------------
sample_source <- sample_source


data <- as.data.frame(table(sample_source, useNA = "ifany")) # # , stringsAsFactors = FALSE
sum(data$Freq) # 52
# missing_count <- length(run_id) - sum(data$Freq) # 1445
# #data$t <- as.character(data$t)
# data <- rbind(data, c("<NA>", missing_count))
data$Freq<- as.numeric(data$Freq)
unique(sample_source)

data <-  mutate(data, sample_source=fct_relevel(sample_source, 
                                         "bronchoalveolar lavage fluid", 
                                         "oro/naso-pharyngeal swab"   ,
                                         "tracheal swab/wash"))


colnames(data) <- c("source", "Freq")
source <- ggplot(data, aes(source, Freq)) + # label=data$t
        geom_col() +
        ggtitle("SAMPLE TYPE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
source  

source_bp <- ggplot(data, aes(source, Freq)) +
        aes(x="", y=Freq, fill=source) +
        geom_bar(width = 1, stat = "identity") 
source_pie <- source_bp + 
        #geom_text(aes(label=Freq, x = 1.3),  position = position_stack(vjust=0.5)) +
        coord_polar("y", start=0) 

source_pie <- source_pie + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("SAMPLE TYPE") +
        theme(axis.text.x = element_blank()) 
source_pie
save(source_pie, file=paste0(eval(resource), "_", "source_pie", ".rda"))
save(source, file=paste0(eval(resource), "_", "source", ".rda"))


# Host Disease -------------------------------------------------------------------------
dis <- disease

# harmonize

data <- as.data.frame(table(dis, useNA = "ifany"))
sum(data$Freq) # 1141

# missing_count <- length(run_id) - sum(data$Freq) # 356
# data <- rbind(data, c("<NA>", missing_count))

data$Freq<- as.numeric(data$Freq)

#data <-  mutate(data, disease=fct_reorder(t, Freq, .desc = T))
data <-  mutate(data, t=fct_relevel(t, 
                                    "COVID-19", "nCoV pneumonia", "SARS",     
                                    "missing"))
colnames(data) <- c("disease", "Freq")
dis <- ggplot(data, aes(disease, Freq)) +
        geom_col() +
        ggtitle("DISEASE") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
dis
dis_bp <- ggplot(data, aes(disease, Freq)) +
        aes(x="", y=Freq, fill=disease) +
        geom_bar(width = 1, stat = "identity") 
dis_pie <- dis_bp + 
        coord_polar("y", start=0) 

dis_pie <- dis_pie  + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("DISEASE") +
        theme(axis.text.x = element_blank()) 
dis_pie 
save(dis_pie , file=paste0(eval(resource), "_", "dis_pie", ".rda"))



# SAMPLE ATTRIBUTES GRID
#pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_sample_grid.pdf")
#grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(loc, sex ,  source,  
#              arrangeGrob(age, date, ncol=2),    
#              nrow=2) #
# 
# #dev.off()
# 
# 
# 
# library(grid)
# #grid.newpage()
# define_region <- function(row, col) {
#         viewport(layout.pos.row =  row,
#                  layout.pos.col = col)        
# }
# 
# # LAYOUT 2 X 3
# pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 3)))
# 
# print(loc, define_region(row=1, col=1))
# print(sex, define_region(row=1, col=2))
# print(source, define_region(row=1, col=3))
# print(age, define_region(row=2, col=1:2))
# print(date, define_region(row=2, col=2:3))




# ----------- GENERAL ACROSS DB : LOC, DATE, SEX, AGE,  for resources comparison -----------#
path = paste0(paste(getwd(),eval(r), sep="/"), "/")  
file <- paste0(path,  eval(r), "_", "gral_grid.pdf")
pdf(file, width = 25, height = 6)

grid.arrange(loc, date, sex_pie, age_pie,
             #dis, age, date,
             ncol=4, nrow=1,
             layout_matrix=rbind(
                     c(1,2,3,4)
             )) #
#cowplot
#p <- plot_grid()
dev.off()




# ----------- # GENERAL for raw: techology, platform, library layout  -----------#
# -------------------------------------------------------------------------       
# EXPERIMENT GRID

# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_experiment_grid.pdf")
# #grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(ep, edlst , edlso, edls, edll,  nrow=2, ncol=3 ) #
# dev.off()
# 
path = paste0(paste(getwd(),eval(r), sep="/"), "/")  
file <- paste0(path,  eval(r), "_", "experiment.pdf")

pdf(file,width = 6, height = 12)
#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(ep_pie , edll_pie, 
             nrow=2, ncol=1 ) #
dev.off()
# -------------------------------------------------------------------------       



















###################################################################################################

# OLD

# Figure for stats page
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_bars.pdf")
# 
# #grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(tec, ep, edll, 
#              loc, sex,  age, 
#              dis, date, source, 
#              ncol=3, nrow=3,
#              layout_matrix=rbind(
#                      c(1,2,3),
#                      c(4,5,6),
#                      c(7,8,9)
#              )) #
# dev.off()



# Stats per files
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_pies1.pdf", width = 25, height = 6)

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(tec_pie, ep_pie, edll_pie,
             ncol=3, nrow=1,
             layout_matrix=rbind(
                     c(1,2,3)
             )) #
dev.off()


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_pies2.pdf", width = 25, height = 6)

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(loc_pie, sex_pie,  age_pie,
             ncol=3, nrow=1,
             layout_matrix=rbind(
                     c(1,2,3)
             )) #
dev.off()


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_pies3.pdf", width = 25, height = 6)

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(dis_pie, date_pie, source_pie,
             ncol=3, nrow=1,
             layout_matrix=rbind(
                     c(1,2,3)
             )) #
dev.off()



# all pies together NO
pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_all_pies.pdf", width = 50, height = 20)

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(tec_pie, ep_pie, edll_pie,
             loc_pie, sex_pie, age_pie,
             dis_pie,  date_pie, source_pie, 
             ncol=3, nrow=3,
             layout_matrix=rbind(
                     c(1,2,3),
                     c(4,5,6),
                     c(7,8,9)
                     
             )) #
dev.off()



# METADATA AVAILABLE
# How many of each value in a tag
#Graph Sample attributes  -------------------------------------------------------------------------
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_sample_attributes.pdf")
# t <- all_tags
# data <- as.data.frame(table(t, useNA = "ifany"))
# 
# tags<- ggplot(data) +
#         geom_col(aes(t, Freq)) +
#         ggtitle("Sample attributes") +
#         theme(title = element_text(size = 3)) +
#         labs(x="") +
#         labs(y="Number of runs") +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
# tags
# dev.off()    
