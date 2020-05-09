library(ggplot2)
library(forcats)
library(gridExtra)

#setwd("/Users/claudiavasallovega/repolab/work/virusbeacon")
source("/Users/claudiavasallovega/repolab/work/virusbeacon/virusbeacon_illumina_xml.R")

#Graph EXPERIMENT > DESIGN > LIBRARY STRATEGY -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_STRATEGY[[1]]
}))
data <- as.data.frame(table(t))

ggplot(data = data,
       aes(Freq, t))

edlst <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY STRATEGY") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edlst



# Graph EXPERIMENT > DESIGN > LIBRARY SOURCE -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SOURCE
}))
data <- as.data.frame(table(t))

ggplot(data = data,
       aes(Freq, t))

edlso <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY SOURCE") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edlso



# Graph EXPERIMENT > DESIGN > LIBRARY SELECTION -------------------------------------------------------------------------
t <-     unlist(sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_SELECTION
}))
data <- as.data.frame(table(t))

ggplot(data = data,
       aes(Freq, t))

edls <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY SELECTION") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edls


# Graph EXPERIMENT > DESIGN > LIBRARY LAYOUT -------------------------------------------------------------------------
t <- sapply(xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
data <- as.data.frame(table(t))

ggplot(data = data,
       aes(Freq, t))

edll<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("LIBRARY LAYOUT") +
        #theme(title = element_text(vjust = 0.5, colour = "red")) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
edll
   
        

#Graph PLATFORM > ILLUMINA MODEL -------------------------------------------------------------------------
t <- sapply(xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]})
data <- as.data.frame(table(t))

ggplot(data = data,
        aes(Freq, t))

ep<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("PLATFORM") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
ep



# -------------------------------------------------------------------------       
# EXPERIMENT GRID
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_experiment_grid.pdf")
# #grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(ep, edlst , edlso, edls, edll,  nrow=2, ncol=3 ) #
# dev.off()
# 

# How many of each value in a tag
#Graph Sample attributes  -------------------------------------------------------------------------
# pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_sample_attributes.pdf")
# t <- all_tags
# data <- as.data.frame(table(t), stringsAsFactors = F)
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


# Graph GEO LOC  -------------------------------------------------------------------------
t <- all_values[which(str_detect(all_tags ,"geo_loc_name|country"))]
australia_pattern <-"Australia"
usa_pattern <-"USA"
china_pattern <-"China"
nepal_pattern <- "Nepal"

loc_pattern <- "Australia|USA|China|Nepal"

#"Seattle, WA"  ,  "USA, WA"  "Tianmen" "Wuhan" "Jingzhou"
#country  USA: CA, San Diego County
t[which(str_detect(string = t, pattern = australia_pattern))] <- "Australia"
t[which(str_detect(string = t, pattern = usa_pattern))] <- "USA"
t[which(str_detect(string = t, pattern = china_pattern))] <- "China"
t[which(str_detect(string = t, pattern = nepal_pattern))] <- "Nepal"
#t[which(str_detect(string = t, pattern = australia_pattern))] <- "Australia"

length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, loc_pattern))]) # 2 missing
#t[(which(!(str_detect(string = t, pattern = loc_pattern))))] <- "missing"

data <- data.frame(table(t), stringsAsFactors = F)
data$t <- as.character(data$t)
data <- rbind(data , c("missing", 2))
data$Freq <- as.numeric(data$Freq)

data <-  mutate(data, t=fct_relevel(t, 
                                    "Australia", "USA", "China", "Nepal", "missing"))

loc <- ggplot(data, aes(t, Freq)) +
        geom_col() +
        ggtitle("Geo location") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
loc


# Host Disease -------------------------------------------------------------------------

sum(grepl("host_disease", all_tags,fixed = T)) # 438
# these two are non reduntant

#t <- all_values[which(str_detect(all_tags ,age_pattern))]
t <- all_values[which(grepl("host_disease", all_tags))]

data <- as.data.frame(table(t))
data <- data[-c(1, 5),]
data$t <- as.character(data$t)
data[which(data$t=="severe acute respiratory syndrome"),]$t <- "SARS"
data <- rbind(data, c("missing", "2"))
data$Freq <- as.numeric(data$Freq)

data <-  mutate(data, t=fct_reorder(t, Freq, .desc = T))

dis <- ggplot(data, aes(t, Freq)) +
        geom_col() +
        ggtitle("Disease") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
dis




# Graph Sex -------------------------------------------------------------------------
t <- all_values[which(str_detect(all_tags ,"sex"))]
data <- as.data.frame(table(t))

sex <- ggplot(data, aes(t, Freq)) +
        geom_col() +
        ggtitle("Sex") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
sex


# Graph Age -------------------------------------------------------------------------
age_pattern <- "^age|host_age"

sum(grepl("^age", all_tags)) # 2
sum(grepl("host_age", all_tags)) # 329
# these two are non reduntant

#t <- all_values[which(str_detect(all_tags ,age_pattern))]
t <- all_values[which(grepl(age_pattern, all_tags))]

t[(which(str_detect(string = t, pattern = "missing")))] <- NA


data <- as.data.frame(table(t))
data$t <- as.numeric(as.character(data$t))
#data[which(data$t==155),]$Freq <- 

# all_values_per_tag_per_xml[setdiff(which(str_detect(all_tags_per_xml,"age")),  # 
#         which(str_detect(all_tags_per_xml,"host_age")))] # 22 ... 2 additional
# 
# 
# length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, "age"))]) # 354 
# length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, "host_age"))]) # 329

# missing
#length(all_values_per_tag_per_xml[which(!(str_detect(all_values_per_tag_per_xml, age_pattern)))]) # 110
# length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, age_pattern))]) # 329

age <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("Age (missing 108)") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 0, hjust=1, vjust = 1))
age



# Graph Collection Date -------------------------------------------------------------------------
t <- all_values[which(str_detect(all_tags ,"collection_date"))]
# t <- sapply(t, function(l) {
#         as.character(as.Date.character(l,  "%d-%B-%Y"))}) # no, this broken good ones, is there a way

#stringi::stri_datetime_format("20-Jan-2010", time = )

# to check valid dates
t[which(t=="01-Mar-2020")] <- as.character(as.Date.character("01-Mar-2020", "%d-%B-%Y")) #"2020-03-01"
t[which(t=="02-Jan-2020")] <- as.character(as.Date.character("02-Jan-2020", "%d-%B-%Y")) #"2020-01-02"
t[which(t=="13-Jan-2020")] <- as.character(as.Date.character("13-Jan-2020", "%d-%B-%Y")) # "2020-01-13"
t[which(t=="27-Feb-2020")] <- as.character(as.Date.character("27-Feb-2020", "%d-%B-%Y")) # "2020-02-27"
t[which(t=="28-Feb-2020")] <- as.character(as.Date.character("28-Feb-2020", "%d-%B-%Y")) # "2020-02-28"
t[which(t=="29-Feb-2020")] <- as.character(as.Date.character("29-Feb-2020", "%d-%B-%Y")) # "2020-02-29"

# there is an error  2019-01-19 is not a valid date for this virus!! # 19 samples
# fixme
unique(t[which(str_detect(t, "2019"))])
t[which(t=="2019-01-19")] <- "2020-01-19" 

data <- as.data.frame(table(t))

date <- ggplot(data, aes(t, Freq)) +
        geom_col() +
        ggtitle("Collection Date (missing 2, wrong date fixed)") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        #geom_text(label=data$Freq, vjust=-0.5 ) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
date



# Graph Isolation Source -------------------------------------------------------------------------
sample_origin_values <- c("Human BALF sample", "Bronchoalveolar lavage fluid", "bronchoalveolar lavage fluid(BALF)",
                          "Oro-pharyngeal swab", "oropharyngeal swab", 
                          "nasopharynx", 
                          "swab"
                          #"passage" , no this is in addition to source of course
)
isolation_pattern <- "isolation_source|tissue|env_medium|host_tissue_sampled"
 
#t <- all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, pattern = isolation_pattern))]
t <- all_values[which(str_detect(all_tags , isolation_pattern))]
# harmonize for now > later on these values will have to be checked against UBERON 
# ontology and or dictionary of terms
swab_pattern <- "Oro-pharyngeal|oropharyngeal|swab|nasopharynx"
balf_pattern <- "bronchoalveolar|Bronchoalveolar|BALF"
#cel_pattern <- "passage|cell|culture"
#unk_pattern <- "missing|unknown"
#source_pattern <- paste(c("Human BALF sample", "Bronchoalveolar lavage fluid", "bronchoalveolar lavage fluid(BALF)",
                       #   "Oro-pharyngeal swab", "oropharyngeal swab", 
                        #  "nasopharynx", 
                         # "swab"), collapse = "|")
source_pattern <-"Oro-pharyngeal|orophar|swab|nasopharynx|bronchoalveolar|Broncho|BALF"
t[which(str_detect(string = t, pattern = swab_pattern))] <- "oro/nasopharynx"
t[which(str_detect(string = t, pattern = balf_pattern))] <- "BALF"
#data$t[which(str_detect(string = t, pattern = cel_pattern))] <- "cell passage"
#data$t[which(str_detect(string = t, pattern = unk_pattern))] <- "missing"

t[(which(!(str_detect(string = t, pattern = source_pattern))))] <- "missing"

data <- as.data.frame(table(t), stringsAsFactors = F)

# update data to remove duplicates in missing

# all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, source_pattern))]
length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, source_pattern))])
# 33 There are 33 xmls with sample origin data
with_no_data <- length(all_values_per_tag_per_xml)- length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, source_pattern))])
data$Freq[which(data$t=="missing")] <- with_no_data # 406 no info on isolation source

data <-  mutate(data, t=fct_reorder(t, Freq))
source <- ggplot(data, aes(t, Freq)) + # label=data$t
        geom_col() +
        ggtitle("Isolation source") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
source  


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
# 


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_sample_gridTRY.pdf")

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(loc, sex,  source, dis, 
             age, date,
            ncol=4, nrow=2,
           layout_matrix=rbind(
                   c(1,2,3, 4),
                   c(5,6,6, 6)
           )) #
dev.off()

# #grid.arrange(ep, nrow=2, ncol=3 ) #
# grid.arrange(loc, 
#              sex,  source,  
#              ncol=2, nrow=2,
#              layout_matrix=rbind(
#                      c(2,3),
#                      c(1,1)
#              )) #





