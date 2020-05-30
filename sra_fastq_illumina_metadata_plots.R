library(ggplot2)
library(forcats)
library(gridExtra)

#setwd("/Users/claudiavasallovega/repolab/work/virusbeacon")
#source("/Users/claudiavasallovega/repolab/work/virusbeacon/virusbeacon_illumina_xml.R")
source("/Users/claudiavasallovega/repolab/work/virusbeacon/sra_fastq_illumina_metadata.R")
#Graph EXPERIMENT > DESIGN > LIBRARY STRATEGY -------------------------------------------------------------------------
t <-     unlist(sapply(illum_xml_list, function(xml){
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
t <-     unlist(sapply(illum_xml_list, function(xml){
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
t <-     unlist(sapply(illum_xml_list, function(xml){
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
t <- sapply(illum_xml_list, function(xml){
        names(xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$DESIGN$LIBRARY_DESCRIPTOR$LIBRARY_LAYOUT) #PAIRED
})
data <- as.data.frame(table(t))
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


        

#Graph PLATFORM > ILLUMINA MODEL -------------------------------------------------------------------------
t <- unlist(sapply(illum_xml_list, function(xml){
        xml$EXPERIMENT_PACKAGE_SET$EXPERIMENT_PACKAGE$EXPERIMENT$PLATFORM$ILLUMINA$INSTRUMENT_MODEL[[1]]}))
t[which(t=="Illumina MiSeq")] <- "MiSeq"  
t[which(t=="Illumina HiSeq 2500")] <- "HiSeq 2500"  
t[which(t=="Illumina iSeq 100")] <-  "iSeq 100" 
t[which(t=="Illumina MiniSeq" )] <- "MiniSeq" 
t[which(t=="Illumina NovaSeq 6000")] <- "NovaSeq 6000"


data <- as.data.frame(table(t))

colnames(data) <- c("model", "Freq")
ep<- ggplot(data) +
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

# Seq technology
data <- data[1:2,]
colnames(data) <- c("technology", "Freq")
data$technology <- as.character(data$technology)
data$technology[1] <- "Illumina"
data$Freq[1] <- 2649
data$technology[2] <- "Oxford Nanopore"
data$Freq[2] <- 2111
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

tec_pie <-  tec_pie + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("SEQUENCING TECHNOLOGY") +
        theme(axis.text.x = element_blank()) 
tec_pie


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
geos_values <- unique(c(sample_attributes_values$geo_loc_name,
                       sample_attributes_values$`geographic location (country and/or sea)` #  "United Kingdom"
                       #sample_attributes_values$`geographic location (region and locality)` # "Scotland"
                       
))


t <- all_values[which(str_detect(all_tags ,"geo_loc_name|country"))]
length(t) #  2489

#t[(which(t== "Scotland" ))] <- "United Kingdom: Scotland"  
#countries
# countries1 <- unique(str_match(
#         t , "^(.*?):"))[,2][!(is.na(unique(str_match(t, "^(.*?):"))[,2]))]


countries <- unique(
        sapply(t, function(l) {
                str_split(
        l, ":")[[1]][1] }
)
)

for (c in countries) {
        t[str_detect(t, c)] <- as.character(c)
        
}


t[(which(t=="missing"))] <- NA       
t[(which(t=="not applicable"))] <- NA   

length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, "geo_loc_name|country|geographic|locality") & str_detect(all_values_per_tag_per_xml, loc_pattern))]) # 2489



#missing_count <- length(illum_xml_list) -length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, loc_pattern))])

data <- data.frame(table(t), stringsAsFactors = F)
data$t <- as.character(data$t)
sum(data$Freq) # 2488
missing_count <- length(illum_xml_list) - sum(data$Freq)

data <- rbind(data , c("missing", missing_count))
data$Freq <- as.numeric(data$Freq)

data <-  mutate(data, t=fct_relevel(t, 
                                    "Australia",  "USA" , "China", "Germany" ,
                                    "Malaysia" , "South Africa", "Nepal",       
                                     "Peru"   ,      "Cambodia" , "India" , "Israel",     
                                      "missing"))
colnames(data) <- c("country", "Freq")
loc <- ggplot(data, aes(country, Freq)) +
        geom_col() +
        ggtitle("GEO LOCATION") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of runs") +
        theme_minimal() +
        geom_text(label=data$Freq, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
loc

my_colors <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", "#666666",
               "#F69F00", "#56B4E9", "#F89F10", "#999999")
blank_theme <- theme_minimal() +
        theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 14, face="bold"))
loc_bp <- ggplot(data, aes(country, Freq)) +
        aes(x="", y=Freq, fill=country) +
        geom_bar(width = 1, stat = "identity") 
loc_pie <- loc_bp + 
        coord_polar("y", start=0) 

loc_pie <- loc_pie + scale_fill_manual(values=my_colors) + 
        blank_theme +
        ggtitle("GEO LOCATION") +
        theme(axis.text.x = element_blank()) 
loc_pie


# Host Disease -------------------------------------------------------------------------
t <- all_values[which(str_detect(all_tags ,"host_disease"))]
#sum(grepl("host_disease", all_tags,fixed = T)) # 438
# these two are non reduntant

#t <- all_values[which(str_detect(all_tags ,age_pattern))]
#t <- all_values[which(grepl("host_disease", all_tags))]
t[which(t=="severe acute respiratory syndrome")] <- "SARS"
disease_values <- c("COVID-19", "nCoV pneumonia", "SARS" )

t <- t[which(t %in% disease_values)] # 1082
data <- as.data.frame(table(t))

data$t <- as.character(data$t)

data <- rbind(data, c("missing", 1094-sum(data$Freq)))
data$Freq <- as.numeric(data$Freq)

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



# Graph Sex -------------------------------------------------------------------------
t <- all_values[which(str_detect(all_tags ,"sex"))] # sex and host sex
data <- as.data.frame(table(t))
missing_count <- length(all_values_per_tag_per_xml) - sum(data$Freq)
data$Freq[which(data$t=="missing")]  <- sum(data$Freq[which(data$t=="missing")],missing_count)
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
        ggtitle("SEX")
        theme(axis.text.x = element_blank()) 
sex_pie


# Graph Age -------------------------------------------------------------------------
age_pattern <- "^age|host_age"

sum(grepl("^age", all_tags)) # 2
sum(grepl("host_age", all_tags)) # 329
# these two are non reduntant

t <- all_values[which(grepl(age_pattern, all_tags))] # 84 values
t[which(t=="missing")] <- NA
t <- t[!(is.na(t))]
age_values <- paste0(t, collapse = "|")

# samples with age
#length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml,age_pattern) & str_detect(all_values_per_tag_per_xml, age_values)]) # 905
# not working, some number can be in other tag

t[which(t=="4 years")] <- 4
t <- as.numeric(t)

# age groups > NCIT
t[which(t<=1)] <- "<1"
t[which(t>1&t<=12)] <- "1-12"
t[which(t>12&t<=45)] <- "12-45"
t[which(t>45&t<=65)] <- "45-65"
t[which(t>65)] <- ">65"


data <- as.data.frame(table(t), stringsAsFactors = FALSE)

# data$t <- as.numeric(data$t)
# order <- as.character(data$t)

sum(data$Freq) # 888

missing_count <- length(all_values_per_tag_per_xml)  - sum(data$Freq) # 206

# missing
data$t <- as.character(data$t)
data <- rbind(data, c("missing", missing_count))



#data <-  mutate(data, t=fct_relevel(t, 
                            # c(as.character(sort(as.numeric(t)[!(is.na(as.numeric(t)))])), "missing")))

data <-  mutate(data, t=fct_relevel(t,
                      c("<1", "1-12", "12-45", "45-65", ">65")))

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
        geom_text(aes(label=Freq, vjust=-0.1), size=2.5) +
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

t[which(t=="01-Mar-2020")] <- as.character(as.Date.character("01-Mar-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="10-Mar-2020")] <- as.character(as.Date.character("10-Mar-2020", "%d-%B-%Y")) #"2020-03-01"
t[which(t=="14-Mar-2020")] <- as.character(as.Date.character("14-Mar-2020", "%d-%B-%Y")) # "2020-02-28"
t[which(t=="17-Mar-2020")] <- as.character(as.Date.character("17-Mar-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="18-Mar-2020")] <- as.character(as.Date.character("18-Mar-2020", "%d-%B-%Y")) #"2020-01-02"
t[which(t=="19-Mar-2020")] <- as.character(as.Date.character("19-Mar-2020", "%d-%B-%Y")) # ""2020-03-19""
t[which(t=="19-MAR-2020")] <- as.character(as.Date.character("19-MAR-2020", "%d-%B-%Y")) # ""2020-03-19""
t[which(t=="20-Mar-2020")] <- as.character(as.Date.character("20-Mar-2020", "%d-%B-%Y")) # "2020-01-13"
t[which(t=="21-Mar-2020")] <- as.character(as.Date.character("21-Mar-2020", "%d-%B-%Y")) # "2020-01-13"
t[which(t=="22-Mar-2020" )] <- as.character(as.Date.character("22-Mar-2020" , "%d-%B-%Y")) # "2020-02-27"
t[which(t=="23-Mar-2020")] <- as.character(as.Date.character("23-Mar-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="29-Mar-2020")] <- as.character(as.Date.character("29-Mar-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="30-Mar-2020")] <- as.character(as.Date.character("30-Mar-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="31-Mar-2020")] <- as.character(as.Date.character("31-Mar-2020", "%d-%B-%Y")) # "2020-02-29"


t[which(t=="01-Apr-2020")] <- as.character(as.Date.character("01-Apr-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="02-Apr-2020")] <- as.character(as.Date.character("02-Apr-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="03-Apr-2020")] <- as.character(as.Date.character("03-Apr-2020", "%d-%B-%Y")) # "2020-02-29"
t[which(t=="05-Apr-2020")] <- as.character(as.Date.character("05-Apr-2020", "%d-%B-%Y")) # "2020-02-29"

t[which(t=="not applicable")]   <- NA


# there is an error  2019-01-19 is not a valid date for this virus!! # 19 samples
# fixme
unique(t[which(str_detect(t, "2019"))])
t[which(t=="2019-01-19")] <- "2020-01-19" 

# Grpup per month
t[str_detect(t, "2019-12")] <- "2019-12"
t[str_detect(t, "2020-01")] <- "2020-01"
t[str_detect(t, "2020-02")] <- "2020-02"
t[str_detect(t, "2020-03")] <- "2020-03"
t[str_detect(t, "2020-04")] <- "2020-04"



# Grpup per month
t[str_detect(t, "2019-12")] <- "Dec 19"
t[str_detect(t, "2020-01")] <- "Jan 20"
t[str_detect(t, "2020-02")] <- "Feb 20"
t[str_detect(t, "2020-03")] <- "Mar 20"
t[str_detect(t, "2020-04")] <- "Apr 20"


# Remove "2020"
t[t=="2020"] <- NA
t[t=="missing"] <- NA




data <- as.data.frame(table(t))
sum(data$Freq) # 2087

missing_count <- length(all_values_per_tag_per_xml)  - sum(data$Freq) #  562
data$t <- as.character(data$t)
data <- rbind(data, c("missing", missing_count))


data$Freq <- as.numeric(data$Freq)
data <-  mutate(data, t=fct_relevel(t, 
                                    "Dec 19", "Jan 20", "Feb 20",     
                                    "Mar 20", "Apr 20"))


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


# Graph Isolation Source -------------------------------------------------------------------------
isolation_pattern <- "isolation_source|tissue|env_medium|host_tissue_sampled"

sample_origin_values <- c(  "Human BALF sample", 
                            "Bronchoalveolar lavage fluid", 
                            "bronchoalveolar lavage fluid(BALF)", 
                            "Oro-pharyngeal swab", 
                            "oropharyngeal swab",  
                            "nasopharynx",  
                            "swab" ,
                            "respiratory nasopharyngeal sample" ,
                            "Nasopharyngeal/throat swab",
                            "Combined nasopharyngeal and oropharyngeal swab", 
                            "oral swab; nasal swab; tracheal wash" , # removed
                            "Diagnostic Swab", 
                            "tracheal swab" ,
                            "bronchoalveolar lavage",
                            # new ones
                            "sputum"     ,
                            "oralpharyngeal" ,
                            "nasal swab", 
                            "oropharynx"
                            #"Stool"
                            )

sample_origin_pattern <- paste0(sample_origin_values, collapse = "|")

        
#t <- all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, pattern = isolation_pattern))]
#t <- all_values[which(all_values %in% sample_origin_values)]
# harmonize for now > later on these values will have to be checked against UBERON 
# ontology and or dictionary of terms
mix_pattern <- "oral swab; nasal swab; tracheal wash"
trachea_pattern <- "tracheal swab"
#pharinx_pattern <- "Oro-pharyngeal|oropharyngeal|Nasopharyngeal/throat|swab|Diagnostic Swab|nasopharynx|nasopharyngeal|nasal"
pharinx_pattern <- "Oro-pharyngeal swab|oropharyngeal swab|nasopharynx|respiratory nasopharyngeal sample|Nasopharyngeal/throat|Combined nasopharyngeal and oropharyngeal swab|Diagnostic Swab|oralpharyngeal|nasal swab|oropharynx"
swab_pattern <- "swab"
balf_pattern <- "bronchoalveolar|Bronchoalveolar|BALF|lavage"
sputum_pattern <- "sputum"
feces_pattern <- "Stool"
#sample_origin_pattern <- paste0(c(mix_pattern, trachea_pattern, pharinx_pattern, swab, balf_pattern ), collapse = "|")

# 
# t[which(str_detect(string = t, pattern = mix_pattern))] <- "mix"        
# t[which(str_detect(string = t, pattern = trachea_pattern))] <- "trachea"
# t[which(str_detect(string = t, pattern = pharinx_pattern))] <- "oro/nasopharynx"
# t[which(str_detect(string = t, pattern = balf_pattern))] <- "BALF"
# t[which(str_detect(string = t, pattern = swab))] <- "swab"


#data$t[which(str_detect(string = t, pattern = cel_pattern))] <- "cell passage"
#data$t[which(str_detect(string = t, pattern = unk_pattern))] <- "missing"

#t[(which(!(str_detect(string = t, pattern = isolation_pattern))))] <- "missing"

#data <- as.data.frame(table(t), stringsAsFactors = F)

length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, sample_origin_pattern)]) # 59.. 183
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, mix_pattern )]) # 6...0
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, balf_pattern)]) # 12 ...9
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, trachea_pattern)]) # 1 .. 1
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, pharinx_pattern)]) # 39 + 1 "swab" ... 85
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & (str_detect(all_values_per_tag_per_xml, swab_pattern) & !str_detect(all_values_per_tag_per_xml, paste0(c(pharinx_pattern, trachea_pattern, mix_pattern), collapse = "|")))]) # 1 ..3
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, sputum_pattern)]) # 85
length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, feces_pattern)]) # 85


t <-  c("mix", "trachea", "BALF", "oro/nasopharynx", "sputum") # , "feces"
Freq <- c(
        length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, mix_pattern )]) ,# 6
        length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, trachea_pattern)]) ,# 1
        length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, balf_pattern)]) ,# 12
        sum(length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, pharinx_pattern)]) , # 39 + 1 "swab"
           length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & (str_detect(all_values_per_tag_per_xml, swab_pattern) & !str_detect(all_values_per_tag_per_xml, paste0(c(pharinx_pattern, trachea_pattern, mix_pattern), collapse = "|")))])) ,# 1
        length(all_values_per_tag_per_xml[str_detect(all_values_per_tag_per_xml, isolation_pattern) & str_detect(all_values_per_tag_per_xml, sputum_pattern)]) # 85
        
)
data <- data.frame(t=t, Freq=Freq )
sum(data$Freq)  # 59..  183


# update data to remove duplicates in missing

# all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, isolation_pattern))]
# length(all_values_per_tag_per_xml[which(str_detect(all_values_per_tag_per_xml, isolation_pattern))&])
# # 1093 There are 1093 xmls with sample origin data
missing_count <- length(all_values_per_tag_per_xml) - sum(data$Freq)
# 2466

#data$Freq[which(data$t=="missing")] <- missing_count # 1044
data$t <- as.character(data$t)

data <- rbind(data, c("missing", missing_count))
data$Freq<- as.numeric(data$Freq)
data <-  mutate(data, t=fct_relevel(t, 
                                    "BALF",  "trachea", "oro/nasopharynx" , "sputum",
                                    "mix",     
                                    "missing"))

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
grid.arrange(loc, sex,  source, 
             dis, age, date,
             ncol=3, nrow=2,
             layout_matrix=rbind(
                     c(1,2,3),
                     c(4,5,6)
             )) #
dev.off()


pdf("/Users/claudiavasallovega/repolab/work/virusbeacon/files_stats_bars.pdf")

#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(tec, ep, edll, 
             loc, sex,  age, 
             dis, date, source, 
             ncol=3, nrow=3,
             layout_matrix=rbind(
                     c(1,2,3),
                     c(4,5,6),
                     c(7,8,9)
             )) #
dev.off()





# Figure for stats page

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


