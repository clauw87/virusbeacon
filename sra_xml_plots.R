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
        labs(y="Number of samples") +
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
        labs(y="Number of samples") +
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
        labs(y="Number of samples") +
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
        labs(y="Number of samples") +
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
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
ep


#Graph PLATFORM > ILLUMINA MODEL -------------------------------------------------------------------------
t <- all_tags
data <- as.data.frame(table(t))

tags<- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("Sample attributes") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
tags
      
# How many of each value in a tag

# Graph GEO LOC  -------------------------------------------------------------------------
#pdf("/Users/claudiavasallovega/repolab/work/viralbeacon/geo_loc_illumina.pdf")
t <- all_values[which(str_detect(all_tags ,"geo_loc_name"))]
data <- as.data.frame(table(t))

loc <- ggplot(data) +
        geom_col(aes(t, Freq)) +
        ggtitle("Geo location - Illumina experiments") +
        theme(title = element_text(size = 3)) +
        labs(x="") +
        labs(y="Number of samples") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1))
loc
#dev.off()




# -------------------------------------------------------------------------       
# EXPERIMENT GRID
pdf("experiment.grid.pdf")
#grid.arrange(ep, nrow=2, ncol=3 ) #
grid.arrange(ep, edlst , edlso, edls, edll,  nrow=2, ncol=3 ) #
dev.off()
        
        

