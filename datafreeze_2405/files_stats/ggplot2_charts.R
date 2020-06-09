library(ggplot2)
library(gridExtra)
library(scales)
library(RColorBrewer)

# Stacked barplot
# df <- data
# bp <- ggplot(df, aes(t, Freq)) +
#         aes(x="", y=Freq, fill=t) +
#         geom_bar(width = 1, stat = "identity") 
# bp


# 
blank_theme <- theme_minimal() +
theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face="bold"))


# Pie chart
my_colors <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", "#666666",
              "#F69F00", "#56B4E9", "#F89F10", "#999999")

my_colors_more <- c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E", "#E6AB02" ,"#A6761D", 
                    "#666666", "limegreen", "#56B4E9", "#F89F10", "lightcoral" , "olivedrab1",  "honeydew2" )
# pie <- bp + 
#         coord_polar("y", start=0) 
#         
# pie 
   
# pie + scale_fill_manual(values=my_colors) + 
#         blank_theme +
#          
#         #geom_text(aes(y=Freq/12 + c(0, cumsum(Freq)[-length(Freq)]),
#                       #label=Freq), size=5)
# 
#         #label=Freq
#         #geom_text(aes(y=Freq/3 + c()     
