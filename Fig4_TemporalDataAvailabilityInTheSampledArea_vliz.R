######################################
###       Data availability       ### 
# Code Jonas Mortelmans             #
######################################
#### Libraries ####
library(ggplot2)

#### Setup ####
# Set working directory
# setwd("~")


#clean all
rm(list=ls(all=TRUE))

# Load data (export from MIDAS) and prepare columns
data <- read.csv("data/Fig4_dataframe.csv", header=TRUE)
data$date <- as.Date(data$StartDate, "%d-%b-%y")
data$Code <- as.character(data$Code)
data <- data[order(data$Code),]
data$Code <- factor(data$Code, levels = c('LW01', 'LW02', 'W10', 'W09', 'W08', 'W07bis', '435', '421', '780', '710', '700', '330', '230', '130', '215', '120', 'ZG02'))
data$Record <- "1"
data$SumPerTrip <- ""
data$AbsoluteCount <- ""

#Count Records per Trip, per ParameterGroup = the intensity of sampling for a group
data <- transform(data, SumPerTrip=ave(as.numeric(Record), TripNR, ParameterGroup, Record, FUN=length))

#Absolute count per Parametergroup
data <- transform(data, AbsoluteCount=ave(as.numeric(SumPerTrip), ParameterGroup, FUN=length))

#Relative count per Parametergroup
data$RelativeCount <- data$SumPerTrip / data$AbsoluteCount


#First graph, dot size
ggplot(data, aes(x=date, y=Code))+
  theme(panel.background = element_blank())+
  ggtitle(label='Data availability')+
  theme(plot.title = element_text(hjust=0.5))+
  geom_point(aes(size = RelativeCount), show.legend = F) +
  scale_x_date(name = "Year", date_breaks = "1 year", date_labels="%Y")+
  theme(axis.text.x = element_text(angle= 90 ))+
  scale_y_discrete(name= "Station")+
  facet_wrap(~ data$ParameterGroup)

#export PDF
ggsave("fig/4_Temporal data availability in the sampled area.pdf", plot = last_plot(), scale = 2, width = NA, height = NA, dpi = 300, limitsize = TRUE)

