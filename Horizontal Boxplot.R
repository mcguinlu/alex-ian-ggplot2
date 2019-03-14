#################
# GENERAL SETUP #
#################

#Install and attach needed libraries
#install.packages("tidyverse") 
library(tidyverse)


##############
# DATA SETUP #
##############

#Read in data from .csv (will need to update location to reflect your own file system)
Data = read.csv("O:/Documents/Stuff/Projects/rrr/Case reports_V2.csv", header = TRUE)
# View(Data)

#Set-up global options
textsizeheadings <- 3
textsizenormal <- 3

# Rename oddly named colums
names(Data)[1] <- "Patient.ID"
names(Data)[4] <- "Population"

#Reformart Duration of treatment
Data$Duration <- sub("^$", 0, Data$Duration) #Replace empty cells with 0
Data$Duration <- sub("<1", 0.5, Data$Duration)
Data$Duration <- as.numeric(Data$Duration) #Change to numeric datatype, as ggplot prefers this

# Reformat Progression Free Survival (PFS)
Data$PFS <- sub("^$", NA, Data$PFS)
Data$PFS <- sub("<1", 0.5, Data$PFS)
Data$PFS <- sub("Not reported", NA, Data$PFS)
Data$PFS <- as.numeric(Data$PFS)

# Reformar Overall Survival (OS)
Data$OS <- sub("^$", NA, Data$OS)
Data$OS <- sub("<1", 0.5, Data$OS)
Data$OS <- sub("Not reported", NA, Data$OS)
Data$OS <- as.numeric(Data$OS)

#Create variables to position columns
Data$labelpositionid <- rep(-12,times=nrow(Data))
Data$labelpositionstudy <- rep(-7,times=nrow(Data))
Data$labelpositionpopulation <- rep(-2, times=nrow(Data))

#Create  variable to represent ongoing treatment
Data$Ongoing<- sub("^$", NA, Data$Ongoing)
Data$arrowhead <- ifelse((is.na(Data$Ongoing)), NA, Data$Duration+0.3)
Data$arrowbody <- ifelse((is.na(Data$Ongoing)), NA, Data$Duration+0.15)

#Select those with data on duration of treatment
NewData <- Data[which(Data$Duration!=0),]

#Sort by inhibitor and then duration of treatment
NewData <- NewData[order(NewData$BRAF.inhibitor, NewData$Reference, NewData$Duration),]

#Overwrite Patient.ID (used to order barplot) and PatientID (used in table column)
#Note: these are just inverted lists
NewData$Patient.ID <- seq(1,nrow(NewData),by=1)
NewData$PatientIDTable <- seq(nrow(NewData),1,by=-1)


###############
# CREATE PLOT #
###############

#Create plot
p2 <- ggplot(dat = NewData, mapping = aes(x=Patient.ID)) + 
  
  #Create background rectangle for plot area
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 30), fill= "lightgrey") +
  
  #Create bar chart, coloured by type of drug
  geom_bar(mapping = aes(y=Duration, fill = BRAF.inhibitor), 
           position=position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity", 
           color = "black",
           width = 0.8,
           show.legend = TRUE) + 
  
  #Points for progression free survival
  geom_point(mapping = aes(y=PFS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "black",
             shape = 16, #Black circle
             size=2, 
             show.legend = FALSE) +  
  
  #Points for overall survival
  geom_point(mapping = aes(y=OS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "black",
             shape = 8, #Black star
             size=2,
             show.legend = FALSE) +
  
  #Create "ongoing" arrow head
  geom_point(mapping = aes(y=arrowhead),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 62,
             colour = "black",
             size=3, 
             show.legend = FALSE) +
  
  #Create "ongoing" arrow body
  geom_point(mapping = aes(y=arrowbody),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 95,
             colour = "black",
             size=3, 
             show.legend = FALSE) +
  
  #Create columns, positioned on the left of the plot area
  annotate("text", x=NewData$Patient.ID, y=NewData$labelpositionstudy, label = NewData$Reference, size = textsizenormal) +
  annotate("text", x=NewData$Patient.ID, y=NewData$labelpositionpopulation, label = NewData$Population, size = textsizenormal) +
  annotate("text", x=NewData$Patient.ID, y=NewData$labelpositionid, label = NewData$PatientIDTable, size = textsizenormal) +
  
  # Column column headings, bolded
  annotate("text", x=56, y=NewData$labelpositionid, label="Patient ID", size=textsizeheadings, fontface="bold") +
  annotate("text", x=56, y=NewData$labelpositionstudy, label="Reference", size=textsizeheadings, fontface="bold") +
  annotate("text", x=56, y=NewData$labelpositionpopulation, label="Population", size=textsizeheadings, fontface="bold") +
  
  
  
  #Add outside borders to entire image
  geom_segment(aes(x=-Inf, y=-14, xend=-Inf, yend=30)) +
  geom_segment(aes(x=Inf, y=-14, xend=Inf, yend=30)) +
  geom_hline(time1, yintercept = -14, linetype="solid") +
  geom_hline(time1, yintercept = 30, linetype="solid") +
  
  #Add solid vertical lines at 0 months
  geom_hline(time1, yintercept = 0, linetype="solid") +
  
  #Add dashed vertical lines every 6 months
  geom_hline(time1, yintercept = 6, linetype="dashed")+
  geom_hline(time1, yintercept = 12, linetype="dashed")+
  geom_hline(time1, yintercept = 18, linetype="dashed") +
  geom_hline(time1, yintercept = 24, linetype="dashed") +
  
  #Flip orientation of boxplot
  coord_flip() +
  
  #Add chart labels, and remove labels from x-axis
  labs(title="Summary of patient survival", 
       x="", y = "Duration of treatment (Months)")+
  
  #Define scale and tick marks (remember, co-ordinates are flipped)
  scale_y_continuous(breaks=c(0,6,12,18,24,30), expand=c(0,0)) +
  
  #Reduce space between the plot and axes
  scale_x_continuous(expand = c(0.01,0.01)) +
  
  #Remove y-axis ticks and background (grey background of plot created using geom_rect above)
  #ggplot plots layers over each other, so geom_rect must come first
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.95,0.95), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  
  #Add colour scheme
  scale_fill_brewer(palette = "Reds")


###############
# OUTPUT PLOT #
###############

p2


#############
# SAVE PLOT #
#############

#Change location and uncomment to use
# ggsave("C:/Users/lm16564/Downloads/horboxplot.jpeg", units = "in", width=12, height = 8, dpi=1200)
