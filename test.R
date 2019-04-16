#################
# GENERAL SETUP #
#################

#Install and attach needed libraries
# install.packages("tidyverse") 
# library(tidyverse)


##############
# DATA SETUP #
##############

#Read in data from .csv (will need to update location to reflect your own file system)
# Data = read.csv("~/Desktop/Genomic Medicine/Unit 6/STATA/Survivalcurves.csv", header = TRUE)
Data <- read.csv("Survivalcurves.csv", header = TRUE)
View(Data)

#Set-up global options
textsizeheadings <- 3
textsizenormal <- 3

#Reformat Duration of treatment
Data$Duration <- sub("<1", 0.5, Data$Duration.of.BRAF.inhibitor.therapy.in.months)
Data$Duration <- as.numeric(Data$Duration.of.BRAF.inhibitor.therapy.in.months) #Change to numeric datatype, as ggplot prefers this

# Reformat Progression Free Survival (PFS)
Data$PFS <- sub("^$", NA, Data$PFS.from.the.start.of.treatment)
Data$PFS <- sub("<1", 0.5, Data$PFS.from.the.start.of.treatment)
Data$PFS <- sub("NR", NA, Data$PFS.from.the.start.of.treatment)
Data$PFS <- as.numeric(Data$PFS.from.the.start.of.treatment)

# Reformat Overall Survival (OS)
Data$OS <- sub("^$", NA, Data$OS.from.the.start.of.treatment)
Data$OS <- sub("<1", 0.5, Data$OS.from.the.start.of.treatment)
Data$OS <- sub("NR", NA, Data$OS.from.the.start.of.treatment)
Data$OS <- as.numeric(Data$OS.from.the.start.of.treatment)

# Reformat Time to Best Response
Data$TR <- sub("^$", NA, Data$Time.to.best.response..months.)
Data$TR <- sub("<1", 0.5, Data$Time.to.best.response..months.)
Data$TR <- sub("NR", NA, Data$Time.to.best.response..months.)
Data$TR <- as.numeric(Data$Time.to.best.response..months.)

#Create variables to position columns
Data$labelpositionPID <- rep(-22,times=nrow(Data))
Data$labelpositionPopulation <- rep(-17,times=nrow(Data))
Data$labelpositionTumourtype <- rep(-12, times=nrow(Data))
Data$labelpositionGrading <- rep(-2, times=nrow(Data))

#Create  variable to represent ongoing treatment
Data$ongoing<- sub("N", NA, Data$Therapy.on.going..Y.N.)
Data$arrowhead <- ifelse((is.na(Data$Therapy.on.going..Y.N.)), NA, Data$Duration.of.BRAF.inhibitor.therapy.in.months+0.25)
Data$arrowbody <- ifelse((is.na(Data$Therapy.on.going..Y.N.)), NA, Data$Duration.of.BRAF.inhibitor.therapy.in.months+0.15)

#Sort by population and then PID and then duration of treatment
Data <- Data[order(Data$Age..paediatric.adult..18, Data$ï..Patient.ID, Data$Duration.of.BRAF.inhibitor.therapy.in.months),]

#Overwrite Patient.ID (used to order barplot) and PatientID (used in table column)
#Note: these are just inverted lists
Data$Patient.ID <- seq(1,nrow(Data),by=1)
Data$PatientIDTable <- seq(nrow(Data),1,by=-1)


###############
# CREATE PLOT #
###############

#Define general ggplot
p <- ggplot(dat = Data, mapping = aes(x=Patient.ID)) + 
  
  #Create background rectangle for plot area
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 60), fill= "white") +
  
  #Create bar chart, coloured by population
  geom_bar(mapping = aes(y=Duration, fill = Age..paediatric.adult..18), 
           position=position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity", 
           color = "black",
           width = 0.8,
           show.legend = TRUE) + 
  
  #Points for progression free survival
  geom_point(mapping = aes(y=PFS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "brown",
             shape = 17, #Brown triangle
             size = 2, 
             show.legend = FALSE) +  
  
  #Points for overall survival
  geom_point(mapping = aes(y=OS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "black",
             shape = 8, #Black star
             size = 3,
             show.legend = FALSE) +

  #Points for time to response
  geom_point(mapping = aes(y=TR),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "yellow",
             shape = 18, #Yellow diamond
             size = 3,
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
  annotate("text", x=Data$Patient.ID, y=Data$labelpositionPopulation, label = Data$Age..paediatric.adult..18 , size = textsizenormal) +
  annotate("text", x=Data$Patient.ID, y=Data$labelpositionPID, label = Data$PatientIDTable, size = textsizenormal) +
  annotate("text", x=Data$Patient.ID, y=Data$labelpositionTumourtype, label = Data$Tumour.type, size = textsizenormal) +
  annotate("text", x=Data$Patient.ID, y=Data$labelpositionGrading, label = Data$Tumour.grade..HG.LG., size = textsizenormal) +

  
  # Column column headings, bolded
  annotate("text", x=228, y=Data$labelpositionPID, label="Patient ID", size=textsizeheadings, fontface="bold") +
  annotate("text", x=228, y=Data$labelpositionPopulation, label="Population", size=textsizeheadings, fontface="bold") +
  annotate("text", x=228, y=Data$labelpositionTumourtype, label="Tumour Type", size=textsizeheadings, fontface="bold") +
  annotate("text", x=228, y=Data$labelpositionGrading, label="Tumour Grading", size=textsizeheadings, fontface="bold") +
  
  
  #Add outside borders to entire image
  geom_segment(aes(x=-Inf, y=-25, xend=-Inf, yend=60)) +
  geom_segment(aes(x=Inf, y=-25, xend=Inf, yend=60)) +
  geom_hline(time1, yintercept = -25, linetype="solid") +
  geom_hline(time1, yintercept = 60, linetype="solid") +
  
  #Add solid vertical lines at 0 months
  geom_hline(time1, yintercept = 0, linetype="solid") +
  
  #Add dashed vertical lines every 6 months
  geom_hline(time1, yintercept = 6, linetype="dashed")+
  geom_hline(time1, yintercept = 12, linetype="dashed")+
  geom_hline(time1, yintercept = 18, linetype="dashed") +
  geom_hline(time1, yintercept = 24, linetype="dashed") +
  geom_hline(time1, yintercept = 30, linetype="dashed") +
  geom_hline(time1, yintercept = 36, linetype="dashed") +
  geom_hline(time1, yintercept = 42, linetype="dashed") +
  geom_hline(time1, yintercept = 48, linetype="dashed") +
  geom_hline(time1, yintercept = 54, linetype="dashed") +

  #Flip orientation of boxplot
  coord_flip() +
  
  #Add chart labels, and remove labels from x-axis
  labs(title="Summary of Patient Survival", 
       x="", y = "Duration of Treatment (Months)")+
  
  #Define scale and tick marks (remember, co-ordinates are flipped)
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48,54), expand=c(0,0)) +
  
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
  scale_fill_brewer(palette = "Blues")


###############
# OUTPUT PLOT #
###############

p


#############
# SAVE PLOT #
#############

#Change location and uncomment to use
# ggsave("C:/Users/lm16564/Downloads/horboxplot.jpeg", units = "in", width=12, height = 8, dpi=1200)
