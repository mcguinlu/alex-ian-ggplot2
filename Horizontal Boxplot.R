#Install tidyverse
#install.packages("tidyverse")

#Attached tidyverse library

library(tidyverse)
library(scales)

#Read in data from .csv

MyData = read.csv("O:/Documents/Stuff/Projects/rrr/Case reports_V2.csv", header = TRUE)

#Set-up golbal option
textsize <- 3
linelimits<- data.frame(y=c(-15,30), x=c(-Inf,Inf))

#Reformart Duration
MyData$Duration <- sub("^$", 0, MyData$Duration)
MyData$Duration <- sub("<1", 0.5, MyData$Duration)
MyData$Duration <- as.numeric(MyData$Duration)

# Reformat Progression Free Survival (PFS)
MyData$PFS <- sub("^$", NA, MyData$PFS)
MyData$PFS <- sub("<1", 0.5, MyData$PFS)
MyData$PFS <- sub("Not reported", NA, MyData$PFS)
MyData$PFS <- as.numeric(MyData$PFS)
MyData$PFSdiscrete <- ifelse(is.na(MyData$PFS),"Progression Free Survival",NA)

# Reformar Overall Survival (OS)
MyData$OS <- sub("^$", NA, MyData$OS)
MyData$OS <- sub("<1", 0.5, MyData$OS)
MyData$OS <- sub("Not reported", NA, MyData$OS)
MyData$OS <- as.numeric(MyData$OS)
MyData$OSdiscrete <- ifelse(is.na(MyData$OS),"Overall Survival",NA)

#Create variables to position text
MyData$labelpositionid <- rep(-12,times=nrow(MyData))
MyData$labelpositionstudy <- rep(-7,times=nrow(MyData))
MyData$labelpositionpopulation <- rep(-2, times=nrow(MyData))

#Create new variable to represent ongoing treatment
MyData$Ongoing<- sub("^$", NA, MyData$Ongoing)
MyData$arrowhead <- ifelse((is.na(MyData$Ongoing)), NA, MyData$Duration+0.3)
MyData$arrowbody <- ifelse((is.na(MyData$Ongoing)), NA, MyData$Duration+0.15)

NewData <- MyData[which(MyData$Duration!=0),]
NewData <- NewData[order(NewData$BRAF.inhibitor, NewData$Duration),]
NewData$ï..Patient.ID <- seq(1,nrow(NewData),by=1)
NewData$PatientIDTable <- seq(nrow(NewData),1,by=-1)


p2 <- ggplot(NewData, aes(x=ï..Patient.ID, y=Duration)) + 
  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 30), fill= "lightgrey") +
  #Points for progression free survival

  #Bar chart
  geom_bar(aes(fill = BRAF.inhibitor), 
           position=position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity", 
           color = "black",
           width = 0.8,
           show.legend = TRUE) + 
  
  geom_point(aes(y=PFS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "black",
             shape = 16, #Black circle
             size=2, 
             show.legend = FALSE) +  #Points for overall survival
  
  geom_point(aes(y=OS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             colour = "black",
             shape = 8, #Black star
             size=2,
             show.legend = FALSE) +
  
  #Create "ongoing" arrow
  geom_point(aes(y=arrowhead),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 62,
             colour = "black",
             size=3, 
             show.legend = FALSE) +
  geom_point(aes(y=arrowbody),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 95,
             colour = "black",
             size=3, 
             show.legend = FALSE) +
  
  #Create columns
  annotate("text", x=NewData$ï..Patient.ID, y=NewData$labelpositionstudy, label = NewData$Reference, size = textsize) +
  annotate("text", x=NewData$ï..Patient.ID, y=NewData$labelpositionpopulation, label = NewData$Infant...1y...Paediatric...16..or.adult, size = textsize) +
  annotate("text", x=NewData$ï..Patient.ID, y=NewData$labelpositionid, label = NewData$PatientIDTable, size = textsize) +
  
  # Column Headings
  annotate("text", x=56, y=NewData$labelpositionid, label="Patient ID", size=textsize, fontface="bold") +
  annotate("text", x=56, y=NewData$labelpositionstudy, label="Reference", size=textsize, fontface="bold") +
  annotate("text", x=56, y=NewData$labelpositionpopulation, label="Population", size=textsize, fontface="bold") +
  
  #Create legend
  
  
  #Flip orientation of boxplot
  coord_flip() +
  
  #Add vertical lines every 6 months
  geom_segment(aes(x=-Inf, y=-14, xend=-Inf, yend=30)) +
  geom_segment(aes(x=Inf, y=-14, xend=Inf, yend=30)) +
  geom_hline(time1, yintercept = -14, linetype="solid") +
  geom_hline(time1, yintercept = 0, linetype="solid") +
  geom_hline(time1, yintercept = 30, linetype="solid") +
  geom_hline(time1, yintercept = 6, linetype="dashed")+
  geom_hline(time1, yintercept = 12, linetype="dashed")+
  geom_hline(time1, yintercept = 18, linetype="dashed") +
  geom_hline(time1, yintercept = 24, linetype="dashed") +
  
  #Add chart labels, and remove labels from x-axis
  labs(title="Summary of patient survival", 
       x="", y = "Duration of treatment (Months)")+
  
  #Define scale and tick marks on x-axis (remember, co-ordinates are flipped)
  scale_y_continuous(breaks=c(0,6,12,18,24,30), expand=c(0,0)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  
  #Remove y-axis ticks and background
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.95,0.95), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  
  #Add colour scheme
  scale_fill_brewer(palette = "Blues")

#Plot chart
p2

ggsave("C:/Users/lm16564/Downloads/horboxplot.jpeg", units = "in", width=12, height = 8, dpi=1200)
