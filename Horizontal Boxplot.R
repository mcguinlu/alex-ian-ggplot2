MyData = read.csv("O:/Documents/Stuff/Projects/rrr/Case reports_V2.csv", header = TRUE)
MyData$Duration <- sub("^$", 0, MyData$Duration)
MyData$Duration <- sub("<1", 0.5, MyData$Duration)
MyData$Duration <- as.numeric(MyData$Duration)

MyData$PFS <- sub("^$", NA, MyData$PFS)
MyData$PFS <- sub("<1", 0.5, MyData$PFS)
MyData$PFS <- sub("Not reported", NA, MyData$PFS)
MyData$PFS <- as.numeric(MyData$PFS)

MyData$OS <- sub("^$", NA, MyData$OS)
MyData$OS <- sub("<1", 0.5, MyData$OS)
MyData$OS <- sub("Not reported", NA, MyData$OS)
MyData$OS <- as.numeric(MyData$OS)

#Create variable to position text
MyData$labelpos <- rep(-10,times=nrow(MyData))

#Create new variable to represent ongoing treatment
MyData$Ongoing<- sub("^$", NA, MyData$Ongoing)
MyData$arrowhead <- ifelse((is.na(MyData$Ongoing) & is.na(MyData$PFS) & is.na(MyData$OS)), MyData$Duration+0.2, NA)
MyData$arrowbody <- ifelse((is.na(MyData$Ongoing) & is.na(MyData$PFS) & is.na(MyData$OS)), MyData$Duration+0.1, NA)

NewData <- MyData[which(MyData$Duration!=0),]
NewData <- NewData[order(NewData$BRAF.inhibitor),]
NewData$ï..Patient.ID <- seq(1,nrow(NewData),by=1)




p2 <- ggplot(NewData, aes(x=ï..Patient.ID, y=Duration, fill = BRAF.inhibitor)) + 
  
  #Bar chart
  geom_bar(position=position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity", 
           color = "black", 
           width = 0.8) + 
  
  #Points for overall survival
  geom_point(aes(y=OS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 10,
             colour = "black",
             size=2, 
             show.legend = FALSE) +
  
  #Points for progression free survival
  geom_point(aes(y=PFS),
             stat="identity",
             position=position_dodge2(width = 0.9, preserve = "single"),
             shape = 16,
             colour = "black",
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
  
  annotate("text", x=NewData$ï..Patient.ID, y=NewData$labelpos, label = NewData$Reference, size = 2) +
  
  #Flip orientation of boxplot
  coord_flip() +
  
  #Add vertical lines every 6 months
  geom_hline(time1, yintercept = 6, linetype="dashed")+
  geom_hline(time1, yintercept = 12, linetype="dashed")+
  geom_hline(time1, yintercept = 18, linetype="dashed") +
  geom_hline(time1, yintercept = 24, linetype="dashed") +
  
  #Add chart labels, and remove labels from x-axis
  labs(title="Survival", 
       x="", y = "Duration (Months)")+
    theme(axis.ticks.y = element_blank(),
        legend.title=element_blank()) +

  #Add colour scheme
  scale_fill_brewer(palette = "Blues")
  
  
#Plot chart
p2

ggsave("C:/Users/lm16564/Downloads/horboxplot.jpeg", dpi=600)


