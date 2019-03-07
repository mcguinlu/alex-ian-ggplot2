df <- data.frame(dose=c("D0.5", "D1", "D2", "D3","D4", "D5"),
                 len=c(4.2, 10, 29.5, 20, 12, 112)
                 )
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity") +
  coord_flip()
p


MyData = data.frame(
  method=rep(c("A","2","3","4","5","6","7","8","9","10"), times = 3),
  time1=rnorm(30,10,3),
  time2=rnorm(30,8,2),
  group=rep(c("PXA","Other Drug","Drug 3"),each=10),
  cat=rep(c(1,2),each=15)
)

p2 <- ggplot(MyData, aes(group, time1,  fill = as.factor(method))) + 
  geom_bar(position=position_dodge2(width = 0.5), stat="identity", show.legend = FALSE) + 
  coord_flip() +
  geom_hline(time1, yintercept = 5, linetype="dashed")+
  geom_hline(time1, yintercept = 10, linetype="dashed")+
  geom_hline(time1, yintercept = 15, linetype="dashed") +
  scale_fill_manual(values = c(rep("blue",times=10)))
  
p2
