#r_date in oxcal 

library(Bchron)
data<-read_xlsx("file directory")
data$BP<-as.numeric(data$BP)
data$er<-as.numeric(data$er)

avector<-data[,'BP'] #to sublist(sub data frame)
class(avector)
avector<-data[['BP']] #to change to actual atomic column
#avector<-data[,6] #to return to the vector
bvector<-data[,'er'] #to sublist(sub data frame)
bvector<-data[['er']] #to change to actual atomic column
#bvector<-data[,7]

#age2=BchronCalibrate(ages=c(1670,1760),ageSds=c(30,30),positions=c(100,200), calCurves=c('intcal20','normal')) 

age2=BchronCalibrate(ages=-avector+2050,ageSds=bvector,positions=seq(from=1000, by=200,length.out=29),
ids=data$코드,calCurves=calcurves) 

plot(age2,dateLabelSize = 3,dateHeight=150,withPostions=TRUE,yaxt='n')+
  scale_x_continuous(breaks=seq(250,600,50),sec.axis = dup_axis())+theme_bw()+
  coord_cartesian(xlim=c(220,600))+
  xlab("Calibrated date (calAD)")+
  theme(axis.title.x=element_text(face="bold",size=13),axis.text.x=element_text(size=11,face="bold"),axis.text.y=element_blank(),axis.ticks.y=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
