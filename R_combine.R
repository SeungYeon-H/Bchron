library(rcarbon)

#individual intcal curve and uncaibrated dates
x1 = calibrate(c(2000,3400),c(20,20),ids=1:2)
x2 = calibrate(c(4000,3000),c(30,30),calCurves=c('intcal20','marine20'),
               resOffsets=c(0,30),resErrors=c(0,20),ids=3:4)

mcurve <- mixCurves('intcal20','marine20',p=0.7,resOffsets=300,resErrors=20)
x3 = calibrate(5300,20,calCurves=mcurve,ids=5)
x = combine(x1,x2,x3)

x15<-calibrate(c(1720,1650),c(30,30),calCurves=c('intcal20','intcal20'))
plot(x15)

#R_combine 
# IF the amount of the T and P value is large, it is acceptable as this implies the original dates don't have the profound difference. 
x = c(1630,1580,1620)
errors = c(60,50,60)
id = c(1,1,1)
poolDates(x,errors,id)

# If the groups have to be differentiated
x = c(4300,4330,5600,5603,5620)
errors = c(20,30,30,30,45)
id = c(1,1,2,2,2)
poolDates(x,errors,id)
