#install.packages("rms")

library(rms)
setwd("D:\\nomogram")                         #Set up the working directory


riskFile="testRisk.txt"     #Risk input file
outFile="Nomogram.pdf"      #Output line graph file name
risk=read.table(riskFile,header=T,sep="\t",check.names=F,row.names=1)        #Read the risk file
rt=risk[,1:(ncol(risk)-2)]

dd <- datadist(rt)
options(datadist="dd")
#Generating function
f <- cph(Surv(futime, fustat) ~ ., x=T, y=T, surv=T, data=rt, time.inc=1)
surv <- Survival(f)
#Establish a nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(2, x), function(x) surv(3, x)), 
    lp=F, funlabel=c("1-year survival", "2-year survival", "3-year survival"), 
    maxscale=100, 
    fun.at=c(0.99, 0.9, 0.8, 0.7, 0.5, 0.3,0.1,0.01))  
#Nomogram visualization
pdf(file=outFile,height=6,width=9)
plot(nom)
dev.off()

#calibration curve
time=2   #Predict the time
f <- cph(Surv(futime, fustat) ~ ., x=T, y=T, surv=T, data=rt, time.inc=time)
cal <- calibrate(f, cmethod="KM", method="boot", u=time, m=50, B=1000)
pdf(file="calibration.pdf",height=6,width=8)
plot(cal,xlab="Nomogram-Predicted Probability of 2-Year OS",ylab="Actual 2-Year OS(proportion)",col="red",sub=F)
dev.off()