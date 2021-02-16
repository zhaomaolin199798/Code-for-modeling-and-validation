

setwd("C:\\Users\\zml18\\Desktop\\riskScore")      #Set up the working directory

#Draw the risk diagram of the train group
rt=read.table("riskTrain.txt",header=T,sep="\t",check.names=F,row.names=1)     #Read the train input file
rt=rt[order(rt$riskScore),]
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScoreTrain.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
trainMedianScore=median(rt$riskScore)
abline(h=trainMedianScore,v=lowLength,lty=2)
dev.off()

#Draw a risk chart for the Test group
rt=read.table("riskTest.txt",header=T,sep="\t",check.names=F,row.names=1)       #Read the test input file
rt=rt[order(rt$riskScore),]
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScoreTest.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
abline(h=trainMedianScore,v=lowLength,lty=2)
dev.off()
