
#install.packages("survival")

library(survival)
setwd("D:\\lncRNAsurvial")        

#Define the survival curve function
surPlot=function(data=null, gene=null, outPdf=null){
		a=ifelse(data[,gene]<=median(data[,gene]),"Low expression","High expression")
		diff=survdiff(Surv(futime, fustat) ~a,data = data)
		pValue=1-pchisq(diff$chisq,df=1)
		pValue=signif(pValue,4)
		pValue=format(pValue, scientific = TRUE)
		fit <- survfit(Surv(futime, fustat) ~ a, data = data)
		pdf(file=outPdf,width=5.5,height=5)
		plot(fit,
		     lwd=2,
		     col=c("red","blue"),
		     xlab="Time (year)",
		     ylab="Survival rate",
		     main=paste("Survival curve (p=", pValue ,")",sep=""),
		     mark.time=T)
		legend("topright",
		       c(paste0(gene," high expression"), paste0(gene," low expression") ),
		       lwd=2,
		       col=c("red","blue"))
		dev.off()
}

#Read input file
rtAll=read.table("riskTrain.txt",header=T,sep="\t",check.names=F)

#Survival curves were drawn for the genes in the constructed model
for(i in colnames(rtAll[,4:(ncol(rtAll)-2)]) ){
		surPlot(data=rtAll, gene=i, outPdf=paste0("survival.",i,".pdf"))
}


