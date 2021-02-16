

setwd("C:\\Users\\zml18\\Desktop\\cliCor")    #     Set up the working directory      

rt=read.table("input.txt",sep="\t",header=T,check.names=F,row.names=1)    #Read the input file   
outTab=data.frame()
score="riskScore"

for(cli in colnames(rt)[1:(ncol(rt)-1)]){
	flag1=levels(factor(rt[,cli]))[1]
	flag2=levels(factor(rt[,cli]))[2]
	group1Flag=rt[rt[,cli]==flag1,]
	group2Flag=rt[rt[,cli]==flag2,]
	
	#Mean, SD and T test P values were calculated for statistical analysis
	group1Mean=round(mean(group1Flag[,score]),3)
	group1Sd=round(sd(group1Flag[,score]),3)
	group2Mean=round(mean(group2Flag[,score]),3)
	group2Sd=round(sd(group2Flag[,score]),3)
	pStat=t.test(group1Flag[,score],group2Flag[,score])
	pValueStat=round(pStat$p.value,3)
	
	#The output
	outTab=rbind(outTab,
	             cbind(Clinical=cli,
	                   Group=flag1,
	                   n=nrow(group1Flag),
	                   Mean=group1Mean,
	                   SD=group1Sd,
	                   t=pStat$statistic,
	                   P=pValueStat))
	outTab=rbind(outTab,
	             cbind(Clinical=cli,
	                   Group=flag2,
	                   n=nrow(group2Flag),
	                   Mean=group2Mean,
	                   SD=group2Sd,
	                   t="",
	                   P=""))
}
write.table(outTab,file="clinical.stat.xls",sep="\t",row.names=F)


