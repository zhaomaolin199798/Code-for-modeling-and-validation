

#install.packages("pheatmap")

library(pheatmap)
setwd("C:\\Users\\zml18\\Desktop\\pheatmap")                   #Set up the working directory

#Draw risk heat map of train group
rt=read.table("riskTrain.txt",sep="\t",header=T,row.names=1,check.names=F)      #Read the train input file
rt=rt[order(rt$riskScore),]
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
rt1=log2(rt1+0.01)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmapTrain.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         fontsize_row=11,
         fontsize_col=3,
         color = colorRampPalette(c("green", "black", "red"))(50) )
dev.off()

#Draw the risk heat map of test group
rt=read.table("riskTest.txt",sep="\t",header=T,row.names=1,check.names=F)      #Read the test input file
rt=rt[order(rt$riskScore),]
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
rt1=log2(rt1+0.01)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmapTest.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         fontsize_row=11,
         fontsize_col=3,
         color = colorRampPalette(c("green", "black", "red"))(50) )
dev.off()

