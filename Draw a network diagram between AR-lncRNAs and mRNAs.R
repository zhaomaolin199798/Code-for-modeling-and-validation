
#install.packages("ggplot2")
#install.packages("ggalluvial")

library(ggalluvial)
library(ggplot2)
library(dplyr)

setwd("C:\\Users\\zml18\\Desktop\\ggalluvial")          #Set up the working directory
rt=read.table("network.txt",sep = "\t",header = T)                    #Read input file

#Read the risk file and get the high and low risk lncRNA
cox=read.table("multiCox.txt",sep="\t",header=T,row.names=1)
protectGene=row.names(cox[cox$HR<1,])
riskType=ifelse(rt$lncRNA%in%protectGene,"Protect","Risk")
newData=cbind(rt[,c(1,2)],riskType)
corLodes=to_lodes_form(newData, axes = 1:3, id = "Cohort")

#Get the output file
pdf(file="ggalluvial.pdf",width=12,height=8)
mycol <- rep(c("#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#029149","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767"),3)
ggplot(corLodes, aes(x = x, stratum = stratum, alluvium = Cohort,fill = stratum, label = stratum)) +
  	 scale_x_discrete(expand = c(0, 0)) +  
  	 geom_flow(width = 1/8,aes.flow = "forward") + 
	 geom_stratum(alpha = .9,width = 1/10) +
	 scale_fill_manual(values = mycol) +
	 geom_text(stat = "stratum", size =2.4,color="black") +
	 xlab("") + ylab("") + theme_bw() + 
	 theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank()) + #È¥µô×ø±êÖá
	 theme(panel.grid =element_blank()) + 
	 theme(panel.border = element_blank()) + 
	 ggtitle("") + guides(fill = FALSE)                            
dev.off()

