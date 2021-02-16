#trainTest.R

#install.packages("caret")

setwd("C:\\Users\\zml18\\Desktop\\trainTest")      #Set up the working directory
rt=read.table("expTime.txt",sep="\t",header=T,check.names=F)

#Divide the training set and test set
library(caret)
inTrain<-createDataPartition(y=rt[,3],p=0.5,list=F)
train<-rt[inTrain,]
test<-rt[-inTrain,]
write.table(train,file="train.txt",sep="\t",quote=F,row.names=F)
write.table(test,file="test.txt",sep="\t",quote=F,row.names=F)

#uniCox
#install.packages('survival')

pFilter=0.01                            

setwd("C:\\Users\\zml18\\Desktop\\uniCox")             
library(survival)                                                 
rt=read.table("train.txt",header=T,sep="\t",check.names=F,row.names=1)     

sigGenes=c("futime","fustat")
outTab=data.frame()
for(i in colnames(rt[,3:ncol(rt)])){
 cox <- coxph(Surv(futime, fustat) ~ rt[,i], data = rt)
 coxSummary = summary(cox)
 coxP=coxSummary$coefficients[,"Pr(>|z|)"]
 outTab=rbind(outTab,
              cbind(id=i,
              HR=coxSummary$conf.int[,"exp(coef)"],
              HR.95L=coxSummary$conf.int[,"lower .95"],
              HR.95H=coxSummary$conf.int[,"upper .95"],
              pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
              )
  if(coxP<pFilter){
      sigGenes=c(sigGenes,i)
  }
}
write.table(outTab,file="uniCox.xls",sep="\t",row.names=F,quote=F)
uniSigExp=rt[,sigGenes]
uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
write.table(uniSigExp,file="uniSigExp.txt",sep="\t",row.names=F,quote=F)

#lasso
#install.packages("glmnet")
#install.packages("survival")


library("glmnet")
library("survival")

setwd("C:\\Users\\zml18\\Desktop\\lasso")                
rt=read.table("uniSigExp.txt",header=T,sep="\t",row.names=1,check.names=F)    
rt$futime[rt$futime<=0]=1

x=as.matrix(rt[,c(3:ncol(rt))])
y=data.matrix(Surv(rt$futime,rt$fustat))

fit <- glmnet(x, y, family = "cox", maxit = 1000)
pdf("lambda.pdf")
plot(fit, xvar = "lambda", label = TRUE)
dev.off()

cvfit <- cv.glmnet(x, y, family="cox", maxit = 1000)
pdf("cvfit.pdf")
plot(cvfit)
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty="dashed")
dev.off()

coef <- coef(fit, s = cvfit$lambda.min)
index <- which(coef != 0)
actCoef <- coef[index]
lassoGene=row.names(coef)[index]
lassoGene=c("futime","fustat",lassoGene)
lassoSigExp=rt[,lassoGene]
lassoSigExp=cbind(id=row.names(lassoSigExp),lassoSigExp)
write.table(lassoSigExp,file="lassoSigExp.txt",sep="\t",row.names=F,quote=F)

#multiCox
#install.packages('survival')
#install.packages("survminer")

library(survival)
library(survminer)

setwd("C:\\Users\\zml18\\Desktop\\multiCox")               
rt=read.table("lassoSigExp.txt",header=T,sep="\t",check.names=F,row.names=1) 
rt[,"futime"]=rt[,"futime"]/365

#The train group was used to construct the Cox model
multiCox=coxph(Surv(futime, fustat) ~ ., data = rt)
multiCox=step(multiCox,direction = "both")
multiCoxSum=summary(multiCox)


outTab=data.frame()
outTab=cbind(
             coef=multiCoxSum$coefficients[,"coef"],
             HR=multiCoxSum$conf.int[,"exp(coef)"],
             HR.95L=multiCoxSum$conf.int[,"lower .95"],
             HR.95H=multiCoxSum$conf.int[,"upper .95"],
             pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])
outTab=cbind(id=row.names(outTab),outTab)
write.table(outTab,file="multiCox.xls",sep="\t",row.names=F,quote=F)

#Map the forest
pdf(file="forest.pdf",
       width = 8,           
       height = 5,            
       )
ggforest(multiCox,
         main = "Hazard ratio",
         cpositions = c(0.02,0.22, 0.4), 
         fontsize = 0.7, 
         refLabel = "reference", 
         noDigits = 2)
dev.off()

#Output the train group risk file
riskScore=predict(multiCox,type="risk",newdata=rt)           
coxGene=rownames(multiCoxSum$coefficients)
coxGene=gsub("`","",coxGene)
outCol=c("futime","fustat",coxGene)
medianTrainRisk=median(riskScore)
risk=as.vector(ifelse(riskScore>medianTrainRisk,"high","low"))
write.table(cbind(id=rownames(cbind(rt[,outCol],riskScore,risk)),cbind(rt[,outCol],riskScore,risk)),
    file="riskTrain.txt",
    sep="\t",
    quote=F,
    row.names=F)

#Output the Test group risk file
rtTest=read.table("test.txt",header=T,sep="\t",check.names=F,row.names=1)         
rtTest[,"futime"]=rtTest[,"futime"]/365
riskScoreTest=predict(multiCox,type="risk",newdata=rtTest)      
riskTest=as.vector(ifelse(riskScoreTest>medianTrainRisk,"high","low"))
write.table(cbind(id=rownames(cbind(rtTest[,outCol],riskScoreTest,riskTest)),cbind(rtTest[,outCol],riskScore=riskScoreTest,risk=riskTest)),
    file="riskTest.txt",
    sep="\t",
    quote=F,
    row.names=F)