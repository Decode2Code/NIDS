#importing libraries required for support

library(e1071)
library(rpart)
library(party)
library(ROCR)
library(rpart.plot)

#importing the datset
read.csv(file.choose(),header = T)
read.csv(file.choose(),header = T)

#decision tree model
J48=rpart(class~.,method = "class",data = KDDTrain)

#plot of model obtained
plot(J48)
rpart.plot(J48)
rpart.plot(J48,type=4,extra=101)

#predicting classes
J48_cp=predict(J48,KDDTest,type="class")
#prediction raw(probablities)
J48_rp=predict(J48,KDDTest,type="prob")

#prediction classes stored in a vector
J48_vp=predict(J48,KDDTest,type="vector")
print(head(J48_rp))

#confusion matrix table of results obtained by model on the test set
ConfusionMatrix_J48=table(KDDTest[,42],J48_cp)
plot(ConfusionMatrix_J48,main="Confusion matrix Decison tree",xlab="Actual value",ylab="Predicted Value",col=c("grey","cyan"))
ConfusionMatrix_J48

J48_pred=prediction(J48_vp,KDDTest[,42],label.ordering = NULL)
J48_perf_ROC=performance(J48_pred,"tpr","fpr")
J48_perf_PrecRec=performance(J48_pred,"prec","rec")
J48_perf_Lift=performance(J48_pred,"lift","rpp")
J48_perf_SensSpec=performance(J48_pred,"sens","spec")



NB=naiveBayes(class~.,method = "class",data = KDDTrain)
plot(NB)
NB_cp=predict(NB,KDDTest,type="class")
NB_rp=predict(NB,KDDTest,type="raw")
NB_vp=as.vector(NB_cp,mode="numeric")
print(NB_rp)
ConfusionMatrix_NB=table(KDDTest[,42],NB_cp)
plot(ConfusionMatrix_NB,xlab="Actual value",ylab="Predicted Value",col=c("grey","cyan"))
ConfusionMatrix_NB

NB_pred=prediction(NB_vp,KDDTest[,42],label.ordering = NULL)
NB_perf_ROC=performance(NB_pred,"tpr","fpr")
NB_perf_PrecRec=performance(NB_pred,"prec","rec")
NB_perf_Lift=performance(NB_pred,"lift","rpp")
NB_perf_SensSpec=performance(NB_pred,"sens","spec")

#J48 graphics

plot(J48)
rpart.plot(J48)
rpart.plot(J48,type=4,extra=101)

plot(J48_ptable,xlab="Actual value",ylab="Predicted Value",col=c("red","green"))

plot(J48_perf_ROC,colorize=T,main=expression("ROC curve decison tree"))
plot(J48_perf_PrecRec,colorize=T,main=expression("Prec Recall curve decison tree"))
plot(J48_perf_SensSpec,colorize=T,main=expression("Senstivity Specificity curve decison tree"))
plot(J48_perf_Lift,colorize=T,main=expression("Lift curve decison tree"))


#nb GRAPHICS

plot(NB_ptable,xlab="Actual value",ylab="Predicted Value")

plot(NB_perf_ROC,colorize=T,main="ROC curve NB")  #roc curve
plot(NB_perf_PrecRec,colorize=T,main="Prec Recall curve NB")   #Precision Recall Curve
plot(NB_perf_SensSpec,colorize=T,main="Senstivity specificity curve NB")    #Senstivity specificity curve
plot(NB_perf_Lift,colorize=T,main="Lift curve NB")       #Lift curve


#combined roc
dev.off()
plot(J48_perf_ROC,col="blue",type="o",main="ROC CURVE",sub="blue: D Tree                                  ")
par(new=T)
plot(NB_perf_ROC,col="red",type="l",main="",sub="                                      red: NB")

#combined Precision/Recall curve
dev.off()
plot(J48_perf_PrecRec,col="blue",lty=3,lwd=3,type="o",main="Precision Recall CURVE",sub="blue: D Tree                                  ")
par(new=T)
plot(NB_perf_PrecRec,lty=4,lwd=3,col="red",type="l",main="",sub="                                      red: NB")

#combined Lift curve
dev.off()
plot(J48_perf_Lift,col="blue",lty=3,lwd=3,type="o",main="Lift CURVE",sub="blue: D Tree                                  ")
par(new=T)
plot(NB_perf_Lift,col="red",lty=4,lwd=3,type="l",main="",sub="                                      red: NB")

#combined Sensitivity curve
dev.off()
plot(J48_perf_SensSpec,col="blue",lty=3,lwd=3,type="o",main="Senstivity specificity CURVE",sub="blue: D Tree                                  ")
par(new=T)
plot(NB_perf_SensSpec,col="red",lty=4,lwd=4,type="l",main="",sub="                                      red: NB")

