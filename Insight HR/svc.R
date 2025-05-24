attach(mydata)
library(caret)
library(e1071)
df<-data.frame(mydata)
set.seed(100) 
part <- sample(1:nrow(mydata), 0.7*nrow(mydata))  #training size = 0.8
training <- mydata[part,]  
testdata  <- mydata[-part,]
#plot(testdata)
svm_model<-svm(Turnover~.,               
               gamma=0.001668101,
               cost=35.93814,             
               data=training,kernel="radial",probability = TRUE,decision.values = TRUE)
summary(svm_model)
#pred=predict(tuning$best.model, newdata=testdata)
#View(pred)
svm_model.predict<-predict(svm_model,testdata,probability=TRUE) 
#View(svm_model.predict)
cm=table(testdata$Turnover,svm_model.predict)
cm
val<-sum(diag(cm))/sum(cm)
print(paste('Accuracy',val))
confusionMatrix(table(testdata$Turnover,svm_model.predict),mode= "prec_recall")
fourfoldplot(cm,color = c("sienna","orchid"),main="Confusion Matrix of SVM")

library(ROCR)
svm_model.prob <-attr(svm_model.predict,"probabilities")
#View(svm_model.prob)
p1<- prediction(svm_model.prob[,2],testdata$Turnover)
fit.svc<- performance(p1,measure = "tpr",x.measure = "fpr")
plot(fit.svc,colorize=TRUE,main="ROC CURVE - SVC")
abline(a=0,b=1)
auc<-performance(p1,measure = "auc")
auc<-auc@y.values[[1]]
auc<-round(auc,4)
legend(0.5,0.4,auc,title = "AUC",cex=1)


gamma.range<-10^(-3:3)
cost.range<-10^(-2:2)
tuning<-tune.svm(
  turnover~.,
  type="C-classification",
  gamma=gamma.range,
  cost=cost.range,
  data=mydata,
  tunecontrol=tune.control(sampling="cross",cross=3)
)
tuning$best.parameters
1-tuning$best.performance 
plot(tuning, transform.x=log10, transform.y=log10)
gamma.range<-10^seq(-3,-1,length=10) # -2 +-1    -2,1
cost.range<-10^seq(0,2,length=10) # 1 +-1   0,2
tuning<-tune.svm(
  turnover~.,
  type="C-classification",
  gamma=gamma.range,
  cost=cost.range,
  data=mydata,
  tunecontrol=tune.control(sampling="cross",cross=3)
)
tuning$best.parameters 
plot(tuning, transform.x=log10, transform.y=log10)