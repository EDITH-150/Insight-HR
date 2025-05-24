library(class)
library(gmodels)
require(FNN)
library(FNN)

set.seed(100) 
trainingRowIndex <- sample(1:nrow(mydata), 0.7*nrow(mydata))  
trainingData <- mydata[trainingRowIndex, ]  
testData  <- mydata[-trainingRowIndex, ]
sqrt(nrow(trainingData))
m<-knn(trainingData[,c(2,3,4,5,9)],testData[,c(2,3,4,5,9)],cl=trainingData$Turnover
       ,k=102,algorithm=c("kd_tree"),prob = TRUE)
summary(m)
p<-as.numeric(m)-1
p
em<-table(testData$Turnover,p)
em
res<-sum(diag(em))/sum(em)
print(paste('Accuracy',res))
fourfoldplot(em,color = c("turquoise","tomato"),main="Confusion Matrix of KNN")
confusionMatrix(em,mode= "prec_recall")
ct<-CrossTable(x=testData$Turnover,y=m,prop.chisq=FALSE)

library(ROCR)
attributes(m)$prob
library(pROC)
r<-roc(testData$Turnover, attributes(m)$prob)
plot(r,print.thres = T,print.auc=T,main = "ROC- KNN")



calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
set.seed(1)
k_to_try = 1:102
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn(train = scale(trainingData[,c(2,3,4,5,9)]), 
             test  = scale(testData[,c(2,3,4,5,9)]), 
             cl    = trainingData$Turnover, 
             algorithm=c("kd_tree"),
             k     = k_to_try[i])
  err_k[i] = calc_class_err(testData$Turnover, pred)
}
plot(err_k, type = "b", col = "red", cex = 1, pch = 20,main = "Error Rate vs Neighbors", 
     xlab="k - number of neighbors", ylab="Classification Error")
# add line for min error seen
abline(h = min(err_k), col = "blue", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(trainingData[,7] == 0), col="green", lty = 2)
min(err_k)
which(err_k == min(err_k))
max(which(err_k == min(err_k)))

