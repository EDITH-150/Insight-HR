library(readxl)
library(caret)
library(tensorflow)
library(rpart)
library(dplyr)
library(rpart.plot)
EmployeeData <- read_excel("C:/Users/Nishkakshat/Desktop/EmployeeData.xlsx", 
                           col_types = c("text", "numeric", "numeric", 
                                         "numeric", "numeric", "text", "text", 
                                         "text", "numeric", "text"))
summary(EmployeeData)
attach(EmployeeData)
EmployeeData$Turnover <-as.factor(Turnover )
EmployeeData$Projects <-as.numeric(Projects )
EmployeeData$Wages <-as.factor(Wages )
EmployeeData$Domain <-as.factor(Domain)
EmployeeData$Casualty <-as.factor(Casualty )
EmployeeData$Incentives<-as.factor(Incentives)
mydata<-EmployeeData
str(mydata)
View(mydata)
attach(mydata)

set.seed(100) 
part <- sample(1:nrow(mydata), 0.7*nrow(mydata))  
training1 <- mydata[part,]  
testData1  <- mydata[-part,]
d.tree1 <- rpart(Turnover~., training1,method = "class")
summary(d.tree1)
a<-varImp(d.tree1,scale=FALSE)
a
library(vip)
vip(d.tree1,num_features=10,bar=TRUE)

d.tree <- rpart(Turnover~Work_Satisfaction+Evaluation_score+Projects+Hrs_per_mnth_avg+
                 Tenure, training1,method = "class")
summary(d.tree)
plotcp(d.tree)
#printcp(d.tree)
dt<-prune.rpart(d.tree,cp=0.05)
rpart.plot(dt,extra = 101)
tree.pred <-predict(dt,training1,type="class")
confusionMatrix(table(training1$Turnover ,tree.pred))
levels(tree.pred)
dtpred<-predict(dt,testData1,type = "class")
#View(dtpred)
table_mat<-table(testData1$Turnover,dtpred)
table_mat
accuracy<-sum(diag(table_mat))/sum(table_mat)
print(paste('Accuracy',accuracy))
confusionMatrix(table(testData1$Turnover ,dtpred),mode= "prec_recall")
fourfoldplot(table_mat,color = c("hotpink","royalblue"),main="Confusion Matrix of Decision Tree")
saveRDS(dt,"C:/Users/Nishkakshat/Desktop/finaldt.rds")

library(ROCR)
pred4<-predict(dt,testData1,type = "prob")
p4<- prediction(pred4[,2],testData1$Turnover)
fit<- performance(p4,measure = "tpr",x.measure = "fpr")
plot(fit,colorize=TRUE,main="ROC-DECISION TREE")
abline(a=0,b=1)
auc<-performance(p4,measure = "auc")
auc<-auc@y.values[[1]]
auc<-round(auc,4)
legend(0.5,0.4,auc,title = "AUC",cex=1)



