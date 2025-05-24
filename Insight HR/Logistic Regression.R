library(caret)
library(pscl)
library(ROCR)

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
trainingRowIndex <- sample(1:nrow(mydata), 0.7*nrow(mydata))  
trainingdata <- mydata[trainingRowIndex, ]  
testingData  <- mydata[-trainingRowIndex, ]
LogModel <- glm(Turnover~.,family=binomial(link = "logit"),data=trainingdata)
summary(LogModel)
anova(LogModel,test = "Chisq")
pR2(LogModel)
Pred <- predict(LogModel, testingData,type = "response")
Pred
Pred <- ifelse(Pred>0.5,1,0)
matlog<-table(testingData$Turnover,Pred)
#LogModel$coef
cl <- mean(Pred != testingData$Turnover)
print(paste("accuracy",1-cl))
fitted.results <- predict(LogModel,newdata=testingData,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
print("Confusion Matrix for Logistic Regression"); table(testingData$Turnover, fitted.results > 0.5)
confusionMatrix(matlog,mode= "prec_recall")
fourfoldplot(matlog,color = c("yellow","deeppink"),main="Confusion Matrix for Logistic Regression")

#ROC curve code
pred2<-predict(LogModel,testingData,type = "response")
hist(pred2,main = "Histogram of predicted probabilities")
p2<- prediction(pred2,testingData$Turnover)
#perf
perf<- performance(p2,measure = "tpr",x.measure = "fpr")
plot(perf,colorize=TRUE,main="ROC CURVE - LOGISTIC REGRESSION")
abline(a=0,b=1)
auc<-performance(p2,measure = "auc")
auc<-auc@y.values[[1]]
auc<-round(auc,4)
legend(0.5,0.4,auc,title = "AUC",cex=1)