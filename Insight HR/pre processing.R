library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(rpivotTable)
install.packages("DataExplorer")
library(DataExplorer)

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

na.omit(mydata)
summary(mydata)

p1<-ggplot(aes(x=Turnover),data = EmployeeData) + 
  geom_bar(color='black',fill="red") +
  xlab("Turnover") + 
  ylab("Frequency") + 
  labs(title="Barplot of Turnover")
p1


mydata %>% plot_histogram()
mydata %>% select_if(is.numeric) %>%  plot_correlation(type = "c")
PT_correlation <- mydata %>% dplyr::select(Work_Satisfaction,Projects,Tenure,Evaluation_score,Hrs_per_mnth_avg)
M <- cor(PT_correlation)
corrplot(M, method="circle")
cor(as.numeric(Turnover),Work_Satisfaction,method = "pearson")
cor.test(Work_Satisfaction,Hrs_per_mnth_avg)
table(mydata$Turnover ,mydata$Wages)
#Testing for the dependence between Turnover and wage Ranges
#Both are categorial variables so we use Chisq Test statistic
chisq.test(Turnover,Wages)
chisq.test(Domain,Wages)


#Satisfaction statistics splitted by Wages ranges
by(Work_Satisfaction,Wages,summary)

p1<-ggplot(aes(x=Work_Satisfaction),data=mydata) + 
  geom_histogram(color="black",fill="blue",bins = 30) +
  labs(title="Job Satisfaction Histogram",x='Job Satisfaction of Employees', y="Frequency")
p1
#Satisfaction histogram facetted by Wages classes

#As predicted the satifaction of employees who left was lower
#Satisfaction level vs Turnover
ggplot(aes(x = Work_Satisfaction),data=mydata) + 
  geom_histogram(color='black',fill='yellow',bins=35) +
  xlab('Satisfaction') + 
  ylab("Frequency")  + 
  facet_wrap(~Turnover)
#Boxplot for Satisfaction vs Turnover
#Boxplot for Satisfaction vs Turnover
ggplot(aes(x = Turnover,y = Work_Satisfaction),data= mydata) + 
            geom_boxplot() + 
            ylab('Satisfaction') + 
            xlab("Personnel left") + 
            labs(fill="Wages Classes")
#Boxplot for Satisfaction level vs left faceted by wage Ranges
ggplot(aes(x = Turnover,y=Work_Satisfaction),data= mydata) + 
  geom_boxplot() + 
  ylab('Satisfaction Level') + 
  xlab("Employee left") + 
  facet_wrap(~Wages)

mydata$Projects<-factor(mydata$Projects)

#analysis on no of Projects
ggplot(aes(x=Projects),data = mydata) + 
  geom_bar(color='black',fill='#234338') +
  xlab("Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot of Number of Projects")
#boxplot of Projects vs  Avg hours/month at workplace of employees
p3=ggplot(aes(x=Projects, y = Hrs_per_mnth_avg),data=mydata)+
  geom_boxplot()

p4=p3+facet_wrap(~Wages)
p4
p5=p3+facet_wrap(~Wages) + labs(title="Projects Vs Avg hours per month worked faceted by Turnover")
p5
#facetted by Wages
ggplot(aes(x=Projects),data = mydata) + 
  geom_bar(color='black',fill='#834338') +
  xlab("Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot Projects faceted by Salary") +
  facet_wrap(~Wages)
#faceted by If a employee left or not
ggplot(aes(x=Projects),data = mydata) + 
  geom_bar(color='black',fill='#547398') +
  xlab("Projects") + 
  ylab("Frequency") + 
  labs(title="Barplot of Projects faceted by Turnover")+  
  facet_wrap(~Turnover)

#Analysis of average hours per month
summary(Hrs_per_mnth_avg)
#Somewhat Normally distributed
ggplot(aes(x= Hrs_per_mnth_avg),data = mydata)+
  geom_histogram(color='black',fill="yellow",bins = 30)

ggplot(aes(x = Hrs_per_mnth_avg),data =mydata ) + 
  geom_histogram(color='black',fill='#443332',bins = 30) + 
  facet_wrap(~Turnover)
by(Hrs_per_mnth_avg , Turnover ,summary)
ggplot(aes(y = Hrs_per_mnth_avg, x = Turnover),data=mydata)+
  geom_boxplot() + 
  xlab(" Turnover or not") + 
  ylab("Average Montly hours worked")

rpivotTable(mydata)

levels(mydata$Wages)<-c(levels(mydata$Wages),1,2,3)
mydata$Wages[mydata$Wages=="low"]<-1
mydata$Wages[mydata$Wages=="medium"]<-2
mydata$Wages[mydata$Wages=="high"]<-3
head(mydata)
Projects<-as.numeric(Projects)
Turnover<-ifelse(Turnover==1,'True','False')
Turnover<-factor(Turnover,levels=c("True","False"))
table(Turnover)
