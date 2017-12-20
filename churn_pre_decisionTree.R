########################################################################
#                      Churn prediction                                #
#                   using decision Tree                                #


churn_data<-read.csv("churn.csv",stringsAsFactors=T)

str(churn_data)
#head(churn_data)
#checking number of NA's

sapply(churn_data,function(x) sum(is.na(x)))

summary(churn_data)


#deleting the rows which are having NA's in total charges column #11 rows

churn_data<-churn_data[-(which(is.na(churn_data$TotalCharges)==TRUE)),]


#converting Senior citizen column into factor
churn_data["SeniorCitizen"]<-as.factor(churn_data[,"SeniorCitizen"])

#levels(churn_data$Churn)<-c()


#deleting the Id column
churn_data<-churn_data[,-1]

#scaling total charges
#churn_data$TotalCharges<-scale(churn_data$TotalCharges,0,scale=TRUE)

minmax =function(x){
  xnew <-(x - min(x))/(max(x)-min(x))
}

churn_data[,17:18]<-apply(churn_data[,17:18],2,minmax)


#check the no. of positive and negative class
table(churn_data[,19])

#data Partition

#subset the data based on classes
class1<-subset(churn_data,Churn=="Yes")
class0<-subset(churn_data,Churn=="No")
set.seed(234)
s1<-sample(nrow(class1),0.7*nrow(class1))
s0<-sample(nrow(class0),0.7*nrow(class0))
train1<-class1[s1,]
test1<-class1[-s1,]

train0<-class0[s0,]
test0<-class0[-s0,]

train<-rbind(train1,train0)
test<-rbind(test1,test0)


###################   MODEL DEVELOPMENT#############################################
#decision tree Algorith 
#rpart pkg

library(rpart)

fit= rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+
             PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
             DeviceProtection+TechSupport+TechSupport+StreamingTV+StreamingMovies+Contract+
             PaperlessBilling+ PaymentMethod+MonthlyCharges+TotalCharges,
           method = "class",control=rpart.control(minsplit = 10),cp=0.01000000,train)


fit= rpart(Churn ~.,method = "class",control=rpart.control(minsplit = 2),train)

fit
#rpart.plot(fit)
fit$cptable
fit$variable.importance
p<-predict(fit,test,type="class")
head(p)
#View(p)
nrow(test)
# length(p)
# 
library(ROCR)
pr<-prediction(as.numeric(p),as.numeric(test$Churn))

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green")
abline(0,1,col="red")


library(caret)
confusionMatrix(p,test$Churn)
d<-table(p,test$Churn)
acc<-sum(diag(d))/sum(d)
acc

#################################################################################
##using information gain split

fit1= rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+
             PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
             DeviceProtection+TechSupport+TechSupport+StreamingTV+StreamingMovies+Contract+
             PaperlessBilling+ PaymentMethod+MonthlyCharges+TotalCharges,
           method = "class",control=rpart.control(minsplit = 10),parms = list(split="information"),train)


fit1

#rpart.plot(fit1)

fit1$cptable

fit1$variable.importance

p<-predict(fit1,test,type="class")
head(p)
#View(p)
nrow(test)
length(p)

d<-table(p,test$Churn)
acc<-sum(diag(d))/sum(d)
acc


library(ROCR)
pr<-prediction(as.numeric(p),as.numeric(test$Churn))

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green")
abline(0,1,col="red")

############################################################################################################33
############using loss matrix



lossmatrix=matrix(c(0,0.2,0.6,0),nrow=2,byrow=TRUE)
fit2= rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+
              PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
              DeviceProtection+TechSupport+TechSupport+StreamingTV+StreamingMovies+Contract+
              PaperlessBilling+ PaymentMethod+MonthlyCharges+TotalCharges,
            method = "class",control=rpart.control(minsplit = 10),parms = list(loss=lossmatrix,split="information"),train)


fit2
#rpart.plot(fit2)
fit2$cptable

fit2$variable.importance

cp1<-fit2$cptable[which.min(fit$cptable[,"xerror"])]
prune(fit2,cp=cp1)
p<-predict(fit2,test,type="class")
head(p)
#View(p)
nrow(test)
length(p)

d<-table(p,test$Churn)
acc<-sum(diag(d))/sum(d)
acc

confusionMatrix(p,test$Churn,positive="Yes")


library(ROCR)
pr<-prediction(as.numeric(p),as.numeric(test$Churn))

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green")
abline(0,1,col="red")

##############################################################################################
# Using surrogate parameter

fit3= rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+
              PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
              DeviceProtection+TechSupport+TechSupport+StreamingTV+StreamingMovies+Contract+
              PaperlessBilling+ PaymentMethod+MonthlyCharges+TotalCharges,
            method = "class",control=rpart.control(minsplit = 10,maxsurrogate = 10),parms = list(split="information"),train)


fit3
#rpart.plot(fit3)
fit3$cptable

fit3$variable.importance


#to get the cp value having minimum xerror against it

fit3$cptable[which.min(fit$cptable[,"xerror"])]
prune(fit3,cp=0.01)
p<-predict(fit3,test,type="class")
head(p)
#View(p)
nrow(test)
length(p)

d<-table(p,test$Churn)
acc<-sum(diag(d))/sum(d)
acc

library(ROCR)
pr<-prediction(as.numeric(p),as.numeric(test$Churn))

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green")
abline(0,1,col="red")

