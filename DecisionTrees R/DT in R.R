install.packages(c("caret","gains","pROC","rpart","rpart.plot"))
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)

df = read.csv('CustomerLoan.csv')
df
df$default = as.factor(df$default)


dt = rpart(default~income+score, data=df,
               method="class")
summary(dt)

prp(dt,extra=1)

dt = rpart(default~income+score, data=df,
           method="class",cp=0,minsplit=2,minbucket=1)
summary(dt)

prp(dt,extra=1)


df = read.csv('Customer Churn.csv')
df
df$Churn = as.factor(df$Churn)

set.seed(1)
randomizedIndex = createDataPartition(df$Churn,p=0.7,list=FALSE)
randomizedIndex

trainSet = df[randomizedIndex,]
trainSet
testSet =  df[-randomizedIndex,]
testSet

dt = rpart(Churn~., data=trainSet,
           method="class",cp=0,minsplit=2,minbucket=1)
summary(dt)
prp(dt,extra=1)

dt = rpart(Churn~., data=trainSet,method="class")
summary(dt)
prp(dt,extra=1)

predict_class = predict(dt,testSet,type="class")

confusionMatrix(data=predict_class, reference = testSet$Churn, positive='1')

predict_prob = predict(dt,testSet,type="prob")

p1_Class =as.factor(ifelse(predict_prob[,2]>0.10,'1','0'))

confusionMatrix(data=p1_Class, reference = testSet$Churn, positive='1')

ROC = roc(testSet$Churn,predict_prob[,2])
plot.roc(ROC)
auc(ROC)
