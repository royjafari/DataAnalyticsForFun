install.packages(c("caret","gains","pROC"))
library(caret)
library(gains)
library(pROC)

df = read.csv('CustomerLoan.csv')

df_norm = scale(df[2:3])
df_norm = data.frame(df_norm,df$default)
df_norm

colnames(df_norm)[3] = 'default'
df_norm

set.seed(3)

randomizedIndex = createDataPartition(df$default,p=0.55,list=FALSE)
randomizedIndex

trainSet = df_norm[randomizedIndex,]
trainSet
testSet =  df_norm[-randomizedIndex,]
testSet

trControl = trainControl(method="cv",number=2)

set.seed(1)
KNN_fit = train(default~income+score,
                data=trainSet,
                method="knn",
                trControl=trControl,
                tuneGrid=expand.grid(k = 1:6),
                metric='Accuracy')
KNN_fit

KNN_P_Class = predict(KNN_fit,newdata = testSet)
KNN_P_Class

confusionMatrix(data=KNN_P_Class, reference = testSet$default, positive='Yes')

KNN_Prob_Class = predict(KNN_fit,newdata = testSet,type='prob')
KNN_Prob_Class

KNN_P_p75_Class =as.factor(ifelse(KNN_Prob_Class[,2]>0.75,'Yes','NO'))
KNN_P_p75_Class

confusionMatrix(data=KNN_P_p75_Class,
                reference = testSet$default,
                positive='Yes')

KNN_Prob_Class = predict(KNN_fit,newdata = df_norm,type='prob')
KNN_Prob_Class

ROC = roc(df_norm$default,KNN_Prob_Class[,2])
plot.roc(ROC)
auc(ROC)
