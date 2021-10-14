install.packages("cluster")
install.packages("BBmisc")
library(BBmisc)
library(cluster)

df = read.csv('WH Report_preprocessed_2019.csv')
df

rownames(df) = df$Name
df = df[,2:10]

df = normalize(df, method = "range", range = c(0, 1))

d = dist(df, method="euclidean")
d

awOutput =agnes(d,diss=TRUE,method="ward")
awOutput

plot(awOutput)

awOutput = agnes(df,diss=FALSE,method="ward")
awOutput
plot(awOutput)

awClusters=cutree(awOutput,7)
df = data.frame(df,awClusters)
View(df)

summary(subset(df,awClusters=1))

summary(as.factor(awClusters))

