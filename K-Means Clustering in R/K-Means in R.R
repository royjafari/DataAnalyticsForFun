install.packages("cluster")
library(cluster)

df = read.csv('WH Report_preprocessed_2019.csv')
df

rownames(df) = df$Name

Xs = scale(df[,2:10])
Xs

kOutput = pam(Xs,k=7)
kOutput
summary(kOutput)

plot(kOutput)

