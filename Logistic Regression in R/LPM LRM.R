df = read.csv('CustomerLoan.csv')
plm = lm(default_binary~income+score,data=df)
summary(plm)
pred = predict(plm,type="response")
lrm = glm(default_binary~income+score,family=binomial(link=logit),data=df)
pred2 = predict(lrm,type="response")

