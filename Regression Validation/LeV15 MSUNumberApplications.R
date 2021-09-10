application_df = read.csv(file = "MSU applications.csv")
msu_lm=lm(formula=N_Applications ~ P_Football_Performance + SMAn2, data=application_df)
msu_lm
summary(msu_lm)
