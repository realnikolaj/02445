load("fosfor_data.Rdata")
Phosphorous$location <- as.factor(Phosphorous$location)
Phosphorous$yield[34] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2
Phosphorous$yield[36] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2

#----plots----
plot(Phosphorous$location,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Field",
     main = "Location")

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [?g/L]",
     main = "DGT")

plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Olsen-P")


# x for plotting models
x.DGT <- seq(0,max(Phosphorous$DGT)+1,length=100)
x.olsen <- seq(min(Phosphorous$olsenP),max(Phosphorous$olsenP),length=100)



#----Michaelis-Menten model for DGT----
phos.DGT.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous,
                    start = list(alfa = 90 , beta = 1))
summary(phos.DGT.model)
fit_model_DGT<- coef(phos.DGT.model)[1]*x.DGT/(coef(phos.DGT.model)[2]+x.DGT)
CI_DGT <- confint(phos.DGT.model)
fit_DGT_CI_low <- CI_DGT[1]*x.DGT/(CI_DGT[4]+x.DGT)
fit_DGT_CI_high <- CI_DGT[3]*x.DGT/(CI_DGT[2]+x.DGT)
plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [?g/L]",
     main = "Michaelis-Menten model for DGT")
lines(x.DGT,fit_model_DGT,lwd=2)
lines(x.DGT,fit_DGT_CI_high,lty=5,lwd=2, col="red")
lines(x.DGT,fit_DGT_CI_low,lty=5,lwd=2,col="green")


plot(coef(phos.DGT.model)[1]*seq(min(Phosphorous$DGT),max(Phosphorous$DGT),length=36)/(coef(phos.DGT.model)[2]+seq(min(Phosphorous$DGT),max(Phosphorous$DGT),length=36)),resid(phos.DGT.model))

par(mfrow=c(1,2))

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [?g/L]",
     main = "Michaelis-Menten model for DGT")
lines(x.DGT,fit_model_DGT,lwd=2)




#----Michaelis-Menten model for Olsen-P----
phos.olsen.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = Phosphorous,
                  start = list(alfa = 90 , beta = 1))
summary(phos.olsen.model)
fit_model_olsen<- coef(phos.olsen.model)[1]*x.olsen/(coef(phos.olsen.model)[2]+x.olsen)
CI_olsen <- confint(phos.olsen.model)
fit_olsen_CI_low <- CI_olsen[1]*x.olsen/(CI_olsen[4]+x.olsen)
fit_olsen_CI_high <- CI_olsen[3]*x.olsen/(CI_olsen[2]+x.olsen)
plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Michaelis-Menten model for Olsen-P",ylim=c(20,110))
lines(x.olsen,fit_model_olsen,lwd=2)
lines(x.olsen,fit_olsen_CI_high,lwd=2,col="red",lty=5)
lines(x.olsen,fit_olsen_CI_low,lwd=2,col="green",lty=5)





#----Linear model for Olsen-P----
olsen.lm <- lm(yield ~ olsenP,data=Phosphorous)
summary(olsen.lm)
anova(olsen.lm)
fit.olsen.lm<- coef(olsen.lm)[1]+coef(olsen.lm)[2]*x.olsen
CI_olsen_linear <- confint(olsen.lm)
fit.olsen.lm.CI.low <- CI_olsen_linear[1]+CI_olsen_linear[2]*x.olsen
fit.olsen.lm.CI.high <- CI_olsen_linear[3]+CI_olsen_linear[4]*x.olsen
plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Linear model for Olsen-P")
lines(x.olsen,fit.olsen.lm,lwd=2)
lines(x.olsen,fit.olsen.lm.CI.high,lwd=2,col="red",lty=5)
lines(x.olsen,fit.olsen.lm.CI.low,lwd=2,col="green",lty=5)

plot(olsen.lm)




#----Linear model for DGT----
DGT.lm <- lm(yield ~ DGT,data=Phosphorous)
summary(DGT.lm)
anova(DGT.lm)
fit.DGT.lm<- coef(DGT.lm)[1]+coef(DGT.lm)[2]*x.DGT
CI_DGT_linear <- confint(DGT.lm)
fit.DGT.lm.CI.low <- CI_DGT_linear[1]+CI_DGT_linear[2]*x.DGT
fit.DGT.lm.CI.high <- CI_DGT_linear[3]+CI_DGT_linear[4]*x.DGT

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [?g/L]",
     main = "Linear model for DGT")
lines(x.DGT,fit.DGT.lm,lwd=2)
lines(x.DGT,fit.DGT.lm.CI.high,lwd=2,col="red",lty=5)
lines(x.DGT,fit.DGT.lm.CI.low,lwd=2,col="green",lty=5)



plot(DGT.lm)


par(mfrow=c(2,1))

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     main = "Michaelis-Menten model for DGT")
lines(x.DGT,fit_model_DGT,lwd=2)

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [?g/L]",
     main = "Linear model for DGT")
lines(x.DGT,fit.DGT.lm,lwd=2)





#----LOOCV of Michaelis-Menten model on DGT----
DGT.error <- rep(0,36)
for(i in 1:36){
        train <- Phosphorous[-i,]
        test <- Phosphorous[i,]
        DGT.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = train,
                         start = list(alfa = 90 , beta = 1))
        estimate <- predict(DGT.model,test)
        DGT.error[i] <- (test$yield-estimate)^2
}
DGT.rmse <- sqrt(mean(DGT.error))








#----LOOCV of Michaelis-Menten model on Olsen-P----
olsenP.error <- rep(0,36)
for(i in 1:36){
        train <- Phosphorous[-i,]
        test <- Phosphorous[i,]
        olsen.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = train,
                           start = list(alfa = 90 , beta = 1))
        estimate <- predict(olsen.model,test)
        olsenP.error[i] <- (test$yield-estimate)^2
}
olsenP.rmse <- sqrt(mean(olsenP.error))







#----LOOCV of linear model on Olsen-P----
olsenP.linear.error <- rep(0,36)
for(i in 1:36){
        train <- Phosphorous[-i,]
        test <- Phosphorous[i,]
        olsen.lm <- lm(yield ~ olsenP,data=train)
        estimate <- predict(olsen.lm,test)
        olsenP.linear.error[i] <- (test$yield-estimate)^2
}
olsenP.linear.rmse <- sqrt(mean(olsenP.linear.error))







#----LOOCV of linear model on DGT----
DGT.linear.error <- rep(0,36)
for(i in 1:36){
        train <- Phosphorous[-i,]
        test <- Phosphorous[i,]
        DGT.lm <- lm(yield ~ DGT,data=train)
        estimate <- predict(DGT.lm,test)
        DGT.linear.error[i] <- (test$yield-estimate)^2
}
DGT.linear.rmse <- sqrt(mean(DGT.linear.error))





#----t-test----
t.test(DGT.error,olsenP.error,paired = T) #0.011 significant
t.test(DGT.error,olsenP.linear.error,paired = T) #0.018 significant
t.test(DGT.error,DGT.linear.error,paired = T) #0.023 significant

t.test(DGT.linear.error,olsenP.linear.error,paired = T) #0.069 not significant
t.test(DGT.linear.error,olsenP.error,paired = T) #0.12 not significant
t.test(olsenP.linear.error,olsenP.error,paired = T) #0.76 not significant


