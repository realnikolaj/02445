load("fosfor_data.Rdata")
#----with location 11----

Phosphorous$location <- as.factor(Phosphorous$location)
Phosphorous$yield[34] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2
Phosphorous$yield[36] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2

model <- lm(DGT~location,data = Phosphorous)
summary(model)

plot(Phosphorous$location,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Field",
     main = "Location")

plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [µg/L]",
     main = "DGT")

plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Olsen-P")


# x for plotting models
x.DGT <- seq(min(Phosphorous$DGT),max(Phosphorous$DGT),length=100)
x.olsen <- seq(min(Phosphorous$olsenP),max(Phosphorous$olsenP),length=100)

#Michaelis-Menten model for DGT
phos.DGT.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous,
                    start = list(alfa = 90 , beta = 1))
summary(phos.DGT.model)

fit_model_DGT<- coef(phos.DGT.model)[1]*x.DGT/(coef(phos.DGT.model)[2]+x.DGT)
plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [µg/L]",
     main = "Michaelis-Menten model for DGT")
lines(x.DGT,fit_model_DGT)
plot(resid(phos.DGT.model))
confint(phos.DGT.model)

#leave one out cross-validation of Michaelis-Menten model on DGT
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


#Michaelis-Menten model for Olsen-P
phos.olsen.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = Phosphorous,
                  start = list(alfa = 90 , beta = 1))
summary(phos.olsen.model)



fit_model_olsen<- coef(phos.olsen.model)[1]*x.olsen/(coef(phos.olsen.model)[2]+x.olsen)
plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Michaelis-Menten model for Olsen-P")
lines(x.olsen,fit_model_olsen)

temp <- confint(phos.olsen.model)

plot(resid(phos.DGT.model))

#leave one out cross-validation of Michaelis-Menten model on Olsen-P
olsenP.error <- rep(0,36)
for(i in 1:36){
        train <- Phosphorous[-i,]
        test <- Phosphorous[i,]
        DGT.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = train,
                         start = list(alfa = 90 , beta = 1))
        estimate <- predict(DGT.model,test)
        olsenP.error[i] <- (test$yield-estimate)^2
}
olsenP.rmse <- sqrt(mean(olsenP.error))

plot(Phosphorous$olsenP,olsenP.error)

plot(Phosphorous$DGT,DGT.error)

cor(olsenP.error,Phosphorous$olsenP)

cor(DGT.error,Phosphorous$DGT)

wilcox.test(olsenP.error,DGT.error)

olsen.lm <- lm(yield ~ olsenP,data=Phosphorous)
summary(olsen.lm)
anova(olsen.lm)

fit.olsen.lm<- coef(olsen.lm)[1]+coef(olsen.lm)[2]*x.olsen
plot(Phosphorous$olsenP,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [mg/100g]",
     main = "Linear model for Olsen-P")
lines(x.olsen,fit.olsen.lm)


DGT.lm <- lm(yield ~ DGT,data=Phosphorous)
summary(DGT.lm)
anova(DGT.lm)

fit.DGT.lm<- coef(DGT.lm)[1]+coef(DGT.lm)[2]*x.DGT
plot(Phosphorous$DGT,Phosphorous$yield,
     ylab = "Yield [100kg/ha]",
     xlab = "Bioavailable phosphorous [µg/L]",
     main = "Linear model for DGT")
lines(x.DGT,fit.DGT.lm)
