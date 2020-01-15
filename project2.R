load("fosfor_data.Rdata")
#----with location 11----

Phosphorous$location <- as.factor(Phosphorous$location)
Phosphorous$yield[34] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2
Phosphorous$yield[36] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2

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

#----without location 11----

# Phosphorous2 <- Phosphorous[-c(33,34,35,36),]
# 
# plot(Phosphorous2$location,Phosphorous2$yield)
# plot(Phosphorous2$DGT,Phosphorous2$yield)
# plot(Phosphorous2$olsenP,Phosphorous2$yield)
# 
# x.DGT2 <- seq(min(Phosphorous2$DGT),max(Phosphorous2$DGT),length=100)
# x.olsen2 <- seq(min(Phosphorous2$olsenP),max(Phosphorous2$olsenP),length=100)
# 
# 
# phos2.DGT.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous2,
#                       start = list(alfa = 90 , beta = 1))
# summary(phos2.DGT.model)
# 
# fit_model_DGT2<- coef(phos2.DGT.model)[1]*x.DGT2/(coef(phos2.DGT.model)[2]+x.DGT2)
# plot(Phosphorous2$DGT,Phosphorous2$yield)
# lines(x.DGT2,fit_model_DGT2)
# 
# 
# phos2.olsen.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = Phosphorous2,
#                         start = list(alfa = 90 , beta = 1))
# summary(phos2.olsen.model)
# 
# fit_model_olsen2<- coef(phos2.olsen.model)[1]*x.olsen2/(coef(phos2.olsen.model)[2]+x.olsen2)
# plot(Phosphorous2$olsenP,Phosphorous2$yield)
# lines(x.olsen2,fit_model_olsen2)
# 
# 
# olsen.lm2 <- lm(yield ~ olsenP,data=Phosphorous2)
# summary(olsen.lm2)
# anova(olsen.lm2)
# 
# fit.olsen.lm2<- coef(olsen.lm2)[1]+coef(olsen.lm2)[2]*x.olsen2
# plot(Phosphorous2$olsenP,Phosphorous2$yield)
# lines(x.olsen2,fit.olsen.lm2)
# 
# 
# DGT.lm2 <- lm(yield ~ DGT,data=Phosphorous2)
# summary(DGT.lm2)
# anova(DGT.lm2)
# 
# fit.DGT.lm2<- coef(DGT.lm2)[1]+coef(DGT.lm2)[2]*x.DGT2
# plot(Phosphorous2$DGT,Phosphorous2$yield)
# lines(x.DGT2,fit.DGT.lm2)
