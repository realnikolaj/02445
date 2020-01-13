setwd("C:/Users/Bruger/OneDrive - Danmarks Tekniske Universitet/DTU/Statistical evaluation of aritifical intelligence/project_2/")
load("fosfor_data.Rdata")
#----with location 11----

Phosphorous$location <- as.factor(Phosphorous$location)
Phosphorous$yield[34] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2
Phosphorous$yield[36] <-(Phosphorous$yield[33]+Phosphorous$yield[35])/2

plot(Phosphorous$location,Phosphorous$yield)
plot(Phosphorous$DGT,Phosphorous$yield)
plot(Phosphorous$olsenP,Phosphorous$yield)

phos.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous,
                    start = list(alfa = 90 , beta = 1))
summary(phos.model)

phos.lm <- lm(yield ~ olsenP,data=Phosphorous)
summary(phos.lm)
anova(phos.lm)

#----without location 11----

Phosphorous2 <- Phosphorous[-c(33,34,35,36),]

plot(Phosphorous2$location,Phosphorous2$yield)
plot(Phosphorous2$DGT,Phosphorous2$yield)
plot(Phosphorous2$olsenP,Phosphorous2$yield)

phos2.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous2,
                  start = list(alfa = 90 , beta = 1))
summary(phos2.model)

phos2.lm <- lm(yield ~ olsenP,data=Phosphorous2)
summary(phos2.lm)
anova(phos2.lm)
