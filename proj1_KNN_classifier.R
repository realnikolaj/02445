library(class)
load("armDF.Rdata")

df2 <- subset(df,experiment==2)

obs <- matrix(data = NA,nrow=100,ncol=300)

for(i in 0:99){
  obs[i+1,] <- c(df2$x[((i*100)+1):((i*100)+100)],
                 df2$y[((i*100)+1):((i*100)+100)],
                 df2$z[((i*100)+1):((i*100)+100)])
}
person <- as.factor(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10)))

tot = 0
for(i in 1:100){
classification <- knn.cv(train=obs,cl=person,k=7, l = 0, prob = FALSE, use.all = TRUE)
tot <- tot +sum(classification == person)
}
tot/100

#k = 7 is best with an average of 64.5% correct classifications
