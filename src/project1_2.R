library(plotly)
load("armDF.RData")

obs <- matrix(data = NA,nrow=1600,ncol=300)
experiment <- as.factor(c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100),rep(7,100),rep(8,100),rep(9,100),rep(10,100),rep(11,100),rep(12,100),rep(13,100),rep(14,100),rep(15,100),rep(16,100)))
for(i in 0:1599){
  obs[i+1,] <- c(df$x[((i*100)+1):((i*100)+100)],
                 df$y[((i*100)+1):((i*100)+100)],
                 df$z[((i*100)+1):((i*100)+100)])
}

standard_curve <- matrix(data=NA,nrow=16,ncol = 300)

for(i in 1:16){
  standard_curve[i,] <- apply(obs[(((i-1)*100)+1):(((i-1)*100)+100),],2,mean)
}

difference <- matrix(data=NA,nrow=16,ncol=16)

for (i in 1:16){
  for (j in 1:16){
    difference[i,j] <- dist(rbind(standard_curve[i,],standard_curve[j,]))
  }
}
difference

sd_curve <- matrix(data=NA,nrow=16,ncol = 300) 
for(i in 1:16){
  sd_curve[i,] <- apply(obs[(((i-1)*100)+1):(((i-1)*100)+100),],2,sd)
}

dist_all <- array(data=NA,dim=c(16,100,100))

for(i in 1:16){
  for(j in 1:100){
    for(k in 1:100){
     dist_all[i,j,k]<-dist(rbind(obs[(((i-1)*100)+j),],obs[(((i-1)*100)+k),]))
    }
  }
}

avg_dist_within <- rep(0,16)
for(i in 1:16){avg_dist_within[i]<-mean(dist_all[i,,])}
