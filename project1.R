library("e1071")
library(plotly)
load("armdata.RData")

exp2 <- armdata[2][[1]]

data <- array(data = NA,dim = c(10,10,100,3))

for (i in 1:10){
  for (j in 1:10){
    data[i,j,,] <- exp2[[i]][[j]]
  }
}

plot(data[1,1,,][,2],data[1,1,,][,3])
plot_ly(x=data[4,1,,][,1],y=data[4,1,,][,2],z = data[4,1,,][,3],type = "scatter3d")


name_temp1<-"data/data"
for (i in 1:10){
  for (j in 1:10){
    temp <- data[i,j,,]
    name_temp2 <- paste(name_temp1,paste(i,j,sep="_"),sep="")
    write.csv(temp,file=name_temp2)
  }
}
# data1 <- data[1,1,,]
# write.csv(data1,file="data1.csv")
# write(data1,file="data1.txt")
# temp <- unlist(exp2)
# 
# write.csv(temp,file="data.csv")
