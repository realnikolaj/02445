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
