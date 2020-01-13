load("armdata.RData")

exp2 <- armdata[2][[1]]

data <- array(data = NA,dim = c(10,10,100,3))

for (i in 1:10){
  for (j in 1:10){
    data[i,j,,] <- exp2[[i]][[j]]
  }
}


