library("e1071")
library(plotly)
library(rlist)
load("armdata.RData")
missing_data <- 0
missing_idx <- list()
for(i in 1:16){
  for(j in 1:10){
    for(k in 1:10){
      if (sum(is.na(armdata[[i]][[j]][[k]])) >= 1){
        missing_idx <- list.append(missing_idx,c(i,j,k))
        missing_data <- missing_data + sum(is.na(armdata[[i]][[j]][[k]]))
      }
    }
  }
}
for(i in 1:length(missing_idx)){
  for(j in 1:5){
    if(sum(is.na(armdata[[missing_idx[[i]][1]]][[missing_idx[[i]][2]]][[missing_idx[[i]][3]]][j,]))<3){
      for(k in 1:j){
        armdata[[missing_idx[[i]][1]]][[missing_idx[[i]][2]]][[missing_idx[[i]][3]]][k,] <- armdata[[missing_idx[[i]][1]]][[missing_idx[[i]][2]]][[missing_idx[[i]][3]]][j,]
      }
    }
  }
}

# exp2 <- armdata[2][[1]]
# 
# data <- array(data = NA,dim = c(10,10,100,3))
# 
# for (i in 1:10){
#   for (j in 1:10){
#     data[i,j,,] <- exp2[[i]][[j]]
#   }
# }
# 
# plot(data[1,1,,][,2],data[1,1,,][,3])
# plot_ly(x=data[4,1,,][,1],y=data[4,1,,][,2],z = data[4,1,,][,3],type = "scatter3d")


# name_temp1<-"data/data"
# for (i in 1:10){
#   for (j in 1:10){
#     temp <- data[i,j,,]
#     name_temp2 <- paste(name_temp1,paste(i,j,sep="_"),sep="")
#     write.csv(temp,file=name_temp2)
#   }
# }
# data1 <- data[1,1,,]
# write.csv(data1,file="data1.csv")
# write(data1,file="data1.txt")
# temp <- unlist(exp2)
# 
# write.csv(temp,file="data.csv")

# ---- all experiments ----

# experiment <- as.factor(c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100),rep(7,100),rep(8,100),rep(9,100),rep(10,100),rep(11,100),rep(12,100),rep(13,100),rep(14,100),rep(15,100),rep(16,100)))
# person <- as.factor(rep(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10)),16))
x <- list()
y <- list()
z <- list()
counter = 1
for(i in 1:16){
  for(j in 1:10){
    for(k in 1:10){
    x[[counter]] <- armdata[[i]][[j]][[k]][,1]
    y[[counter]] <- armdata[[i]][[j]][[k]][,2]
    z[[counter]] <- armdata[[i]][[j]][[k]][,3]
    counter = counter + 1
}}}

# df <- data.frame(experiment,person,I(x),I(y),I(z))

x <- unlist(x)
y <- unlist(y)
z <- unlist(z)

experiment <- as.factor(c(rep(1,100*100),rep(2,100*100),rep(3,100*100),rep(4,100*100),rep(5,100*100),rep(6,100*100),rep(7,100*100),rep(8,100*100),rep(9,100*100),rep(10,100*100),rep(11,100*100),rep(12,100*100),rep(13,100*100),rep(14,100*100),rep(15,100*100),rep(16,100*100)))
person <- as.factor(rep(c(rep(1,10*100),rep(2,10*100),rep(3,10*100),rep(4,10*100),rep(5,10*100),rep(6,10*100),rep(7,10*100),rep(8,10*100),rep(9,10*100),rep(10,10*100)),16))
timestep <- as.factor(rep(c(1:100),16*10*10))


df <- data.frame(experiment,person,timestep,x,y,z)

p = plot_ly(x = df$x[1:100], y = df$y[1:100], z = df$z[1:100],type="scatter3d",colors = c("red","blue"),name = "Experiment 1")
add_trace(p=p,x = df$x[1001:1100], y = df$y[1001:1100], z = df$z[1001:1100],name="Experiment 2")
# save(df,file = "armDF.Rdata")

