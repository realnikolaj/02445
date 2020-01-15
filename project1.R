library("e1071")
library(plotly)
load("armdata.RData")

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

data <- array(data = NA,dim = c(16,100,100,3))
for (i in 1:16){
  counter = 0
  for(j in 1:10){
    for (k in 1:10){
      data[i,counter,,] <- armdata[i][[1]][[j]][[k]]
      counter = counter +1
    }
  }
}

experiment <- as.factor(c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100),rep(7,100),rep(8,100),rep(9,100),rep(10,100),rep(11,100),rep(12,100),rep(13,100),rep(14,100),rep(15,100),rep(16,100)))
person <- as.factor(rep(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10)),16))
x <- list()
y <- list()
z <- list()
counter = 1
for(i in 1:16){
  for(j in 1:100){
    x[[counter]] <- data[i,j,,1]
    y[[counter]] <- data[i,j,,2]
    z[[counter]] <- data[i,j,,3]
    counter = counter + 1
}}

df <- data.frame(experiment,person,I(x),I(y),I(z))

lm(x~experiment,data=df)

plot(df$experiment,df$x)
