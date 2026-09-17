library(randomForest)

source("preprocess.R")
data<-read.csv("aircraft_data.csv")

y<-as.factor(data$failure)
x<-preprocess(data[,1:4])

model<-randomForest(x,y,ntree=200)

save(model,file="model.RData")

print("TRAINED")
