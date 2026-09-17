set.seed(42)
n<-3000
temperature<-sample(50:130,n,replace=TRUE)

vibration<-runif(n,0,10)

pressure<-sample(20:80,n,replace=TRUE)
rpm<-sample(1000:5000,n,replace=TRUE)

failure<-ifelse(temperature>100|vibration>7|(rpm<1500&pressure<30)|pressure>75,1,0)
df<-data.frame(temperature,vibration,pressure,rpm,failure)

write.csv(df,"aircraft_data.csv",row.names=FALSE)
print("DATA RETRIVE SUCCESSFUL")
