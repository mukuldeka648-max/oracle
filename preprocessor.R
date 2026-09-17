preprocess<-function(df)
{
  df$temperature<-(df$temperature-50)/100
  df$vibration<-(df$vibration)/10
  df$pressure<-df$pressure/80
  df$rpm<-df$rpm/5000
  
  return(df)
}
