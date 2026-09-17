health_score<-function(temp,vib)
{
  score<-100-((temp-50)*0.5+vib*5)
  
  score<-max(0,min(100,score))
  return(score)
}
