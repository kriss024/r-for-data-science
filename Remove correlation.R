rm(list=ls())

setwd('C:/Temp')
getwd()

library(sas7bdat)

stats<-read.sas7bdat("stats_final.sas7bdat")

summary(stats)

stats <-data.frame(stats, Marker=1)

attach(stats)
stats <- stats[order(-Score),] 
detach(stats)

head(stats)

corr<-read.sas7bdat("corrcoeff.sas7bdat")

summary(corr)

r2_threshold<-0.3

#-------------------------------------

nrow<-nrow(stats)


for(y in 1:nrow) {
  
  #*****************
  
  if (stats[y,c("Marker")]==1) {
  
  score_var_name<-stats[y,c("Name")]
  
  corr_row<-corr[which(stats$Name==score_var_name),]
  
  ncol<-ncol(corr_row)
  
  #+++++++++

  for(x in 2:ncol) 
  {
    corr_row_name<-colnames(corr_row)[x]  
  
    if (score_var_name!=corr_row_name){
    
      if (corr_row[x]>=r2_threshold){
      
        for(y2 in y:nrow){
        
          if (stats[y2,c("Name")]==corr_row_name){
            stats[y2,c("Marker")]<-0 }
        
        } 

      
      }
    
    }
  
  }
  
  #+++++++++

  }
  message("done % ",round(y/nrow,4)*100)
  #*****************

}

#-------------------------------------

stats_final<-stats[which(stats$Marker==1),]

library(xlsx)
write.xlsx(stats_final, "stats.xlsx") 
