SmokHeighwaya<- function(x,y,l){
  smok<-mat.or.vec(l, 2)
  r=runif(l)
  
  for (i in 1:l){
    if (r[i] < 0.5){
      xp=-0.4*x-1
      yp=-0.4*y+0.1
      smok[i,1]=xp
      smok[i,2]=yp
      x=xp
      y=yp
    }
    if (r[i] >= 0.5){
      xp=0.76*x-0.4*y
      yp=0.4*x+0.76*y
      smok[i,1]=xp
      smok[i,2]=yp
      x=xp
      y=yp
    }
  }
  return(smok)
  
}


PaprocBarnsleya<- function(x,y,l){
  paproc<-mat.or.vec(l, 2)
  p<-85+7+7+1
  r=sample(1:p, l, replace=T)
  
  for (i in 1:l){
    
    if (r[i] >= 16 & r[i] <= 100 ){
      xp=0.85*x+0.04*y
      yp=-0.04*x+0.85*y+1.6
      paproc[i,1]=xp
      paproc[i,2]=yp
      x=xp
      y=yp
    }
    
    if (r[i] >= 9 & r[i] <= 15 ){
      xp=-0.15*x+0.28*y
      yp=0.26*x+0.24*y+0.44
      paproc[i,1]=xp
      paproc[i,2]=yp
      x=xp
      y=yp
    }
    
    if (r[i] >= 2 & r[i] <= 8 ){
      xp=0.20*x-0.26*y
      yp=0.23*x+0.22*y+1.6
      paproc[i,1]=xp
      paproc[i,2]=yp
      x=xp
      y=yp
    }
    
    if (r[i] == 1){
      xp=0.0
      yp=0.16*y
      paproc[i,1]=xp
      paproc[i,2]=yp
      x=xp
      y=yp
    }
    
  }
  return(paproc)
  
}

s<-SmokHeighwaya(1,1,100000)
p<-PaprocBarnsleya(1,1,100000)

sx<-s[,1]
sy<-s[,2]

px<-p[,1]
py<-p[,2]

par(mfrow=c(1,2))
plot(sx,sy, pch=16, cex=0.2, ,col="red", main="Smok Heighwaya")
plot(px,py, pch=16, cex=0.2,,col="green", main="Paproæ Barnsleya")
