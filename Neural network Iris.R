rm(list = ls())

ds<-iris[1:4]
X<-scale(ds, center = TRUE, scale = TRUE)
Y<-iris$Species

train_ANN<-function(X,Y){
  
  neuron_proc<-function(weight,bias,input,target) {
    
    error <- function(d,y) {
      E<-0.5*(d-y)^2
      return(E)
    }
    
    neuron <- function(weight,bias,input) {
      f<-bias+sum(weight*input)  
      y<-exp(f)/(1+exp(f))
      return(y)
    }
    
    d<-target
    wi<-weight
    bs<-bias
    ds<-input
    l<-length(ds)
    mi<-0.05
    th_acc<-0.01
    e<-1
    
    while(e>th_acc){  
      y<-neuron(wi,bs,ds)
      e<-error(d,y)
      
      for (i in 1:l) {
        wi[i]<-wi[i]+mi*(d-y)*ds[i]
        
      }
      bs<-bs+mi*(d-y)
      
    }
    
    output<-list("weight"= unlist(wi),"bias" = unlist(bs))
    return(output)
    
  }  
  

uc <- unique(Y)
class_no<-length(uc)
we<-matrix(0, class_no, ncol(X))
bi<-matrix(0, class_no, 1)
len<-ncol(X)
epoki<-50
progres<-0
maxprogres<-class_no*epoki
cls<-list() 

for (c in 1:class_no) {
  
  class<-uc[c]
  Xs<-subset(X, Y == class)
  Ys<-matrix(1, nrow(Xs), 1)
  
  Xs2<-subset(X, Y != class)
  Ys2<-matrix(0, nrow(Xs2), 1)
  
  Xf<-rbind(Xs,Xs2)
  Yf<-rbind(Ys,Ys2)
  
  wi<-rnorm(len, mean=0, sd=1)  
  bs<-rnorm(1, mean=0, sd=1)  

  
  for (ep in 1:epoki) {
      
      perm<-sample(1:nrow(Xf), nrow(Xf), replace=FALSE)
  
      Xp<-Xf[perm,]
      Yp<-Yf[perm,]
  
      rs<-nrow(Xp)

      for (r in 1:rs) {
          inp<-Xp[r,]
          tr<-Yp[r]
          net<-neuron_proc(wi,bs,inp,tr)
          wi<-net$weight
          bs<-net$bias
      }
      progres<-progres+1
      message("Progres % ",round(progres/maxprogres,4)*100)
  }
  
we[c,]<-wi
bi[c,]<-bs
cls<-append(cls, list(class))
  
}


output<-list("class"= unlist(cls), "weight"=we,"bias"=bi, "length"=len)
return(output)

}


net<-train_ANN(X,Y)

net$class
net$weight
net$bias
net$length


test_ANN<-function (ANN, X) {
  
  neuron <- function(weight,bias,input) {
    f<-bias+sum(weight*input)  
    y<-exp(f)/(1+exp(f))
    return(y)
  }
  
  fin_ans<-list()   
  
  for (r in 1:nrow(X))
  {
    obs<-X[r,]
    class_n<-length(ANN$class)
    yy<-matrix(0, class_n, 1)
    for (c in 1:class_n)
    {
      wi<-ANN$weight[c,]
      bi<-ANN$bias[c,]
      ans<-neuron(wi,bi,obs)
      yy[c,]<-ans      
    }
    mx<-max(yy)
    i<-1
    while (TRUE) {
      if(mx==yy[i,])
      {
        break
      }
    
      i<-i+1
    }
    
    class<-ANN$class[i]
    fin_ans<-append(fin_ans, list(class))
    
    
  }
  Y<-unlist(fin_ans)
  return (Y)
}

Y2<-test_ANN(net, X)


print(tabl<- table(Predicted=Y2,Original=iris$Species))
print(1-sum(diag(tabl))/sum(tabl))





