rm(list = ls())

library(mlbench)
data(PimaIndiansDiabetes)
Data.Colnames <- colnames(PimaIndiansDiabetes)

Train.Data <- PimaIndiansDiabetes[,1:8]
Train.Target <- PimaIndiansDiabetes[,9]


library(VSURF)
PimaIndians.vsurf <- VSURF(Train.Data, Train.Target, parallel = TRUE, ncores = 2, ntree = 50)

plot(PimaIndians.vsurf, var.names=TRUE)
plot(PimaIndians.vsurf, var.names=TRUE, step="thres")

best.two.variables <- PimaIndians.vsurf$varselect.thres[1:2]

PimaIndians2 <- data.frame(Train.Data[,best.two.variables], Train.Target)
Train.Data.Colnames <- names(PimaIndians2)
print(Train.Data.Colnames)

library(ggplot2)
library(RColorBrewer)
ggplot(PimaIndians2, aes(x=PimaIndians2[,1], y=PimaIndians2[,2], color=PimaIndians2[,3])) + 
  geom_point(shape = 16, size = 5, alpha = 1/2) +
  labs(colour = Data.Colnames[9], x = Train.Data.Colnames[1], y = Train.Data.Colnames[2], title = paste("Wykres punktowy dla zbioru","PimaIndiansDiabetes")) + 
  scale_color_brewer(palette="Set2")


library(randomForest)
PimaIndians.randomForest=randomForest(Train.Data, Train.Target)
VI_F<-importance(PimaIndians.randomForest)
VI_F<-data.frame(VI_F, names=row.names(VI_F))
require(data.table)
VI_F <- data.table(VI_F, key="MeanDecreaseGini")
rc<-c(nrow(VI_F), nrow(VI_F)-1)
VI_F.best.two <- VI_F[rc,]
print(VI_F.best.two)

library(caret)
varImp(PimaIndians.randomForest)
varImpPlot(PimaIndians.randomForest,type=2)