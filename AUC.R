
library("caret")
library("ROCR")

data = read.table("D:/RProjects/data.csv", 
                  sep=",", 
                  fill=FALSE, 
                  strip.white=TRUE)

data = data[with(data, order(V1)), ]
write.csv(data, "D:/RProjects/data.csv")

pred <- prediction(data$V1, data$V2)
perf <- performance(pred,"auc")
wj=perf@y.values[[1]]    
print(wj)
plot(perf) 

pred <- prediction(data$V3, data$V4)
perf <- performance(pred,"auc")
wj=perf@y.values[[1]]    
print(wj)


pred <- prediction(data$V5, data$V6)
perf <- performance(pred,"auc")
wj=perf@y.values[[1]]    
print(wj)

pred <- prediction(data$V7, data$V8)
perf <- performance(pred,"auc")
wj=perf@y.values[[1]]    
print(wj)




plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")

with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  
  lines(FPR, TPR, type='b', lwd=3, col="red")
})