x <- data.frame("Lable" = sample(0:1, 100,replace = TRUE), "Seq" = 1:100, "Value" = sample(20:70, 100,replace = TRUE))
x
x2 = Descritize(x,3)
x2

pred <- prediction(x[,3], x$Lable)  
perf <- performance(pred,"auc")
aucx=perf@y.values[[1]]
aucx

pred <- prediction(x2$Catg, x2$Lable)  
perf <- performance(pred,"auc")
aucx2=perf@y.values[[1]]
aucx2

Descritize = function(SrcData, FrID)
{
  SrcData=SrcData[with(SrcData, order(Value)), ]
  N=NROW(SrcData)
  trainInstances=matrix(data = NA,N,1)
  trainLabel=matrix(data = NA,N,1)
  Categ=matrix(data = NA,N,1)
  i = 1
  for (i in 1:NROW(SrcData))
  { 
    trainInstances[i]  = SrcData[i,3]
    trainLabel[i]  = SrcData[i,1]
  }
  rocPoints = calculateROCPoints(trainInstances, trainLabel)
  K=NROW(rocPoints)
  Z=K-1
  i=2
  for (i in 1:Z){
    X=rocPoints[i+1,]
    Y=rocPoints[i,]
    
    nextSlope = Inf 
    if (Y[1]-X[1] != 0)
    {
      nextSlope=(Y[2]-X[2])/(Y[1]-X[1])
    }   
    rocPoints[i,6]=nextSlope
    rocPoints[i,7]=(Y[1]-X[1])*(Y[2]+X[2])/2
  }
  
  rocPoints[K,6]=Inf
  AUC=0
  for (i in 1:NROW(rocPoints)){
    if(!is.na(rocPoints[i,7]))
    {
      AUC=AUC+rocPoints[i,7]
    }
  }
  if (AUC < 0.5) {
    trainLabel = (trainLabel + 1) %% 2
    rocPoints = calculateROCPoints(trainInstances, trainLabel)
    
    K=NROW(rocPoints)
    Z=K-1
    i=2
    for (i in 1:Z){
      X=rocPoints[i+1,]
      Y=rocPoints[i,]
      
      nextSlope = Inf 
      if (Y[1]-X[1] != 0)
      {
        nextSlope=(Y[2]-X[2])/(Y[1]-X[1])
      }   
      rocPoints[i,6]=nextSlope
      rocPoints[i,7]=(Y[1]-X[1])*(Y[2]+X[2])/2
    }
    
    rocPoints[K,6]=Inf
    AUC=0
    for (i in 1:NROW(rocPoints)){
      if(!is.na(rocPoints[i,7]))
      {
        AUC=AUC+rocPoints[i,7]
      }
    }
  }
  
  pointsKept = findConvexHull(rocPoints, FrID)
  
  SrcData[,"Catg"]=Categ[,1]
  SrcData[,"LowB"]=Categ[,1]
  SrcData[,"UprB"]=Categ[,1]
  Lb = -Inf
  i = 2
  for (i in 1:N){
    CtgId=-1
    j=2
    for (j in 1:N){
      CtgId=CtgId+1
      if ((! is.na(SrcData[i,3])) 
      )
        if (SrcData[i,3] < pointsKept[j,3]){
          if (j > 1)
            Lb = pointsKept[j-1,3]
          Ub = pointsKept[j,3]
          SrcData[i,4] = CtgId
          SrcData[i,5] = Lb
          SrcData[i,6] = Ub
          break
        }
    }
  }    
  return(SrcData)
}   


calculateROCPoints=function(trainInstances, trainLabel)
{
  positiveClass = 1
  N=NROW(trainInstances)
  M = N-1
  N2 = N+1
  totalPositive = 0
  totalNegative = 0
  rocPoints = matrix(data = NA,N2,7) 
 
  j = 2
  
  totalPositive <- length(which(trainLabel == 1)) 
  totalNegative <- length(which(trainLabel == 0)) 
  
  curPos=totalPositive
  curNeg=totalNegative
  
  rocPoints[1,1] = 1
  rocPoints[1,2] = 1
  rocPoints[1,3] = -Inf
  rocPoints[1,4] = totalPositive
  rocPoints[1,5] = totalNegative
  i =1
  for (i in 1:M){
    if (trainLabel[i] == positiveClass)
    {curPos = curPos - 1}     
    else {curNeg = curNeg - 1}
    if ((! is.na(trainInstances[i])) & (! is.na(trainInstances[i+1]) )) 
      if( trainInstances[i] != trainInstances[i+1] )
      {
        cutValue=((trainInstances[i]+trainInstances[i+1])/2);
        TPR= curPos/totalPositive;
        FPR= curNeg/totalNegative;
        rocPoints[j,1] = FPR
        rocPoints[j,2] = TPR
        rocPoints[j,3] = cutValue
        rocPoints[j,4] = curPos
        rocPoints[j,5] = curNeg
        j = j + 1

      }
  }
  
  rocPoints[j,1] = 0
  rocPoints[j,2] = 0
  rocPoints[j,3] = Inf
  rocPoints[j,4] = 0
  rocPoints[j,5] = 0
  rocPoints=rocPoints[!is.na(rocPoints[,1]),]
  return(rocPoints)    
}

findConvexHull=function(rocPoints, FrID)
{
  rocPoints2=rocPoints    
  rocPoints=rocPoints2   
  N=NROW(rocPoints)
  
  pointsKept=matrix(data = NA,N,7)
  itr = 1
  repeat
  {
    for (i in 1:N){
      pointsKept[i,1]=NA
      pointsKept[i,2]=NA
      pointsKept[i,3]=NA
      pointsKept[i,4]=NA
      pointsKept[i,5]=NA
      pointsKept[i,6]=NA
      pointsKept[i,7]=NA
    }
    
    K=0
    for (i in 1:N){
      if ( !is.na(rocPoints[i,1]) )
      {K= K+1}
    }
    Z=K-1
    i=2
    for (i in 1:Z){
      X=rocPoints[i+1,]
      Y=rocPoints[i,]
      
      nextSlope = Inf 
      if (Y[1]-X[1] != 0)
      {
        nextSlope=(Y[2]-X[2])/(Y[1]-X[1])
      }   
      rocPoints[i,6]=nextSlope
      rocPoints[i,7]=(Y[1]-X[1])*(Y[2]+X[2])/2
    }
    
    rocPoints[K,6]=Inf
    itr = itr + 1
    
    if (Z >= 2)
    {    
      i = 2
      for (i in 2:Z){
        if (rocPoints[i,6] <= rocPoints[i-1,6])
        {
          rocPoints[i,1]=NA
          rocPoints[i,2]=NA
        }   
      }
    }
    
    t=1
    for (i in 1:K){
      if(!is.na(rocPoints[i,1]))
      {
        pointsKept[t,1]=rocPoints[i,1]
        pointsKept[t,2]=rocPoints[i,2]
        pointsKept[t,3]=rocPoints[i,3]
        pointsKept[t,4]=rocPoints[i,4]
        pointsKept[t,5]=rocPoints[i,5]
        pointsKept[t,6]=rocPoints[i,6]
        pointsKept[t,7]=rocPoints[i,7]
        t=t+1
      }
    }
    
    rocPoints = pointsKept
    roccnt = length(which(!is.na(rocPoints[,1]))) 
    
    if (roccnt==K)
    {
      break
    }    
  }
  
  pointsKept=pointsKept[!is.na(pointsKept[,1]),]
  
  return(pointsKept)
}