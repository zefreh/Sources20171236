#Positive=1
#Negative=0

SetState = function(ConfigList, st, DBN)
{
  ConfigList <- data.frame(
    Opt1 = integer(),
    Opt2 = integer(),
    spCounts = integer(),
    SamplePrc = numeric(),
    SampleRep = logical(),
    IsPrintWToFile = logical(),
    IsPrintCtgToFile = logical())
  ConfigList <- ConfigList[0,]
  
  ConfigList[1,"DBName"] = DBN
  ConfigList[1,"st"] = st
  ConfigList[1,"fold"] = 0
  ConfigList[1,"sp"] = 0
  ConfigList[1,"Opt1"] = 0
  ConfigList[1,"Opt2"] = 0
  ConfigList[1,"spCounts"] = 1
  ConfigList[1,"SamplePrc"] = 100
  ConfigList[1,"SampleRep"] = FALSE
  ConfigList[1,"IsPrintWToFile"] = FALSE
  ConfigList[1,"IsPrintCtgToFile"] = FALSE
  ConfigList[1,"KBalanceSampl"] = 0
  ConfigList[1,"KRemoveExtraSample"] = 0
  ConfigList[1,"IsNormalizeCOunt"] = FALSE
  ConfigList[1,"SMOTE_RATE"] = 0
  ConfigList[1,"UnderS_RATE"] = 0
  ConfigList[1,"KFeed"] = 0
  ConfigList[1,"KGenerate"] = 0
  ConfigList[1,"KCalcWeight"] = 0
  ConfigList[1,"KAddExtraField"] = 0
  ConfigList[1,"CntTopExtraField"] = 0
  
  if (st == 7){
    ConfigList[1,"KBalanceSampl"] = 7 
    ConfigList[1,"spCounts"] = 10
  } else if (st == 9){
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
  } else if (st == 10){
    ConfigList[1,"spCounts"] = 10
    ConfigList[1,"KBalanceSampl"] = 7
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
  } else if (st == 20 ){
    ConfigList[1,"KBalanceSampl"] = 20
  } else if (st == 24 ){
    ConfigList[1,"KBalanceSampl"] = 24
  } else if (st == 25 ){
    ConfigList[1,"KBalanceSampl"] = 25
  } else if (st == 26 ){
    ConfigList[1,"KBalanceSampl"] = 26
  } else if (st == 27 ){
    ConfigList[1,"KBalanceSampl"] = 27
  } else if (st == 28 ){
    ConfigList[1,"KBalanceSampl"] = 28
  } else if (st == 29 ){
    ConfigList[1,"KBalanceSampl"] = 29
  } else if (st == 30 ){
    ConfigList[1,"KBalanceSampl"] = 30
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 3
  } else if (st == 31 ){
    ConfigList[1,"KBalanceSampl"] = 31
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 2
  } else if (st == 32 ){
    ConfigList[1,"KBalanceSampl"] = 32
    ConfigList[1,"SMOTE_RATE"] = 1.5
    ConfigList[1,"UnderS_RATE"] = 0.9
  } else if (st == 33 ){
    ConfigList[1,"KBalanceSampl"] = 33
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 1.5
    ConfigList[1,"UnderS_RATE"] = 0.9
  } else if (st == 34 ){
    ConfigList[1,"KBalanceSampl"] = 34
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 2
  } else if (st == 35 ){
    ConfigList[1,"KBalanceSampl"] = 35
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 2
  } else if (st == 36 ){
    ConfigList[1,"KBalanceSampl"] = 36
    ConfigList[1,"Opt1"] = 35
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 3
  } else if (st == 37 ){
    ConfigList[1,"KBalanceSampl"] = 37
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
    ConfigList[1,"SMOTE_RATE"] = 4
  } else if (st == 38 ){
    ConfigList[1,"KBalanceSampl"] = 38
  } else if (st == 39 ){
    ConfigList[1,"KBalanceSampl"] = 39
  } else if (st == 40 ){
    ConfigList[1,"Opt2"] = 1
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
  } else if (st == 41 ){
    ConfigList[1,"Opt2"] = 2
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
  } else if (st == 42 ){
    ConfigList[1,"Opt2"] = 1
  } else if (st == 43 ){
    ConfigList[1,"Opt2"] = 3
    ConfigList[1,"IsNormalizeCOunt"] = TRUE    
  } else if (st == 44 ){
    ConfigList[1,"Opt2"] = 4
    ConfigList[1,"IsNormalizeCOunt"] = TRUE
  } else if (st == 45 ){
    ConfigList[1,"Opt2"] = 2
  } else if (st == 46 ){
    ConfigList[1,"Opt2"] = 5
  } else if (st == 47 ){
    ConfigList[1,"KBalanceSampl"] = 7 
    ConfigList[1,"spCounts"] = 10
    ConfigList[1,"KCalcWeight"] = 1
  } else if (st == 48 ){
    ConfigList[1,"KBalanceSampl"] = 7 
    ConfigList[1,"spCounts"] = 10
    ConfigList[1,"KCalcWeight"] = 1
    ConfigList[1,"KRemoveExtraSample"] = 4
  } else if (st == 49 ){
    ConfigList[1,"Opt2"] = 6
  } else if (st == 50 ){
    ConfigList[1,"Opt2"] = 7
  } else if (st == 51 ){
    ConfigList$KAddExtraField = 1
    ConfigList$CntTopExtraField = 5
    ConfigList$KCalcWeight = 2
  } else if (st == 52 ){
    ConfigList$KAddExtraField = 1
    ConfigList$CntTopExtraField = 10
    ConfigList$KCalcWeight = 2
  } else if (st == 53 ){
    ConfigList$KAddExtraField = 1
    ConfigList$CntTopExtraField = 5
    ConfigList$KCalcWeight = 2    
  } else if (st == 54 ){
    ConfigList$KAddExtraField = 2
    ConfigList$CntTopExtraField = 5
    ConfigList$KCalcWeight = 2
  }
  return(ConfigList)
}

GetNameVer=function(ConfigList, foldVar, sp)
{
  tmp = paste( 
    ConfigList$DBName, "#", 
    toString(ConfigList$st), "#",
    toString(ConfigList$Opt1), "_", 
    toString(ConfigList$Opt2), "_", 
    toString(ConfigList$spCounts), "_",
    toString(ConfigList$SamplePrc), "_",
    substr(toString(ConfigList$SampleRep), 1, 1),
    substr(toString(ConfigList$IsNormalizeCOunt), 1, 1), "_", 
    toString(ConfigList$KBalanceSampl), "_",
    toString(ConfigList$KRemoveExtraSample), "_",
    toString(foldVar), "_", 
    toString(sp), 
    sep = "")
  
  return(tmp)
}


InitializeAlg=function(ConfigList, data)
{
  RowCnt = NROW(data)
  PosCnt = NROW(subset(data, ClassLabel == 1))
  NegCnt = NROW(subset(data, ClassLabel == 0))
  CtrCnt = NROW(subset(data, ClassLabel != 0 &  ClassLabel != 1))
  FCnt = ncol(data) - 2
  DF = length(unlist(strsplit(ConfigList$CategryFields, split=",")))
  CF = FCnt - DF
    
  NameVer = GetNameVer(ConfigList, 0, 0)
  
  message("Start ", 
          NameVer,
          ",T_P_N_C_FC_C_D=",toString(RowCnt),
          "_",toString(PosCnt),
          "_",toString(NegCnt),
          "_",toString(CtrCnt),
          "_",toString(FCnt),
          "_",toString(DF),
          "_",toString(CF),
          ",Time=",Sys.time())
  
  TestAUCList = list()
  TestAUCList <- data.frame(
    auc = numeric(), 
    acc = numeric(), 
    spc = numeric(), 
    sen = numeric(), 
    SenSpc = numeric())
  TestAUCList <- TestAUCList[0,]
  return(TestAUCList)
}

GetAllWList =  function()
{
  AllWList <- data.frame(
    auc = numeric(),
    wf = numeric(), 
    acc = numeric(),
    FId = numeric(),
    aucof = numeric(), 
    wof = numeric(), 
    foldVar = numeric(),
    sp = numeric())
  
  AllWList <- AllWList[0,]
  return(AllWList)
}

GetFtrCtgList =  function()
{
  FtrCtgList <- data.frame(
    FtrID = integer(),
    CtgID = integer(),
    Lb = numeric(),
    Ub = numeric(),
    Pi = integer(),
    Ni = integer(),
    ri = numeric(),
    SP = numeric())
  FtrCtgList <- FtrCtgList[0,]
  return(FtrCtgList)
}

GetExtraFields =  function()
{
  ExtraFields <- data.frame(
    FId = numeric(),
    FId1 = numeric(),
    FId2 = numeric(),
    CId1 = numeric(),
    CId2 = numeric(),
    PCnt = numeric(),
    NCnt = numeric(), 
    acc = numeric(),
    aucof = numeric(), 
    wof = numeric(), 
    foldVar = numeric(),
    sp = numeric())
  
  ExtraFields <- ExtraFields[0,]
  return(ExtraFields)
}


GetSamlpeList=function(SampleList, sp)
{
  SPList = subset(SampleList, SPID == sp)
  return(sort(SPList[,3]))
}

GetAuc =  function(ConfigList, Data1, Lable1, IsModify)
{
  wj = 1
  PosCont = length(which(Lable1 == 1)) 
  NegCont = length(which(Lable1 == 0))
  if ((PosCont != 0) & (NegCont != 0))
  {
    pred <- prediction(Data1, Lable1)  
    perf <- performance(pred,"auc")
    wj=perf@y.values[[1]]
    if (IsModify)
      if (wj < 0.5) 
        wj = 1 - wj
  }
  else
  {
    NameVer = GetNameVer(ConfigList, 0, 0)
    FileName = "D:/RProjects/OptTask/Err/Only1Catg.txt"
    Msg = paste(NameVer,  " Only1Catg Fold= ", ConfigList$fold, " sp= ",ConfigList$sp,  sep = " ")
    cat(Msg, file=FileName, append=TRUE, sep = "\n")
  }
  
  if (max(Data1) > 1)
  {
    NameVer = GetNameVer(ConfigList, 0, 0)
    FileName = "D:/RProjects/OptTask/Err/WMoreThan1.txt"
    Msg = paste(NameVer,  " Only1Catg Fold= ", ConfigList$fold, " sp= ",ConfigList$sp,  sep = " ")
    cat(Msg, file=FileName, append=TRUE, sep = "\n")
  }
  
  return(wj)  
}  

CalcTestACC=function(testData)
{
  #ACC = TP + TN / (TN+FP) OR T / N

  PosCont = NROW(subset(testData, testData$ClassLabel == 1))
  NegCont = NROW(subset(testData, testData$ClassLabel == 0))
  
  if ((PosCont != 0) & (NegCont != 0)){
    pred <- prediction(testData$rnk , testData$ClassLabel)
    perf2 <- performance(pred,"acc")
    ix <- which.min(abs(perf2@x.values[[1]] - .5))
    if(perf2@x.values[[1]][ix] < 0.5) #to make sure that 0.5 is the selected threshold!!!
    {ix <- ix - 1}
    acc=perf2@y.values[[1]][ix]
  } else
    acc = 0.5 
  return(acc)
}

GetSEN =  function(ConfigList, Data1, Lable1)
{
  #SEN = TP / (TP+FN) OR TP / P

  wj = 0.5
  PosCont = length(which(Lable1 == 1)) 
  NegCont = length(which(Lable1 == 0))
  
  if ((PosCont != 0) & (NegCont != 0))
  {
    pred <- prediction(Data1, Lable1)
    perf <- performance(pred,"sens","spec")
    ix <- which.min(abs(perf@alpha.values[[1]] - .5))
    wj <- perf@y.values[[1]][ix]
  }
  
  return(wj)  
}  

GetSPC =  function(ConfigList, Data1, Lable1)
{
  #SPC = TN / (TN+FP) OR TN / N
  #
  #Data1 = testData$rnk
  #Lable1 = testData$ClassLabel
  
  wj = 0.5
  PosCont = length(which(Lable1 == 1)) 
  NegCont = length(which(Lable1 == 0))
  
  if ((PosCont != 0) & (NegCont != 0))
  {
    pred <- prediction(Data1, Lable1)
    perf <- performance(pred,"sens","spec")
    ix <- which.min(abs(perf@alpha.values[[1]] - .5))
    wj <- perf@x.values[[1]][ix]
  }
  return(wj)  
}  

MakeSampleList = function(ConfigList, TotalRecord)
{
  IsReplace = ConfigList$SampleRep
  SPListAll <- data.frame(
    SPID = integer(),
    SPPrc = integer(),
    RowID = integer())
  SPListAll <- SPListAll[0,]
  
  SmpID = 1
  
  if (ConfigList$spCounts == 1)
  {
    SPList1 = list(sample(TotalRecord, TotalRecord,replace = IsReplace))
    for (j in 1:length(SPList1[[1]]))
      SPListAll[nrow(SPListAll)+1,] = c(SmpID, 100,SPList1[[1]][j])
    
  }
  else if (ConfigList$spCounts == 10)
  { 
    SampleRecord = ConfigList$SamplePrc * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))

      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, ConfigList$SamplePrc,SPList1[[1]][j])
      
      SmpID = SmpID + 1
    }
  }
  else if (ConfigList$spCounts == 60){
    
    SampleRecord = 60 * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 60,SPList1[[1]][j])
      
      SmpID = SmpID + 1
    }
    
    SampleRecord = 67 * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 67,SPList1[[1]][j])
      SmpID = SmpID + 1
      
    }
    
    SampleRecord = 70 * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 70,SPList1[[1]][j])
      SmpID = SmpID + 1
      
    }
    
    
    SampleRecord = 80 * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 80,SPList1[[1]][j])
      SmpID = SmpID + 1
    }
    
    SampleRecord = 90 * TotalRecord / 100
    for (i in 1:10)
    {
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 90,SPList1[[1]][j])
      SmpID = SmpID + 1
    }
    
    for (i in 1:10)
    {
      SampleRecord = 100 * TotalRecord / 100
      SPList1 = list(sample(TotalRecord, SampleRecord,replace = IsReplace))
      for (j in 1:length(SPList1[[1]]))
        SPListAll[nrow(SPListAll)+1,] = c(SmpID, 100,SPList1[[1]][j])
      SmpID = SmpID + 1
    }
  }
  SPListAll = SPListAll[with(SPListAll, order(SPID , RowID)), ]
  
  return(SPListAll)
}

FindDistance = function(ConfigList, iData, jData, CategryFields)
{
  message('Errrrrrrrrrrrrrrrrrrrroooooooooooooooooooooorrrrrrrrrrrrrrrrr')
  #iData = MData3[i,]
  #jData = MData3[j,]
  distij = 0
  p = 7
  ncoliData = ncol(iData) - 1
  for (p in 3:ncoliData)
  {
    if (p %in% CategryFields)
    {
      if (iData[p] == jData[p]) 
        dist1 = 1
      else
        dist1 = 0
    }
    else
    {
      dist1 = iData[p] - jData[p]
      dist1 = dist1 * dist1
    }
    if (!is.na(dist1)) 
      distij = distij + dist1
  }
  distij = sqrt(distij)
  return(distij)
}

MakeDistanceMatrix = function(MData, CategryFields)
{
  #MData = MData3
  DistanceMatrix <- data.frame(
    i1 = integer(),
    i2 = integer(),
    distnc = numeric())
  DistanceMatrix <- DistanceMatrix[0,]

  i=2
  for (i in 1:nrow(MData))
  {
    k=i+1
    j=2
    for (j in k:nrow(MData))
    {
      #dist_ij = FindDistance(ConfigList, MData3[i,], MData3[j,], CategryFields)
      
      iData = MData[i,]
      jData = MData[j,]
      distij = 0
      p = 3
      ncoliData = ncol(iData) - 1
      for (p in 3:ncoliData)
      {
        #message(i,"   ",j,"  ",k, "  ", p)
        if (p %in% CategryFields)
        {
          if (iData[p] == jData[p]) 
            dist1 = 1
          else
            dist1 = 0
        }
        else
        {
          dist1 = iData[p] - jData[p]
          dist1 = dist1 * dist1
        }
        if (!is.na(dist1)) 
          distij = distij + dist1
      }
      distij = sqrt(distij)
      DistanceMatrix[nrow(DistanceMatrix)+1,] = c(i, j, distij)
      DistanceMatrix[nrow(DistanceMatrix)+1,] = c(j, i, distij)
    }
  }
  
  return(DistanceMatrix)
}

Normalize0 = function(MData)
{
  #MData = MData[,i]
  SumC = sum(MData)
  MData = MData / SumC
  return(MData)
}

downSamplegh = function(ConfigList, MData, NewCnt, CategryFields)
{
  #MData = NData
  MData3 = MData
  i = 3
  for (i in 3:ncol(MData))
    if (!i %in% CategryFields)
      MData3[,i] = Normalize0(MData3[,i])
  
  DistMatrix = MakeDistanceMatrix(MData3, CategryFields)
  
  i=1
  for (i in 1:nrow(MData3))
  {
    distSum = sum(DistMatrix$distnc[which(DistMatrix$i1 == i)])
    MData3[i,"dist"] = sqrt(distSum) 
  }
  
  MData3=MData3[with(MData3, order(dist)), ]
  rownames(MData3) <- NULL
  MData2 = MData3[rownames(MData3) <= NewCnt ,-ncol(MData3)] 
  ind = MData2$Sequence
  MData4 = MData[MData$Sequence %in% ind,]
  return(MData4)
}

SMOTEDataSet = function(ConfigList, MData, NewCnt, CategryFields)
{
  #MData = PData
  OvSmData = MData
  MCnt = nrow(MData)
  InsNeed = NewCnt - MCnt 
  FeatureType <- sapply(MData, is.factor)
  
  if (NewCnt < MCnt) return(OvSmData)
  
  j = 3
  for (j in 3:ncol(MData))
    if (j %in% CategryFields)
      FeatureType[j] = TRUE  
  
  for (i in 1:InsNeed)
  {
    OCnt = nrow(OvSmData)
    OvSmData[OCnt+1,1]=OvSmData[MCnt,1]
    
    if (ConfigList$KFeed == 1) {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      FeedData = MData
    }
    else if (ConfigList$KFeed == 2) {
      r1 = sample(1:OCnt, 1, replace=FALSE)
      r2 = sample(1:OCnt, 1, replace=FALSE)
      FeedData = OvSmData
    }
    
    if (ConfigList$KGenerate == 1) {
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[OCnt+1,j]= FeedData[r1,j]
        else if (j1 == 2)
          OvSmData[OCnt+1,j]= FeedData[r2,j]
      }
    } 
    else if (ConfigList$KGenerate == 2) {
      for (j in 3:ncol(MData))
        OvSmData[MCnt+1,j]= (FeedData[r1,j] + FeedData[r2,j]) / 2
    }
    else if (ConfigList$KGenerate == 3) {
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
      }
    }
    else if (ConfigList$KGenerate == 4) {
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[MCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[MCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[MCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }
    
  }
  return(OvSmData)
}

MakeSMOTE = function(ConfigList, MData, NewCnt, CategryFields)
{
  #MData = PData
  MCnt = nrow(MData)
  InsNeed = NewCnt - MCnt 
  OvSmData = MData
  FeatureType <- sapply(MData, is.factor)
  
  if (NewCnt < MCnt) return(OvSmData)
  
  j = 3
  for (j in 3:ncol(MData))
    if (j %in% CategryFields)
      FeatureType[j] = TRUE  
  
  if (ConfigList$KBalanceSampl == 12) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]=MCnt+1
      OvSmData[OCnt+1,1]=OvSmData[MCnt,1]
      OvSmData[r1,]
      OvSmData[r2,]
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[OCnt+1,j]= MData[r1,j]
        else if (j1 == 2)
          OvSmData[OCnt+1,j]= MData[r2,j]
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 13) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      OvSmData[r1,]
      OvSmData[r2,]
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[MCnt+1,j]= OvSmData[r1,j]
        else if (j1 == 2)
          OvSmData[MCnt+1,j]= OvSmData[r2,j]
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 14) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        OvSmData[MCnt+1,j]= (OvSmData[r1,j] + OvSmData[r2,j]) / 2
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 15) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        OvSmData[MCnt+1,j]= (OvSmData[r1,j] + OvSmData[r2,j]) / 2
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 16) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        OvSmData[MCnt+1,j]= (OvSmData[r1,j] + OvSmData[r2,j] / 2)
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 17) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[MCnt+1,j]= OvSmData[r1,j]
        else if (j1 == 2)
          OvSmData[MCnt+1,j]= OvSmData[r2,j]
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 18) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[MCnt+1,j]= OvSmData[r1,j]
        else if (j1 == 2)
          OvSmData[MCnt+1,j]= OvSmData[r2,j]
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 19) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      for (j in 3:ncol(MData))
      {
        j1 = sample(1:2, 1, replace=FALSE)
        if (j1 == 1)
          OvSmData[OCnt+1,j]= MData[r1,j]
        else if (j1 == 2)
          OvSmData[OCnt+1,j]= MData[r2,j]
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 20) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 21) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 22) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
        
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 23) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
        
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 24) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[MCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[MCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[MCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 25) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[OCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[OCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[OCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 26) {
    rownames(MData) <- NULL
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt+1,j]= MData[r1,j]
              else 
                OvSmData[MCnt+1,j]= MData[r2,j]   
            }
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 27) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[OCnt+1,j]= MData[r1,j]
              else 
                OvSmData[OCnt+1,j]= MData[r2,j]   
            }
        
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 28) {
    rownames(MData) <- NULL
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      MCnt = nrow(OvSmData)+1
      OvSmData[MCnt,1] = OvSmData[MCnt-1,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[MCnt,j]= MData[r1,j]
              else 
                OvSmData[MCnt,j]= MData[r2,j]   
            }
        
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 29) {
    rownames(MData) <- NULL
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      j = sample(3:ncol(MData), 1, replace=FALSE)
      MCnt = nrow(OvSmData)+1
      OvSmData[MCnt,1] = OvSmData[MCnt-1,1]
      OvSmData[MCnt,]= MData[r1,]
      OvSmData[MCnt,j]= MData[r2,j]
    }
  }
  else if (ConfigList$KBalanceSampl == 31) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      j=12
      for (j in 3:ncol(MData))
      {
        MidC = mean(MData[,j], na.rm=TRUE)
        #message(MidC)
        if (!is.na(MidC))
          if (!is.na(MData[r1,j]))
            if (!is.na(MData[r2,j])){
              if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                OvSmData[OCnt+1,j]= MData[r1,j]
              else 
                OvSmData[OCnt+1,j]= MData[r2,j]   
            }
      }
    }
  }
  else if (ConfigList$KBalanceSampl == 32) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[OCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[OCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[OCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 33) {
    for (i in 1:InsNeed)
    {
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OCnt = nrow(OvSmData)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[MCnt,1]
      j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[OCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[OCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[OCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[OCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[OCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 34) {
    for (i in 1:InsNeed)
    {
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(OvSmData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(OvSmData[r1,j]))
              if (!is.na(OvSmData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[MCnt+1,j]= OvSmData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[MCnt+1,j]= OvSmData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[MCnt+1,j]= OvSmData[r1,j]
                  else if (j1 == 2)
                    OvSmData[MCnt+1,j]= OvSmData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(OvSmData[r1,j]))
              if (!is.na(OvSmData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(OvSmData[r2,j] - MidC))
                  OvSmData[MCnt+1,j]= OvSmData[r1,j]
                else 
                  OvSmData[MCnt+1,j]= OvSmData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 38) {
    #i=11
    for (i in 1:InsNeed)
    {
      #message(i)
      MCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[MCnt+1,2]= MCnt+1
      OvSmData[MCnt+1,1]= OvSmData[MCnt,1]
      #j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(OvSmData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(OvSmData[r1,j]))
              if (!is.na(OvSmData[r2,j]))
              {
                if (OvSmData[r1,j] == MidC2)
                  OvSmData[MCnt+1,j]= OvSmData[r1,j]
                else if (OvSmData[r2,j] == MidC2)
                  OvSmData[MCnt+1,j]= OvSmData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[MCnt+1,j]= OvSmData[r1,j]
                  else if (j1 == 2)
                    OvSmData[MCnt+1,j]= OvSmData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(OvSmData[,j], na.rm=TRUE)
          #message(i)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(OvSmData[r1,j]))
              if (!is.na(OvSmData[r2,j]))
              {
                if (abs(OvSmData[r1,j] - MidC) < abs(OvSmData[r2,j] - MidC))
                  OvSmData[MCnt+1,j]= OvSmData[r1,j]
                else 
                  OvSmData[MCnt+1,j]= OvSmData[r2,j]  
              }
        }
      }
    }  
  }
  else if (ConfigList$KBalanceSampl == 39) {
    #i=11
    for (i in 1:InsNeed)
    {
      #message(i)
      OCnt = nrow(OvSmData)
      r1 = sample(1:MCnt, 1, replace=FALSE)
      r2 = sample(1:MCnt, 1, replace=FALSE)
      OvSmData[OCnt+1,2]= MCnt+1
      OvSmData[OCnt+1,1]= OvSmData[OCnt,1]
      #j=3
      for (j in 3:ncol(MData))
      {
        if (FeatureType[j]) 
        {
          #message(j)
          MidC2 = tail(names(sort(table(MData[,j]))), 1)
          if (!is.na(MidC2))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (MData[r1,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else if (MData[r2,j] == MidC2)
                  OvSmData[MCnt+1,j]= MData[r2,j]  
                else
                {
                  j1 = sample(1:2, 1, replace=FALSE)
                  if (j1 == 1)
                    OvSmData[MCnt+1,j]= MData[r1,j]
                  else if (j1 == 2)
                    OvSmData[MCnt+1,j]= MData[r2,j]
                }
              }
        }
        else
        {
          MidC = mean(MData[,j], na.rm=TRUE)
          #message(i)
          #message(MidC)
          if (!is.na(MidC))
            if (!is.na(MData[r1,j]))
              if (!is.na(MData[r2,j]))
              {
                if (abs(MData[r1,j] - MidC) < abs(MData[r2,j] - MidC))
                  OvSmData[MCnt+1,j]= MData[r1,j]
                else 
                  OvSmData[MCnt+1,j]= MData[r2,j]  
              }
        }
      }
    }  
  }

  return(OvSmData)
}

BalanceSamples = function(ConfigList, trainData, foldVar, sp){
  CategryFields = unlist(strsplit(ConfigList$CategryFields, split=","))
  if (ConfigList$KBalanceSampl == 2){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      SN = sample(nrow(NData), nrow(PData),replace = FALSE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      SP = sample(nrow(PData), nrow(NData),replace = FALSE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 3){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 1.5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = FALSE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 1.5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = FALSE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 4){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = FALSE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = FALSE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 5){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 3)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = FALSE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 3)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = FALSE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 6){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 4)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = FALSE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 4)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = FALSE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 7){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) )
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) )
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 8){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 1.5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 1.5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 9){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 10){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 3)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 3)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 11){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 4)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
      
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 4)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 12){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = nrow(NData)
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = nrow(PData)
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 13){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = nrow(NData)
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = nrow(PData)
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 14){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = nrow(NData)
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = nrow(PData)
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  }  
  else if (ConfigList$KBalanceSampl == 15){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 1.5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 1.5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 16){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 17){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 18){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 3)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 3)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 19){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 20){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 21){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 1.5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 1.5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 22){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 3)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 3)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 23){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  }   
  else if (ConfigList$KBalanceSampl == 24){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 25){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        NData = downSamplegh(ConfigList, NData, NewCnt, CategryFields)
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        PData = downSamplegh(ConfigList, PData, NewCnt, CategryFields)
      }
      
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 26){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 27){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 5)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 5)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 28){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 29){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * 2)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * 2)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 30){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 31){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 32){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCntP = round(nrow(PData) * ConfigList$SMOTE_RATE)
      NewCntN = round(nrow(NData) * ConfigList$UnderS_RATE)
      
      if (NewCntN > NewCntP)
        NewCntP = NewCntN
      SN = sample(nrow(NData), NewCntN,replace = FALSE)
      NData = NData[SN,]
      PData = MakeSMOTE(ConfigList, PData, NewCntP, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCntN = round(nrow(NData) * ConfigList$SMOTE_RATE)
      NewCntP = round(nrow(PData) * ConfigList$UnderS_RATE)
      if (NewCntP < NewCntN)
        NewCntP = NewCntN
      SN = sample(nrow(PData), NewCntP,replace = FALSE)
      PData = PData[SN,]
      NData = MakeSMOTE(ConfigList, NData, NewCntN, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 33){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCntP = round(nrow(PData) * ConfigList$SMOTE_RATE)
      NewCntN = round(nrow(NData) * ConfigList$UnderS_RATE)
      
      if (NewCntN > NewCntP)
        NewCntP = NewCntN
      SN = sample(nrow(NData), NewCntN,replace = FALSE)
      NData = NData[SN,]
      PData = MakeSMOTE(ConfigList, PData, NewCntP, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCntN = round(nrow(NData) * ConfigList$SMOTE_RATE)
      NewCntP = round(nrow(PData) * ConfigList$UnderS_RATE)
      if (NewCntP < NewCntN)
        NewCntP = NewCntN  #  Error
      SN = sample(nrow(PData), NewCntP,replace = FALSE)
      PData = PData[SN,]
      NData = MakeSMOTE(ConfigList, NData, NewCntN, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 34){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData) 
      else {
        SN = sample(nrow(NData), NewCnt,replace = FALSE)
        NData = NData[SN,]
      }
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData) 
      else {
        SN = sample(nrow(PData), NewCnt,replace = FALSE)
        PData = PData[SN,]
      }
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
      trainData = rbind(NData, PData)
    }
  } 
  else if (ConfigList$KBalanceSampl == 35){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 36){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 37){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    if (nrow(NData) > nrow(PData)) {
      NewCnt = round(nrow(PData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(NData))
        NewCnt = nrow(NData)       
      SN = sample(nrow(NData), NewCnt,replace = TRUE)
      NData = NData[SN,]
      trainData = rbind(NData, PData)
    }
    else if (nrow(NData) < nrow(PData)) {
      NewCnt = round(nrow(NData) * ConfigList$SMOTE_RATE)
      if (NewCnt > nrow(PData))
        NewCnt = nrow(PData)       
      SP = sample(nrow(PData), NewCnt,replace = TRUE)
      PData = PData[SP,]
      trainData = rbind(NData, PData)
    }
  }
  else if (ConfigList$KBalanceSampl == 38){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    NCnt = nrow(NData)
    PCnt = nrow(PData)
    NewCnt = round(abs(PCnt - NCnt) / 2.0)
    if (NCnt > PCnt) {
      NewCnt = PCnt + NewCnt 
      SN = sample(NCnt, NewCnt,replace = TRUE)
      NData = NData[SN,]
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
    }
    else if (NCnt < PCnt) {
      NewCnt = NCnt + NewCnt 
      SN = sample(PCnt, NewCnt,replace = TRUE)
      PData = PData[SN,]
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
    }
    trainData = rbind(NData, PData)
  }
  else if (ConfigList$KBalanceSampl == 39){
    NData = subset(trainData, ClassLabel == 0)
    PData = subset(trainData, ClassLabel == 1)
    NCnt = nrow(NData)
    PCnt = nrow(PData)
    NewCnt = round(abs(PCnt - NCnt) / 2.0)
    if (NCnt > PCnt) {
      NewCnt = PCnt + NewCnt 
      SN = sample(NCnt, NewCnt,replace = TRUE)
      NData = NData[SN,]
      PData = MakeSMOTE(ConfigList, PData, NewCnt, CategryFields)
    }
    else if (NCnt < PCnt) {
      NewCnt = NCnt + NewCnt 
      SN = sample(PCnt, NewCnt,replace = TRUE)
      PData = PData[SN,]
      NData = MakeSMOTE(ConfigList, NData, NewCnt, CategryFields)
    }
    trainData = rbind(NData, PData)
  }

  RowCnt = NROW(trainData)
  PosCnt = NROW(subset(trainData, ClassLabel == 1))
  NegCnt = NROW(subset(trainData, ClassLabel == 0))
  CtrCnt = NROW(subset(trainData, ClassLabel != 0 &  ClassLabel != 1))
  
  NameVer = GetNameVer(ConfigList, foldVar, sp)
  
  message(
    "          Blnc_", 
    NameVer,
    "_",toString(ConfigList$KBalanceSampl),
    ", T_P_N_C = ",toString(RowCnt),
          "_",toString(PosCnt),
          "_",toString(NegCnt),
          "_",toString(CtrCnt),
          ", start time = ",Sys.time())
  return(trainData)
  
}



ShowProcessAlg = function(ConfigList, TestAUCList, foldVar, sp, StartTime)
{
  if (sp != 0) 
    return(-1)
  
  DifTime <- Sys.time() - StartTime # calculate difference
  st1 = "     "
  if (sp != 0) 
    st1 = "          "
  
  NameVer = GetNameVer(ConfigList, foldVar, sp)
 
  tmp = paste(
    st1,
    NameVer,
    " , AUC = ", round(TestAUCList[foldVar,1], 5),
    " , RunTime = ",  round(as.numeric(DifTime,units="mins"), 5),
    " , Time = ",  Sys.time(),
    sep = "")

  message(tmp)
}

CalcOrigWeights  = function(ConfigList, trainData2, FtrCtgList, WList, foldVar, sp)
{
  FtrCnt = ncol(trainData2)
  j=3
  for (j in 3:FtrCnt)
  { 
    #print(j)
    pred <- prediction(trainData2[,j], trainData2$ClassLabel)  
    perf <- performance(pred,"auc")
    wj=perf@y.values[[1]]
    if (wj < 0.5) 
      wj = 1 - wj
    WList[which(WList$sp == sp & WList$FId ==j),"aucof"] = wj
  }      

  WList$wof = 2 * (WList$aucof - 0.5)  
  return(WList)
}

DescritizeDataset = function(ConfigList, trainData0, FtrCtgList, sp)
{
  FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == ConfigList$sp)  
  FtrCnt = NCOL(trainData0)
  #t=trainData0
  #f=FtrCtgList
  #trainData0=t
  #FtrCtgList=f
  
  j=12
  for (j in 3:FtrCnt)
  { 
    CategryFields = unlist(strsplit(ConfigList$CategryFields, split=","))
    if (!j %in% CategryFields){
      i=647
      for (i in 1:NROW(trainData0))
      {
        #print("=========")
        #print(i)
        #print(j)
        FV = trainData0[i,j] 
        if (! is.na(FV))
        {
          trainData0[i,j] = NA
          FtrCtg = subset(FtrCtgListS, FtrCtgListS$FtrID == j & ((FtrCtgListS$Lb == FtrCtgListS$Ub & FtrCtgListS$Lb == FV)
                                                                 |(FtrCtgListS$Lb < FV & FV <= FtrCtgListS$Ub))) 

          if (nrow(FtrCtg) == 1)
          {
            CID = FtrCtgListS[which(FtrCtgListS$FtrID == j & ((FtrCtgListS$Lb == FtrCtgListS$Ub & FtrCtgListS$Lb == FV)
                                                              |(FtrCtgListS$Lb < FV & FV <= FtrCtgListS$Ub))), 2] 
            if (! is.na(CID))
              trainData0[i,j]= CID
          }
          else if (nrow(FtrCtg) > 1)
            message("Error IN DescritizeDataset DescritizeDataset DescritizeDataset DescritizeDataset")
        }
      }   
      
    }
      

  }
  return(trainData0)
}

Descritize  = function(SrcData, FrID)
{
  #positiveClass = 1
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
    # print("=====================")
    # print(AUC)  
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
    #print(AUC)        
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
      if ((! is.na(SrcData[i,3])) ###& (! is.na(pointsKept[j,3]) )
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
  # rocPoints <- data.frame(
  #   FtrID = integer(),
  #   CtgID = integer(),
  #   Lb = integer(),
  #   Ub = integer(),
  #   Pi = integer(),
  #   Ni = integer(),
  #   ri = numeric())
  
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
    # print("=============================================")
    # print(i)
    # print(trainInstances[i])
    # print(trainInstances[i+1])
    if ((! is.na(trainInstances[i])) & (! is.na(trainInstances[i+1]) )) 
      if( trainInstances[i] != trainInstances[i+1] )
      {
        cutValue=((trainInstances[i]+trainInstances[i+1])/2);
        TPR= curPos/totalPositive;
        FPR= curNeg/totalNegative;
        # if(TPR < FPR){
        rocPoints[j,1] = FPR
        rocPoints[j,2] = TPR
        rocPoints[j,3] = cutValue
        rocPoints[j,4] = curPos
        rocPoints[j,5] = curNeg
        j = j + 1
        #}
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
  rocPoints2=rocPoints   #Temp 
  rocPoints=rocPoints2   #Temp
  N=NROW(rocPoints)
  
  # for (i in 1:N){
  #   a = rocPoints[i,1]
  #   rocPoints[i,1]=rocPoints[i,2]
  #   rocPoints[i,2]=a
  # }
  
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
    
    # AUC=0
    # for (i in 1:K){
    #   if(!is.na(rocPoints[i,7]))
    #   {
    #     AUC=AUC+rocPoints[i,7]
    #   }
    # }
    # print(AUC)
    
    # tmp = paste("D:/RProjects/OptTask/Output/plot/Plot_", toString(FrID), "_",toString(itr), sep = "")
    # tmp = paste(tmp,  ".png", sep = "")
    # png(tmp)
    # Lab=paste("ROC Curve",  toString(AUC), sep = " ")
    # plot(rocPoints[,1],rocPoints[,2],main=Lab,col="red",lwd=2)
    # text(rocPoints[,1], rocPoints[,2], rocPoints[,3], cex=0.6, pos=4, col="blue")
    # dev.off()
    #     tmp = paste("D:/R_Project161102/", SrcName, "/Output/Data_", toString(itr), sep = "")
    #     tmp = paste(tmp,  ".xlsx", sep = "")
    #     write.xlsx(rocPoints, tmp)
    itr = itr + 1
    
    if (Z >= 2)
    {    
      i = 2
      for (i in 2:Z){
        # message("==========================")
        # message(rocPoints[i,6])
        # message(rocPoints[i-1,6])
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
    #rocPoints=rocPoints[!is.na(rocPoints[,1]),]
  }
  
  
  pointsKept=pointsKept[!is.na(pointsKept[,1]),]
  
  # AUC=0
  # for (i in 1:NROW(rocPoints)){
  #   if(!is.na(rocPoints[i,7]))
  #   {
  #     AUC=AUC+rocPoints[i,7]
  #   }
  # }
  # print(AUC)
  
  return(pointsKept)
}

DescritizeFeature=function(trainData, FrID)
{
  #FrID=9
  SrcData = trainData[c(1,2,FrID)]
  colnames(SrcData) <- c("Lable", "Seq","Value")
  SrcData = Descritize(SrcData, FrID)
  SrcData = SrcData[with(SrcData, order(Seq)), ]
  trainData[,FrID] = SrcData[,4]
  trainData[,"LowB"] = SrcData[,5]
  trainData[,"UprB"] = SrcData[,6]
  return(trainData)
}

GetAllCategoryList = function(ConfigList, FtrCtgList, trainData, FrID, sp)
{
  # All Features are categorized.
  #FrID=6
  #trainData  = trainData0[SPList, ]
  #trainData <- trainData[ order(trainData[,FrID]), ]
  
  #trainData = trainData[with(trainData, order(FrID)), ] 
  i=1
  for (i in 1:NROW(trainData))
  {
    CtID = trainData[i,FrID]
    
    if (!is.na(CtID))
    {
      #if (CtID != as.numeric(trainData[i,FrID])) 
      # message("Error In GetAllCategoryList0 CtID = ", CtID, " in FId = ", FId, " trainData[i,FrID] = ", trainData[i,FrID], " as.numeric(trainData[i,FrID]) = ", as.numeric(trainData[i,FrID]))
      
      
      if (CtID != trainData[i,FrID]) 
        message("Error In GetAllCategoryList1 CtID = ", CtID, " in FId = ", FId )
      
      CtID = trainData[i,FrID]
      if (NROW(subset(FtrCtgList, FtrID == FrID & CtgID == CtID & SP == sp)) == 0)
      {
        lb = trainData[i,"LowB"]
        ub = trainData[i,"UprB"]
        Pi = NROW(subset(trainData, trainData[,FrID] == CtID & trainData$ClassLabel == 1))
        Ni = NROW(subset(trainData, trainData[,FrID] == CtID & trainData$ClassLabel == 0))
        
        # Pii <- length(which((trainData$ClassLabel == 1)&(as.integer(trainData[,FrID])  == CtID))) 
        # Nii <- length(which((trainData$ClassLabel == 0)&(as.integer(trainData[,FrID])  == CtID))) 
        # 
        # if ((Pi != Pii) || (Ni != Nii))
        #   print("Error in Pi and Pii")
        
        P <- length(which(trainData$ClassLabel == 1)) 
        N <- length(which(trainData$ClassLabel == 0)) 
        
        if ((Pi + Ni) == 0) 
        {
          ri = 0
          message("Error In GetAllCategoryList3 (Pi + Ni) == 0 CtID = ", CtID, " in FId = ", FId, " trainData[i,FrID] = ", trainData[i,FrID], " as.numeric(trainData[i,FrID]) = ", as.numeric(trainData[i,FrID]))
          #print(Pi)
          #print(Ni)
          #print(CtID)
          #print(FrID)
        }
        else
        {
          ri = Pi / (Pi + Ni) 
        }
        
        if (is.numeric(ri)) {
          if (ConfigList$Opt1 == 1) ri = ri+((Pi+Ni)/(P+N))
          if (ConfigList$Opt1 == 2) ri = ri*(1+(Pi/P))
          if (ConfigList$Opt1 == 3) ri = ri*(1+(Pi/P))*(1+(Pi/P))
          if (ConfigList$Opt1 == 4) ri = ri*(1+(Pi+Ni)/(P+N))
          if (ConfigList$Opt1 == 5) ri = ri*(1+(Ni/N))
          if (ConfigList$Opt1 == 6) {if (ri < 0.25) ri = 0}
          if (ConfigList$Opt1 == 7) {if (ri < 0.5) ri = 0}
          if (ConfigList$Opt1 == 8) {if (ri < 0.75) ri = 0}
          if (ConfigList$Opt1 == 9) ri = ri+ 0.5 * ((Pi+Ni)/(P+N))
          if (ConfigList$Opt1 == 10) ri = ri + 2.0 * ((Pi+Ni)/(P+N))
          if (ConfigList$Opt1 == 11) ri = ri * (1+(Pi/P))*(1+(Pi/P))*(1+(Pi/P))
          if (ConfigList$Opt1 == 12) ri = Pi / P 
          if (ConfigList$Opt1 == 13) ri = Pi / (P + N)
          if (ConfigList$Opt1 == 14) ri = ri * (6 +(Pi/P))
          if (ConfigList$Opt1 == 15) ri = ri * 2.0 * (1+(Pi/P))
          if (ConfigList$Opt1 == 16) ri = ri * (2 +(Pi/P))
          if (ConfigList$Opt1 == 17) ri = Pi/(Pi+Ni) + Pi/P
          if (ConfigList$Opt1 == 18) ri = ri * (3 +(Pi/P))
          if (ConfigList$Opt1 == 19) ri = ri * (4 +(Pi/P))
          if (ConfigList$Opt1 == 20) ri = ri * (3.14 +(Pi/P))
          if (ConfigList$Opt1 == 21) ri = ri * (3.5 +(Pi/P))
          if (ConfigList$Opt1 == 22) ri = ri * (3.25 +(Pi/P))
          if (ConfigList$Opt1 == 23) ri = 3.14 * Pi / (Pi + Ni) + (Pi/P)
          if (ConfigList$Opt1 == 24) ri = Pi / (Pi + Ni) + 2 * (Pi/P)
          if (ConfigList$Opt1 == 25) ri = Pi / (Pi + Ni) + 3 * (Pi/P)
          if (ConfigList$Opt1 == 26) ri = ri * (3.141592 +(Pi/P))
          if (ConfigList$Opt1 == 27) ri = ri * log2(2+Pi)
          if (ConfigList$Opt1 == 28) ri = ri * log2(2+Pi/P)
          ##
          if (ConfigList$Opt1 == 29) ri = ri * (-1) * log2(1-Pi/P)  
          if (ConfigList$Opt1 == 30) ri = log2(2+Pi/P)  
          if (ConfigList$Opt1 == 31) ri = log2(3+Pi/(Pi+Ni))  
          ###
          if (ConfigList$Opt1 == 32) ri = ri * log2((3.141592 +(Pi/P)))
          if (ConfigList$Opt1 == 33) ri = ri * log2(1 +Pi)
          if (ConfigList$Opt1 == 34) ri = ri * log2(1 +(Pi/P))
          if (ConfigList$Opt1 == 35) ri = ri * log2(2+Pi/(Pi+Ni))
          
          if (ConfigList$Opt1 == 36) ri = ri * (0 +(Pi/P))
          if (ConfigList$Opt1 == 37) ri = ri * (0.5 +(Pi/P))
          if (ConfigList$Opt1 == 38) ri = ri * (1 +(Pi/P))
          if (ConfigList$Opt1 == 39) ri = ri * (1.5 +(Pi/P))
          if (ConfigList$Opt1 == 40) ri = ri * (2 +(Pi/P))
          if (ConfigList$Opt1 == 41) ri = ri * (2.5 +(Pi/P))
          if (ConfigList$Opt1 == 42) ri = ri * (50 +(Pi/P))
          if (ConfigList$Opt1 == 43) ri = ri * (100 +(Pi/P))
          if (ConfigList$Opt1 == 44) ri = ri * (10 +(Pi/P))
          if (ConfigList$Opt1 == 45) ri = ri * (4.5 +(Pi/P))
          if (ConfigList$Opt1 == 46) ri = ri * (5 +(Pi/P))
          if (ConfigList$Opt1 == 47) { 
            ri = ri * (3.141592 +(Pi/P))
            if (((Pi+Ni)/(P+N)) < 0.5) ri = 0
          }
          if (ConfigList$Opt1 == 48) { 
            ri = ri * (3.141592 +(Pi/P))
            if (((Pi+Ni)/(P+N)) < 0.25) ri = 0
          }
          if (ConfigList$Opt1 == 49)  { 
            ri = ri * (3.141592 +(Pi/P))
            if (((Pi+Ni)/(P+N)) < 0.1) ri = 0
          }
          if (ConfigList$Opt1 == 50)  { 
            if (((Pi+Ni)/(P+N)) < 0.1) ri = 0
          }
          if (ConfigList$Opt1 == 51) ri = ri * (3.05 +(Pi/P))
          if (ConfigList$Opt1 == 52) ri = ri * (3.10 +(Pi/P))
          if (ConfigList$Opt1 == 53) ri = ri * (3.15 +(Pi/P))
          if (ConfigList$Opt1 == 54) ri = ri * (3.20 +(Pi/P))
          if (ConfigList$Opt1 == 55) ri = ri * (3.25 +(Pi/P))
          if (ConfigList$Opt1 == 56) ri = ri * (3.30 +(Pi/P))
          if (ConfigList$Opt1 == 57) ri = ri * (3.35 +(Pi/P))
          if (ConfigList$Opt1 == 58) ri = ri * (3.40 +(Pi/P))
          if (ConfigList$Opt1 == 59) ri = ri * (3.45 +(Pi/P))
        }        
        else
        {
          print("Error")
        }
        
        lb2 = lb
        ##if (lb2 != -Inf) lb2 = as.numeric(lb)  ###Mistake
        
        ub2 = ub
        ##if (ub2 != Inf) ub2 = as.numeric(ub)   ###Mistake
        
        FtrCtgList[nrow(FtrCtgList)+1,]= c(FrID, CtID, lb2, ub2, Pi, Ni, ri,sp)
      }
    }
    
    # else if (NROW(subset(FtrCtgList, FtrID == FrID & CtgID == CtID)) == 1)
    # {
    #   FtrCtgList[which(FtrID == FrID & CtgID == CtID, arr.ind=TRUE), 3] <- lb 
    #   FtrCtgList[which(FtrID == FrID & CtgID == CtID, arr.ind=TRUE), 4] <- ub 
    #   FtrCtgList[which(FtrID == FrID & CtgID == CtID, arr.ind=TRUE), 5] <- Pi 
    #   FtrCtgList[which(FtrID == FrID & CtgID == CtID, arr.ind=TRUE), 6] <- Ni 
    #   FtrCtgList[which(FtrID == FrID & CtgID == CtID, arr.ind=TRUE), 7] <- ri 
    # }
  }
  
  #Test = aggregate(Pi+Ni ~ FtrID, FtrCtgList, sum)
  FtrCtgList = FtrCtgList[with(FtrCtgList, order(SP, FtrID, Lb)), ]
  return(FtrCtgList)
}

Categorize=function(trainData, FtrCtgList, sp)
{
  FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == sp)
  FtrCnt = NCOL(trainData)
  j = 3
  for (j in 3:FtrCnt)
  { 
    i=1
    for (i in 1:NROW(trainData))
    {
      FV = trainData[i,j] 
      if (! is.na(FV)){
        r1 = subset(FtrCtgListS, FtrCtgListS$FtrID == j & ((FtrCtgListS$Lb == FtrCtgListS$Ub & FtrCtgListS$Lb == FV)
                                                           | (FtrCtgListS$Lb < FV & FV <= FtrCtgListS$Ub)))
        
        if (NROW(r1)!=1)
          message("Error In Categorize = ", j , " in Row = ", i , " In value =  ", trainData[i,j], " NROW(r1) = ",NROW(r1), " sp = ",sp)

        trainData[i,j]= r1$CtgID
      }
    }
  }
  return(trainData) 
}


CalcFeatureWeightsByCategory=function(ConfigList, trainData, ExtraFields, FtrCtgList, foldVar, sp)
{
  if (sp == 0)
    message("ErrorErrorErrorErrorErrorErrorErrorErrorErrorErrorErrorErrorErrorError")
  
  trainData0 = trainData 
  FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == sp)  
  FtrCnt = NCOL(trainData0) - 2   

  WList = GetAllWList()
  WList[1,]=1
  WList[2,]=1
  WList
  j = 3
  for (j in 3:FtrCnt)
  { 
    #print(j)
    data3=trainData0[c(1,2,j)]
    colnames(data3) <- c("Lable", "Seq","Value")
    data3=data3[!is.na(data3$Value),]
    PosCont = NROW(subset(data3, data3$Lable == 1))
    NegCont = NROW(subset(data3, data3$Lable == 0))
    if ((PosCont != 0) & (NegCont != 0))
    {
      FtrCtgListSF = subset(FtrCtgListS, FtrCtgListS$FtrID == j)
      
      data3=data3[with(data3, order(Value)), ]
      pred <- prediction(data3$Value, data3$Lable)  #### OR prediction(data3$value, data3$Lable) ???
      perf <- performance(pred,"auc")
      wj=perf@y.values[[1]]
      
      if (wj < 0.5)
        wj = 1 - wj
      WList[j,1] = wj 
      WList[j,"FId"] = j 
    }
  } 
  
  FCnt = max(WList$FId)
  i=1
  if (NROW(ExtraFields) > 0)
    for (i in 1:NROW(ExtraFields))
    {
      FCnt = FCnt + 1
      WList[nrow(WList)+1,1] = ExtraFields[i,"auc"] 
      WList[nrow(WList),"FId"] = FCnt 
    }
  
  WList$wf = 2 * (WList$auc - 0.5)    # .9  .4  .8   --- .8  .3   .6
  
  if (is.numeric(WList$auc)) {
    if (ConfigList$Opt2 == 1) WList$wf = sqrt(2 * (WList$auc - 0.5))    # .9  .4  .8  .89   -- .8  .3  .6 .77 
    else if (ConfigList$Opt2 == 2) WList$wf = sqrt(WList$auc)     # .9  .94    --- .8   .89
    else if (ConfigList$Opt2 == 3) WList$wf = sqrt(3 * (WList$auc - 0.5))      # .9  .4  1.2  1.09   -- .8  .3  .9 .94 
    else if (ConfigList$Opt2 == 4) {
      WList$wf = sqrt(2 * (WList$auc - 0.5))
      WList[which(WList$auc < 0.6),"wf"] = 0 
    }
    else if (ConfigList$Opt2 == 5) WList$wf = (2 * (WList$auc - 0.5))^(1/3)    # .9  .4  .8  .89   -- .8  .3  .6 .77 
    else if (ConfigList$Opt2 == 6) WList$wf = (2 * (WList$auc - 0.5)) * (2 * (WList$auc - 0.5)) * (2 * (WList$auc - 0.5))
    else if (ConfigList$Opt2 == 7) WList$wf = (2 * (WList$auc - 0.5)) * (2 * (WList$auc - 0.5))
  }  
  
  WList[1,]=0
  WList[2,]=0
  WList[,"foldVar"]=foldVar
  WList[,"sp"]=sp
  #print("End OF CalcFeatureWeightsByCategory")
  PrintWListToFile(ConfigList, WList, foldVar, sp)
  return(WList)
}

CalcFeatureWeightsByValue=function(ConfigList, trainData0, FtrCtgList, foldVar)
{
  FtrCtgListS = FtrCtgList  
  FtrCnt = NCOL(trainData0) 
  
  WList = GetAllWList()
  WList[1,]=1
  WList[2,]=1
  WList
  j = 3
  for (j in 3:FtrCnt)
  { 
    #print(j)
    data3=trainData0[c(1,2,j)]
    colnames(data3) <- c("Lable", "Seq","Value")
    data3=data3[!is.na(data3$Value),]
    PosCont = NROW(subset(data3, data3$Lable == 1))
    NegCont = NROW(subset(data3, data3$Lable == 0))
    if ((PosCont != 0) & (NegCont != 0))
    {
        {
        pred <- prediction(data3$Value, data3$Lable)  ####
        perf <- performance(pred,"auc")
        wFirst=perf@y.values[[1]]
        wFirst
        
        if (wFirst < 0.5)
        {
          wFirst = 1 - wFirst
          # data3$Lable = (data3$Lable + 1) %% 2 
          # pred <- prediction(data3$Value, data3$Lable)
          # perf <- performance(pred,"auc")
          # wFirst=perf@y.values[[1]]    
        }
      }
      
      # i=1
      # for (i in 1:NROW(data3))
      # {
      #   FV = data3[i,3] 
      #   if (! is.na(FV)){
      #     r1 = subset(FtrCtgListS, FtrCtgListS$FtrID == j & ((FtrCtgListS$Lb == FtrCtgListS$Ub & FtrCtgListS$Lb == FV)
      #                                                        |(FtrCtgListS$Lb < FV & FV <= FtrCtgListS$Ub)))
      #     
      #     data3[i,4]= mean(r1$ri)
      #   }
      # }
      # 
 
      WList[j,1] = wFirst
      WList[j,"FId"] = j 
      #WList[j,1] = max() 
      #WList[j,3] = acc
    }
  } 
  
  WList$wf = 2 * (WList$auc - 0.5)    # .9  .4  .8   --- .8  .3   .6
  
  if (is.numeric(WList$auc)) {
    if (ConfigList$Opt2 == 1) WList$wf = sqrt(2 * (WList$auc - 0.5))    # .9  .4  .8  .89   -- .8  .3  .6 .77 
    else if (ConfigList$Opt2 == 2) WList$wf = sqrt(WList$auc)     # .9  .94    --- .8   .89
    else if (ConfigList$Opt2 == 3) WList$wf = sqrt(3 * (WList$auc - 0.5))      # .9  .4  1.2  1.09   -- .8  .3  .9 .94 
    else if (ConfigList$Opt2 == 4) {
      WList$wf = sqrt(2 * (WList$auc - 0.5))
      WList[which(WList$auc < 0.6),"wf"] = 0 
    }
    else if (ConfigList$Opt2 == 5) WList$wf = (2 * (WList$auc - 0.5))^(1/3)    # .9  .4  .8  .89   -- .8  .3  .6 .77 
  }  
  
  WList[1,]=0
  WList[2,]=0
  WList[,"foldVar"]=foldVar
  WList[,"sp"]=0
  #print("End OF CalcFeatureWeightsByValue")
  PrintWListToFile(ConfigList, WList, foldVar, 0)
  return(WList)
}

RemoveExtraSamples=function(ConfigList, FtrCtgList, SampleWeight)
{
  RoCntS1 = nrow(SampleWeight)
  RoCntF1 = nrow(FtrCtgList)
  
  #FtrCtgListLast = FtrCtgList
  #FtrCtgList = FtrCtgListLast
  
  if (ConfigList$KRemoveExtraSample == 1)  {
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    for (i in 1:ConfigList$spCounts)
    {
      s1 = subset(SampleWeight, SampleWeight$SID == i)
      if (NROW(s1) == 0) 
      {  
        FtrCtgList = subset(FtrCtgList, FtrCtgList$SP != i)
      } 
    }
  } 
  else if (ConfigList$KRemoveExtraSample == 2)  {
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    for (i in 1:ConfigList$spCounts)
    {
      s1 = subset(SampleWeight, SampleWeight$SID == i)
      if (NROW(s1) == 0) 
      {  
        FtrCtgList = subset(FtrCtgList, FtrCtgList$SP != i)
      } 
    }
  } 
  else if (ConfigList$KRemoveExtraSample == 3)  {
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    SampleWeight = subset(SampleWeight, SampleWeight$SumW > mean(SampleWeight$SumW))
    for (i in 1:ConfigList$spCounts)
    {
      s1 = subset(SampleWeight, SampleWeight$SID == i)
      if (NROW(s1) == 0) 
      {  
        FtrCtgList = subset(FtrCtgList, FtrCtgList$SP != i)
      } 
    }
  }  
  else if (ConfigList$KRemoveExtraSample == 4)  {
    FCnt = max(FtrCtgList$FtrID)
    f=7
    for (f in 3:FCnt)
    {
      SmpCtgList <- data.frame(
        sp = integer(),
        StrCtg = character(),
        stringsAsFactors=F)
      SmpCtgList <- SmpCtgList[0,]
      i=1
      for (i in 1:ConfigList$spCounts)
      {
        s1 = subset(FtrCtgList, FtrCtgList$SP == i & FtrCtgList$FtrID == f)
        SmpCtgList[i,1] = i
        SmpCtgList[i,"StrCtg"] = toString(s1$Ub)
      }
      
      dupCtg = SmpCtgList[duplicated(SmpCtgList[,2]),]
      i=2
      if (nrow(dupCtg) > 0) 
        for (i in 1:nrow(dupCtg))
        {
          dupID = dupCtg[i,1]
          FtrCtgList = FtrCtgList[which(!(FtrCtgList$SP == dupID & FtrCtgList$FtrID == f)),]
        }
    }
  }  
  
  RoCntS2 = nrow(SampleWeight)
  RoCntF2 = nrow(FtrCtgList)
  
  NameVer = GetNameVer(ConfigList, 0, 0)
  
  tmp1 = paste(
    "     ", 
    NameVer,
    " RoCntS1 = ",toString(RoCntS1),
    " RoCntS2 = ",toString(RoCntS2),
    " RoCntF1 = ",toString(RoCntF1),
    " RoCntF2 = ",toString(RoCntF2),
    sep = "")
  
  message(tmp1)  
  FtrCtgList = FtrCtgList[with(FtrCtgList, order(SP, FtrID, Lb)), ]
  return(FtrCtgList)
}

CalcScore=function(FtrCtgList, FId, FV, spCounts)
{
  FtrCtg = subset(FtrCtgList, FtrCtgList$FtrID == FId & ((FtrCtgList$Lb == FtrCtgList$Ub & FtrCtgList$Lb == FV)
                                                   |(FtrCtgList$Lb < FV & FV <= FtrCtgList$Ub) ))

  if (NROW(FtrCtg) == 0) 
  { 
    scr = 0
    message("CalcScore 0 - There is no score for FId : ", FId, " In value :  ", FV , " NROW(FtrCtg) = ", NROW(FtrCtg))
  } else if ((spCounts != 1) & (NROW(FtrCtg) == 1))
  { 
    scr = FtrCtg$ri
    message("CalcScore 1 - There is only one score for FId : ", FId, " In value :  ", FV , " spCounts = ", spCounts, " NROW(FtrCtg) = ", NROW(FtrCtg))
  } else if (NROW(FtrCtg) == spCounts) 
  { 
    scr = mean(FtrCtg$ri)
  } else if ((spCounts != 1) & (NROW(FtrCtg) < spCounts)) 
  { 
    scr = mean(FtrCtg$ri)
  } else if (NROW(FtrCtg) > spCounts) 
  { 
    FtrCtg = FtrCtg[with(FtrCtg, order(CtgID)), ]
    scr = FtrCtg[NROW(FtrCtg), "ri"]
    message("CalcScore 2 - There is more score for FId : ", FId, " In value :  ", FV , " spCounts = ", spCounts, " NROW(FtrCtg) = ", NROW(FtrCtg))
  } else
  { 
    scr = 0
  }
  return(scr)
}

CalcScoreSample=function(FtrCtgList, FId, FV, spCounts, sp)
{
  scr = 0
  if (sp != 0){
    FtrCtg = subset(FtrCtgList, 
                    FtrCtgList$FtrID == FId 
                    & (FtrCtgList$SP == sp)
                    & ((FtrCtgList$Lb == FtrCtgList$Ub & FtrCtgList$Lb == FV)
                       |(FtrCtgList$Lb < FV & FV <= FtrCtgList$Ub) ))
    if (nrow(FtrCtg) == 1)
      scr = FtrCtg[which(FtrCtg$SP == sp), "ri"]
    else if (nrow(FtrCtg) > 1)
      message("CalcScoreSample  - There is more score for FId : ", FId, " In value :  ", FV , " spCounts = ", spCounts, " NROW(FtrCtg) = ", NROW(FtrCtg))
  } 
  return(scr)
}

CalcTestRank=function(ConfigList, FtrCtgList, AllWList, testData, ExtraFields, TestAUCList, foldVar)
{
  sp = ConfigList$sp
  if (ConfigList$KCalcWeight == 0)
  {
    i=1
    col=NCOL(testData)
    for (i in 1:NROW(testData))
    { 
      totalScore=0
      totalWeight=0
      FId=3
      for (FId in 3:col)      
      {
        if (! is.na(testData[i,FId])){
          FV = testData[i,FId]  ####mistake
          
          score = CalcScore(FtrCtgList,FId,FV, ConfigList$spCounts)  ####avg sampling
          totalScore = totalScore + AllWList[FId,2] * score
          totalWeight = totalWeight + AllWList[FId,2]
        }
      }
      testData[i,"rnk"] = totalScore / totalWeight
    }
    TestAUCList[foldVar,1]=GetAuc(ConfigList, testData$rnk, testData$ClassLabel, FALSE) 
    TestAUCList[foldVar,2]=CalcTestACC(testData)
    TestAUCList[foldVar,3]=GetSPC(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,4]=GetSEN(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,5]=sqrt(TestAUCList[foldVar,3] * TestAUCList[foldVar,4])
  }
  else if (ConfigList$KCalcWeight == 1)
  {
    col=NCOL(testData)
    i=1
    for (i in 1:NROW(testData))
    { 
      totalScore=0
      totalWeight=0
      FId=5
      for (FId in 3:col)      
      {
        if (! is.na(testData[i,FId]))
        {
          #print("i==========")
          #print(FId)
          #print(i)
          FV = testData[i,FId] 
          FWeightSSum = 0
          FScore = 0
          cnt = 0
          sp = 1
          for (sp in 1:ConfigList$spCounts)
          {
            FriS = CalcScoreSample(FtrCtgList,FId,FV, ConfigList$spCounts, sp) ####avg 
            FWeightS = AllWList[which(AllWList$foldVar == foldVar & AllWList$sp == sp & AllWList$FId ==FId), "wof"] 
            #print(FriS)
            #print(FWeightS)
            if ((FWeightS > 0) ) 
            {
              FScore = FScore + FWeightS * FriS
              FWeightSSum = FWeightSSum + FWeightS
              cnt = cnt + 1
            }
          }
          # if (cnt != 10) 
          # {
          #   print(cnt)
          #   print(FId)
          #   print(FV)
          #   print("++++++++++++++++++++++++++++++")
          #   
          # }      
          if (cnt > 0)
          {
            FScore = FScore / cnt 
            FWeightSSum = FWeightSSum / cnt
            totalScore = totalScore + FScore
            totalWeight = totalWeight + FWeightSSum
          }
        }
      }
      if (totalWeight == 0)       print("totalWeight")
      if (totalWeight > 0)
        testData[i,"rnk"] = totalScore / totalWeight
      else
        testData[i,"rnk"] = 0
    }
    TestAUCList[foldVar,1]=GetAuc(ConfigList, testData$rnk, testData$ClassLabel, FALSE) 
    TestAUCList[foldVar,2]=CalcTestACC(testData)
    TestAUCList[foldVar,3]=GetSPC(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,4]=GetSEN(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,5]=sqrt(TestAUCList[foldVar,3] * TestAUCList[foldVar,4])
  }
  else if (ConfigList$KCalcWeight == 2)
  {
    #a=testData
    #b=FtrCtgList
    testData = DescritizeDataset(ConfigList, testData, FtrCtgList)
    
    i = 1
    if (NROW(ExtraFields) > 0)
      for (i in 1:NROW(ExtraFields))
      {
        FName = ExtraFields[i,"FName"]
        testData[,FName] = 0
        testData[which(testData[,ExtraFields[i,"FId1"]] == ExtraFields[i,"CId1"] 
                       & testData[,ExtraFields[i,"FId2"]] == ExtraFields[i,"CId2"]), FName] = 1
      }
    
    col=NCOL(testData) 
    testData[,"rnk"]=0
    i=1
    for (i in 1:NROW(testData))
    { 
      totalScore=0
      totalWeight=0
      FId=3
      for (FId in 3:col)      
      {
        if (! is.na(testData[i,FId])){
          FV = testData[i,FId]  ####mistake
          score = CalcScoreSample(FtrCtgList,FId,FV, ConfigList$spCounts, sp) 
          totalScore = totalScore + AllWList[FId,2] * score
          totalWeight = totalWeight + AllWList[FId,2]
        }
      }
      testData[i,"rnk"] = totalScore / totalWeight
    }
    TestAUCList[foldVar,1]=GetAuc(ConfigList, testData$rnk, testData$ClassLabel, FALSE) 
    TestAUCList[foldVar,2]=CalcTestACC(testData)
    TestAUCList[foldVar,3]=GetSPC(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,4]=GetSEN(ConfigList, testData$rnk, testData$ClassLabel) 
    TestAUCList[foldVar,5]=sqrt(TestAUCList[foldVar,3] * TestAUCList[foldVar,4])
  }
  
  return(TestAUCList)
}

# CalcTestAUC=function(testData)
# {
#   PosCont = NROW(subset(testData, testData$ClassLabel == 1))
#   NegCont = NROW(subset(testData, testData$ClassLabel == 0))
# 
#   if ((PosCont != 0) & (NegCont != 0))
#   {
#     pred <- prediction(testData$rnk , testData$ClassLabel)
#     perf <- performance(pred,"auc")
#     wj=perf@y.values[[1]]
#   } else
#     wj=1
# 
#   return(wj)
# }


GetExtraFeature=function(ConfigList, FtrCtgList, trainData, foldVar, sp)
{
  #a=trainData1
  #trainData=a
  ExtraFields = GetExtraFields()
  if (ConfigList$KAddExtraField == 1)
  {
    if (sp != 0)
      FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == sp)  
    else
      FtrCtgListS = FtrCtgList
    
    CF1 = 1
    for (CF1 in 1:NROW(FtrCtgListS))
    {
      FIDF1 = FtrCtgListS[CF1,"FtrID"]
      CIDF1 = FtrCtgListS[CF1,"CtgID"]
      CF2 = 5
      for (CF2 in CF1:NROW(FtrCtgListS))
      {
        FIDF2 = FtrCtgListS[CF2,"FtrID"]
        CIDF2 = FtrCtgListS[CF2,"CtgID"]
        if (FIDF1 != FIDF2)
        {
          IntrsRow = length(which(trainData[,FIDF1] == CIDF1 & trainData[,FIDF2] == CIDF2))
          if (IntrsRow > 0)
          {
            #print(IntrsRow)
             FName = paste("F", FIDF1, "C", CIDF1, "_", "F", FIDF2, "C", CIDF2, sep = "")
             trainData[,FName] = 0
             trainData[which(trainData[,FIDF1] == CIDF1 & trainData[,FIDF2] == CIDF2), FName] = 1
             
             ExtraFields[nrow(ExtraFields) + 1,"FId"] = nrow(ExtraFields) + 1
             ExtraFields[nrow(ExtraFields),"FName"] = FName
             ExtraFields[nrow(ExtraFields),"sp"] = sp
             ExtraFields[nrow(ExtraFields),"FId1"] = FIDF1
             ExtraFields[nrow(ExtraFields),"FId2"] = FIDF2
             ExtraFields[nrow(ExtraFields),"CId1"] = CIDF1
             ExtraFields[nrow(ExtraFields),"CId2"] = CIDF2
             ExtraFields[nrow(ExtraFields),"Cnt0"] = length(which(trainData[,FName] == 0))
             ExtraFields[nrow(ExtraFields),"Cnt1"] = length(which(trainData[,FName] == 1))
             ExtraFields[nrow(ExtraFields),"auc"] = GetAuc(ConfigList, trainData[,FName], trainData[,"ClassLabel"], TRUE)
             ExtraFields[nrow(ExtraFields),"Cnt0P"] = length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1))
             ExtraFields[nrow(ExtraFields),"Cnt0N"] = length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 0))
             ExtraFields[nrow(ExtraFields),"Cnt1P"] = length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 1))
             ExtraFields[nrow(ExtraFields),"Cnt1N"] = length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0))
             if ((length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1)) != 0)
                 & (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0)) != 0) )
             {
               ExtraFields[nrow(ExtraFields),"R"] = 
                 (
                 (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 1)) 
               * length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 0)))
               / (
                (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0))
                 * length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1)))
               ))
             }
          }
        }
      }
    }
    ExtraFields = head(ExtraFields[order(ExtraFields$R, decreasing=TRUE), ], ConfigList$CntTopExtraField)
    rownames(ExtraFields) <- NULL
    ExtraFields$FId <- seq.int(nrow(ExtraFields))
  }
  else if (ConfigList$KAddExtraField == 2)
  {
    if (sp != 0)
      FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == sp)  
    else
      FtrCtgListS = FtrCtgList
    
    i = 1
    for (i in 1:ncol(trainData))
    {
      for (j in i:ncol(trainData))
      {
        if (i != j)
        {
          IntrsRow = length(which(trainData[,FIDF1] == CIDF1 & trainData[,FIDF2] == CIDF2))
          if (IntrsRow > 0)
          {
            #print(IntrsRow)
            FName = paste("F", i, "_", "F", j, sep = "")
            trainData[,FName] = 0
            trainData[which(trainData[,FIDF1] == CIDF1 & trainData[,FIDF2] == CIDF2), FName] = 1
            
            ExtraFields[nrow(ExtraFields) + 1,"FId"] = nrow(ExtraFields) + 1
            ExtraFields[nrow(ExtraFields),"FName"] = FName
            ExtraFields[nrow(ExtraFields),"sp"] = sp
            ExtraFields[nrow(ExtraFields),"FId1"] = FIDF1
            ExtraFields[nrow(ExtraFields),"FId2"] = FIDF2
            ExtraFields[nrow(ExtraFields),"CId1"] = CIDF1
            ExtraFields[nrow(ExtraFields),"CId2"] = CIDF2
            ExtraFields[nrow(ExtraFields),"Cnt0"] = length(which(trainData[,FName] == 0))
            ExtraFields[nrow(ExtraFields),"Cnt1"] = length(which(trainData[,FName] == 1))
            ExtraFields[nrow(ExtraFields),"auc"] = GetAuc(ConfigList, trainData[,FName], trainData[,"ClassLabel"], TRUE)
            ExtraFields[nrow(ExtraFields),"Cnt0P"] = length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1))
            ExtraFields[nrow(ExtraFields),"Cnt0N"] = length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 0))
            ExtraFields[nrow(ExtraFields),"Cnt1P"] = length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 1))
            ExtraFields[nrow(ExtraFields),"Cnt1N"] = length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0))
            if ((length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1)) != 0)
                & (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0)) != 0) )
            {
              ExtraFields[nrow(ExtraFields),"R"] = 
                (
                  (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 1)) 
                   * length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 0)))
                  / (
                    (length(which(trainData[,FName] == 1 & trainData[,"ClassLabel"] == 0))
                     * length(which(trainData[,FName] == 0 & trainData[,"ClassLabel"] == 1)))
                  ))
            }
          }
        }
      }
    }
    ExtraFields = head(ExtraFields[order(ExtraFields$R, decreasing=TRUE), ], ConfigList$CntTopExtraField)
    rownames(ExtraFields) <- NULL
    ExtraFields$FId <- seq.int(nrow(ExtraFields))
  }
  return(ExtraFields)
}

AddExtraFeatureCategoryList=function(ConfigList, FtrCtgList, ExtraFields, foldVar, sp)
{
  if (NROW(ExtraFields) > 0)
    if (ConfigList$KAddExtraField == 1)
    {
      FCnt = max(FtrCtgList$FtrID)
      i=1
      for (i in 1:NROW(ExtraFields))
      {
        FCnt = FCnt + 1
        FtrCtgList[nrow(FtrCtgList)+1,"FtrID"] = FCnt
        
        FtrCtgList[nrow(FtrCtgList),"CtgID"] = 1
        FtrCtgList[nrow(FtrCtgList),"SP"] = ExtraFields[i,"sp"]
        FtrCtgList[nrow(FtrCtgList),"Lb"] = 0
        FtrCtgList[nrow(FtrCtgList),"Ub"] = 0
        FtrCtgList[nrow(FtrCtgList),"Pi"] = ExtraFields[i,"Cnt0P"]
        FtrCtgList[nrow(FtrCtgList),"Ni"] = ExtraFields[i,"Cnt0N"]
        if (ConfigList$st == 53)
          FtrCtgList[nrow(FtrCtgList),"ri"] = 0
        else
          FtrCtgList[nrow(FtrCtgList),"ri"] = ExtraFields[i,"Cnt0P"] / (ExtraFields[i,"Cnt0P"] + ExtraFields[i,"Cnt0N"])
        
        FtrCtgList[nrow(FtrCtgList)+1,"FtrID"] = FCnt
        FtrCtgList[nrow(FtrCtgList),"SP"] = ExtraFields[i,"sp"]
        FtrCtgList[nrow(FtrCtgList),"CtgID"] = 2
        FtrCtgList[nrow(FtrCtgList),"Lb"] = 1
        FtrCtgList[nrow(FtrCtgList),"Ub"] = 1
        FtrCtgList[nrow(FtrCtgList),"Pi"] = ExtraFields[i,"Cnt1P"]
        FtrCtgList[nrow(FtrCtgList),"Ni"] = ExtraFields[i,"Cnt1N"]
        FtrCtgList[nrow(FtrCtgList),"ri"] = ExtraFields[i,"Cnt1P"] / (ExtraFields[i,"Cnt1P"] + ExtraFields[i,"Cnt1N"])
      }
    }
  return(FtrCtgList)
}

NormalizeCategoryList=function(ConfigList, FtrCtgList, foldVar, sp)
{
  #sp = 0 OR 1-10
  if (ConfigList$IsNormalizeCOunt)
  {
    if (sp != 0)
      FtrCtgListS = subset(FtrCtgList, FtrCtgList$SP == sp)  
    else
      FtrCtgListS = FtrCtgList
    
    i=1
    for (i in 1:NROW(FtrCtgListS)){
      Fid = FtrCtgListS[i,"FtrID"]
      Cid = FtrCtgListS[i,"CtgID"]
      PCnt = FtrCtgListS[i,"Pi"]
      NCnt = FtrCtgListS[i,"Ni"]
      FtrCtgListSF = subset(FtrCtgListS, FtrCtgListS$SP == sp & FtrCtgListS$FtrID == Fid)  
      PCntTot = sum(FtrCtgListSF$Pi)
      NCntTot = sum(FtrCtgListSF$Ni)
      if (PCntTot > NCntTot)
      {
        PiNew = NCntTot * PCnt / PCntTot
        FtrCtgList$Pi[FtrCtgList$SP == sp & FtrCtgList$FtrID == Fid & FtrCtgList$CtgID == Cid] <- PiNew
        FtrCtgList$ri[FtrCtgList$SP == sp & FtrCtgList$FtrID == Fid & FtrCtgList$CtgID == Cid] <- PiNew / (PiNew + NCnt)
      }
      else if (PCntTot < NCntTot)
      {
        NiNew = round(PCntTot * NCnt / NCntTot)
        FtrCtgList$Ni[FtrCtgList$SP == sp & FtrCtgList$FtrID == Fid & FtrCtgList$CtgID == Cid] <- NiNew
        FtrCtgList$ri[FtrCtgList$SP == sp & FtrCtgList$FtrID == Fid & FtrCtgList$CtgID == Cid] <- PCnt / (PCnt + NiNew)
      } 
    }
  }      
  return(FtrCtgList)
}

PrintCtgListToFile=function(ConfigList, FtrCtgList, foldVar, sp)
{
  #sp = 0 OR 1-10
  
  if (ConfigList$IsPrintCtgToFile)
  {
    NameVer = GetNameVer(ConfigList, foldVar, sp)
    tmp = paste("D:/RProjects/OptTask/Output/", 
                ConfigList$DBName, "/", 
                ConfigList$DBName, "_Ctgry_", 
                NameVer ,
                sep = "")
    tmp = paste(tmp,  ".csv", sep = "")
    
    if (ConfigList$spCounts != 0)
      FtrCtgList = subset(FtrCtgList, FtrCtgList$SP == sp)
    
    ind <- FtrCtgList$Lb == -Inf     
    FtrCtgList[ind, "Lb"] <- -9999   
    
    FtrCtgList = FtrCtgList[with(FtrCtgList, order(SP, FtrID, Lb)), ]
    write.csv(FtrCtgList, tmp)
  }
}

PrintWListToFile=function(ConfigList, WList, foldVar, sp)
{
  if (ConfigList$IsPrintWToFile){
    
    NameVer = GetNameVer(ConfigList, foldVar, sp)
    
    tmp = paste("D:/RProjects/OptTask/Output/",  
                ConfigList$DBName, "/", 
                ConfigList$DBName, "_WList_", 
                NameVer ,
                sep = "")
    
    tmp = paste(tmp,  ".csv", sep = "")
    write.csv(WList, tmp)
  }
}

PrintSumWeightToFile=function(ConfigList, SampleWeight)
{
 
  if (ConfigList$IsPrintWToFile){
    
    NameVer = GetNameVer(ConfigList, 0, 0)
    
    tmp = paste("D:/RProjects/OptTask/Output/",  
                ConfigList$DBName, "/", 
                ConfigList$DBName, "_SumW_", 
                NameVer,
                sep = "")
    
    tmp = paste(tmp,  ".csv", sep = "")
    write.csv(SampleWeight, tmp)
  }
}

SaveResultToFile=function(ConfigList, TestAUCList, old)
{
  new <- Sys.time() - old # calculate difference
  
  mAUC = round(mean(TestAUCList[,1]),5)
  mACC = round(mean(TestAUCList[,2]),5)
  mSPC = round(mean(TestAUCList[,3]),5)
  mSEN = round(mean(TestAUCList[,4]),5)
  mSENSPC = round(mean(TestAUCList[,5]),5)
  
  NameVer = GetNameVer(ConfigList, 0, 0)
  
  tmp1 = paste(
    NameVer,
    " AUC=#",toString(mAUC),
    "#ACC=#",toString(mACC),
    "#SPC=#",toString(mSPC),
    "#SEN=#",toString(mSEN),
    "#SENSPC=#",toString(mSENSPC),
    "#Time=",Sys.time(),
    " RunTime=", round(as.numeric(new,units="mins"),5),
    sep = "")
  
  message(tmp1)
  cat(tmp1, file="D:/RProjects/OptTask/Results/All_Res_file.txt", append=TRUE, sep = "\n")
  tmp2 = paste("D:/RProjects/OptTask/Results/ByName/", ConfigList$DBName, "_Res_file.txt", sep = "")   
  cat(tmp1, file=tmp2, append=TRUE, sep = "\n")
  tmp3 = paste("D:/RProjects/OptTask/Results/ByMethod/Method_", ConfigList$st, "_Res_file.txt", sep = "")   
  cat(tmp1, file=tmp3, append=TRUE, sep = "\n")
  print(tmp1)
}


PrintLine=function(ConfigList, st)
{
  ConfigList = SetState(ConfigList, st, "aaaaaaaaaaa")
  new <- Sys.time() 
  NameVer = GetNameVer(ConfigList, 0, 0)
  tmp1 = paste(" Time= ",Sys.time()," ================================================= ", sep = "")
   
  tmp3 = paste("D:/RProjects/OptTask/Results/ByMethod/Method_", ConfigList$st, "_Res_file.txt", sep = "")   
  cat(tmp1, file=tmp3, append=TRUE, sep = "\n")
  print(tmp1)
}

SourceAlgorithms=function()
{
  source('D:/RProjects/OptTask/Sources/Haberman.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/australian.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Bupa.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/BupaTest.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Heart.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Hypothyroid.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Mammographic.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Pima.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/SickEuthyroid.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/SPECTF.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Wisconsin.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Chronic.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Climate.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Diabetic.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Ionosphere.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Indian.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Blood.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ROSE01.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/abalone19.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/abalone9_18.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/vehicle0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/vehicle1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/vehicle2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/vehicle3.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Pima2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Iris0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ecoli_0_vs_1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ecoli1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ecoli2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ecoli3.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/ecoli4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Ecoli0137vs26.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass5.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass6.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Glass0123_456.R', echo=FALSE) 
  source('D:/RProjects/OptTask/Sources/Glass016vs2.R', echo=FALSE) 
  source('D:/RProjects/OptTask/Sources/Glass016vs5.R', echo=FALSE) 
  source('D:/RProjects/OptTask/Sources/Wisconsin2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/vowel0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/yeast1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/yeast3.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/yeast2_vs_4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/yeast05679_vs_4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast1vs7.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast1458vs7.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast2vs8.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast5.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast6.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Yeast1289vs7.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/shuttle0vs4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Shuttle2vs4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Haberman2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/segment0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/new_thyroid1.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/new_thyroid2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/page_blocks0.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/page_blocks13_vs_4.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/page_blocks13_vs_2.R', echo=FALSE)  
  source('D:/RProjects/OptTask/Sources/Seismicbumps.R', echo=FALSE)  
}

DoAllAlgorithms=function()
{
  PrintLine(ConfigList,st)
  1
  mAUC = Wisconsin(ConfigList, st)
  mAUC = Ionosphere(ConfigList,st)
  mAUC = Glass1(ConfigList, st)
  mAUC = ecoli_0_vs_1(ConfigList, st)
  mAUC = Wisconsin2(ConfigList, st)
  mAUC = Pima2(ConfigList, st)
  mAUC = Iris0(ConfigList, st)
  mAUC = Glass0(ConfigList, st)
  mAUC = yeast1(ConfigList, st)
  mAUC = Indian(ConfigList,st)
  2
  mAUC = Haberman2(ConfigList, st)
  mAUC = vehicle2(ConfigList, st)
  mAUC = vehicle1(ConfigList, st)
  mAUC = vehicle3(ConfigList, st)
  mAUC = Glass0123_456(ConfigList, st)
  mAUC = Blood(ConfigList,st)
  mAUC = vehicle0(ConfigList, st)
  mAUC = ecoli1(ConfigList, st)
  mAUC = SPECTF(ConfigList,st)
  mAUC = new_thyroid2(ConfigList, st)
  3
  #mAUC = new_thyroid1(ConfigList, st)
  mAUC = ecoli2(ConfigList, st)
  mAUC = segment0(ConfigList, st)
  mAUC = Glass6(ConfigList, st)
  mAUC = yeast3(ConfigList, st)
  mAUC = ecoli3(ConfigList, st)
  mAUC = page_blocks0(ConfigList, st)
  mAUC = yeast2_vs_4(ConfigList, st)
  mAUC = yeast05679_vs_4(ConfigList, st)
  mAUC = vowel0(ConfigList, st)
  4
  mAUC = Glass016vs2(ConfigList, st)
  mAUC = Climate(ConfigList,st)
  mAUC = Glass2(ConfigList, st)
  mAUC = Shuttle0vs4(ConfigList, st)
  mAUC = Yeast1vs7(ConfigList, st)
  mAUC = Seismicbumps(ConfigList, st)
  #mAUC = Glass4(ConfigList, st)
  mAUC = ecoli4(ConfigList, st)
  mAUC = page_blocks13_vs_4(ConfigList, st)
  mAUC = abalone9_18(ConfigList, st)
  5
  #mAUC = Glass016vs5(ConfigList, st)
  mAUC = Shuttle2vs4(ConfigList, st)
  mAUC = Yeast1458vs7(ConfigList, st)
  mAUC = Glass5(ConfigList, st)
  mAUC = Yeast2vs8(ConfigList, st)
  mAUC = Yeast4(ConfigList, st)
  mAUC = Yeast1289vs7(ConfigList, st)
  mAUC = Yeast5(ConfigList, st)
  mAUC = Ecoli0137vs26(ConfigList, st)
  mAUC = Yeast6(ConfigList, st)
  6
  mAUC = ROSE01(ConfigList, st)
  mAUC = abalone19(ConfigList, st)
  
}

