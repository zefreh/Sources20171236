

Seismicbumps  = function(ConfigList, st){
  ConfigList = SetState(ConfigList, st, "Smc")
  set.seed(0)
  
  old <- Sys.time() # get start time
  #  Read and Correct data
  data = read.table("D:/RProjects/OptTask/Data/Seismicbumps1.txt", 
                    sep=",", 
                    fill=FALSE, 
                    strip.white=TRUE)
  data$Sequence <- seq.int(nrow(data))
  data$ClassLabel <- data$V19
  
  data = data[,c(21,20,1:13,17,18)]
  ConfigList$CategryFields="3,4,5,10,11,12,13,14,15"
  
  
  data$V1 <- as.integer(data$V1)
  data$V2 <- as.integer(data$V2)
  data$V3 <- as.integer(data$V3)
  data$V8 <- as.integer(data$V8)

  ### Start
  
  label = data$ClassLabel
  #positiveClass = 1
  #crossValidCounts=10
  folds = createFolds(label,k=10)
  TestAUCList = InitializeAlg(ConfigList, data)
  AllWList = GetAllWList()
  
  foldVar = 1
  for (foldVar in 1:10){
    ConfigList$fold = foldVar
    StartTime <- Sys.time() # get start time
    FtrCtgList = GetFtrCtgList()
    
    SampleWeight <- data.frame(FoldID = integer(), SID = integer(), SumW = numeric())
    SampleWeight <- SampleWeight[0,]
    
    testData   = data[folds[[foldVar]],]
    trainData0  = data[-folds[[foldVar]],]
    
    TotalRecord = nrow(trainData0) 
    SampleList = MakeSampleList(ConfigList, TotalRecord)
    sp = 1
    
    for (sp in 1:ConfigList$spCounts){
      ConfigList$sp = sp
      
      SPList = GetSamlpeList(SampleList, sp)
      trainData = trainData0[SPList, ]
      
      trainData = BalanceSamples(ConfigList, trainData, foldVar, sp)
      
      trainData = trainData[with(trainData, order(Sequence)), ]
      
      #######

      trainData[,"LowB"] = trainData[,3]
      trainData[,"UprB"] = trainData[,3]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 3, sp)
      
      trainData[,"LowB"] = trainData[,4]
      trainData[,"UprB"] = trainData[,4]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 4, sp)
      
      trainData[,"LowB"] = trainData[,5]
      trainData[,"UprB"] = trainData[,5]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 5, sp)
      
      trainData = DescritizeFeature(trainData, 6)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 6, sp)

      trainData = DescritizeFeature(trainData, 7)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 7, sp)
      
      trainData = DescritizeFeature(trainData, 8)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 8, sp)
      
      trainData = DescritizeFeature(trainData, 9)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 9, sp)
      
      trainData[,"LowB"] = trainData[,10]
      trainData[,"UprB"] = trainData[,10]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 10, sp)
      
      trainData[,"LowB"] = trainData[,11]
      trainData[,"UprB"] = trainData[,11]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 11, sp)
      
      trainData[,"LowB"] = trainData[,12]
      trainData[,"UprB"] = trainData[,12]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 12, sp)
      
      trainData[,"LowB"] = trainData[,13]
      trainData[,"UprB"] = trainData[,13]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 13, sp)
      
      trainData[,"LowB"] = trainData[,14]
      trainData[,"UprB"] = trainData[,14]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 14, sp)
      
      trainData[,"LowB"] = trainData[,15]
      trainData[,"UprB"] = trainData[,15]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 15, sp)
      
      trainData = DescritizeFeature(trainData, 16)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 16, sp)
      
      trainData = DescritizeFeature(trainData, 17)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 17, sp)
      
      ##################1
      ExtraFields = GetExtraFeature(ConfigList, FtrCtgList, trainData, foldVar, sp)
      FtrCtgList = AddExtraFeatureCategoryList(ConfigList, FtrCtgList, ExtraFields, foldVar, sp)
      
      PrintCtgListToFile(ConfigList, FtrCtgList, foldVar, sp)
      FtrCtgList = NormalizeCategoryList(ConfigList, FtrCtgList, foldVar, sp)
      WList = CalcFeatureWeightsByCategory(ConfigList, trainData, ExtraFields, FtrCtgList, foldVar, sp)
      SampleWeight[nrow(SampleWeight)+1,] = c(foldVar, sp, sum(WList$auc))
      ShowProcessAlg(ConfigList, TestAUCList, foldVar, sp, StartTime)
      
      if (ConfigList$spCounts != 1){
        trainData2 = DescritizeDataset(ConfigList, trainData0, FtrCtgList, sp)
        WList = CalcOrigWeights(ConfigList, trainData2, FtrCtgList, WList, foldVar, sp)
        AllWList = rbind(AllWList, WList)
      }
      else
        AllWList = WList
      
    }  # sp
    
    if (ConfigList$spCounts != 1){
      FtrCtgList = RemoveExtraSamples(ConfigList, FtrCtgList, SampleWeight)
      WList = CalcFeatureWeightsByValue(ConfigList, trainData0, FtrCtgList, foldVar)
      AllWList = rbind(AllWList, WList)
    }
    
    TestAUCList = CalcTestRank(ConfigList, FtrCtgList, AllWList, testData, ExtraFields, TestAUCList, foldVar)
    ShowProcessAlg(ConfigList, TestAUCList, foldVar, 0, StartTime)
  }   # fold
  PrintSumWeightToFile(ConfigList, SampleWeight)
  SaveResultToFile(ConfigList, TestAUCList, old)
  return(0)
}