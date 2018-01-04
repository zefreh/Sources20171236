#   Wisconsin(3)

Adult  = function(ConfigList){
  ConfigList[1,"DBName"] = "Adt"
  set.seed(0)
  
  old <- Sys.time() # get start time
  #  Read and Correct data
  data = read.table("D:/RProjects/OptTask/Data/Adult.txt", 
                    sep=",", 
                    fill=FALSE, 
                    strip.white=TRUE)
  data$Sequence <- seq.int(nrow(data))

  data$ClassLabel <- as.integer(data$V15) - 1

  data = data[,c(17,16,1:14)]
  ConfigList$CategryFields="4,6,8,9,10,11,12,16"
  
  data$V2 <- as.integer(data$V2)
  data$V4 <- as.integer(data$V4)
  data$V6 <- as.integer(data$V6)
  data$V7 <- as.integer(data$V7)
  data$V8 <- as.integer(data$V8)
  data$V9 <- as.integer(data$V9)
  data$V10 <- as.integer(data$V10)
  data$V14 <- as.integer(data$V14)
  
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

      trainData = DescritizeFeature(trainData, 3)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 3, sp)

      trainData[,"LowB"] = trainData[,4]
      trainData[,"UprB"] = trainData[,4]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 4, sp)

      trainData = DescritizeFeature(trainData, 5)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 5, sp)

      trainData[,"LowB"] = trainData[,6]
      trainData[,"UprB"] = trainData[,6]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 6, sp)
      
      trainData = DescritizeFeature(trainData, 7)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 7, sp)

      trainData[,"LowB"] = trainData[,8]
      trainData[,"UprB"] = trainData[,8]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 8, sp)
      
      trainData[,"LowB"] = trainData[,9]
      trainData[,"UprB"] = trainData[,9]
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
      
      trainData = DescritizeFeature(trainData, 13)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 13, sp)
      
      trainData = DescritizeFeature(trainData, 14)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 14, sp)
      
      trainData = DescritizeFeature(trainData, 15)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 15, sp)
      
      trainData[,"LowB"] = trainData[,16]
      trainData[,"UprB"] = trainData[,16]
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 16, sp)
      
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