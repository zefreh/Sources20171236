#    Ionosphere()

Ionosphere  = function(ConfigList, st){
  ConfigList = SetState(ConfigList, st, "Ins")
  set.seed(0)
  old <- Sys.time() # get start time
  #  Read and Correct data
  data = read.table("D:/RProjects/OptTask/Data/Ionosphere.txt", 
                    sep=",", 
                    fill=FALSE, 
                    strip.white=TRUE)
  data$Sequence <- seq.int(nrow(data))
  data$ClassLabel <- -1
  ind <- data$V35 %in% "b"     
  data[ind, "ClassLabel"] <- 0   
  ind <- data$V35 %in% "g"     
  data[ind, "ClassLabel"] <- 1   
  
  data = data[,c(37,36,1:34)]
  ConfigList$CategryFields=""
  
  ### Start
  
  label = data$ClassLabel
  #positiveClass = 1
  #crossValidCounts=10
  folds = createFolds(label,k=10)
  TestAUCList = InitializeAlg(ConfigList, data)
  AllWList = GetAllWList()
  
  foldVar = 10
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
      
      trainData = DescritizeFeature(trainData, 4)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 4, sp)
      
      trainData = DescritizeFeature(trainData, 5)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 5, sp)
      
      trainData = DescritizeFeature(trainData, 6)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 6, sp)
      
      trainData = DescritizeFeature(trainData, 7)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 7, sp)
      
      trainData = DescritizeFeature(trainData, 8)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 8, sp)
      
      trainData = DescritizeFeature(trainData, 9)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 9, sp)
      
      trainData = DescritizeFeature(trainData, 10)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 10, sp)
      
      trainData = DescritizeFeature(trainData, 11)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 11, sp)
      
      trainData = DescritizeFeature(trainData, 12)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 12, sp)
      
      trainData = DescritizeFeature(trainData, 13)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 13, sp)
      
      trainData = DescritizeFeature(trainData, 14)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 14, sp)
      
      trainData = DescritizeFeature(trainData, 15)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 15, sp)
      
      trainData = DescritizeFeature(trainData, 16)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 16, sp)
      
      trainData = DescritizeFeature(trainData, 17)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 17, sp)
      
      trainData = DescritizeFeature(trainData, 18)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 18, sp)
      
      trainData = DescritizeFeature(trainData, 19)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 19, sp)
      
      trainData = DescritizeFeature(trainData, 20)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 20, sp)
      
      trainData = DescritizeFeature(trainData, 21)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 21, sp)
      
      trainData = DescritizeFeature(trainData, 22)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 22, sp)
      
      trainData = DescritizeFeature(trainData, 23)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 23, sp)
      
      trainData = DescritizeFeature(trainData, 24)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 24, sp)
      
      trainData = DescritizeFeature(trainData, 25)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 25, sp)
      
      trainData = DescritizeFeature(trainData, 26)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 26, sp)
      
      trainData = DescritizeFeature(trainData, 27)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 27, sp)
      
      trainData = DescritizeFeature(trainData, 28)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 28, sp)
      
      trainData = DescritizeFeature(trainData, 29)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 29, sp)
      
      trainData = DescritizeFeature(trainData, 30)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 30, sp)
      
      trainData = DescritizeFeature(trainData, 31)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 31, sp)
      
      trainData = DescritizeFeature(trainData, 32)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 32, sp)
      
      trainData = DescritizeFeature(trainData, 33)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 33, sp)
      
      trainData = DescritizeFeature(trainData, 34)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 34, sp)
      
      trainData = DescritizeFeature(trainData, 35)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 35, sp)
      
      trainData = DescritizeFeature(trainData, 36)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 36, sp)
      
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