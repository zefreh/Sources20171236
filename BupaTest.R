#     Bupa(3)

BupaTest = function(ConfigList, st){
  ConfigList = SetState(ConfigList, st, "BupaTest") 

  set.seed(0)
  old <- Sys.time() # get start time
  #  Read and Correct data
  data = read.table("D:/RProjects/OptTask/Data/BupaTest.txt", 
                    sep=",", 
                    fill=FALSE, 
                    strip.white=TRUE)
  data$Sequence <- seq.int(nrow(data))
  data = data[,c(7,8,1:6)]
  ConfigList$CategryFields=""
  colnames(data)[1] <- "ClassLabel"
  
  ind <- data$ClassLabel == 1     
  data[ind, 1] <- 0   
  ind <- data$ClassLabel == 2     
  data[ind, 1] <- 1   
  
####
  write.csv(data, "D:/RProjects/BupaTest/BupaTest.csv")
  F1Data <- data.frame(
    F1 = numeric(),
    L1 = numeric(), 
    sp = numeric())
  F1Data <- F1Data[0,]
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
      
      tmp = paste("D:/RProjects/BupaTest/trainDataOrg1_", sp, ".csv", sep = "")   
      write.csv(trainData, tmp)
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