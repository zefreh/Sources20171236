#   ROSE01
# 
# install.packages("ROSE")
# library(ROSE)
# data(hacide)
# tmp = "D:/RProjects/OptTask/Data/ROSE01.txt"
# write.csv(rbind(hacide.train, hacide.test), tmp)

ROSE01 = function(ConfigList, st){
  ConfigList = SetState(ConfigList, st, "Rse")
  set.seed(0)
  
  old <- Sys.time() # get start time
  #  Read and Correct data
  data = read.table("D:/RProjects/OptTask/Data/ROSE01.txt", 
                    sep=",", 
                    fill=FALSE, 
                    strip.white=TRUE)
  colnames(data)[1] <- "Sequence"
  colnames(data)[2] <- "ClassLabel"
  data = data[,c(2,1,3,4)]
  ConfigList$CategryFields=""
  
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

      trainData = DescritizeFeature(trainData, 4)
      FtrCtgList = GetAllCategoryList(ConfigList, FtrCtgList, trainData, 4, sp)

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