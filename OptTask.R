
#library(xlsx)

library("caret")
library("ROCR")

source('D:/RProjects/OptTask/Sources/FuncMAD2C.R', echo=FALSE)
SourceAlgorithms()
st=0

DoAllAlgorithms()

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

#####################


mAUC = page_blocks13_vs_2(ConfigList, 9)
mAUC = australian(ConfigList)
mAUC = Bupa(ConfigList)
mAUC = Heart(ConfigList)
mAUC = Mammographic(ConfigList)
mAUC = Pima(ConfigList)
mAUC = Diabetic(ConfigList)
mAUC = Haberman(ConfigList,9)
mAUC = Haberman2(ConfigList,10)
mAUC = Hypothyroid(ConfigList)
mAUC = SickEuthyroid(ConfigList)
mAUC = Chronic(ConfigList)

mAUC = Seismicbumps(ConfigList, 9)
mAUC = Seismicbumps(ConfigList, 10)

mAUC = Shuttle0vs4(ConfigList, 10)

mAUC = Shuttle2vs4(ConfigList, 9)
mAUC = Shuttle2vs4(ConfigList, 10)

mAUC = ROSE01(ConfigList, 9)
mAUC = ROSE01(ConfigList, 10)

mAUC = page_blocks0(ConfigList, 9)
mAUC = page_blocks0(ConfigList, 10)

mAUC = page_blocks13_vs_4(ConfigList, 9)
mAUC = page_blocks13_vs_4(ConfigList, 10)

mAUC = page_blocks13_vs_2(ConfigList, 9)
mAUC = page_blocks13_vs_2(ConfigList, 10)

mAUC = new_thyroid1(ConfigList, 9)
mAUC = new_thyroid1(ConfigList, 10)

mAUC = new_thyroid2(ConfigList, 9)
mAUC = new_thyroid2(ConfigList, 10)

mAUC = segment0(ConfigList, 9)
mAUC = segment0(ConfigList, 10)

mAUC = ecoli1(ConfigList, 9)
mAUC = ecoli1(ConfigList, 10)

mAUC = ecoli2(ConfigList, 9)
mAUC = ecoli2(ConfigList, 10)

mAUC = ecoli3(ConfigList, 9)
mAUC = ecoli3(ConfigList, 10)

mAUC = ecoli4(ConfigList, 9)
mAUC = ecoli4(ConfigList, 10)

mAUC = Ecoli0137vs26(ConfigList, 9)
mAUC = Ecoli0137vs26(ConfigList, 10)

mAUC = Haberman2(ConfigList, 9)
mAUC = Haberman2(ConfigList, 10)

mAUC = yeast1(ConfigList, 9)
mAUC = yeast1(ConfigList, 10)

mAUC = yeast3(ConfigList, 9)
mAUC = yeast3(ConfigList, 10)

mAUC = Yeast4(ConfigList, 9)
mAUC = Yeast4(ConfigList, 10)

mAUC = Yeast5(ConfigList, 9)
mAUC = Yeast5(ConfigList, 10)

mAUC = Yeast6(ConfigList, 9)
mAUC = Yeast6(ConfigList, 10)

mAUC = yeast2_vs_4(ConfigList, 9)
mAUC = yeast2_vs_4(ConfigList, 10)

mAUC = yeast05679_vs_4(ConfigList, 9)
mAUC = yeast05679_vs_4(ConfigList, 10)

mAUC = Yeast1vs7(ConfigList, 9)
mAUC = Yeast1vs7(ConfigList, 10)

mAUC = Yeast1458vs7(ConfigList, 9)
mAUC = Yeast1458vs7(ConfigList, 10)

mAUC = Yeast2vs8(ConfigList, 9)
mAUC = Yeast2vs8(ConfigList, 10)

mAUC = Yeast1289vs7(ConfigList, 9)
mAUC = Yeast1289vs7(ConfigList, 10)

mAUC = vowel0(ConfigList, 9)
mAUC = vowel0(ConfigList, 10)

mAUC = Wisconsin2(ConfigList, 9)
mAUC = Wisconsin2(ConfigList, 10)

mAUC = Glass0(ConfigList, 9)
mAUC = Glass0(ConfigList, 10)

mAUC = Glass1(ConfigList, 9)
mAUC = Glass1(ConfigList, 10)

mAUC = Glass2(ConfigList, 9)
mAUC = Glass2(ConfigList, 10)

mAUC = Glass3(ConfigList, 9)
mAUC = Glass3(ConfigList, 10)

mAUC = Glass4(ConfigList, 9)
mAUC = Glass4(ConfigList, 10)

mAUC = Glass5(ConfigList, 9)
mAUC = Glass5(ConfigList, 10)

mAUC = Glass6(ConfigList, 9)
mAUC = Glass6(ConfigList, 10)

mAUC = Glass0123_456(ConfigList, 9)
mAUC = Glass0123_456(ConfigList, 10)

mAUC = Glass016vs2(ConfigList, 9)
mAUC = Glass016vs2(ConfigList, 10)

mAUC = Glass016vs5(ConfigList, 9)
mAUC = Glass016vs5(ConfigList, 10)

mAUC = Pima2(ConfigList, 9)
mAUC = Pima2(ConfigList, 10)

mAUC = Iris0(ConfigList, 9)
mAUC = Iris0(ConfigList, 10)

mAUC = ecoli_0_vs_1(ConfigList, 9)
mAUC = ecoli_0_vs_1(ConfigList, 10)

mAUC = Wisconsin2(ConfigList, 9)
mAUC = Wisconsin2(ConfigList, 10)

mAUC = vehicle0(ConfigList, 9)
mAUC = vehicle0(ConfigList, 10)

mAUC = vehicle1(ConfigList, 9)
mAUC = vehicle1(ConfigList, 10)

mAUC = vehicle2(ConfigList, 9)
mAUC = vehicle2(ConfigList, 10)

mAUC = vehicle3(ConfigList, 9)
mAUC = vehicle3(ConfigList, 10)

mAUC = abalone19(ConfigList, st)

mAUC = abalone9_18(ConfigList, 9)
mAUC = abalone9_18(ConfigList, 12)


mAUC = ROSE01(ConfigList, 9)
mAUC = australian(ConfigList)

mAUC = Bupa(ConfigList)

mAUC = Heart(ConfigList)
mAUC = Mammographic(ConfigList)
mAUC = Pima(ConfigList)

mAUC = SPECTF(ConfigList,9)
mAUC = SPECTF(ConfigList,10)

mAUC = Wisconsin(ConfigList, 9)

mAUC = Climate(ConfigList,9)
mAUC = Climate(ConfigList,7)

mAUC = Diabetic(ConfigList)

mAUC = Haberman(ConfigList,9)
mAUC = Haberman(ConfigList,10)

mAUC = Haberman2(ConfigList,9)
mAUC = Haberman2(ConfigList,10)

mAUC = Ionosphere(ConfigList,9)
mAUC = Ionosphere(ConfigList,7)

mAUC = Indian(ConfigList,9)
mAUC = Indian(ConfigList,10)

mAUC = Blood(ConfigList,9)
mAUC = Blood(ConfigList,7)

mAUC = Hypothyroid(ConfigList)
mAUC = SickEuthyroid(ConfigList)

mAUC = Chronic(ConfigList)





#for (seed in 0:1000) Bupa2(0, seed)
#for (OptTyp in 1:21) Hypothyroid(OptTyp)
#for (OptTyp in 1:21) SickEuthyroid(OptTyp)

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = australian(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Bupa(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Haberman(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Mammographic(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Heart(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Wisconsin(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Diabetic(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Haberman(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Pima(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Blood(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Climate(ConfigList)
}

for (OptTyp in 30:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Ionosphere(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = SPECTF(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Indian(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Chronic(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = Hypothyroid(ConfigList)
}

for (OptTyp in 0:59){  
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = SickEuthyroid(ConfigList)
}


for (OptTyp in 29:31)
{
  ConfigList[1,"Opt1"] = OptTyp  
  mAUC = australian(ConfigList)
  mAUC = Bupa(ConfigList)
  mAUC = Crx(ConfigList)
  mAUC = Heart(ConfigList)
  mAUC = Mammographic(ConfigList)
  mAUC = Pima(ConfigList)
  mAUC = SPECTF(ConfigList)
  mAUC = Wisconsin(ConfigList)
  mAUC = Chronic(ConfigList)
  mAUC = Climate(ConfigList)
  mAUC = Diabetic(ConfigList)
  mAUC = Haberman(ConfigList)
  mAUC = Ionosphere(ConfigList)
  mAUC = Indian(ConfigList)
  mAUC = Blood(ConfigList)
  mAUC = Hypothyroid(ConfigList)
  mAUC = SickEuthyroid(ConfigList)
}

#Idea  Normalize ri   Sum(ri) =1


# source('D:/RProjects/OptTask/Sources/Haberman.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/australian.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Bupa.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Heart.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Hypothyroid.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Mammographic.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Pima.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/SickEuthyroid.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/SPECTF.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Wisconsin.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Chronic.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Climate.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Diabetic.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Ionosphere.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Indian.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Blood.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ROSE01.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/abalone19.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/abalone9_18.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/vehicle0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/vehicle1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/vehicle2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/vehicle3.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Pima2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Iris0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ecoli_0_vs_1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ecoli1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ecoli2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ecoli3.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/ecoli4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Ecoli0137vs26.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass5.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass6.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Glass0123_456.R', echo=FALSE) 
# source('D:/RProjects/OptTask/Sources/Glass016vs2.R', echo=FALSE) 
# source('D:/RProjects/OptTask/Sources/Glass016vs5.R', echo=FALSE) 
# source('D:/RProjects/OptTask/Sources/Wisconsin2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/vowel0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/yeast1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/yeast3.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/yeast2_vs_4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/yeast05679_vs_4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast1vs7.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast1458vs7.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast2vs8.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast5.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast6.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Yeast1289vs7.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/shuttle0vs4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Shuttle2vs4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Haberman2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/segment0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/new_thyroid1.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/new_thyroid2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/page_blocks0.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/page_blocks13_vs_4.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/page_blocks13_vs_2.R', echo=FALSE)  
# source('D:/RProjects/OptTask/Sources/Seismicbumps.R', echo=FALSE)  
# 
