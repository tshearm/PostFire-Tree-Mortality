library(randomForest)
library(caret)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggpubr)

#####Load Data#############
# This code uses data from the Fire and Tree Mortality Database 
#(Cansler et al. 2020). The data are publicly available at
#https://www.fs.usda.gov/rds/archive/catalog/RDS-2020-0001
#You will need to download the data and change the line below to properly
#import the dataset.
#
#
FTM_DB_Working <- read.csv("C:/projects/FTM_Scorch/FTM_Scorch_home/FTM_trees_Feb12_2021.csv")
#####Clean up the data##########
All_trees <-FTM_DB_Working
All_trees<-subset(All_trees, Times_burned==1) #We're only going to look at trees burned once for now.
All_trees$Species<-factor(All_trees$Species)
#These are the species we will use for the analyses
spp_subset5<-c("PIPO",
               "PSME",
               "PICO",
               "ABCO",
               "ABGR",
               "PILA",
               "CADE27",
               "LAOC")

Tree_data<-All_trees[All_trees$Species %in% spp_subset5,]
Tree_data$Species<-factor(Tree_data$Species)

##Remove trees with no DBH measures
Tree_data<-Tree_data[!is.na(Tree_data$DBH_cm),]
##Remove trees with calculated CVS
Tree_data<-subset(Tree_data, CVS_percent_source=='F')
##Only use trees >= 10 cm DBH
Tree_data<-subset(Tree_data, DBH_cm >= 10)
#Remove zombie trees if present
Tree_data$zombie <- 0                         
Tree_data$zombie <- ifelse((!is.na(Tree_data$yr1status)) & Tree_data$yr1status == 1 & (Tree_data$yr2status == 0 | Tree_data$yr3status == 0 | Tree_data$yr4status == 0 | Tree_data$yr5status == 0),1,
                           ifelse(Tree_data$yr2status == 1 & (Tree_data$yr3status == 0 | Tree_data$yr4status == 0 | Tree_data$yr5status == 0),1,
                                  ifelse(Tree_data$yr3status == 1 & (Tree_data$yr4status == 0 | Tree_data$yr5status == 0),1,
                                         ifelse(Tree_data$yr4status == 1 & Tree_data$yr5status == 0,1,0))))
Tree_data <- Tree_data[-which(Tree_data$zombie==1),]

###Now make a variable that evaluates if the tree died in year 1 or anytime in years 2-5###
#NA values will mess up the ifelse statements below, so we are going to copy the status fields
#and replace the NA's with -1 

Tree_data$yr1temp <- Tree_data$yr1status
Tree_data$yr1temp[is.na(Tree_data$yr1temp)] <- -1

Tree_data$yr5temp <- Tree_data$yr5status
Tree_data$yr5temp[is.na(Tree_data$yr5temp)] <- -1

Tree_data$yr6temp <- Tree_data$yr6status
Tree_data$yr6temp[is.na(Tree_data$yr6temp)] <- -1

Tree_data$yr7temp <- Tree_data$yr7status
Tree_data$yr7temp[is.na(Tree_data$yr7temp)] <- -1

Tree_data$yr8temp <- Tree_data$yr8status
Tree_data$yr8temp[is.na(Tree_data$yr8temp)] <- -1

Tree_data$yr9temp <- Tree_data$yr9status
Tree_data$yr9temp[is.na(Tree_data$yr9temp)] <- -1

Tree_data$yr10temp <- Tree_data$yr10status
Tree_data$yr10temp[is.na(Tree_data$yr10temp)] <- -1


Tree_data$dead_check <- ifelse(Tree_data$yr1temp == 1, 1,
                               ifelse(Tree_data$yr1temp == 0 & rowSums(Tree_data[,23:26], na.rm=TRUE) >= 1, 2, 
                                      ifelse(Tree_data$yr5temp == 0 | Tree_data$yr6temp == 0 | Tree_data$yr7temp == 0 | Tree_data$yr8temp == 0 | Tree_data$yr9temp == 0 | Tree_data$yr10temp == 0, 0,9)))


###########ABCO#######################

#############subset species###########

ABCO <- subset(Tree_data, Species == 'ABCO')
ABCO$OBS_ID <- 1:nrow(ABCO)

###Get complete cases##########
ABCO_yr1 <- select(ABCO, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% drop_na(yr1status, CVS_percent, DBH_cm, BCH_percent) %>% rename(Status = yr1status)

ABCO_yr2 <- select(ABCO, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

ABCO_yr3 <- select(ABCO, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

ABCO_yr4 <- select(ABCO, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

ABCO_yr5 <- select(ABCO, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
ABCO_flds1<-createFolds(ABCO_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
ABCO_flds2<-createFolds(ABCO_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABCO_flds3<-createFolds(ABCO_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABCO_flds4<-createFolds(ABCO_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABCO_flds5<-createFolds(ABCO_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 




##1
set.seed(1980)
samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[1]],]$Status))))
ABCO_train.rf1.1 <- randomForest(data = ABCO_yr1[-ABCO_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr2[-ABCO_flds2[[1]],]$Status))))
ABCO_train.rf2.1 <- randomForest(data = ABCO_yr2[-ABCO_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABCO_yr3[-ABCO_flds3[[1]],]$Status))))
ABCO_train.rf3.1 <- randomForest(data = ABCO_yr3[-ABCO_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr4[-ABCO_flds4[[1]],]$Status))))
ABCO_train.rf4.1 <- randomForest(data = ABCO_yr4[-ABCO_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr5[-ABCO_flds5[[1]],]$Status))))
ABCO_train.rf5.1 <- randomForest(data = ABCO_yr5[-ABCO_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(ABCO_train.rf5.1)

##2

samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[2]],]$Status))))
ABCO_train.rf1.2 <- randomForest(data = ABCO_yr1[-ABCO_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr2[-ABCO_flds2[[2]],]$Status))))
ABCO_train.rf2.2 <- randomForest(data = ABCO_yr2[-ABCO_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABCO_yr3[-ABCO_flds3[[2]],]$Status))))
ABCO_train.rf3.2 <- randomForest(data = ABCO_yr3[-ABCO_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr4[-ABCO_flds4[[2]],]$Status))))
ABCO_train.rf4.2 <- randomForest(data = ABCO_yr4[-ABCO_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr5[-ABCO_flds5[[2]],]$Status))))
ABCO_train.rf5.2 <- randomForest(data = ABCO_yr5[-ABCO_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[3]],]$Status))))
ABCO_train.rf1.3 <- randomForest(data = ABCO_yr1[-ABCO_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr2[-ABCO_flds2[[3]],]$Status))))
ABCO_train.rf2.3 <- randomForest(data = ABCO_yr2[-ABCO_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABCO_yr3[-ABCO_flds3[[3]],]$Status))))
ABCO_train.rf3.3 <- randomForest(data = ABCO_yr3[-ABCO_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr4[-ABCO_flds4[[3]],]$Status))))
ABCO_train.rf4.3 <- randomForest(data = ABCO_yr4[-ABCO_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr5[-ABCO_flds5[[3]],]$Status))))
ABCO_train.rf5.3 <- randomForest(data = ABCO_yr5[-ABCO_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[4]],]$Status))))
ABCO_train.rf1.4 <- randomForest(data = ABCO_yr1[-ABCO_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr2[-ABCO_flds2[[4]],]$Status))))
ABCO_train.rf2.4 <- randomForest(data = ABCO_yr2[-ABCO_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABCO_yr3[-ABCO_flds3[[4]],]$Status))))
ABCO_train.rf3.4 <- randomForest(data = ABCO_yr3[-ABCO_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr4[-ABCO_flds4[[4]],]$Status))))
ABCO_train.rf4.4 <- randomForest(data = ABCO_yr4[-ABCO_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr5[-ABCO_flds5[[4]],]$Status))))
ABCO_train.rf5.4 <- randomForest(data = ABCO_yr5[-ABCO_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(ABCO_yr1[-ABCO_flds1[[5]],]$Status))))
ABCO_train.rf1.5 <- randomForest(data = ABCO_yr1[-ABCO_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr2[-ABCO_flds2[[5]],]$Status))))
ABCO_train.rf2.5 <- randomForest(data = ABCO_yr2[-ABCO_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABCO_yr3[-ABCO_flds3[[5]],]$Status))))
ABCO_train.rf3.5 <- randomForest(data = ABCO_yr3[-ABCO_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr4[-ABCO_flds4[[5]],]$Status))))
ABCO_train.rf4.5 <- randomForest(data = ABCO_yr4[-ABCO_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABCO_yr5[-ABCO_flds5[[5]],]$Status))))
ABCO_train.rf5.5 <- randomForest(data = ABCO_yr5[-ABCO_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)




##################Create test datasets#####################
##1
ABCO_test1.1 <- ABCO_yr1[ABCO_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent, dead_check) 




ABCO_test2.1 <- ABCO_yr2[ABCO_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent, dead_check) 



ABCO_test3.1 <- ABCO_yr3[ABCO_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent, dead_check) 



ABCO_test4.1 <- ABCO_yr4[ABCO_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent, dead_check) 



ABCO_test5.1 <- ABCO_yr5[ABCO_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



##2
ABCO_test1.2 <- ABCO_yr1[ABCO_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test2.2 <- ABCO_yr2[ABCO_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test3.2 <- ABCO_yr3[ABCO_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test4.2 <- ABCO_yr4[ABCO_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test5.2 <- ABCO_yr5[ABCO_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



##3
ABCO_test1.3 <- ABCO_yr1[ABCO_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test2.3 <- ABCO_yr2[ABCO_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test3.3 <- ABCO_yr3[ABCO_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test4.3 <- ABCO_yr4[ABCO_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test5.3 <- ABCO_yr5[ABCO_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



##4
ABCO_test1.4 <- ABCO_yr1[ABCO_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test2.4 <- ABCO_yr2[ABCO_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test3.4 <- ABCO_yr3[ABCO_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test4.4 <- ABCO_yr4[ABCO_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test5.4 <- ABCO_yr5[ABCO_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



##5
ABCO_test1.5 <- ABCO_yr1[ABCO_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test2.5 <- ABCO_yr2[ABCO_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test3.5 <- ABCO_yr3[ABCO_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test4.5 <- ABCO_yr4[ABCO_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



ABCO_test5.5 <- ABCO_yr5[ABCO_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 




#######################Predicting###############################
ABCO_pred.results <-data.frame() 

ABCO_1.1pred <- predict(ABCO_train.rf1.1, newdata=ABCO_test1.1)
ABCO_2.1pred <- predict(ABCO_train.rf2.1, newdata=ABCO_test2.1)
ABCO_3.1pred <- predict(ABCO_train.rf3.1, newdata=ABCO_test3.1)
ABCO_4.1pred <- predict(ABCO_train.rf4.1, newdata=ABCO_test4.1)
ABCO_5.1pred <- predict(ABCO_train.rf5.1, newdata=ABCO_test5.1)

ABCO_1.2pred <- predict(ABCO_train.rf1.2, newdata=ABCO_test1.2)
ABCO_2.2pred <- predict(ABCO_train.rf2.2, newdata=ABCO_test2.2)
ABCO_3.2pred <- predict(ABCO_train.rf3.2, newdata=ABCO_test3.2)
ABCO_4.2pred <- predict(ABCO_train.rf4.2, newdata=ABCO_test4.2)
ABCO_5.2pred <- predict(ABCO_train.rf5.2, newdata=ABCO_test5.2)

ABCO_1.3pred <- predict(ABCO_train.rf1.3, newdata=ABCO_test1.3)
ABCO_2.3pred <- predict(ABCO_train.rf2.3, newdata=ABCO_test2.3)
ABCO_3.3pred <- predict(ABCO_train.rf3.3, newdata=ABCO_test3.3)
ABCO_4.3pred <- predict(ABCO_train.rf4.3, newdata=ABCO_test4.3)
ABCO_5.3pred <- predict(ABCO_train.rf5.3, newdata=ABCO_test5.3)

ABCO_1.4pred <- predict(ABCO_train.rf1.4, newdata=ABCO_test1.4)
ABCO_2.4pred <- predict(ABCO_train.rf2.4, newdata=ABCO_test2.4)
ABCO_3.4pred <- predict(ABCO_train.rf3.4, newdata=ABCO_test3.4)
ABCO_4.4pred <- predict(ABCO_train.rf4.4, newdata=ABCO_test4.4)
ABCO_5.4pred <- predict(ABCO_train.rf5.4, newdata=ABCO_test5.4)

ABCO_1.5pred <- predict(ABCO_train.rf1.5, newdata=ABCO_test1.5)
ABCO_2.5pred <- predict(ABCO_train.rf2.5, newdata=ABCO_test2.5)
ABCO_3.5pred <- predict(ABCO_train.rf3.5, newdata=ABCO_test3.5)
ABCO_4.5pred <- predict(ABCO_train.rf4.5, newdata=ABCO_test4.5)
ABCO_5.5pred <- predict(ABCO_train.rf5.5, newdata=ABCO_test5.5)

length(ABCO_3.1pred)
length(ABCO_test3.1$Status)

ABCO.test.preds <-data.frame(Status = c(ABCO_test1.1$Status,ABCO_test2.1$Status,ABCO_test3.1$Status,ABCO_test4.1$Status,ABCO_test5.1$Status,
                                        ABCO_test1.2$Status,ABCO_test2.2$Status,ABCO_test3.2$Status,ABCO_test4.2$Status,ABCO_test5.2$Status,
                                        ABCO_test1.3$Status,ABCO_test2.3$Status,ABCO_test3.3$Status,ABCO_test4.3$Status,ABCO_test5.3$Status,
                                        ABCO_test1.4$Status,ABCO_test2.4$Status,ABCO_test3.4$Status,ABCO_test4.4$Status,ABCO_test5.4$Status,
                                        ABCO_test1.5$Status,ABCO_test2.5$Status,ABCO_test3.5$Status,ABCO_test4.5$Status,ABCO_test5.5$Status),
                             preds = c(ABCO_1.1pred,ABCO_2.1pred,ABCO_3.1pred,ABCO_4.1pred,ABCO_5.1pred,
                                       ABCO_1.2pred,ABCO_2.2pred,ABCO_3.2pred,ABCO_4.2pred,ABCO_5.2pred,
                                       ABCO_1.3pred,ABCO_2.3pred,ABCO_3.3pred,ABCO_4.3pred,ABCO_5.3pred,
                                       ABCO_1.4pred,ABCO_2.4pred,ABCO_3.4pred,ABCO_4.4pred,ABCO_5.4pred,
                                       ABCO_1.5pred,ABCO_2.5pred,ABCO_3.5pred,ABCO_4.5pred,ABCO_5.5pred),
                             model = c(rep(1,length(ABCO_1.1pred)),rep(2,length(ABCO_2.1pred)),rep(3,length(ABCO_3.1pred)),rep(4,length(ABCO_4.1pred)),rep(5,length(ABCO_5.1pred)),
                                       rep(1,length(ABCO_1.2pred)),rep(2,length(ABCO_2.2pred)),rep(3,length(ABCO_3.2pred)),rep(4,length(ABCO_4.2pred)),rep(5,length(ABCO_5.2pred)),
                                       rep(1,length(ABCO_1.3pred)),rep(2,length(ABCO_2.3pred)),rep(3,length(ABCO_3.3pred)),rep(4,length(ABCO_4.3pred)),rep(5,length(ABCO_5.3pred)),
                                       rep(1,length(ABCO_1.4pred)),rep(2,length(ABCO_2.4pred)),rep(3,length(ABCO_3.4pred)),rep(4,length(ABCO_4.4pred)),rep(5,length(ABCO_5.4pred)),
                                       rep(1,length(ABCO_1.5pred)),rep(2,length(ABCO_2.5pred)),rep(3,length(ABCO_3.5pred)),rep(4,length(ABCO_4.5pred)),rep(5,length(ABCO_5.5pred))),
                             rep = c(rep(1,length(ABCO_1.1pred)),rep(1,length(ABCO_2.1pred)),rep(1,length(ABCO_3.1pred)),rep(1,length(ABCO_4.1pred)),rep(1,length(ABCO_5.1pred)),
                                     rep(2,length(ABCO_1.2pred)),rep(2,length(ABCO_2.2pred)),rep(2,length(ABCO_3.2pred)),rep(2,length(ABCO_4.2pred)),rep(2,length(ABCO_5.2pred)),
                                     rep(3,length(ABCO_1.3pred)),rep(3,length(ABCO_2.3pred)),rep(3,length(ABCO_3.3pred)),rep(3,length(ABCO_4.3pred)),rep(3,length(ABCO_5.3pred)),
                                     rep(4,length(ABCO_1.4pred)),rep(4,length(ABCO_2.4pred)),rep(4,length(ABCO_3.4pred)),rep(4,length(ABCO_4.4pred)),rep(4,length(ABCO_5.4pred)),
                                     rep(5,length(ABCO_1.5pred)),rep(5,length(ABCO_2.5pred)),rep(5,length(ABCO_3.5pred)),rep(5,length(ABCO_4.5pred)),rep(5,length(ABCO_5.5pred))),
                             dead_check = c(ABCO_test1.1$dead_check, ABCO_test2.1$dead_check, ABCO_test3.1$dead_check, ABCO_test4.1$dead_check, ABCO_test5.1$dead_check,
                                            ABCO_test1.2$dead_check, ABCO_test2.2$dead_check, ABCO_test3.2$dead_check, ABCO_test4.2$dead_check, ABCO_test5.2$dead_check,
                                            ABCO_test1.3$dead_check, ABCO_test2.3$dead_check, ABCO_test3.3$dead_check, ABCO_test4.3$dead_check, ABCO_test5.3$dead_check,
                                            ABCO_test1.4$dead_check, ABCO_test2.4$dead_check, ABCO_test3.4$dead_check, ABCO_test4.4$dead_check, ABCO_test5.4$dead_check,
                                            ABCO_test1.5$dead_check, ABCO_test2.5$dead_check, ABCO_test3.5$dead_check, ABCO_test4.5$dead_check, ABCO_test5.5$dead_check))

ABCO.test.preds$wrong <- ifelse(ABCO.test.preds$Status == ABCO.test.preds$preds, 0,1)
ABCO.test.preds$correct <- ifelse(ABCO.test.preds$Status == ABCO.test.preds$preds, 1,0)

ABCO.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check,model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
              pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> ABCO.wrong.preds



ABCO.wrong.p <- ggplot(data=ABCO.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")



ABCO_RF1.1.tab <-table(factor(ABCO_test1.1$Status, levels = 0:1), factor(ABCO_1.1pred, levels = 0:1))
ABCO_LiveAcc1.1<-ABCO_RF1.1.tab[1,1]/sum(ABCO_RF1.1.tab[1,])
ABCO_DeadAcc1.1<-ABCO_RF1.1.tab[2,2]/sum(ABCO_RF1.1.tab[2,])
ABCO_TotalAcc1.1<-(ABCO_RF1.1.tab[1,1]+ABCO_RF1.1.tab[2,2])/sum(ABCO_RF1.1.tab)

ABCO_RF2.1.tab <-table(factor(ABCO_test2.1$Status, levels = 0:1), factor(ABCO_2.1pred, levels = 0:1))
ABCO_LiveAcc2.1<-ABCO_RF2.1.tab[1,1]/sum(ABCO_RF2.1.tab[1,])
ABCO_DeadAcc2.1<-ABCO_RF2.1.tab[2,2]/sum(ABCO_RF2.1.tab[2,])
ABCO_TotalAcc2.1<-(ABCO_RF2.1.tab[1,1]+ABCO_RF2.1.tab[2,2])/sum(ABCO_RF2.1.tab)

ABCO_RF3.1.tab <-table(factor(ABCO_test3.1$Status, levels = 0:1), factor(ABCO_3.1pred, levels = 0:1))
ABCO_LiveAcc3.1<-ABCO_RF3.1.tab[1,1]/sum(ABCO_RF3.1.tab[1,])
ABCO_DeadAcc3.1<-ABCO_RF3.1.tab[2,2]/sum(ABCO_RF3.1.tab[2,])
ABCO_TotalAcc3.1<-(ABCO_RF3.1.tab[1,1]+ABCO_RF3.1.tab[2,2])/sum(ABCO_RF3.1.tab)

ABCO_RF4.1.tab <-table(factor(ABCO_test4.1$Status, levels = 0:1), factor(ABCO_4.1pred, levels = 0:1))
ABCO_LiveAcc4.1<-ABCO_RF4.1.tab[1,1]/sum(ABCO_RF4.1.tab[1,])
ABCO_DeadAcc4.1<-ABCO_RF4.1.tab[2,2]/sum(ABCO_RF4.1.tab[2,])
ABCO_TotalAcc4.1<-(ABCO_RF4.1.tab[1,1]+ABCO_RF4.1.tab[2,2])/sum(ABCO_RF4.1.tab)

ABCO_RF5.1.tab <-table(factor(ABCO_test5.1$Status, levels = 0:1), factor(ABCO_5.1pred, levels = 0:1))
ABCO_LiveAcc5.1<-ABCO_RF5.1.tab[1,1]/sum(ABCO_RF5.1.tab[1,])
ABCO_DeadAcc5.1<-ABCO_RF5.1.tab[2,2]/sum(ABCO_RF5.1.tab[2,])
ABCO_TotalAcc5.1<-(ABCO_RF5.1.tab[1,1]+ABCO_RF5.1.tab[2,2])/sum(ABCO_RF5.1.tab)

ABCO_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(ABCO_LiveAcc1.1,
                                                                              ABCO_LiveAcc2.1,
                                                                              ABCO_LiveAcc3.1,
                                                                              ABCO_LiveAcc4.1,
                                                                              ABCO_LiveAcc5.1),
                              DeadAcc = c(ABCO_DeadAcc1.1,
                                          ABCO_DeadAcc2.1,
                                          ABCO_DeadAcc3.1,
                                          ABCO_DeadAcc4.1,
                                          ABCO_DeadAcc5.1),
                              TotalAcc = c(ABCO_TotalAcc1.1,
                                           ABCO_TotalAcc2.1,
                                           ABCO_TotalAcc3.1,
                                           ABCO_TotalAcc4.1,
                                           ABCO_TotalAcc5.1))


ABCO_RF1.2.tab <-table(factor(ABCO_test1.2$Status, levels = 0:1), factor(ABCO_1.2pred, levels = 0:1))
ABCO_LiveAcc1.2<-ABCO_RF1.2.tab[1,1]/sum(ABCO_RF1.2.tab[1,])
ABCO_DeadAcc1.2<-ABCO_RF1.2.tab[2,2]/sum(ABCO_RF1.2.tab[2,])
ABCO_TotalAcc1.2<-(ABCO_RF1.2.tab[1,1]+ABCO_RF1.2.tab[2,2])/sum(ABCO_RF1.2.tab)

ABCO_RF2.2.tab <-table(factor(ABCO_test2.2$Status, levels = 0:1), factor(ABCO_2.2pred, levels = 0:1))
ABCO_LiveAcc2.2<-ABCO_RF2.2.tab[1,1]/sum(ABCO_RF2.2.tab[1,])
ABCO_DeadAcc2.2<-ABCO_RF2.2.tab[2,2]/sum(ABCO_RF2.2.tab[2,])
ABCO_TotalAcc2.2<-(ABCO_RF2.2.tab[1,1]+ABCO_RF2.2.tab[2,2])/sum(ABCO_RF2.2.tab)

ABCO_RF3.2.tab <-table(factor(ABCO_test3.2$Status, levels = 0:1), factor(ABCO_3.2pred, levels = 0:1))
ABCO_LiveAcc3.2<-ABCO_RF3.2.tab[1,1]/sum(ABCO_RF3.2.tab[1,])
ABCO_DeadAcc3.2<-ABCO_RF3.2.tab[2,2]/sum(ABCO_RF3.2.tab[2,])
ABCO_TotalAcc3.2<-(ABCO_RF3.2.tab[1,1]+ABCO_RF3.2.tab[2,2])/sum(ABCO_RF3.2.tab)

ABCO_RF4.2.tab <-table(factor(ABCO_test4.2$Status, levels = 0:1), factor(ABCO_4.2pred, levels = 0:1))
ABCO_LiveAcc4.2<-ABCO_RF4.2.tab[1,1]/sum(ABCO_RF4.2.tab[1,])
ABCO_DeadAcc4.2<-ABCO_RF4.2.tab[2,2]/sum(ABCO_RF4.2.tab[2,])
ABCO_TotalAcc4.2<-(ABCO_RF4.2.tab[1,1]+ABCO_RF4.2.tab[2,2])/sum(ABCO_RF4.2.tab)

ABCO_RF5.2.tab <-table(factor(ABCO_test5.2$Status, levels = 0:1), factor(ABCO_5.2pred, levels = 0:1))
ABCO_LiveAcc5.2<-ABCO_RF5.2.tab[1,1]/sum(ABCO_RF5.2.tab[1,])
ABCO_DeadAcc5.2<-ABCO_RF5.2.tab[2,2]/sum(ABCO_RF5.2.tab[2,])
ABCO_TotalAcc5.2<-(ABCO_RF5.2.tab[1,1]+ABCO_RF5.2.tab[2,2])/sum(ABCO_RF5.2.tab)

ABCO_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(ABCO_LiveAcc1.2,
                                                                              ABCO_LiveAcc2.2,
                                                                              ABCO_LiveAcc3.2,
                                                                              ABCO_LiveAcc4.2,
                                                                              ABCO_LiveAcc5.2),
                              DeadAcc = c(ABCO_DeadAcc1.2,
                                          ABCO_DeadAcc2.2,
                                          ABCO_DeadAcc3.2,
                                          ABCO_DeadAcc4.2,
                                          ABCO_DeadAcc5.2),
                              TotalAcc = c(ABCO_TotalAcc1.2,
                                           ABCO_TotalAcc2.2,
                                           ABCO_TotalAcc3.2,
                                           ABCO_TotalAcc4.2,
                                           ABCO_TotalAcc5.2))

ABCO_RF1.3.tab <-table(factor(ABCO_test1.3$Status, levels = 0:1), factor(ABCO_1.3pred, levels = 0:1))
ABCO_LiveAcc1.3<-ABCO_RF1.3.tab[1,1]/sum(ABCO_RF1.3.tab[1,])
ABCO_DeadAcc1.3<-ABCO_RF1.3.tab[2,2]/sum(ABCO_RF1.3.tab[2,])
ABCO_TotalAcc1.3<-(ABCO_RF1.3.tab[1,1]+ABCO_RF1.3.tab[2,2])/sum(ABCO_RF1.3.tab)

ABCO_RF2.3.tab <-table(factor(ABCO_test2.3$Status, levels = 0:1), factor(ABCO_2.3pred, levels = 0:1))
ABCO_LiveAcc2.3<-ABCO_RF2.3.tab[1,1]/sum(ABCO_RF2.3.tab[1,])
ABCO_DeadAcc2.3<-ABCO_RF2.3.tab[2,2]/sum(ABCO_RF2.3.tab[2,])
ABCO_TotalAcc2.3<-(ABCO_RF2.3.tab[1,1]+ABCO_RF2.3.tab[2,2])/sum(ABCO_RF2.3.tab)

ABCO_RF3.3.tab <-table(factor(ABCO_test3.3$Status, levels = 0:1), factor(ABCO_3.3pred, levels = 0:1))
ABCO_LiveAcc3.3<-ABCO_RF3.3.tab[1,1]/sum(ABCO_RF3.3.tab[1,])
ABCO_DeadAcc3.3<-ABCO_RF3.3.tab[2,2]/sum(ABCO_RF3.3.tab[2,])
ABCO_TotalAcc3.3<-(ABCO_RF3.3.tab[1,1]+ABCO_RF3.3.tab[2,2])/sum(ABCO_RF3.3.tab)

ABCO_RF4.3.tab <-table(factor(ABCO_test4.3$Status, levels = 0:1), factor(ABCO_4.3pred, levels = 0:1))
ABCO_LiveAcc4.3<-ABCO_RF4.3.tab[1,1]/sum(ABCO_RF4.3.tab[1,])
ABCO_DeadAcc4.3<-ABCO_RF4.3.tab[2,2]/sum(ABCO_RF4.3.tab[2,])
ABCO_TotalAcc4.3<-(ABCO_RF4.3.tab[1,1]+ABCO_RF4.3.tab[2,2])/sum(ABCO_RF4.3.tab)

ABCO_RF5.3.tab <-table(factor(ABCO_test5.3$Status, levels = 0:1), factor(ABCO_5.3pred, levels = 0:1))
ABCO_LiveAcc5.3<-ABCO_RF5.3.tab[1,1]/sum(ABCO_RF5.3.tab[1,])
ABCO_DeadAcc5.3<-ABCO_RF5.3.tab[2,2]/sum(ABCO_RF5.3.tab[2,])
ABCO_TotalAcc5.3<-(ABCO_RF5.3.tab[1,1]+ABCO_RF5.3.tab[2,2])/sum(ABCO_RF5.3.tab)

ABCO_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(ABCO_LiveAcc1.3,
                                                                              ABCO_LiveAcc2.3,
                                                                              ABCO_LiveAcc3.3,
                                                                              ABCO_LiveAcc4.3,
                                                                              ABCO_LiveAcc5.3),
                              DeadAcc = c(ABCO_DeadAcc1.3,
                                          ABCO_DeadAcc2.3,
                                          ABCO_DeadAcc3.3,
                                          ABCO_DeadAcc4.3,
                                          ABCO_DeadAcc5.3),
                              TotalAcc = c(ABCO_TotalAcc1.3,
                                           ABCO_TotalAcc2.3,
                                           ABCO_TotalAcc3.3,
                                           ABCO_TotalAcc4.3,
                                           ABCO_TotalAcc5.3))

ABCO_RF1.4.tab <-table(factor(ABCO_test1.4$Status, levels = 0:1), factor(ABCO_1.4pred, levels = 0:1))
ABCO_LiveAcc1.4<-ABCO_RF1.4.tab[1,1]/sum(ABCO_RF1.4.tab[1,])
ABCO_DeadAcc1.4<-ABCO_RF1.4.tab[2,2]/sum(ABCO_RF1.4.tab[2,])
ABCO_TotalAcc1.4<-(ABCO_RF1.4.tab[1,1]+ABCO_RF1.4.tab[2,2])/sum(ABCO_RF1.4.tab)

ABCO_RF2.4.tab <-table(factor(ABCO_test2.4$Status, levels = 0:1), factor(ABCO_2.4pred, levels = 0:1))
ABCO_LiveAcc2.4<-ABCO_RF2.4.tab[1,1]/sum(ABCO_RF2.4.tab[1,])
ABCO_DeadAcc2.4<-ABCO_RF2.4.tab[2,2]/sum(ABCO_RF2.4.tab[2,])
ABCO_TotalAcc2.4<-(ABCO_RF2.4.tab[1,1]+ABCO_RF2.4.tab[2,2])/sum(ABCO_RF2.4.tab)

ABCO_RF3.4.tab <-table(factor(ABCO_test3.4$Status, levels = 0:1), factor(ABCO_3.4pred, levels = 0:1))
ABCO_LiveAcc3.4<-ABCO_RF3.4.tab[1,1]/sum(ABCO_RF3.4.tab[1,])
ABCO_DeadAcc3.4<-ABCO_RF3.4.tab[2,2]/sum(ABCO_RF3.4.tab[2,])
ABCO_TotalAcc3.4<-(ABCO_RF3.4.tab[1,1]+ABCO_RF3.4.tab[2,2])/sum(ABCO_RF3.4.tab)

ABCO_RF4.4.tab <-table(factor(ABCO_test4.4$Status, levels = 0:1), factor(ABCO_4.4pred, levels = 0:1))
ABCO_LiveAcc4.4<-ABCO_RF4.4.tab[1,1]/sum(ABCO_RF4.4.tab[1,])
ABCO_DeadAcc4.4<-ABCO_RF4.4.tab[2,2]/sum(ABCO_RF4.4.tab[2,])
ABCO_TotalAcc4.4<-(ABCO_RF4.4.tab[1,1]+ABCO_RF4.4.tab[2,2])/sum(ABCO_RF4.4.tab)

ABCO_RF5.4.tab <-table(factor(ABCO_test5.4$Status, levels = 0:1), factor(ABCO_5.4pred, levels = 0:1))
ABCO_LiveAcc5.4<-ABCO_RF5.4.tab[1,1]/sum(ABCO_RF5.4.tab[1,])
ABCO_DeadAcc5.4<-ABCO_RF5.4.tab[2,2]/sum(ABCO_RF5.4.tab[2,])
ABCO_TotalAcc5.4<-(ABCO_RF5.4.tab[1,1]+ABCO_RF5.4.tab[2,2])/sum(ABCO_RF5.4.tab)

ABCO_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(ABCO_LiveAcc1.4,
                                                                              ABCO_LiveAcc2.4,
                                                                              ABCO_LiveAcc3.4,
                                                                              ABCO_LiveAcc4.4,
                                                                              ABCO_LiveAcc5.4),
                              DeadAcc = c(ABCO_DeadAcc1.4,
                                          ABCO_DeadAcc2.4,
                                          ABCO_DeadAcc3.4,
                                          ABCO_DeadAcc4.4,
                                          ABCO_DeadAcc5.4),
                              TotalAcc = c(ABCO_TotalAcc1.4,
                                           ABCO_TotalAcc2.4,
                                           ABCO_TotalAcc3.4,
                                           ABCO_TotalAcc4.4,
                                           ABCO_TotalAcc5.4))

ABCO_RF1.5.tab <-table(factor(ABCO_test1.5$Status, levels = 0:1), factor(ABCO_1.5pred, levels = 0:1))
ABCO_LiveAcc1.5<-ABCO_RF1.5.tab[1,1]/sum(ABCO_RF1.5.tab[1,])
ABCO_DeadAcc1.5<-ABCO_RF1.5.tab[2,2]/sum(ABCO_RF1.5.tab[2,])
ABCO_TotalAcc1.5<-(ABCO_RF1.5.tab[1,1]+ABCO_RF1.5.tab[2,2])/sum(ABCO_RF1.5.tab)

ABCO_RF2.5.tab <-table(factor(ABCO_test2.5$Status, levels = 0:1), factor(ABCO_2.5pred, levels = 0:1))
ABCO_LiveAcc2.5<-ABCO_RF2.5.tab[1,1]/sum(ABCO_RF2.5.tab[1,])
ABCO_DeadAcc2.5<-ABCO_RF2.5.tab[2,2]/sum(ABCO_RF2.5.tab[2,])
ABCO_TotalAcc2.5<-(ABCO_RF2.5.tab[1,1]+ABCO_RF2.5.tab[2,2])/sum(ABCO_RF2.5.tab)

ABCO_RF3.5.tab <-table(factor(ABCO_test3.5$Status, levels = 0:1), factor(ABCO_3.5pred, levels = 0:1))
ABCO_LiveAcc3.5<-ABCO_RF3.5.tab[1,1]/sum(ABCO_RF3.5.tab[1,])
ABCO_DeadAcc3.5<-ABCO_RF3.5.tab[2,2]/sum(ABCO_RF3.5.tab[2,])
ABCO_TotalAcc3.5<-(ABCO_RF3.5.tab[1,1]+ABCO_RF3.5.tab[2,2])/sum(ABCO_RF3.5.tab)

ABCO_RF4.5.tab <-table(factor(ABCO_test4.5$Status, levels = 0:1), factor(ABCO_4.5pred, levels = 0:1))
ABCO_LiveAcc4.5<-ABCO_RF4.5.tab[1,1]/sum(ABCO_RF4.5.tab[1,])
ABCO_DeadAcc4.5<-ABCO_RF4.5.tab[2,2]/sum(ABCO_RF4.5.tab[2,])
ABCO_TotalAcc4.5<-(ABCO_RF4.5.tab[1,1]+ABCO_RF4.5.tab[2,2])/sum(ABCO_RF4.5.tab)

ABCO_RF5.5.tab <-table(factor(ABCO_test5.5$Status, levels = 0:1), factor(ABCO_5.5pred, levels = 0:1))
ABCO_LiveAcc5.5<-ABCO_RF5.5.tab[1,1]/sum(ABCO_RF5.5.tab[1,])
ABCO_DeadAcc5.5<-ABCO_RF5.5.tab[2,2]/sum(ABCO_RF5.5.tab[2,])
ABCO_TotalAcc5.5<-(ABCO_RF5.5.tab[1,1]+ABCO_RF5.5.tab[2,2])/sum(ABCO_RF5.5.tab)

ABCO_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(ABCO_LiveAcc1.5,
                                                                              ABCO_LiveAcc2.5,
                                                                              ABCO_LiveAcc3.5,
                                                                              ABCO_LiveAcc4.5,
                                                                              ABCO_LiveAcc5.5),
                              DeadAcc = c(ABCO_DeadAcc1.5,
                                          ABCO_DeadAcc2.5,
                                          ABCO_DeadAcc3.5,
                                          ABCO_DeadAcc4.5,
                                          ABCO_DeadAcc5.5),
                              TotalAcc = c(ABCO_TotalAcc1.5,
                                           ABCO_TotalAcc2.5,
                                           ABCO_TotalAcc3.5,
                                           ABCO_TotalAcc4.5,
                                           ABCO_TotalAcc5.5))

ABCO_pred.results <-rbind(ABCO_Pre.results1,ABCO_Pre.results2,ABCO_Pre.results3,ABCO_Pre.results4,ABCO_Pre.results5)



ABCO_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "ABCO",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> ABCO_RF_preds.sum




#############ABGR######################

#############subset species###########

ABGR <- subset(Tree_data, Species == 'ABGR')
ABGR$OBS_ID <- 1:nrow(ABGR)

###Get complete cases##########
ABGR_yr1 <- select(ABGR, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check,  OBS_ID) %>% na.omit %>% rename(Status = yr1status)

ABGR_yr2 <- select(ABGR, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

ABGR_yr3 <- select(ABGR, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

ABGR_yr4 <- select(ABGR, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

ABGR_yr5 <- select(ABGR, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
ABGR_flds1<-createFolds(ABGR_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
ABGR_flds2<-createFolds(ABGR_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABGR_flds3<-createFolds(ABGR_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABGR_flds4<-createFolds(ABGR_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
ABGR_flds5<-createFolds(ABGR_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 



##1
set.seed(1980)
samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[1]],]$Status))))
ABGR_train.rf1.1 <- randomForest(data = ABGR_yr1[-ABGR_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr2[-ABGR_flds2[[1]],]$Status))))
ABGR_train.rf2.1 <- randomForest(data = ABGR_yr2[-ABGR_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABGR_yr3[-ABGR_flds3[[1]],]$Status))))
ABGR_train.rf3.1 <- randomForest(data = ABGR_yr3[-ABGR_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr4[-ABGR_flds4[[1]],]$Status))))
ABGR_train.rf4.1 <- randomForest(data = ABGR_yr4[-ABGR_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr5[-ABGR_flds5[[1]],]$Status))))
ABGR_train.rf5.1 <- randomForest(data = ABGR_yr5[-ABGR_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(ABGR_train.rf5.1)

##2

samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[2]],]$Status))))
ABGR_train.rf1.2 <- randomForest(data = ABGR_yr1[-ABGR_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr2[-ABGR_flds2[[2]],]$Status))))
ABGR_train.rf2.2 <- randomForest(data = ABGR_yr2[-ABGR_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABGR_yr3[-ABGR_flds3[[2]],]$Status))))
ABGR_train.rf3.2 <- randomForest(data = ABGR_yr3[-ABGR_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr4[-ABGR_flds4[[2]],]$Status))))
ABGR_train.rf4.2 <- randomForest(data = ABGR_yr4[-ABGR_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr5[-ABGR_flds5[[2]],]$Status))))
ABGR_train.rf5.2 <- randomForest(data = ABGR_yr5[-ABGR_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[3]],]$Status))))
ABGR_train.rf1.3 <- randomForest(data = ABGR_yr1[-ABGR_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr2[-ABGR_flds2[[3]],]$Status))))
ABGR_train.rf2.3 <- randomForest(data = ABGR_yr2[-ABGR_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABGR_yr3[-ABGR_flds3[[3]],]$Status))))
ABGR_train.rf3.3 <- randomForest(data = ABGR_yr3[-ABGR_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr4[-ABGR_flds4[[3]],]$Status))))
ABGR_train.rf4.3 <- randomForest(data = ABGR_yr4[-ABGR_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr5[-ABGR_flds5[[3]],]$Status))))
ABGR_train.rf5.3 <- randomForest(data = ABGR_yr5[-ABGR_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[4]],]$Status))))
ABGR_train.rf1.4 <- randomForest(data = ABGR_yr1[-ABGR_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr2[-ABGR_flds2[[4]],]$Status))))
ABGR_train.rf2.4 <- randomForest(data = ABGR_yr2[-ABGR_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABGR_yr3[-ABGR_flds3[[4]],]$Status))))
ABGR_train.rf3.4 <- randomForest(data = ABGR_yr3[-ABGR_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr4[-ABGR_flds4[[4]],]$Status))))
ABGR_train.rf4.4 <- randomForest(data = ABGR_yr4[-ABGR_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr5[-ABGR_flds5[[4]],]$Status))))
ABGR_train.rf5.4 <- randomForest(data = ABGR_yr5[-ABGR_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(ABGR_yr1[-ABGR_flds1[[5]],]$Status))))
ABGR_train.rf1.5 <- randomForest(data = ABGR_yr1[-ABGR_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr2[-ABGR_flds2[[5]],]$Status))))
ABGR_train.rf2.5 <- randomForest(data = ABGR_yr2[-ABGR_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(ABGR_yr3[-ABGR_flds3[[5]],]$Status))))
ABGR_train.rf3.5 <- randomForest(data = ABGR_yr3[-ABGR_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr4[-ABGR_flds4[[5]],]$Status))))
ABGR_train.rf4.5 <- randomForest(data = ABGR_yr4[-ABGR_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(ABGR_yr5[-ABGR_flds5[[5]],]$Status))))
ABGR_train.rf5.5 <- randomForest(data = ABGR_yr5[-ABGR_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)



##################Create test datasets#####################
##1
ABGR_test1.1 <- ABGR_yr1[ABGR_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(ABGR_yr1[ABGR_flds1[[1]],] $  exclude=NULL)
table(ABGR_test1.1$  exclude=NULL)

ABGR_test2.1 <- ABGR_yr2[ABGR_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test3.1 <- ABGR_yr3[ABGR_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test4.1 <- ABGR_yr4[ABGR_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test5.1 <- ABGR_yr5[ABGR_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
ABGR_test1.2 <- ABGR_yr1[ABGR_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test2.2 <- ABGR_yr2[ABGR_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test3.2 <- ABGR_yr3[ABGR_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test4.2 <- ABGR_yr4[ABGR_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test5.2 <- ABGR_yr5[ABGR_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
ABGR_test1.3 <- ABGR_yr1[ABGR_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test2.3 <- ABGR_yr2[ABGR_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test3.3 <- ABGR_yr3[ABGR_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test4.3 <- ABGR_yr4[ABGR_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test5.3 <- ABGR_yr5[ABGR_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##4
ABGR_test1.4 <- ABGR_yr1[ABGR_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test2.4 <- ABGR_yr2[ABGR_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test3.4 <- ABGR_yr3[ABGR_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test4.4 <- ABGR_yr4[ABGR_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


ABGR_test5.4 <- ABGR_yr5[ABGR_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

##5
ABGR_test1.5 <- ABGR_yr1[ABGR_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

ABGR_test2.5 <- ABGR_yr2[ABGR_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

ABGR_test3.5 <- ABGR_yr3[ABGR_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

ABGR_test4.5 <- ABGR_yr4[ABGR_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

ABGR_test5.5 <- ABGR_yr5[ABGR_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


#######################Predicting###############################
ABGR_pred.results <-data.frame() 

ABGR_1.1pred <- predict(ABGR_train.rf1.1, newdata=ABGR_test1.1)
ABGR_2.1pred <- predict(ABGR_train.rf2.1, newdata=ABGR_test2.1)
ABGR_3.1pred <- predict(ABGR_train.rf3.1, newdata=ABGR_test3.1)
ABGR_4.1pred <- predict(ABGR_train.rf4.1, newdata=ABGR_test4.1)
ABGR_5.1pred <- predict(ABGR_train.rf5.1, newdata=ABGR_test5.1)

ABGR_1.2pred <- predict(ABGR_train.rf1.2, newdata=ABGR_test1.2)
ABGR_2.2pred <- predict(ABGR_train.rf2.2, newdata=ABGR_test2.2)
ABGR_3.2pred <- predict(ABGR_train.rf3.2, newdata=ABGR_test3.2)
ABGR_4.2pred <- predict(ABGR_train.rf4.2, newdata=ABGR_test4.2)
ABGR_5.2pred <- predict(ABGR_train.rf5.2, newdata=ABGR_test5.2)

ABGR_1.3pred <- predict(ABGR_train.rf1.3, newdata=ABGR_test1.3)
ABGR_2.3pred <- predict(ABGR_train.rf2.3, newdata=ABGR_test2.3)
ABGR_3.3pred <- predict(ABGR_train.rf3.3, newdata=ABGR_test3.3)
ABGR_4.3pred <- predict(ABGR_train.rf4.3, newdata=ABGR_test4.3)
ABGR_5.3pred <- predict(ABGR_train.rf5.3, newdata=ABGR_test5.3)

ABGR_1.4pred <- predict(ABGR_train.rf1.4, newdata=ABGR_test1.4)
ABGR_2.4pred <- predict(ABGR_train.rf2.4, newdata=ABGR_test2.4)
ABGR_3.4pred <- predict(ABGR_train.rf3.4, newdata=ABGR_test3.4)
ABGR_4.4pred <- predict(ABGR_train.rf4.4, newdata=ABGR_test4.4)
ABGR_5.4pred <- predict(ABGR_train.rf5.4, newdata=ABGR_test5.4)

ABGR_1.5pred <- predict(ABGR_train.rf1.5, newdata=ABGR_test1.5)
ABGR_2.5pred <- predict(ABGR_train.rf2.5, newdata=ABGR_test2.5)
ABGR_3.5pred <- predict(ABGR_train.rf3.5, newdata=ABGR_test3.5)
ABGR_4.5pred <- predict(ABGR_train.rf4.5, newdata=ABGR_test4.5)
ABGR_5.5pred <- predict(ABGR_train.rf5.5, newdata=ABGR_test5.5)


ABGR.test.preds <-data.frame(Status = c(ABGR_test1.1$Status,ABGR_test2.1$Status,ABGR_test3.1$Status,ABGR_test4.1$Status,ABGR_test5.1$Status,
                                        ABGR_test1.2$Status,ABGR_test2.2$Status,ABGR_test3.2$Status,ABGR_test4.2$Status,ABGR_test5.2$Status,
                                        ABGR_test1.3$Status,ABGR_test2.3$Status,ABGR_test3.3$Status,ABGR_test4.3$Status,ABGR_test5.3$Status,
                                        ABGR_test1.4$Status,ABGR_test2.4$Status,ABGR_test3.4$Status,ABGR_test4.4$Status,ABGR_test5.4$Status,
                                        ABGR_test1.5$Status,ABGR_test2.5$Status,ABGR_test3.5$Status,ABGR_test4.5$Status,ABGR_test5.5$Status),
                             preds = c(ABGR_1.1pred,ABGR_2.1pred,ABGR_3.1pred,ABGR_4.1pred,ABGR_5.1pred,
                                       ABGR_1.2pred,ABGR_2.2pred,ABGR_3.2pred,ABGR_4.2pred,ABGR_5.2pred,
                                       ABGR_1.3pred,ABGR_2.3pred,ABGR_3.3pred,ABGR_4.3pred,ABGR_5.3pred,
                                       ABGR_1.4pred,ABGR_2.4pred,ABGR_3.4pred,ABGR_4.4pred,ABGR_5.4pred,
                                       ABGR_1.5pred,ABGR_2.5pred,ABGR_3.5pred,ABGR_4.5pred,ABGR_5.5pred),
                             model = c(rep(1,length(ABGR_1.1pred)),rep(2,length(ABGR_2.1pred)),rep(3,length(ABGR_3.1pred)),rep(4,length(ABGR_4.1pred)),rep(5,length(ABGR_5.1pred)),
                                       rep(1,length(ABGR_1.2pred)),rep(2,length(ABGR_2.2pred)),rep(3,length(ABGR_3.2pred)),rep(4,length(ABGR_4.2pred)),rep(5,length(ABGR_5.2pred)),
                                       rep(1,length(ABGR_1.3pred)),rep(2,length(ABGR_2.3pred)),rep(3,length(ABGR_3.3pred)),rep(4,length(ABGR_4.3pred)),rep(5,length(ABGR_5.3pred)),
                                       rep(1,length(ABGR_1.4pred)),rep(2,length(ABGR_2.4pred)),rep(3,length(ABGR_3.4pred)),rep(4,length(ABGR_4.4pred)),rep(5,length(ABGR_5.4pred)),
                                       rep(1,length(ABGR_1.5pred)),rep(2,length(ABGR_2.5pred)),rep(3,length(ABGR_3.5pred)),rep(4,length(ABGR_4.5pred)),rep(5,length(ABGR_5.5pred))),
                             rep = c(rep(1,length(ABGR_1.1pred)),rep(1,length(ABGR_2.1pred)),rep(1,length(ABGR_3.1pred)),rep(1,length(ABGR_4.1pred)),rep(1,length(ABGR_5.1pred)),
                                     rep(2,length(ABGR_1.2pred)),rep(2,length(ABGR_2.2pred)),rep(2,length(ABGR_3.2pred)),rep(2,length(ABGR_4.2pred)),rep(2,length(ABGR_5.2pred)),
                                     rep(3,length(ABGR_1.3pred)),rep(3,length(ABGR_2.3pred)),rep(3,length(ABGR_3.3pred)),rep(3,length(ABGR_4.3pred)),rep(3,length(ABGR_5.3pred)),
                                     rep(4,length(ABGR_1.4pred)),rep(4,length(ABGR_2.4pred)),rep(4,length(ABGR_3.4pred)),rep(4,length(ABGR_4.4pred)),rep(4,length(ABGR_5.4pred)),
                                     rep(5,length(ABGR_1.5pred)),rep(5,length(ABGR_2.5pred)),rep(5,length(ABGR_3.5pred)),rep(5,length(ABGR_4.5pred)),rep(5,length(ABGR_5.5pred))),
                            
                             dead_check = c(ABGR_test1.1$dead_check, ABGR_test2.1$dead_check, ABGR_test3.1$dead_check, ABGR_test4.1$dead_check, ABGR_test5.1$dead_check,
                                            ABGR_test1.2$dead_check, ABGR_test2.2$dead_check, ABGR_test3.2$dead_check, ABGR_test4.2$dead_check, ABGR_test5.2$dead_check,
                                            ABGR_test1.3$dead_check, ABGR_test2.3$dead_check, ABGR_test3.3$dead_check, ABGR_test4.3$dead_check, ABGR_test5.3$dead_check,
                                            ABGR_test1.4$dead_check, ABGR_test2.4$dead_check, ABGR_test3.4$dead_check, ABGR_test4.4$dead_check, ABGR_test5.4$dead_check,
                                            ABGR_test1.5$dead_check, ABGR_test2.5$dead_check, ABGR_test3.5$dead_check, ABGR_test4.5$dead_check, ABGR_test5.5$dead_check))



ABGR.test.preds$wrong <- ifelse(ABGR.test.preds$Status == ABGR.test.preds$preds, 0,1)

ABGR.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check) -> ABGR.wrong.preds


ABGR_RF1.1.tab <-table(factor(ABGR_test1.1$Status, levels = 0:1), factor(ABGR_1.1pred, levels = 0:1))
ABGR_LiveAcc1.1<-ABGR_RF1.1.tab[1,1]/sum(ABGR_RF1.1.tab[1,])
ABGR_DeadAcc1.1<-ABGR_RF1.1.tab[2,2]/sum(ABGR_RF1.1.tab[2,])
ABGR_TotalAcc1.1<-(ABGR_RF1.1.tab[1,1]+ABGR_RF1.1.tab[2,2])/sum(ABGR_RF1.1.tab)

ABGR_RF2.1.tab <-table(factor(ABGR_test2.1$Status, levels = 0:1), factor(ABGR_2.1pred, levels = 0:1))
ABGR_LiveAcc2.1<-ABGR_RF2.1.tab[1,1]/sum(ABGR_RF2.1.tab[1,])
ABGR_DeadAcc2.1<-ABGR_RF2.1.tab[2,2]/sum(ABGR_RF2.1.tab[2,])
ABGR_TotalAcc2.1<-(ABGR_RF2.1.tab[1,1]+ABGR_RF2.1.tab[2,2])/sum(ABGR_RF2.1.tab)

ABGR_RF3.1.tab <-table(factor(ABGR_test3.1$Status, levels = 0:1), factor(ABGR_3.1pred, levels = 0:1))
ABGR_LiveAcc3.1<-ABGR_RF3.1.tab[1,1]/sum(ABGR_RF3.1.tab[1,])
ABGR_DeadAcc3.1<-ABGR_RF3.1.tab[2,2]/sum(ABGR_RF3.1.tab[2,])
ABGR_TotalAcc3.1<-(ABGR_RF3.1.tab[1,1]+ABGR_RF3.1.tab[2,2])/sum(ABGR_RF3.1.tab)

ABGR_RF4.1.tab <-table(factor(ABGR_test4.1$Status, levels = 0:1), factor(ABGR_4.1pred, levels = 0:1))
ABGR_LiveAcc4.1<-ABGR_RF4.1.tab[1,1]/sum(ABGR_RF4.1.tab[1,])
ABGR_DeadAcc4.1<-ABGR_RF4.1.tab[2,2]/sum(ABGR_RF4.1.tab[2,])
ABGR_TotalAcc4.1<-(ABGR_RF4.1.tab[1,1]+ABGR_RF4.1.tab[2,2])/sum(ABGR_RF4.1.tab)

ABGR_RF5.1.tab <-table(factor(ABGR_test5.1$Status, levels = 0:1), factor(ABGR_5.1pred, levels = 0:1))
ABGR_LiveAcc5.1<-ABGR_RF5.1.tab[1,1]/sum(ABGR_RF5.1.tab[1,])
ABGR_DeadAcc5.1<-ABGR_RF5.1.tab[2,2]/sum(ABGR_RF5.1.tab[2,])
ABGR_TotalAcc5.1<-(ABGR_RF5.1.tab[1,1]+ABGR_RF5.1.tab[2,2])/sum(ABGR_RF5.1.tab)

ABGR_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(ABGR_LiveAcc1.1,
                                                                              ABGR_LiveAcc2.1,
                                                                              ABGR_LiveAcc3.1,
                                                                              ABGR_LiveAcc4.1,
                                                                              ABGR_LiveAcc5.1),
                              DeadAcc = c(ABGR_DeadAcc1.1,
                                          ABGR_DeadAcc2.1,
                                          ABGR_DeadAcc3.1,
                                          ABGR_DeadAcc4.1,
                                          ABGR_DeadAcc5.1),
                              TotalAcc = c(ABGR_TotalAcc1.1,
                                           ABGR_TotalAcc2.1,
                                           ABGR_TotalAcc3.1,
                                           ABGR_TotalAcc4.1,
                                           ABGR_TotalAcc5.1))


ABGR_RF1.2.tab <-table(factor(ABGR_test1.2$Status, levels = 0:1), factor(ABGR_1.2pred, levels = 0:1))
ABGR_LiveAcc1.2<-ABGR_RF1.2.tab[1,1]/sum(ABGR_RF1.2.tab[1,])
ABGR_DeadAcc1.2<-ABGR_RF1.2.tab[2,2]/sum(ABGR_RF1.2.tab[2,])
ABGR_TotalAcc1.2<-(ABGR_RF1.2.tab[1,1]+ABGR_RF1.2.tab[2,2])/sum(ABGR_RF1.2.tab)

ABGR_RF2.2.tab <-table(factor(ABGR_test2.2$Status, levels = 0:1), factor(ABGR_2.2pred, levels = 0:1))
ABGR_LiveAcc2.2<-ABGR_RF2.2.tab[1,1]/sum(ABGR_RF2.2.tab[1,])
ABGR_DeadAcc2.2<-ABGR_RF2.2.tab[2,2]/sum(ABGR_RF2.2.tab[2,])
ABGR_TotalAcc2.2<-(ABGR_RF2.2.tab[1,1]+ABGR_RF2.2.tab[2,2])/sum(ABGR_RF2.2.tab)

ABGR_RF3.2.tab <-table(factor(ABGR_test3.2$Status, levels = 0:1), factor(ABGR_3.2pred, levels = 0:1))
ABGR_LiveAcc3.2<-ABGR_RF3.2.tab[1,1]/sum(ABGR_RF3.2.tab[1,])
ABGR_DeadAcc3.2<-ABGR_RF3.2.tab[2,2]/sum(ABGR_RF3.2.tab[2,])
ABGR_TotalAcc3.2<-(ABGR_RF3.2.tab[1,1]+ABGR_RF3.2.tab[2,2])/sum(ABGR_RF3.2.tab)

ABGR_RF4.2.tab <-table(factor(ABGR_test4.2$Status, levels = 0:1), factor(ABGR_4.2pred, levels = 0:1))
ABGR_LiveAcc4.2<-ABGR_RF4.2.tab[1,1]/sum(ABGR_RF4.2.tab[1,])
ABGR_DeadAcc4.2<-ABGR_RF4.2.tab[2,2]/sum(ABGR_RF4.2.tab[2,])
ABGR_TotalAcc4.2<-(ABGR_RF4.2.tab[1,1]+ABGR_RF4.2.tab[2,2])/sum(ABGR_RF4.2.tab)

ABGR_RF5.2.tab <-table(factor(ABGR_test5.2$Status, levels = 0:1), factor(ABGR_5.2pred, levels = 0:1))
ABGR_LiveAcc5.2<-ABGR_RF5.2.tab[1,1]/sum(ABGR_RF5.2.tab[1,])
ABGR_DeadAcc5.2<-ABGR_RF5.2.tab[2,2]/sum(ABGR_RF5.2.tab[2,])
ABGR_TotalAcc5.2<-(ABGR_RF5.2.tab[1,1]+ABGR_RF5.2.tab[2,2])/sum(ABGR_RF5.2.tab)

ABGR_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(ABGR_LiveAcc1.2,
                                                                              ABGR_LiveAcc2.2,
                                                                              ABGR_LiveAcc3.2,
                                                                              ABGR_LiveAcc4.2,
                                                                              ABGR_LiveAcc5.2),
                              DeadAcc = c(ABGR_DeadAcc1.2,
                                          ABGR_DeadAcc2.2,
                                          ABGR_DeadAcc3.2,
                                          ABGR_DeadAcc4.2,
                                          ABGR_DeadAcc5.2),
                              TotalAcc = c(ABGR_TotalAcc1.2,
                                           ABGR_TotalAcc2.2,
                                           ABGR_TotalAcc3.2,
                                           ABGR_TotalAcc4.2,
                                           ABGR_TotalAcc5.2))

ABGR_RF1.3.tab <-table(factor(ABGR_test1.3$Status, levels = 0:1), factor(ABGR_1.3pred, levels = 0:1))
ABGR_LiveAcc1.3<-ABGR_RF1.3.tab[1,1]/sum(ABGR_RF1.3.tab[1,])
ABGR_DeadAcc1.3<-ABGR_RF1.3.tab[2,2]/sum(ABGR_RF1.3.tab[2,])
ABGR_TotalAcc1.3<-(ABGR_RF1.3.tab[1,1]+ABGR_RF1.3.tab[2,2])/sum(ABGR_RF1.3.tab)

ABGR_RF2.3.tab <-table(factor(ABGR_test2.3$Status, levels = 0:1), factor(ABGR_2.3pred, levels = 0:1))
ABGR_LiveAcc2.3<-ABGR_RF2.3.tab[1,1]/sum(ABGR_RF2.3.tab[1,])
ABGR_DeadAcc2.3<-ABGR_RF2.3.tab[2,2]/sum(ABGR_RF2.3.tab[2,])
ABGR_TotalAcc2.3<-(ABGR_RF2.3.tab[1,1]+ABGR_RF2.3.tab[2,2])/sum(ABGR_RF2.3.tab)

ABGR_RF3.3.tab <-table(factor(ABGR_test3.3$Status, levels = 0:1), factor(ABGR_3.3pred, levels = 0:1))
ABGR_LiveAcc3.3<-ABGR_RF3.3.tab[1,1]/sum(ABGR_RF3.3.tab[1,])
ABGR_DeadAcc3.3<-ABGR_RF3.3.tab[2,2]/sum(ABGR_RF3.3.tab[2,])
ABGR_TotalAcc3.3<-(ABGR_RF3.3.tab[1,1]+ABGR_RF3.3.tab[2,2])/sum(ABGR_RF3.3.tab)

ABGR_RF4.3.tab <-table(factor(ABGR_test4.3$Status, levels = 0:1), factor(ABGR_4.3pred, levels = 0:1))
ABGR_LiveAcc4.3<-ABGR_RF4.3.tab[1,1]/sum(ABGR_RF4.3.tab[1,])
ABGR_DeadAcc4.3<-ABGR_RF4.3.tab[2,2]/sum(ABGR_RF4.3.tab[2,])
ABGR_TotalAcc4.3<-(ABGR_RF4.3.tab[1,1]+ABGR_RF4.3.tab[2,2])/sum(ABGR_RF4.3.tab)

ABGR_RF5.3.tab <-table(factor(ABGR_test5.3$Status, levels = 0:1), factor(ABGR_5.3pred, levels = 0:1))
ABGR_LiveAcc5.3<-ABGR_RF5.3.tab[1,1]/sum(ABGR_RF5.3.tab[1,])
ABGR_DeadAcc5.3<-ABGR_RF5.3.tab[2,2]/sum(ABGR_RF5.3.tab[2,])
ABGR_TotalAcc5.3<-(ABGR_RF5.3.tab[1,1]+ABGR_RF5.3.tab[2,2])/sum(ABGR_RF5.3.tab)

ABGR_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(ABGR_LiveAcc1.3,
                                                                              ABGR_LiveAcc2.3,
                                                                              ABGR_LiveAcc3.3,
                                                                              ABGR_LiveAcc4.3,
                                                                              ABGR_LiveAcc5.3),
                              DeadAcc = c(ABGR_DeadAcc1.3,
                                          ABGR_DeadAcc2.3,
                                          ABGR_DeadAcc3.3,
                                          ABGR_DeadAcc4.3,
                                          ABGR_DeadAcc5.3),
                              TotalAcc = c(ABGR_TotalAcc1.3,
                                           ABGR_TotalAcc2.3,
                                           ABGR_TotalAcc3.3,
                                           ABGR_TotalAcc4.3,
                                           ABGR_TotalAcc5.3))

ABGR_RF1.4.tab <-table(factor(ABGR_test1.4$Status, levels = 0:1), factor(ABGR_1.4pred, levels = 0:1))
ABGR_LiveAcc1.4<-ABGR_RF1.4.tab[1,1]/sum(ABGR_RF1.4.tab[1,])
ABGR_DeadAcc1.4<-ABGR_RF1.4.tab[2,2]/sum(ABGR_RF1.4.tab[2,])
ABGR_TotalAcc1.4<-(ABGR_RF1.4.tab[1,1]+ABGR_RF1.4.tab[2,2])/sum(ABGR_RF1.4.tab)

ABGR_RF2.4.tab <-table(factor(ABGR_test2.4$Status, levels = 0:1), factor(ABGR_2.4pred, levels = 0:1))
ABGR_LiveAcc2.4<-ABGR_RF2.4.tab[1,1]/sum(ABGR_RF2.4.tab[1,])
ABGR_DeadAcc2.4<-ABGR_RF2.4.tab[2,2]/sum(ABGR_RF2.4.tab[2,])
ABGR_TotalAcc2.4<-(ABGR_RF2.4.tab[1,1]+ABGR_RF2.4.tab[2,2])/sum(ABGR_RF2.4.tab)

ABGR_RF3.4.tab <-table(factor(ABGR_test3.4$Status, levels = 0:1), factor(ABGR_3.4pred, levels = 0:1))
ABGR_LiveAcc3.4<-ABGR_RF3.4.tab[1,1]/sum(ABGR_RF3.4.tab[1,])
ABGR_DeadAcc3.4<-ABGR_RF3.4.tab[2,2]/sum(ABGR_RF3.4.tab[2,])
ABGR_TotalAcc3.4<-(ABGR_RF3.4.tab[1,1]+ABGR_RF3.4.tab[2,2])/sum(ABGR_RF3.4.tab)

ABGR_RF4.4.tab <-table(factor(ABGR_test4.4$Status, levels = 0:1), factor(ABGR_4.4pred, levels = 0:1))
ABGR_LiveAcc4.4<-ABGR_RF4.4.tab[1,1]/sum(ABGR_RF4.4.tab[1,])
ABGR_DeadAcc4.4<-ABGR_RF4.4.tab[2,2]/sum(ABGR_RF4.4.tab[2,])
ABGR_TotalAcc4.4<-(ABGR_RF4.4.tab[1,1]+ABGR_RF4.4.tab[2,2])/sum(ABGR_RF4.4.tab)

ABGR_RF5.4.tab <-table(factor(ABGR_test5.4$Status, levels = 0:1), factor(ABGR_5.4pred, levels = 0:1))
ABGR_LiveAcc5.4<-ABGR_RF5.4.tab[1,1]/sum(ABGR_RF5.4.tab[1,])
ABGR_DeadAcc5.4<-ABGR_RF5.4.tab[2,2]/sum(ABGR_RF5.4.tab[2,])
ABGR_TotalAcc5.4<-(ABGR_RF5.4.tab[1,1]+ABGR_RF5.4.tab[2,2])/sum(ABGR_RF5.4.tab)

ABGR_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(ABGR_LiveAcc1.4,
                                                                              ABGR_LiveAcc2.4,
                                                                              ABGR_LiveAcc3.4,
                                                                              ABGR_LiveAcc4.4,
                                                                              ABGR_LiveAcc5.4),
                              DeadAcc = c(ABGR_DeadAcc1.4,
                                          ABGR_DeadAcc2.4,
                                          ABGR_DeadAcc3.4,
                                          ABGR_DeadAcc4.4,
                                          ABGR_DeadAcc5.4),
                              TotalAcc = c(ABGR_TotalAcc1.4,
                                           ABGR_TotalAcc2.4,
                                           ABGR_TotalAcc3.4,
                                           ABGR_TotalAcc4.4,
                                           ABGR_TotalAcc5.4))

ABGR_RF1.5.tab <-table(factor(ABGR_test1.5$Status, levels = 0:1), factor(ABGR_1.5pred, levels = 0:1))
ABGR_LiveAcc1.5<-ABGR_RF1.5.tab[1,1]/sum(ABGR_RF1.5.tab[1,])
ABGR_DeadAcc1.5<-ABGR_RF1.5.tab[2,2]/sum(ABGR_RF1.5.tab[2,])
ABGR_TotalAcc1.5<-(ABGR_RF1.5.tab[1,1]+ABGR_RF1.5.tab[2,2])/sum(ABGR_RF1.5.tab)

ABGR_RF2.5.tab <-table(factor(ABGR_test2.5$Status, levels = 0:1), factor(ABGR_2.5pred, levels = 0:1))
ABGR_LiveAcc2.5<-ABGR_RF2.5.tab[1,1]/sum(ABGR_RF2.5.tab[1,])
ABGR_DeadAcc2.5<-ABGR_RF2.5.tab[2,2]/sum(ABGR_RF2.5.tab[2,])
ABGR_TotalAcc2.5<-(ABGR_RF2.5.tab[1,1]+ABGR_RF2.5.tab[2,2])/sum(ABGR_RF2.5.tab)

ABGR_RF3.5.tab <-table(factor(ABGR_test3.5$Status, levels = 0:1), factor(ABGR_3.5pred, levels = 0:1))
ABGR_LiveAcc3.5<-ABGR_RF3.5.tab[1,1]/sum(ABGR_RF3.5.tab[1,])
ABGR_DeadAcc3.5<-ABGR_RF3.5.tab[2,2]/sum(ABGR_RF3.5.tab[2,])
ABGR_TotalAcc3.5<-(ABGR_RF3.5.tab[1,1]+ABGR_RF3.5.tab[2,2])/sum(ABGR_RF3.5.tab)

ABGR_RF4.5.tab <-table(factor(ABGR_test4.5$Status, levels = 0:1), factor(ABGR_4.5pred, levels = 0:1))
ABGR_LiveAcc4.5<-ABGR_RF4.5.tab[1,1]/sum(ABGR_RF4.5.tab[1,])
ABGR_DeadAcc4.5<-ABGR_RF4.5.tab[2,2]/sum(ABGR_RF4.5.tab[2,])
ABGR_TotalAcc4.5<-(ABGR_RF4.5.tab[1,1]+ABGR_RF4.5.tab[2,2])/sum(ABGR_RF4.5.tab)

ABGR_RF5.5.tab <-table(factor(ABGR_test5.5$Status, levels = 0:1), factor(ABGR_5.5pred, levels = 0:1))
ABGR_LiveAcc5.5<-ABGR_RF5.5.tab[1,1]/sum(ABGR_RF5.5.tab[1,])
ABGR_DeadAcc5.5<-ABGR_RF5.5.tab[2,2]/sum(ABGR_RF5.5.tab[2,])
ABGR_TotalAcc5.5<-(ABGR_RF5.5.tab[1,1]+ABGR_RF5.5.tab[2,2])/sum(ABGR_RF5.5.tab)

ABGR_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(ABGR_LiveAcc1.5,
                                                                              ABGR_LiveAcc2.5,
                                                                              ABGR_LiveAcc3.5,
                                                                              ABGR_LiveAcc4.5,
                                                                              ABGR_LiveAcc5.5),
                              DeadAcc = c(ABGR_DeadAcc1.5,
                                          ABGR_DeadAcc2.5,
                                          ABGR_DeadAcc3.5,
                                          ABGR_DeadAcc4.5,
                                          ABGR_DeadAcc5.5),
                              TotalAcc = c(ABGR_TotalAcc1.5,
                                           ABGR_TotalAcc2.5,
                                           ABGR_TotalAcc3.5,
                                           ABGR_TotalAcc4.5,
                                           ABGR_TotalAcc5.5))

ABGR_pred.results <-rbind(ABGR_Pre.results1,ABGR_Pre.results2,ABGR_Pre.results3,ABGR_Pre.results4,ABGR_Pre.results5)


ABGR_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "ABGR",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> ABGR_RF_preds.sum




#############CADE27######################
#############subset species###########

CADE27 <- subset(Tree_data, Species == 'CADE27')
CADE27$OBS_ID <- 1:nrow(CADE27)

###Get complete cases##########
CADE27_yr1 <- select(CADE27, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)

CADE27_yr2 <- select(CADE27, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

CADE27_yr3 <- select(CADE27, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

CADE27_yr4 <- select(CADE27, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

CADE27_yr5 <- select(CADE27, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
CADE27_flds1<-createFolds(CADE27_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
CADE27_flds2<-createFolds(CADE27_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
CADE27_flds3<-createFolds(CADE27_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
CADE27_flds4<-createFolds(CADE27_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
CADE27_flds5<-createFolds(CADE27_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 



##1
set.seed(1980)
samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[1]],]$Status))))
CADE27_train.rf1.1 <- randomForest(data = CADE27_yr1[-CADE27_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr2[-CADE27_flds2[[1]],]$Status))))
CADE27_train.rf2.1 <- randomForest(data = CADE27_yr2[-CADE27_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)
samp <-trunc((0.2*min(table(CADE27_yr3[-CADE27_flds3[[1]],]$Status))))
CADE27_train.rf3.1 <- randomForest(data = CADE27_yr3[-CADE27_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr4[-CADE27_flds4[[1]],]$Status))))
CADE27_train.rf4.1 <- randomForest(data = CADE27_yr4[-CADE27_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr5[-CADE27_flds5[[1]],]$Status))))
CADE27_train.rf5.1 <- randomForest(data = CADE27_yr5[-CADE27_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)
varImpPlot(CADE27_train.rf5.1)

##2

samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[2]],]$Status))))
CADE27_train.rf1.2 <- randomForest(data = CADE27_yr1[-CADE27_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr2[-CADE27_flds2[[2]],]$Status))))
CADE27_train.rf2.2 <- randomForest(data = CADE27_yr2[-CADE27_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)
samp <-trunc((0.2*min(table(CADE27_yr3[-CADE27_flds3[[2]],]$Status))))
CADE27_train.rf3.2 <- randomForest(data = CADE27_yr3[-CADE27_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr4[-CADE27_flds4[[2]],]$Status))))
CADE27_train.rf4.2 <- randomForest(data = CADE27_yr4[-CADE27_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr5[-CADE27_flds5[[2]],]$Status))))
CADE27_train.rf5.2 <- randomForest(data = CADE27_yr5[-CADE27_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

##3

samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[3]],]$Status))))
CADE27_train.rf1.3 <- randomForest(data = CADE27_yr1[-CADE27_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr2[-CADE27_flds2[[3]],]$Status))))
CADE27_train.rf2.3 <- randomForest(data = CADE27_yr2[-CADE27_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)
samp <-trunc((0.2*min(table(CADE27_yr3[-CADE27_flds3[[3]],]$Status))))
CADE27_train.rf3.3 <- randomForest(data = CADE27_yr3[-CADE27_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr4[-CADE27_flds4[[3]],]$Status))))
CADE27_train.rf4.3 <- randomForest(data = CADE27_yr4[-CADE27_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr5[-CADE27_flds5[[3]],]$Status))))
CADE27_train.rf5.3 <- randomForest(data = CADE27_yr5[-CADE27_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

##4

samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[4]],]$Status))))
CADE27_train.rf1.4 <- randomForest(data = CADE27_yr1[-CADE27_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr2[-CADE27_flds2[[4]],]$Status))))
CADE27_train.rf2.4 <- randomForest(data = CADE27_yr2[-CADE27_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)
samp <-trunc((0.2*min(table(CADE27_yr3[-CADE27_flds3[[4]],]$Status))))
CADE27_train.rf3.4 <- randomForest(data = CADE27_yr3[-CADE27_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr4[-CADE27_flds4[[4]],]$Status))))
CADE27_train.rf4.4 <- randomForest(data = CADE27_yr4[-CADE27_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr5[-CADE27_flds5[[4]],]$Status))))
CADE27_train.rf5.4 <- randomForest(data = CADE27_yr5[-CADE27_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

##5

samp <-trunc((0.2*min(table(CADE27_yr1[-CADE27_flds1[[5]],]$Status))))
CADE27_train.rf1.5 <- randomForest(data = CADE27_yr1[-CADE27_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr2[-CADE27_flds2[[5]],]$Status))))
CADE27_train.rf2.5 <- randomForest(data = CADE27_yr2[-CADE27_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)
samp <-trunc((0.2*min(table(CADE27_yr3[-CADE27_flds3[[5]],]$Status))))
CADE27_train.rf3.5 <- randomForest(data = CADE27_yr3[-CADE27_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr4[-CADE27_flds4[[5]],]$Status))))
CADE27_train.rf4.5 <- randomForest(data = CADE27_yr4[-CADE27_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                     BCH_percent,
                                   importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)

samp <-trunc((0.2*min(table(CADE27_yr5[-CADE27_flds5[[5]],]$Status))))
CADE27_train.rf5.5 <- randomForest(data = CADE27_yr5[-CADE27_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                     BCH_percent, 
                                   importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                   na.action=na.omit)




##################Create test datasets#####################
##1
CADE27_test1.1 <- CADE27_yr1[CADE27_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(CADE27_yr1[CADE27_flds1[[1]],] $  exclude=NULL)
table(CADE27_test1.1$  exclude=NULL)

CADE27_test2.1 <- CADE27_yr2[CADE27_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test3.1 <- CADE27_yr3[CADE27_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test4.1 <- CADE27_yr4[CADE27_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test5.1 <- CADE27_yr5[CADE27_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
CADE27_test1.2 <- CADE27_yr1[CADE27_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test2.2 <- CADE27_yr2[CADE27_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test3.2 <- CADE27_yr3[CADE27_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test4.2 <- CADE27_yr4[CADE27_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test5.2 <- CADE27_yr5[CADE27_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
CADE27_test1.3 <- CADE27_yr1[CADE27_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test2.3 <- CADE27_yr2[CADE27_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



CADE27_test3.3 <- CADE27_yr3[CADE27_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test4.3 <- CADE27_yr4[CADE27_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test5.3 <- CADE27_yr5[CADE27_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

##4
CADE27_test1.4 <- CADE27_yr1[CADE27_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test2.4 <- CADE27_yr2[CADE27_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

CADE27_test3.4 <- CADE27_yr3[CADE27_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test4.4 <- CADE27_yr4[CADE27_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test5.4 <- CADE27_yr5[CADE27_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

##5
CADE27_test1.5 <- CADE27_yr1[CADE27_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

CADE27_test2.5 <- CADE27_yr2[CADE27_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

CADE27_test3.5 <- CADE27_yr3[CADE27_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test4.5 <- CADE27_yr4[CADE27_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


CADE27_test5.5 <- CADE27_yr5[CADE27_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


#######################Predicting###############################
CADE27_pred.results <-data.frame() 

CADE27_1.1pred <- predict(CADE27_train.rf1.1, newdata=CADE27_test1.1)
CADE27_2.1pred <- predict(CADE27_train.rf2.1, newdata=CADE27_test2.1)
CADE27_3.1pred <- predict(CADE27_train.rf3.1, newdata=CADE27_test3.1)
CADE27_4.1pred <- predict(CADE27_train.rf4.1, newdata=CADE27_test4.1)
CADE27_5.1pred <- predict(CADE27_train.rf5.1, newdata=CADE27_test5.1)

CADE27_1.2pred <- predict(CADE27_train.rf1.2, newdata=CADE27_test1.2)
CADE27_2.2pred <- predict(CADE27_train.rf2.2, newdata=CADE27_test2.2)
CADE27_3.2pred <- predict(CADE27_train.rf3.2, newdata=CADE27_test3.2)
CADE27_4.2pred <- predict(CADE27_train.rf4.2, newdata=CADE27_test4.2)
CADE27_5.2pred <- predict(CADE27_train.rf5.2, newdata=CADE27_test5.2)

CADE27_1.3pred <- predict(CADE27_train.rf1.3, newdata=CADE27_test1.3)
CADE27_2.3pred <- predict(CADE27_train.rf2.3, newdata=CADE27_test2.3)
CADE27_3.3pred <- predict(CADE27_train.rf3.3, newdata=CADE27_test3.3)
CADE27_4.3pred <- predict(CADE27_train.rf4.3, newdata=CADE27_test4.3)
CADE27_5.3pred <- predict(CADE27_train.rf5.3, newdata=CADE27_test5.3)

CADE27_1.4pred <- predict(CADE27_train.rf1.4, newdata=CADE27_test1.4)
CADE27_2.4pred <- predict(CADE27_train.rf2.4, newdata=CADE27_test2.4)
CADE27_3.4pred <- predict(CADE27_train.rf3.4, newdata=CADE27_test3.4)
CADE27_4.4pred <- predict(CADE27_train.rf4.4, newdata=CADE27_test4.4)
CADE27_5.4pred <- predict(CADE27_train.rf5.4, newdata=CADE27_test5.4)

CADE27_1.5pred <- predict(CADE27_train.rf1.5, newdata=CADE27_test1.5)
CADE27_2.5pred <- predict(CADE27_train.rf2.5, newdata=CADE27_test2.5)
CADE27_3.5pred <- predict(CADE27_train.rf3.5, newdata=CADE27_test3.5)
CADE27_4.5pred <- predict(CADE27_train.rf4.5, newdata=CADE27_test4.5)
CADE27_5.5pred <- predict(CADE27_train.rf5.5, newdata=CADE27_test5.5)

length(CADE27_3.1pred)
length(CADE27_test3.1$Status)
View(CADE27.test.preds)
CADE27.test.preds <-data.frame(Status = c(CADE27_test1.1$Status,CADE27_test2.1$Status,CADE27_test3.1$Status,CADE27_test4.1$Status,CADE27_test5.1$Status,
                                          CADE27_test1.2$Status,CADE27_test2.2$Status,CADE27_test3.2$Status,CADE27_test4.2$Status,CADE27_test5.2$Status,
                                          CADE27_test1.3$Status,CADE27_test2.3$Status,CADE27_test3.3$Status,CADE27_test4.3$Status,CADE27_test5.3$Status,
                                          CADE27_test1.4$Status,CADE27_test2.4$Status,CADE27_test3.4$Status,CADE27_test4.4$Status,CADE27_test5.4$Status,
                                          CADE27_test1.5$Status,CADE27_test2.5$Status,CADE27_test3.5$Status,CADE27_test4.5$Status,CADE27_test5.5$Status),
                               preds = c(CADE27_1.1pred,CADE27_2.1pred,CADE27_3.1pred,CADE27_4.1pred,CADE27_5.1pred,
                                         CADE27_1.2pred,CADE27_2.2pred,CADE27_3.2pred,CADE27_4.2pred,CADE27_5.2pred,
                                         CADE27_1.3pred,CADE27_2.3pred,CADE27_3.3pred,CADE27_4.3pred,CADE27_5.3pred,
                                         CADE27_1.4pred,CADE27_2.4pred,CADE27_3.4pred,CADE27_4.4pred,CADE27_5.4pred,
                                         CADE27_1.5pred,CADE27_2.5pred,CADE27_3.5pred,CADE27_4.5pred,CADE27_5.5pred),
                               model = c(rep(1,length(CADE27_1.1pred)),rep(2,length(CADE27_2.1pred)),rep(3,length(CADE27_3.1pred)),rep(4,length(CADE27_4.1pred)),rep(5,length(CADE27_5.1pred)),
                                         rep(1,length(CADE27_1.2pred)),rep(2,length(CADE27_2.2pred)),rep(3,length(CADE27_3.2pred)),rep(4,length(CADE27_4.2pred)),rep(5,length(CADE27_5.2pred)),
                                         rep(1,length(CADE27_1.3pred)),rep(2,length(CADE27_2.3pred)),rep(3,length(CADE27_3.3pred)),rep(4,length(CADE27_4.3pred)),rep(5,length(CADE27_5.3pred)),
                                         rep(1,length(CADE27_1.4pred)),rep(2,length(CADE27_2.4pred)),rep(3,length(CADE27_3.4pred)),rep(4,length(CADE27_4.4pred)),rep(5,length(CADE27_5.4pred)),
                                         rep(1,length(CADE27_1.5pred)),rep(2,length(CADE27_2.5pred)),rep(3,length(CADE27_3.5pred)),rep(4,length(CADE27_4.5pred)),rep(5,length(CADE27_5.5pred))),
                               rep = c(rep(1,length(CADE27_1.1pred)),rep(1,length(CADE27_2.1pred)),rep(1,length(CADE27_3.1pred)),rep(1,length(CADE27_4.1pred)),rep(1,length(CADE27_5.1pred)),
                                       rep(2,length(CADE27_1.2pred)),rep(2,length(CADE27_2.2pred)),rep(2,length(CADE27_3.2pred)),rep(2,length(CADE27_4.2pred)),rep(2,length(CADE27_5.2pred)),
                                       rep(3,length(CADE27_1.3pred)),rep(3,length(CADE27_2.3pred)),rep(3,length(CADE27_3.3pred)),rep(3,length(CADE27_4.3pred)),rep(3,length(CADE27_5.3pred)),
                                       rep(4,length(CADE27_1.4pred)),rep(4,length(CADE27_2.4pred)),rep(4,length(CADE27_3.4pred)),rep(4,length(CADE27_4.4pred)),rep(4,length(CADE27_5.4pred)),
                                       rep(5,length(CADE27_1.5pred)),rep(5,length(CADE27_2.5pred)),rep(5,length(CADE27_3.5pred)),rep(5,length(CADE27_4.5pred)),rep(5,length(CADE27_5.5pred))),

                               dead_check = c(CADE27_test1.1$dead_check, CADE27_test2.1$dead_check, CADE27_test3.1$dead_check, CADE27_test4.1$dead_check, CADE27_test5.1$dead_check,
                                              CADE27_test1.2$dead_check, CADE27_test2.2$dead_check, CADE27_test3.2$dead_check, CADE27_test4.2$dead_check, CADE27_test5.2$dead_check,
                                              CADE27_test1.3$dead_check, CADE27_test2.3$dead_check, CADE27_test3.3$dead_check, CADE27_test4.3$dead_check, CADE27_test5.3$dead_check,
                                              CADE27_test1.4$dead_check, CADE27_test2.4$dead_check, CADE27_test3.4$dead_check, CADE27_test4.4$dead_check, CADE27_test5.4$dead_check,
                                              CADE27_test1.5$dead_check, CADE27_test2.5$dead_check, CADE27_test3.5$dead_check, CADE27_test4.5$dead_check, CADE27_test5.5$dead_check))


CADE27.test.preds$wrong <- ifelse(CADE27.test.preds$Status == CADE27.test.preds$preds, 0,1)

CADE27.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> CADE27.wrong.preds





CADE27_RF1.1.tab <-table(factor(CADE27_test1.1$Status, levels = 0:1), factor(CADE27_1.1pred, levels = 0:1))
CADE27_LiveAcc1.1<-CADE27_RF1.1.tab[1,1]/sum(CADE27_RF1.1.tab[1,])
CADE27_DeadAcc1.1<-CADE27_RF1.1.tab[2,2]/sum(CADE27_RF1.1.tab[2,])
CADE27_TotalAcc1.1<-(CADE27_RF1.1.tab[1,1]+CADE27_RF1.1.tab[2,2])/sum(CADE27_RF1.1.tab)

CADE27_RF2.1.tab <-table(factor(CADE27_test2.1$Status, levels = 0:1), factor(CADE27_2.1pred, levels = 0:1))
CADE27_LiveAcc2.1<-CADE27_RF2.1.tab[1,1]/sum(CADE27_RF2.1.tab[1,])
CADE27_DeadAcc2.1<-CADE27_RF2.1.tab[2,2]/sum(CADE27_RF2.1.tab[2,])
CADE27_TotalAcc2.1<-(CADE27_RF2.1.tab[1,1]+CADE27_RF2.1.tab[2,2])/sum(CADE27_RF2.1.tab)

CADE27_RF3.1.tab <-table(factor(CADE27_test3.1$Status, levels = 0:1), factor(CADE27_3.1pred, levels = 0:1))
CADE27_LiveAcc3.1<-CADE27_RF3.1.tab[1,1]/sum(CADE27_RF3.1.tab[1,])
CADE27_DeadAcc3.1<-CADE27_RF3.1.tab[2,2]/sum(CADE27_RF3.1.tab[2,])
CADE27_TotalAcc3.1<-(CADE27_RF3.1.tab[1,1]+CADE27_RF3.1.tab[2,2])/sum(CADE27_RF3.1.tab)

CADE27_RF4.1.tab <-table(factor(CADE27_test4.1$Status, levels = 0:1), factor(CADE27_4.1pred, levels = 0:1))
CADE27_LiveAcc4.1<-CADE27_RF4.1.tab[1,1]/sum(CADE27_RF4.1.tab[1,])
CADE27_DeadAcc4.1<-CADE27_RF4.1.tab[2,2]/sum(CADE27_RF4.1.tab[2,])
CADE27_TotalAcc4.1<-(CADE27_RF4.1.tab[1,1]+CADE27_RF4.1.tab[2,2])/sum(CADE27_RF4.1.tab)

CADE27_RF5.1.tab <-table(factor(CADE27_test5.1$Status, levels = 0:1), factor(CADE27_5.1pred, levels = 0:1))
CADE27_LiveAcc5.1<-CADE27_RF5.1.tab[1,1]/sum(CADE27_RF5.1.tab[1,])
CADE27_DeadAcc5.1<-CADE27_RF5.1.tab[2,2]/sum(CADE27_RF5.1.tab[2,])
CADE27_TotalAcc5.1<-(CADE27_RF5.1.tab[1,1]+CADE27_RF5.1.tab[2,2])/sum(CADE27_RF5.1.tab)

CADE27_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(CADE27_LiveAcc1.1,
                                                                                CADE27_LiveAcc2.1,
                                                                                CADE27_LiveAcc3.1,
                                                                                CADE27_LiveAcc4.1,
                                                                                CADE27_LiveAcc5.1),
                                DeadAcc = c(CADE27_DeadAcc1.1,
                                            CADE27_DeadAcc2.1,
                                            CADE27_DeadAcc3.1,
                                            CADE27_DeadAcc4.1,
                                            CADE27_DeadAcc5.1),
                                TotalAcc = c(CADE27_TotalAcc1.1,
                                             CADE27_TotalAcc2.1,
                                             CADE27_TotalAcc3.1,
                                             CADE27_TotalAcc4.1,
                                             CADE27_TotalAcc5.1))


CADE27_RF1.2.tab <-table(factor(CADE27_test1.2$Status, levels = 0:1), factor(CADE27_1.2pred, levels = 0:1))
CADE27_LiveAcc1.2<-CADE27_RF1.2.tab[1,1]/sum(CADE27_RF1.2.tab[1,])
CADE27_DeadAcc1.2<-CADE27_RF1.2.tab[2,2]/sum(CADE27_RF1.2.tab[2,])
CADE27_TotalAcc1.2<-(CADE27_RF1.2.tab[1,1]+CADE27_RF1.2.tab[2,2])/sum(CADE27_RF1.2.tab)

CADE27_RF2.2.tab <-table(factor(CADE27_test2.2$Status, levels = 0:1), factor(CADE27_2.2pred, levels = 0:1))
CADE27_LiveAcc2.2<-CADE27_RF2.2.tab[1,1]/sum(CADE27_RF2.2.tab[1,])
CADE27_DeadAcc2.2<-CADE27_RF2.2.tab[2,2]/sum(CADE27_RF2.2.tab[2,])
CADE27_TotalAcc2.2<-(CADE27_RF2.2.tab[1,1]+CADE27_RF2.2.tab[2,2])/sum(CADE27_RF2.2.tab)

CADE27_RF3.2.tab <-table(factor(CADE27_test3.2$Status, levels = 0:1), factor(CADE27_3.2pred, levels = 0:1))
CADE27_LiveAcc3.2<-CADE27_RF3.2.tab[1,1]/sum(CADE27_RF3.2.tab[1,])
CADE27_DeadAcc3.2<-CADE27_RF3.2.tab[2,2]/sum(CADE27_RF3.2.tab[2,])
CADE27_TotalAcc3.2<-(CADE27_RF3.2.tab[1,1]+CADE27_RF3.2.tab[2,2])/sum(CADE27_RF3.2.tab)

CADE27_RF4.2.tab <-table(factor(CADE27_test4.2$Status, levels = 0:1), factor(CADE27_4.2pred, levels = 0:1))
CADE27_LiveAcc4.2<-CADE27_RF4.2.tab[1,1]/sum(CADE27_RF4.2.tab[1,])
CADE27_DeadAcc4.2<-CADE27_RF4.2.tab[2,2]/sum(CADE27_RF4.2.tab[2,])
CADE27_TotalAcc4.2<-(CADE27_RF4.2.tab[1,1]+CADE27_RF4.2.tab[2,2])/sum(CADE27_RF4.2.tab)

CADE27_RF5.2.tab <-table(factor(CADE27_test5.2$Status, levels = 0:1), factor(CADE27_5.2pred, levels = 0:1))
CADE27_LiveAcc5.2<-CADE27_RF5.2.tab[1,1]/sum(CADE27_RF5.2.tab[1,])
CADE27_DeadAcc5.2<-CADE27_RF5.2.tab[2,2]/sum(CADE27_RF5.2.tab[2,])
CADE27_TotalAcc5.2<-(CADE27_RF5.2.tab[1,1]+CADE27_RF5.2.tab[2,2])/sum(CADE27_RF5.2.tab)

CADE27_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(CADE27_LiveAcc1.2,
                                                                                CADE27_LiveAcc2.2,
                                                                                CADE27_LiveAcc3.2,
                                                                                CADE27_LiveAcc4.2,
                                                                                CADE27_LiveAcc5.2),
                                DeadAcc = c(CADE27_DeadAcc1.2,
                                            CADE27_DeadAcc2.2,
                                            CADE27_DeadAcc3.2,
                                            CADE27_DeadAcc4.2,
                                            CADE27_DeadAcc5.2),
                                TotalAcc = c(CADE27_TotalAcc1.2,
                                             CADE27_TotalAcc2.2,
                                             CADE27_TotalAcc3.2,
                                             CADE27_TotalAcc4.2,
                                             CADE27_TotalAcc5.2))

CADE27_RF1.3.tab <-table(factor(CADE27_test1.3$Status, levels = 0:1), factor(CADE27_1.3pred, levels = 0:1))
CADE27_LiveAcc1.3<-CADE27_RF1.3.tab[1,1]/sum(CADE27_RF1.3.tab[1,])
CADE27_DeadAcc1.3<-CADE27_RF1.3.tab[2,2]/sum(CADE27_RF1.3.tab[2,])
CADE27_TotalAcc1.3<-(CADE27_RF1.3.tab[1,1]+CADE27_RF1.3.tab[2,2])/sum(CADE27_RF1.3.tab)

CADE27_RF2.3.tab <-table(factor(CADE27_test2.3$Status, levels = 0:1), factor(CADE27_2.3pred, levels = 0:1))
CADE27_LiveAcc2.3<-CADE27_RF2.3.tab[1,1]/sum(CADE27_RF2.3.tab[1,])
CADE27_DeadAcc2.3<-CADE27_RF2.3.tab[2,2]/sum(CADE27_RF2.3.tab[2,])
CADE27_TotalAcc2.3<-(CADE27_RF2.3.tab[1,1]+CADE27_RF2.3.tab[2,2])/sum(CADE27_RF2.3.tab)

CADE27_RF3.3.tab <-table(factor(CADE27_test3.3$Status, levels = 0:1), factor(CADE27_3.3pred, levels = 0:1))
CADE27_LiveAcc3.3<-CADE27_RF3.3.tab[1,1]/sum(CADE27_RF3.3.tab[1,])
CADE27_DeadAcc3.3<-CADE27_RF3.3.tab[2,2]/sum(CADE27_RF3.3.tab[2,])
CADE27_TotalAcc3.3<-(CADE27_RF3.3.tab[1,1]+CADE27_RF3.3.tab[2,2])/sum(CADE27_RF3.3.tab)

CADE27_RF4.3.tab <-table(factor(CADE27_test4.3$Status, levels = 0:1), factor(CADE27_4.3pred, levels = 0:1))
CADE27_LiveAcc4.3<-CADE27_RF4.3.tab[1,1]/sum(CADE27_RF4.3.tab[1,])
CADE27_DeadAcc4.3<-CADE27_RF4.3.tab[2,2]/sum(CADE27_RF4.3.tab[2,])
CADE27_TotalAcc4.3<-(CADE27_RF4.3.tab[1,1]+CADE27_RF4.3.tab[2,2])/sum(CADE27_RF4.3.tab)

CADE27_RF5.3.tab <-table(factor(CADE27_test5.3$Status, levels = 0:1), factor(CADE27_5.3pred, levels = 0:1))
CADE27_LiveAcc5.3<-CADE27_RF5.3.tab[1,1]/sum(CADE27_RF5.3.tab[1,])
CADE27_DeadAcc5.3<-CADE27_RF5.3.tab[2,2]/sum(CADE27_RF5.3.tab[2,])
CADE27_TotalAcc5.3<-(CADE27_RF5.3.tab[1,1]+CADE27_RF5.3.tab[2,2])/sum(CADE27_RF5.3.tab)

CADE27_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(CADE27_LiveAcc1.3,
                                                                                CADE27_LiveAcc2.3,
                                                                                CADE27_LiveAcc3.3,
                                                                                CADE27_LiveAcc4.3,
                                                                                CADE27_LiveAcc5.3),
                                DeadAcc = c(CADE27_DeadAcc1.3,
                                            CADE27_DeadAcc2.3,
                                            CADE27_DeadAcc3.3,
                                            CADE27_DeadAcc4.3,
                                            CADE27_DeadAcc5.3),
                                TotalAcc = c(CADE27_TotalAcc1.3,
                                             CADE27_TotalAcc2.3,
                                             CADE27_TotalAcc3.3,
                                             CADE27_TotalAcc4.3,
                                             CADE27_TotalAcc5.3))

CADE27_RF1.4.tab <-table(factor(CADE27_test1.4$Status, levels = 0:1), factor(CADE27_1.4pred, levels = 0:1))
CADE27_LiveAcc1.4<-CADE27_RF1.4.tab[1,1]/sum(CADE27_RF1.4.tab[1,])
CADE27_DeadAcc1.4<-CADE27_RF1.4.tab[2,2]/sum(CADE27_RF1.4.tab[2,])
CADE27_TotalAcc1.4<-(CADE27_RF1.4.tab[1,1]+CADE27_RF1.4.tab[2,2])/sum(CADE27_RF1.4.tab)

CADE27_RF2.4.tab <-table(factor(CADE27_test2.4$Status, levels = 0:1), factor(CADE27_2.4pred, levels = 0:1))
CADE27_LiveAcc2.4<-CADE27_RF2.4.tab[1,1]/sum(CADE27_RF2.4.tab[1,])
CADE27_DeadAcc2.4<-CADE27_RF2.4.tab[2,2]/sum(CADE27_RF2.4.tab[2,])
CADE27_TotalAcc2.4<-(CADE27_RF2.4.tab[1,1]+CADE27_RF2.4.tab[2,2])/sum(CADE27_RF2.4.tab)

CADE27_RF3.4.tab <-table(factor(CADE27_test3.4$Status, levels = 0:1), factor(CADE27_3.4pred, levels = 0:1))
CADE27_LiveAcc3.4<-CADE27_RF3.4.tab[1,1]/sum(CADE27_RF3.4.tab[1,])
CADE27_DeadAcc3.4<-CADE27_RF3.4.tab[2,2]/sum(CADE27_RF3.4.tab[2,])
CADE27_TotalAcc3.4<-(CADE27_RF3.4.tab[1,1]+CADE27_RF3.4.tab[2,2])/sum(CADE27_RF3.4.tab)

CADE27_RF4.4.tab <-table(factor(CADE27_test4.4$Status, levels = 0:1), factor(CADE27_4.4pred, levels = 0:1))
CADE27_LiveAcc4.4<-CADE27_RF4.4.tab[1,1]/sum(CADE27_RF4.4.tab[1,])
CADE27_DeadAcc4.4<-CADE27_RF4.4.tab[2,2]/sum(CADE27_RF4.4.tab[2,])
CADE27_TotalAcc4.4<-(CADE27_RF4.4.tab[1,1]+CADE27_RF4.4.tab[2,2])/sum(CADE27_RF4.4.tab)

CADE27_RF5.4.tab <-table(factor(CADE27_test5.4$Status, levels = 0:1), factor(CADE27_5.4pred, levels = 0:1))
CADE27_LiveAcc5.4<-CADE27_RF5.4.tab[1,1]/sum(CADE27_RF5.4.tab[1,])
CADE27_DeadAcc5.4<-CADE27_RF5.4.tab[2,2]/sum(CADE27_RF5.4.tab[2,])
CADE27_TotalAcc5.4<-(CADE27_RF5.4.tab[1,1]+CADE27_RF5.4.tab[2,2])/sum(CADE27_RF5.4.tab)

CADE27_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(CADE27_LiveAcc1.4,
                                                                                CADE27_LiveAcc2.4,
                                                                                CADE27_LiveAcc3.4,
                                                                                CADE27_LiveAcc4.4,
                                                                                CADE27_LiveAcc5.4),
                                DeadAcc = c(CADE27_DeadAcc1.4,
                                            CADE27_DeadAcc2.4,
                                            CADE27_DeadAcc3.4,
                                            CADE27_DeadAcc4.4,
                                            CADE27_DeadAcc5.4),
                                TotalAcc = c(CADE27_TotalAcc1.4,
                                             CADE27_TotalAcc2.4,
                                             CADE27_TotalAcc3.4,
                                             CADE27_TotalAcc4.4,
                                             CADE27_TotalAcc5.4))

CADE27_RF1.5.tab <-table(factor(CADE27_test1.5$Status, levels = 0:1), factor(CADE27_1.5pred, levels = 0:1))
CADE27_LiveAcc1.5<-CADE27_RF1.5.tab[1,1]/sum(CADE27_RF1.5.tab[1,])
CADE27_DeadAcc1.5<-CADE27_RF1.5.tab[2,2]/sum(CADE27_RF1.5.tab[2,])
CADE27_TotalAcc1.5<-(CADE27_RF1.5.tab[1,1]+CADE27_RF1.5.tab[2,2])/sum(CADE27_RF1.5.tab)

CADE27_RF2.5.tab <-table(factor(CADE27_test2.5$Status, levels = 0:1), factor(CADE27_2.5pred, levels = 0:1))
CADE27_LiveAcc2.5<-CADE27_RF2.5.tab[1,1]/sum(CADE27_RF2.5.tab[1,])
CADE27_DeadAcc2.5<-CADE27_RF2.5.tab[2,2]/sum(CADE27_RF2.5.tab[2,])
CADE27_TotalAcc2.5<-(CADE27_RF2.5.tab[1,1]+CADE27_RF2.5.tab[2,2])/sum(CADE27_RF2.5.tab)

CADE27_RF3.5.tab <-table(factor(CADE27_test3.5$Status, levels = 0:1), factor(CADE27_3.5pred, levels = 0:1))
CADE27_LiveAcc3.5<-CADE27_RF3.5.tab[1,1]/sum(CADE27_RF3.5.tab[1,])
CADE27_DeadAcc3.5<-CADE27_RF3.5.tab[2,2]/sum(CADE27_RF3.5.tab[2,])
CADE27_TotalAcc3.5<-(CADE27_RF3.5.tab[1,1]+CADE27_RF3.5.tab[2,2])/sum(CADE27_RF3.5.tab)

CADE27_RF4.5.tab <-table(factor(CADE27_test4.5$Status, levels = 0:1), factor(CADE27_4.5pred, levels = 0:1))
CADE27_LiveAcc4.5<-CADE27_RF4.5.tab[1,1]/sum(CADE27_RF4.5.tab[1,])
CADE27_DeadAcc4.5<-CADE27_RF4.5.tab[2,2]/sum(CADE27_RF4.5.tab[2,])
CADE27_TotalAcc4.5<-(CADE27_RF4.5.tab[1,1]+CADE27_RF4.5.tab[2,2])/sum(CADE27_RF4.5.tab)

CADE27_RF5.5.tab <-table(factor(CADE27_test5.5$Status, levels = 0:1), factor(CADE27_5.5pred, levels = 0:1))
CADE27_LiveAcc5.5<-CADE27_RF5.5.tab[1,1]/sum(CADE27_RF5.5.tab[1,])
CADE27_DeadAcc5.5<-CADE27_RF5.5.tab[2,2]/sum(CADE27_RF5.5.tab[2,])
CADE27_TotalAcc5.5<-(CADE27_RF5.5.tab[1,1]+CADE27_RF5.5.tab[2,2])/sum(CADE27_RF5.5.tab)

CADE27_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(CADE27_LiveAcc1.5,
                                                                                CADE27_LiveAcc2.5,
                                                                                CADE27_LiveAcc3.5,
                                                                                CADE27_LiveAcc4.5,
                                                                                CADE27_LiveAcc5.5),
                                DeadAcc = c(CADE27_DeadAcc1.5,
                                            CADE27_DeadAcc2.5,
                                            CADE27_DeadAcc3.5,
                                            CADE27_DeadAcc4.5,
                                            CADE27_DeadAcc5.5),
                                TotalAcc = c(CADE27_TotalAcc1.5,
                                             CADE27_TotalAcc2.5,
                                             CADE27_TotalAcc3.5,
                                             CADE27_TotalAcc4.5,
                                             CADE27_TotalAcc5.5))

CADE27_pred.results <-rbind(CADE27_Pre.results1,CADE27_Pre.results2,CADE27_Pre.results3,CADE27_Pre.results4,CADE27_Pre.results5)



CADE27_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "CADE27",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> CADE27_RF_preds.sum




#############LAOC######################
#############subset species###########

LAOC <- subset(Tree_data, Species == 'LAOC')
LAOC$OBS_ID <- 1:nrow(LAOC)

###Get complete cases##########
LAOC_yr1 <- select(LAOC, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)

LAOC_yr2 <- select(LAOC, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

LAOC_yr3 <- select(LAOC, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

LAOC_yr4 <- select(LAOC, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

LAOC_yr5 <- select(LAOC, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
LAOC_flds1<-createFolds(LAOC_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
LAOC_flds2<-createFolds(LAOC_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
LAOC_flds3<-createFolds(LAOC_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
LAOC_flds4<-createFolds(LAOC_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
LAOC_flds5<-createFolds(LAOC_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 






##1
set.seed(1980)
samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[1]],]$Status))))
LAOC_train.rf1.1 <- randomForest(data = LAOC_yr1[-LAOC_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr2[-LAOC_flds2[[1]],]$Status))))
LAOC_train.rf2.1 <- randomForest(data = LAOC_yr2[-LAOC_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(LAOC_yr3[-LAOC_flds3[[1]],]$Status))))
LAOC_train.rf3.1 <- randomForest(data = LAOC_yr3[-LAOC_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr4[-LAOC_flds4[[1]],]$Status))))
LAOC_train.rf4.1 <- randomForest(data = LAOC_yr4[-LAOC_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr5[-LAOC_flds5[[1]],]$Status))))
LAOC_train.rf5.1 <- randomForest(data = LAOC_yr5[-LAOC_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(LAOC_train.rf5.1)

##2

samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[2]],]$Status))))
LAOC_train.rf1.2 <- randomForest(data = LAOC_yr1[-LAOC_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr2[-LAOC_flds2[[2]],]$Status))))
LAOC_train.rf2.2 <- randomForest(data = LAOC_yr2[-LAOC_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(LAOC_yr3[-LAOC_flds3[[2]],]$Status))))
LAOC_train.rf3.2 <- randomForest(data = LAOC_yr3[-LAOC_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr4[-LAOC_flds4[[2]],]$Status))))
LAOC_train.rf4.2 <- randomForest(data = LAOC_yr4[-LAOC_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr5[-LAOC_flds5[[2]],]$Status))))
LAOC_train.rf5.2 <- randomForest(data = LAOC_yr5[-LAOC_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[3]],]$Status))))
LAOC_train.rf1.3 <- randomForest(data = LAOC_yr1[-LAOC_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr2[-LAOC_flds2[[3]],]$Status))))
LAOC_train.rf2.3 <- randomForest(data = LAOC_yr2[-LAOC_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(LAOC_yr3[-LAOC_flds3[[3]],]$Status))))
LAOC_train.rf3.3 <- randomForest(data = LAOC_yr3[-LAOC_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr4[-LAOC_flds4[[3]],]$Status))))
LAOC_train.rf4.3 <- randomForest(data = LAOC_yr4[-LAOC_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr5[-LAOC_flds5[[3]],]$Status))))
LAOC_train.rf5.3 <- randomForest(data = LAOC_yr5[-LAOC_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[4]],]$Status))))
LAOC_train.rf1.4 <- randomForest(data = LAOC_yr1[-LAOC_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr2[-LAOC_flds2[[4]],]$Status))))
LAOC_train.rf2.4 <- randomForest(data = LAOC_yr2[-LAOC_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(LAOC_yr3[-LAOC_flds3[[4]],]$Status))))
LAOC_train.rf3.4 <- randomForest(data = LAOC_yr3[-LAOC_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr4[-LAOC_flds4[[4]],]$Status))))
LAOC_train.rf4.4 <- randomForest(data = LAOC_yr4[-LAOC_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr5[-LAOC_flds5[[4]],]$Status))))
LAOC_train.rf5.4 <- randomForest(data = LAOC_yr5[-LAOC_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(LAOC_yr1[-LAOC_flds1[[5]],]$Status))))
LAOC_train.rf1.5 <- randomForest(data = LAOC_yr1[-LAOC_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr2[-LAOC_flds2[[5]],]$Status))))
LAOC_train.rf2.5 <- randomForest(data = LAOC_yr2[-LAOC_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(LAOC_yr3[-LAOC_flds3[[5]],]$Status))))
LAOC_train.rf3.5 <- randomForest(data = LAOC_yr3[-LAOC_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr4[-LAOC_flds4[[5]],]$Status))))
LAOC_train.rf4.5 <- randomForest(data = LAOC_yr4[-LAOC_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(LAOC_yr5[-LAOC_flds5[[5]],]$Status))))
LAOC_train.rf5.5 <- randomForest(data = LAOC_yr5[-LAOC_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)




##################Create test datasets#####################
##1
LAOC_test1.1 <- LAOC_yr1[LAOC_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(LAOC_yr1[LAOC_flds1[[1]],] $  exclude=NULL)
table(LAOC_test1.1$  exclude=NULL)

LAOC_test2.1 <- LAOC_yr2[LAOC_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test3.1 <- LAOC_yr3[LAOC_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test4.1 <- LAOC_yr4[LAOC_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test5.1 <- LAOC_yr5[LAOC_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
LAOC_test1.2 <- LAOC_yr1[LAOC_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test2.2 <- LAOC_yr2[LAOC_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test3.2 <- LAOC_yr3[LAOC_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test4.2 <- LAOC_yr4[LAOC_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test5.2 <- LAOC_yr5[LAOC_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
LAOC_test1.3 <- LAOC_yr1[LAOC_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test2.3 <- LAOC_yr2[LAOC_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test3.3 <- LAOC_yr3[LAOC_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test4.3 <- LAOC_yr4[LAOC_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test5.3 <- LAOC_yr5[LAOC_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

##4
LAOC_test1.4 <- LAOC_yr1[LAOC_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test2.4 <- LAOC_yr2[LAOC_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test3.4 <- LAOC_yr3[LAOC_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test4.4 <- LAOC_yr4[LAOC_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test5.4 <- LAOC_yr5[LAOC_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##5
LAOC_test1.5 <- LAOC_yr1[LAOC_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test2.5 <- LAOC_yr2[LAOC_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test3.5 <- LAOC_yr3[LAOC_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test4.5 <- LAOC_yr4[LAOC_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


LAOC_test5.5 <- LAOC_yr5[LAOC_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


#######################Predicting###############################
LAOC_pred.results <-data.frame() 

LAOC_1.1pred <- predict(LAOC_train.rf1.1, newdata=LAOC_test1.1)
LAOC_2.1pred <- predict(LAOC_train.rf2.1, newdata=LAOC_test2.1)
LAOC_3.1pred <- predict(LAOC_train.rf3.1, newdata=LAOC_test3.1)
LAOC_4.1pred <- predict(LAOC_train.rf4.1, newdata=LAOC_test4.1)
LAOC_5.1pred <- predict(LAOC_train.rf5.1, newdata=LAOC_test5.1)

LAOC_1.2pred <- predict(LAOC_train.rf1.2, newdata=LAOC_test1.2)
LAOC_2.2pred <- predict(LAOC_train.rf2.2, newdata=LAOC_test2.2)
LAOC_3.2pred <- predict(LAOC_train.rf3.2, newdata=LAOC_test3.2)
LAOC_4.2pred <- predict(LAOC_train.rf4.2, newdata=LAOC_test4.2)
LAOC_5.2pred <- predict(LAOC_train.rf5.2, newdata=LAOC_test5.2)

LAOC_1.3pred <- predict(LAOC_train.rf1.3, newdata=LAOC_test1.3)
LAOC_2.3pred <- predict(LAOC_train.rf2.3, newdata=LAOC_test2.3)
LAOC_3.3pred <- predict(LAOC_train.rf3.3, newdata=LAOC_test3.3)
LAOC_4.3pred <- predict(LAOC_train.rf4.3, newdata=LAOC_test4.3)
LAOC_5.3pred <- predict(LAOC_train.rf5.3, newdata=LAOC_test5.3)

LAOC_1.4pred <- predict(LAOC_train.rf1.4, newdata=LAOC_test1.4)
LAOC_2.4pred <- predict(LAOC_train.rf2.4, newdata=LAOC_test2.4)
LAOC_3.4pred <- predict(LAOC_train.rf3.4, newdata=LAOC_test3.4)
LAOC_4.4pred <- predict(LAOC_train.rf4.4, newdata=LAOC_test4.4)
LAOC_5.4pred <- predict(LAOC_train.rf5.4, newdata=LAOC_test5.4)

LAOC_1.5pred <- predict(LAOC_train.rf1.5, newdata=LAOC_test1.5)
LAOC_2.5pred <- predict(LAOC_train.rf2.5, newdata=LAOC_test2.5)
LAOC_3.5pred <- predict(LAOC_train.rf3.5, newdata=LAOC_test3.5)
LAOC_4.5pred <- predict(LAOC_train.rf4.5, newdata=LAOC_test4.5)
LAOC_5.5pred <- predict(LAOC_train.rf5.5, newdata=LAOC_test5.5)

length(LAOC_3.1pred)
length(LAOC_test3.1$Status)

LAOC.test.preds <-data.frame(Status = c(LAOC_test1.1$Status,LAOC_test2.1$Status,LAOC_test3.1$Status,LAOC_test4.1$Status,LAOC_test5.1$Status,
                                        LAOC_test1.2$Status,LAOC_test2.2$Status,LAOC_test3.2$Status,LAOC_test4.2$Status,LAOC_test5.2$Status,
                                        LAOC_test1.3$Status,LAOC_test2.3$Status,LAOC_test3.3$Status,LAOC_test4.3$Status,LAOC_test5.3$Status,
                                        LAOC_test1.4$Status,LAOC_test2.4$Status,LAOC_test3.4$Status,LAOC_test4.4$Status,LAOC_test5.4$Status,
                                        LAOC_test1.5$Status,LAOC_test2.5$Status,LAOC_test3.5$Status,LAOC_test4.5$Status,LAOC_test5.5$Status),
                             preds = c(LAOC_1.1pred,LAOC_2.1pred,LAOC_3.1pred,LAOC_4.1pred,LAOC_5.1pred,
                                       LAOC_1.2pred,LAOC_2.2pred,LAOC_3.2pred,LAOC_4.2pred,LAOC_5.2pred,
                                       LAOC_1.3pred,LAOC_2.3pred,LAOC_3.3pred,LAOC_4.3pred,LAOC_5.3pred,
                                       LAOC_1.4pred,LAOC_2.4pred,LAOC_3.4pred,LAOC_4.4pred,LAOC_5.4pred,
                                       LAOC_1.5pred,LAOC_2.5pred,LAOC_3.5pred,LAOC_4.5pred,LAOC_5.5pred),
                             model = c(rep(1,length(LAOC_1.1pred)),rep(2,length(LAOC_2.1pred)),rep(3,length(LAOC_3.1pred)),rep(4,length(LAOC_4.1pred)),rep(5,length(LAOC_5.1pred)),
                                       rep(1,length(LAOC_1.2pred)),rep(2,length(LAOC_2.2pred)),rep(3,length(LAOC_3.2pred)),rep(4,length(LAOC_4.2pred)),rep(5,length(LAOC_5.2pred)),
                                       rep(1,length(LAOC_1.3pred)),rep(2,length(LAOC_2.3pred)),rep(3,length(LAOC_3.3pred)),rep(4,length(LAOC_4.3pred)),rep(5,length(LAOC_5.3pred)),
                                       rep(1,length(LAOC_1.4pred)),rep(2,length(LAOC_2.4pred)),rep(3,length(LAOC_3.4pred)),rep(4,length(LAOC_4.4pred)),rep(5,length(LAOC_5.4pred)),
                                       rep(1,length(LAOC_1.5pred)),rep(2,length(LAOC_2.5pred)),rep(3,length(LAOC_3.5pred)),rep(4,length(LAOC_4.5pred)),rep(5,length(LAOC_5.5pred))),
                             rep = c(rep(1,length(LAOC_1.1pred)),rep(1,length(LAOC_2.1pred)),rep(1,length(LAOC_3.1pred)),rep(1,length(LAOC_4.1pred)),rep(1,length(LAOC_5.1pred)),
                                     rep(2,length(LAOC_1.2pred)),rep(2,length(LAOC_2.2pred)),rep(2,length(LAOC_3.2pred)),rep(2,length(LAOC_4.2pred)),rep(2,length(LAOC_5.2pred)),
                                     rep(3,length(LAOC_1.3pred)),rep(3,length(LAOC_2.3pred)),rep(3,length(LAOC_3.3pred)),rep(3,length(LAOC_4.3pred)),rep(3,length(LAOC_5.3pred)),
                                     rep(4,length(LAOC_1.4pred)),rep(4,length(LAOC_2.4pred)),rep(4,length(LAOC_3.4pred)),rep(4,length(LAOC_4.4pred)),rep(4,length(LAOC_5.4pred)),
                                     rep(5,length(LAOC_1.5pred)),rep(5,length(LAOC_2.5pred)),rep(5,length(LAOC_3.5pred)),rep(5,length(LAOC_4.5pred)),rep(5,length(LAOC_5.5pred))),
                            
                             dead_check = c(LAOC_test1.1$dead_check, LAOC_test2.1$dead_check, LAOC_test3.1$dead_check, LAOC_test4.1$dead_check, LAOC_test5.1$dead_check,
                                            LAOC_test1.2$dead_check, LAOC_test2.2$dead_check, LAOC_test3.2$dead_check, LAOC_test4.2$dead_check, LAOC_test5.2$dead_check,
                                            LAOC_test1.3$dead_check, LAOC_test2.3$dead_check, LAOC_test3.3$dead_check, LAOC_test4.3$dead_check, LAOC_test5.3$dead_check,
                                            LAOC_test1.4$dead_check, LAOC_test2.4$dead_check, LAOC_test3.4$dead_check, LAOC_test4.4$dead_check, LAOC_test5.4$dead_check,
                                            LAOC_test1.5$dead_check, LAOC_test2.5$dead_check, LAOC_test3.5$dead_check, LAOC_test4.5$dead_check, LAOC_test5.5$dead_check))



LAOC.test.preds$wrong <- ifelse(LAOC.test.preds$Status == LAOC.test.preds$preds, 0,1)

LAOC.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> LAOC.wrong.preds




LAOC_RF1.1.tab <-table(factor(LAOC_test1.1$Status, levels = 0:1), factor(LAOC_1.1pred, levels = 0:1))
LAOC_LiveAcc1.1<-LAOC_RF1.1.tab[1,1]/sum(LAOC_RF1.1.tab[1,])
LAOC_DeadAcc1.1<-LAOC_RF1.1.tab[2,2]/sum(LAOC_RF1.1.tab[2,])
LAOC_TotalAcc1.1<-(LAOC_RF1.1.tab[1,1]+LAOC_RF1.1.tab[2,2])/sum(LAOC_RF1.1.tab)

LAOC_RF2.1.tab <-table(factor(LAOC_test2.1$Status, levels = 0:1), factor(LAOC_2.1pred, levels = 0:1))
LAOC_LiveAcc2.1<-LAOC_RF2.1.tab[1,1]/sum(LAOC_RF2.1.tab[1,])
LAOC_DeadAcc2.1<-LAOC_RF2.1.tab[2,2]/sum(LAOC_RF2.1.tab[2,])
LAOC_TotalAcc2.1<-(LAOC_RF2.1.tab[1,1]+LAOC_RF2.1.tab[2,2])/sum(LAOC_RF2.1.tab)

LAOC_RF3.1.tab <-table(factor(LAOC_test3.1$Status, levels = 0:1), factor(LAOC_3.1pred, levels = 0:1))
LAOC_LiveAcc3.1<-LAOC_RF3.1.tab[1,1]/sum(LAOC_RF3.1.tab[1,])
LAOC_DeadAcc3.1<-LAOC_RF3.1.tab[2,2]/sum(LAOC_RF3.1.tab[2,])
LAOC_TotalAcc3.1<-(LAOC_RF3.1.tab[1,1]+LAOC_RF3.1.tab[2,2])/sum(LAOC_RF3.1.tab)

LAOC_RF4.1.tab <-table(factor(LAOC_test4.1$Status, levels = 0:1), factor(LAOC_4.1pred, levels = 0:1))
LAOC_LiveAcc4.1<-LAOC_RF4.1.tab[1,1]/sum(LAOC_RF4.1.tab[1,])
LAOC_DeadAcc4.1<-LAOC_RF4.1.tab[2,2]/sum(LAOC_RF4.1.tab[2,])
LAOC_TotalAcc4.1<-(LAOC_RF4.1.tab[1,1]+LAOC_RF4.1.tab[2,2])/sum(LAOC_RF4.1.tab)

LAOC_RF5.1.tab <-table(factor(LAOC_test5.1$Status, levels = 0:1), factor(LAOC_5.1pred, levels = 0:1))
LAOC_LiveAcc5.1<-LAOC_RF5.1.tab[1,1]/sum(LAOC_RF5.1.tab[1,])
LAOC_DeadAcc5.1<-LAOC_RF5.1.tab[2,2]/sum(LAOC_RF5.1.tab[2,])
LAOC_TotalAcc5.1<-(LAOC_RF5.1.tab[1,1]+LAOC_RF5.1.tab[2,2])/sum(LAOC_RF5.1.tab)

LAOC_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(LAOC_LiveAcc1.1,
                                                                              LAOC_LiveAcc2.1,
                                                                              LAOC_LiveAcc3.1,
                                                                              LAOC_LiveAcc4.1,
                                                                              LAOC_LiveAcc5.1),
                              DeadAcc = c(LAOC_DeadAcc1.1,
                                          LAOC_DeadAcc2.1,
                                          LAOC_DeadAcc3.1,
                                          LAOC_DeadAcc4.1,
                                          LAOC_DeadAcc5.1),
                              TotalAcc = c(LAOC_TotalAcc1.1,
                                           LAOC_TotalAcc2.1,
                                           LAOC_TotalAcc3.1,
                                           LAOC_TotalAcc4.1,
                                           LAOC_TotalAcc5.1))


LAOC_RF1.2.tab <-table(factor(LAOC_test1.2$Status, levels = 0:1), factor(LAOC_1.2pred, levels = 0:1))
LAOC_LiveAcc1.2<-LAOC_RF1.2.tab[1,1]/sum(LAOC_RF1.2.tab[1,])
LAOC_DeadAcc1.2<-LAOC_RF1.2.tab[2,2]/sum(LAOC_RF1.2.tab[2,])
LAOC_TotalAcc1.2<-(LAOC_RF1.2.tab[1,1]+LAOC_RF1.2.tab[2,2])/sum(LAOC_RF1.2.tab)

LAOC_RF2.2.tab <-table(factor(LAOC_test2.2$Status, levels = 0:1), factor(LAOC_2.2pred, levels = 0:1))
LAOC_LiveAcc2.2<-LAOC_RF2.2.tab[1,1]/sum(LAOC_RF2.2.tab[1,])
LAOC_DeadAcc2.2<-LAOC_RF2.2.tab[2,2]/sum(LAOC_RF2.2.tab[2,])
LAOC_TotalAcc2.2<-(LAOC_RF2.2.tab[1,1]+LAOC_RF2.2.tab[2,2])/sum(LAOC_RF2.2.tab)

LAOC_RF3.2.tab <-table(factor(LAOC_test3.2$Status, levels = 0:1), factor(LAOC_3.2pred, levels = 0:1))
LAOC_LiveAcc3.2<-LAOC_RF3.2.tab[1,1]/sum(LAOC_RF3.2.tab[1,])
LAOC_DeadAcc3.2<-LAOC_RF3.2.tab[2,2]/sum(LAOC_RF3.2.tab[2,])
LAOC_TotalAcc3.2<-(LAOC_RF3.2.tab[1,1]+LAOC_RF3.2.tab[2,2])/sum(LAOC_RF3.2.tab)

LAOC_RF4.2.tab <-table(factor(LAOC_test4.2$Status, levels = 0:1), factor(LAOC_4.2pred, levels = 0:1))
LAOC_LiveAcc4.2<-LAOC_RF4.2.tab[1,1]/sum(LAOC_RF4.2.tab[1,])
LAOC_DeadAcc4.2<-LAOC_RF4.2.tab[2,2]/sum(LAOC_RF4.2.tab[2,])
LAOC_TotalAcc4.2<-(LAOC_RF4.2.tab[1,1]+LAOC_RF4.2.tab[2,2])/sum(LAOC_RF4.2.tab)

LAOC_RF5.2.tab <-table(factor(LAOC_test5.2$Status, levels = 0:1), factor(LAOC_5.2pred, levels = 0:1))
LAOC_LiveAcc5.2<-LAOC_RF5.2.tab[1,1]/sum(LAOC_RF5.2.tab[1,])
LAOC_DeadAcc5.2<-LAOC_RF5.2.tab[2,2]/sum(LAOC_RF5.2.tab[2,])
LAOC_TotalAcc5.2<-(LAOC_RF5.2.tab[1,1]+LAOC_RF5.2.tab[2,2])/sum(LAOC_RF5.2.tab)

LAOC_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(LAOC_LiveAcc1.2,
                                                                              LAOC_LiveAcc2.2,
                                                                              LAOC_LiveAcc3.2,
                                                                              LAOC_LiveAcc4.2,
                                                                              LAOC_LiveAcc5.2),
                              DeadAcc = c(LAOC_DeadAcc1.2,
                                          LAOC_DeadAcc2.2,
                                          LAOC_DeadAcc3.2,
                                          LAOC_DeadAcc4.2,
                                          LAOC_DeadAcc5.2),
                              TotalAcc = c(LAOC_TotalAcc1.2,
                                           LAOC_TotalAcc2.2,
                                           LAOC_TotalAcc3.2,
                                           LAOC_TotalAcc4.2,
                                           LAOC_TotalAcc5.2))

LAOC_RF1.3.tab <-table(factor(LAOC_test1.3$Status, levels = 0:1), factor(LAOC_1.3pred, levels = 0:1))
LAOC_LiveAcc1.3<-LAOC_RF1.3.tab[1,1]/sum(LAOC_RF1.3.tab[1,])
LAOC_DeadAcc1.3<-LAOC_RF1.3.tab[2,2]/sum(LAOC_RF1.3.tab[2,])
LAOC_TotalAcc1.3<-(LAOC_RF1.3.tab[1,1]+LAOC_RF1.3.tab[2,2])/sum(LAOC_RF1.3.tab)

LAOC_RF2.3.tab <-table(factor(LAOC_test2.3$Status, levels = 0:1), factor(LAOC_2.3pred, levels = 0:1))
LAOC_LiveAcc2.3<-LAOC_RF2.3.tab[1,1]/sum(LAOC_RF2.3.tab[1,])
LAOC_DeadAcc2.3<-LAOC_RF2.3.tab[2,2]/sum(LAOC_RF2.3.tab[2,])
LAOC_TotalAcc2.3<-(LAOC_RF2.3.tab[1,1]+LAOC_RF2.3.tab[2,2])/sum(LAOC_RF2.3.tab)

LAOC_RF3.3.tab <-table(factor(LAOC_test3.3$Status, levels = 0:1), factor(LAOC_3.3pred, levels = 0:1))
LAOC_LiveAcc3.3<-LAOC_RF3.3.tab[1,1]/sum(LAOC_RF3.3.tab[1,])
LAOC_DeadAcc3.3<-LAOC_RF3.3.tab[2,2]/sum(LAOC_RF3.3.tab[2,])
LAOC_TotalAcc3.3<-(LAOC_RF3.3.tab[1,1]+LAOC_RF3.3.tab[2,2])/sum(LAOC_RF3.3.tab)

LAOC_RF4.3.tab <-table(factor(LAOC_test4.3$Status, levels = 0:1), factor(LAOC_4.3pred, levels = 0:1))
LAOC_LiveAcc4.3<-LAOC_RF4.3.tab[1,1]/sum(LAOC_RF4.3.tab[1,])
LAOC_DeadAcc4.3<-LAOC_RF4.3.tab[2,2]/sum(LAOC_RF4.3.tab[2,])
LAOC_TotalAcc4.3<-(LAOC_RF4.3.tab[1,1]+LAOC_RF4.3.tab[2,2])/sum(LAOC_RF4.3.tab)

LAOC_RF5.3.tab <-table(factor(LAOC_test5.3$Status, levels = 0:1), factor(LAOC_5.3pred, levels = 0:1))
LAOC_LiveAcc5.3<-LAOC_RF5.3.tab[1,1]/sum(LAOC_RF5.3.tab[1,])
LAOC_DeadAcc5.3<-LAOC_RF5.3.tab[2,2]/sum(LAOC_RF5.3.tab[2,])
LAOC_TotalAcc5.3<-(LAOC_RF5.3.tab[1,1]+LAOC_RF5.3.tab[2,2])/sum(LAOC_RF5.3.tab)

LAOC_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(LAOC_LiveAcc1.3,
                                                                              LAOC_LiveAcc2.3,
                                                                              LAOC_LiveAcc3.3,
                                                                              LAOC_LiveAcc4.3,
                                                                              LAOC_LiveAcc5.3),
                              DeadAcc = c(LAOC_DeadAcc1.3,
                                          LAOC_DeadAcc2.3,
                                          LAOC_DeadAcc3.3,
                                          LAOC_DeadAcc4.3,
                                          LAOC_DeadAcc5.3),
                              TotalAcc = c(LAOC_TotalAcc1.3,
                                           LAOC_TotalAcc2.3,
                                           LAOC_TotalAcc3.3,
                                           LAOC_TotalAcc4.3,
                                           LAOC_TotalAcc5.3))

LAOC_RF1.4.tab <-table(factor(LAOC_test1.4$Status, levels = 0:1), factor(LAOC_1.4pred, levels = 0:1))
LAOC_LiveAcc1.4<-LAOC_RF1.4.tab[1,1]/sum(LAOC_RF1.4.tab[1,])
LAOC_DeadAcc1.4<-LAOC_RF1.4.tab[2,2]/sum(LAOC_RF1.4.tab[2,])
LAOC_TotalAcc1.4<-(LAOC_RF1.4.tab[1,1]+LAOC_RF1.4.tab[2,2])/sum(LAOC_RF1.4.tab)

LAOC_RF2.4.tab <-table(factor(LAOC_test2.4$Status, levels = 0:1), factor(LAOC_2.4pred, levels = 0:1))
LAOC_LiveAcc2.4<-LAOC_RF2.4.tab[1,1]/sum(LAOC_RF2.4.tab[1,])
LAOC_DeadAcc2.4<-LAOC_RF2.4.tab[2,2]/sum(LAOC_RF2.4.tab[2,])
LAOC_TotalAcc2.4<-(LAOC_RF2.4.tab[1,1]+LAOC_RF2.4.tab[2,2])/sum(LAOC_RF2.4.tab)

LAOC_RF3.4.tab <-table(factor(LAOC_test3.4$Status, levels = 0:1), factor(LAOC_3.4pred, levels = 0:1))
LAOC_LiveAcc3.4<-LAOC_RF3.4.tab[1,1]/sum(LAOC_RF3.4.tab[1,])
LAOC_DeadAcc3.4<-LAOC_RF3.4.tab[2,2]/sum(LAOC_RF3.4.tab[2,])
LAOC_TotalAcc3.4<-(LAOC_RF3.4.tab[1,1]+LAOC_RF3.4.tab[2,2])/sum(LAOC_RF3.4.tab)

LAOC_RF4.4.tab <-table(factor(LAOC_test4.4$Status, levels = 0:1), factor(LAOC_4.4pred, levels = 0:1))
LAOC_LiveAcc4.4<-LAOC_RF4.4.tab[1,1]/sum(LAOC_RF4.4.tab[1,])
LAOC_DeadAcc4.4<-LAOC_RF4.4.tab[2,2]/sum(LAOC_RF4.4.tab[2,])
LAOC_TotalAcc4.4<-(LAOC_RF4.4.tab[1,1]+LAOC_RF4.4.tab[2,2])/sum(LAOC_RF4.4.tab)

LAOC_RF5.4.tab <-table(factor(LAOC_test5.4$Status, levels = 0:1), factor(LAOC_5.4pred, levels = 0:1))
LAOC_LiveAcc5.4<-LAOC_RF5.4.tab[1,1]/sum(LAOC_RF5.4.tab[1,])
LAOC_DeadAcc5.4<-LAOC_RF5.4.tab[2,2]/sum(LAOC_RF5.4.tab[2,])
LAOC_TotalAcc5.4<-(LAOC_RF5.4.tab[1,1]+LAOC_RF5.4.tab[2,2])/sum(LAOC_RF5.4.tab)

LAOC_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(LAOC_LiveAcc1.4,
                                                                              LAOC_LiveAcc2.4,
                                                                              LAOC_LiveAcc3.4,
                                                                              LAOC_LiveAcc4.4,
                                                                              LAOC_LiveAcc5.4),
                              DeadAcc = c(LAOC_DeadAcc1.4,
                                          LAOC_DeadAcc2.4,
                                          LAOC_DeadAcc3.4,
                                          LAOC_DeadAcc4.4,
                                          LAOC_DeadAcc5.4),
                              TotalAcc = c(LAOC_TotalAcc1.4,
                                           LAOC_TotalAcc2.4,
                                           LAOC_TotalAcc3.4,
                                           LAOC_TotalAcc4.4,
                                           LAOC_TotalAcc5.4))

LAOC_RF1.5.tab <-table(factor(LAOC_test1.5$Status, levels = 0:1), factor(LAOC_1.5pred, levels = 0:1))
LAOC_LiveAcc1.5<-LAOC_RF1.5.tab[1,1]/sum(LAOC_RF1.5.tab[1,])
LAOC_DeadAcc1.5<-LAOC_RF1.5.tab[2,2]/sum(LAOC_RF1.5.tab[2,])
LAOC_TotalAcc1.5<-(LAOC_RF1.5.tab[1,1]+LAOC_RF1.5.tab[2,2])/sum(LAOC_RF1.5.tab)

LAOC_RF2.5.tab <-table(factor(LAOC_test2.5$Status, levels = 0:1), factor(LAOC_2.5pred, levels = 0:1))
LAOC_LiveAcc2.5<-LAOC_RF2.5.tab[1,1]/sum(LAOC_RF2.5.tab[1,])
LAOC_DeadAcc2.5<-LAOC_RF2.5.tab[2,2]/sum(LAOC_RF2.5.tab[2,])
LAOC_TotalAcc2.5<-(LAOC_RF2.5.tab[1,1]+LAOC_RF2.5.tab[2,2])/sum(LAOC_RF2.5.tab)

LAOC_RF3.5.tab <-table(factor(LAOC_test3.5$Status, levels = 0:1), factor(LAOC_3.5pred, levels = 0:1))
LAOC_LiveAcc3.5<-LAOC_RF3.5.tab[1,1]/sum(LAOC_RF3.5.tab[1,])
LAOC_DeadAcc3.5<-LAOC_RF3.5.tab[2,2]/sum(LAOC_RF3.5.tab[2,])
LAOC_TotalAcc3.5<-(LAOC_RF3.5.tab[1,1]+LAOC_RF3.5.tab[2,2])/sum(LAOC_RF3.5.tab)

LAOC_RF4.5.tab <-table(factor(LAOC_test4.5$Status, levels = 0:1), factor(LAOC_4.5pred, levels = 0:1))
LAOC_LiveAcc4.5<-LAOC_RF4.5.tab[1,1]/sum(LAOC_RF4.5.tab[1,])
LAOC_DeadAcc4.5<-LAOC_RF4.5.tab[2,2]/sum(LAOC_RF4.5.tab[2,])
LAOC_TotalAcc4.5<-(LAOC_RF4.5.tab[1,1]+LAOC_RF4.5.tab[2,2])/sum(LAOC_RF4.5.tab)

LAOC_RF5.5.tab <-table(factor(LAOC_test5.5$Status, levels = 0:1), factor(LAOC_5.5pred, levels = 0:1))
LAOC_LiveAcc5.5<-LAOC_RF5.5.tab[1,1]/sum(LAOC_RF5.5.tab[1,])
LAOC_DeadAcc5.5<-LAOC_RF5.5.tab[2,2]/sum(LAOC_RF5.5.tab[2,])
LAOC_TotalAcc5.5<-(LAOC_RF5.5.tab[1,1]+LAOC_RF5.5.tab[2,2])/sum(LAOC_RF5.5.tab)

LAOC_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(LAOC_LiveAcc1.5,
                                                                              LAOC_LiveAcc2.5,
                                                                              LAOC_LiveAcc3.5,
                                                                              LAOC_LiveAcc4.5,
                                                                              LAOC_LiveAcc5.5),
                              DeadAcc = c(LAOC_DeadAcc1.5,
                                          LAOC_DeadAcc2.5,
                                          LAOC_DeadAcc3.5,
                                          LAOC_DeadAcc4.5,
                                          LAOC_DeadAcc5.5),
                              TotalAcc = c(LAOC_TotalAcc1.5,
                                           LAOC_TotalAcc2.5,
                                           LAOC_TotalAcc3.5,
                                           LAOC_TotalAcc4.5,
                                           LAOC_TotalAcc5.5))

LAOC_pred.results <-rbind(LAOC_Pre.results1,LAOC_Pre.results2,LAOC_Pre.results3,LAOC_Pre.results4,LAOC_Pre.results5)



LAOC_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "LAOC",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> LAOC_RF_preds.sum





#############PICO######################
#############subset species###########

PICO <- subset(Tree_data, Species == 'PICO')
PICO$OBS_ID <- 1:nrow(PICO)

###Get complete cases##########
PICO_yr1 <- select(PICO, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)

PICO_yr2 <- select(PICO, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

PICO_yr3 <- select(PICO, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

PICO_yr4 <- select(PICO, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

PICO_yr5 <- select(PICO, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
PICO_flds1<-createFolds(PICO_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
PICO_flds2<-createFolds(PICO_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PICO_flds3<-createFolds(PICO_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PICO_flds4<-createFolds(PICO_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PICO_flds5<-createFolds(PICO_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 



##1
set.seed(1980)
samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[1]],]$Status))))
PICO_train.rf1.1 <- randomForest(data = PICO_yr1[-PICO_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr2[-PICO_flds2[[1]],]$Status))))
PICO_train.rf2.1 <- randomForest(data = PICO_yr2[-PICO_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PICO_yr3[-PICO_flds3[[1]],]$Status))))
PICO_train.rf3.1 <- randomForest(data = PICO_yr3[-PICO_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr4[-PICO_flds4[[1]],]$Status))))
PICO_train.rf4.1 <- randomForest(data = PICO_yr4[-PICO_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr5[-PICO_flds5[[1]],]$Status))))
PICO_train.rf5.1 <- randomForest(data = PICO_yr5[-PICO_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(PICO_train.rf5.1)

##2

samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[2]],]$Status))))
PICO_train.rf1.2 <- randomForest(data = PICO_yr1[-PICO_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr2[-PICO_flds2[[2]],]$Status))))
PICO_train.rf2.2 <- randomForest(data = PICO_yr2[-PICO_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PICO_yr3[-PICO_flds3[[2]],]$Status))))
PICO_train.rf3.2 <- randomForest(data = PICO_yr3[-PICO_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr4[-PICO_flds4[[2]],]$Status))))
PICO_train.rf4.2 <- randomForest(data = PICO_yr4[-PICO_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr5[-PICO_flds5[[2]],]$Status))))
PICO_train.rf5.2 <- randomForest(data = PICO_yr5[-PICO_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[3]],]$Status))))
PICO_train.rf1.3 <- randomForest(data = PICO_yr1[-PICO_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr2[-PICO_flds2[[3]],]$Status))))
PICO_train.rf2.3 <- randomForest(data = PICO_yr2[-PICO_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PICO_yr3[-PICO_flds3[[3]],]$Status))))
PICO_train.rf3.3 <- randomForest(data = PICO_yr3[-PICO_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr4[-PICO_flds4[[3]],]$Status))))
PICO_train.rf4.3 <- randomForest(data = PICO_yr4[-PICO_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr5[-PICO_flds5[[3]],]$Status))))
PICO_train.rf5.3 <- randomForest(data = PICO_yr5[-PICO_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[4]],]$Status))))
PICO_train.rf1.4 <- randomForest(data = PICO_yr1[-PICO_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr2[-PICO_flds2[[4]],]$Status))))
PICO_train.rf2.4 <- randomForest(data = PICO_yr2[-PICO_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PICO_yr3[-PICO_flds3[[4]],]$Status))))
PICO_train.rf3.4 <- randomForest(data = PICO_yr3[-PICO_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr4[-PICO_flds4[[4]],]$Status))))
PICO_train.rf4.4 <- randomForest(data = PICO_yr4[-PICO_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr5[-PICO_flds5[[4]],]$Status))))
PICO_train.rf5.4 <- randomForest(data = PICO_yr5[-PICO_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PICO_yr1[-PICO_flds1[[5]],]$Status))))
PICO_train.rf1.5 <- randomForest(data = PICO_yr1[-PICO_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr2[-PICO_flds2[[5]],]$Status))))
PICO_train.rf2.5 <- randomForest(data = PICO_yr2[-PICO_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PICO_yr3[-PICO_flds3[[5]],]$Status))))
PICO_train.rf3.5 <- randomForest(data = PICO_yr3[-PICO_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr4[-PICO_flds4[[5]],]$Status))))
PICO_train.rf4.5 <- randomForest(data = PICO_yr4[-PICO_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PICO_yr5[-PICO_flds5[[5]],]$Status))))
PICO_train.rf5.5 <- randomForest(data = PICO_yr5[-PICO_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)


##################Create test datasets#####################
##1
PICO_test1.1 <- PICO_yr1[PICO_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(PICO_yr1[PICO_flds1[[1]],] $  exclude=NULL)
table(PICO_test1.1$  exclude=NULL)

PICO_test2.1 <- PICO_yr2[PICO_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test3.1 <- PICO_yr3[PICO_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test4.1 <- PICO_yr4[PICO_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test5.1 <- PICO_yr5[PICO_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
PICO_test1.2 <- PICO_yr1[PICO_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test2.2 <- PICO_yr2[PICO_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test3.2 <- PICO_yr3[PICO_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test4.2 <- PICO_yr4[PICO_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test5.2 <- PICO_yr5[PICO_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
PICO_test1.3 <- PICO_yr1[PICO_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test2.3 <- PICO_yr2[PICO_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test3.3 <- PICO_yr3[PICO_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test4.3 <- PICO_yr4[PICO_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test5.3 <- PICO_yr5[PICO_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##4
PICO_test1.4 <- PICO_yr1[PICO_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test2.4 <- PICO_yr2[PICO_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test3.4 <- PICO_yr3[PICO_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test4.4 <- PICO_yr4[PICO_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test5.4 <- PICO_yr5[PICO_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##5
PICO_test1.5 <- PICO_yr1[PICO_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test2.5 <- PICO_yr2[PICO_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test3.5 <- PICO_yr3[PICO_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test4.5 <- PICO_yr4[PICO_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PICO_test5.5 <- PICO_yr5[PICO_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


#######################Predicting###############################
PICO_pred.results <-data.frame() 

PICO_1.1pred <- predict(PICO_train.rf1.1, newdata=PICO_test1.1)
PICO_2.1pred <- predict(PICO_train.rf2.1, newdata=PICO_test2.1)
PICO_3.1pred <- predict(PICO_train.rf3.1, newdata=PICO_test3.1)
PICO_4.1pred <- predict(PICO_train.rf4.1, newdata=PICO_test4.1)
PICO_5.1pred <- predict(PICO_train.rf5.1, newdata=PICO_test5.1)

PICO_1.2pred <- predict(PICO_train.rf1.2, newdata=PICO_test1.2)
PICO_2.2pred <- predict(PICO_train.rf2.2, newdata=PICO_test2.2)
PICO_3.2pred <- predict(PICO_train.rf3.2, newdata=PICO_test3.2)
PICO_4.2pred <- predict(PICO_train.rf4.2, newdata=PICO_test4.2)
PICO_5.2pred <- predict(PICO_train.rf5.2, newdata=PICO_test5.2)

PICO_1.3pred <- predict(PICO_train.rf1.3, newdata=PICO_test1.3)
PICO_2.3pred <- predict(PICO_train.rf2.3, newdata=PICO_test2.3)
PICO_3.3pred <- predict(PICO_train.rf3.3, newdata=PICO_test3.3)
PICO_4.3pred <- predict(PICO_train.rf4.3, newdata=PICO_test4.3)
PICO_5.3pred <- predict(PICO_train.rf5.3, newdata=PICO_test5.3)

PICO_1.4pred <- predict(PICO_train.rf1.4, newdata=PICO_test1.4)
PICO_2.4pred <- predict(PICO_train.rf2.4, newdata=PICO_test2.4)
PICO_3.4pred <- predict(PICO_train.rf3.4, newdata=PICO_test3.4)
PICO_4.4pred <- predict(PICO_train.rf4.4, newdata=PICO_test4.4)
PICO_5.4pred <- predict(PICO_train.rf5.4, newdata=PICO_test5.4)

PICO_1.5pred <- predict(PICO_train.rf1.5, newdata=PICO_test1.5)
PICO_2.5pred <- predict(PICO_train.rf2.5, newdata=PICO_test2.5)
PICO_3.5pred <- predict(PICO_train.rf3.5, newdata=PICO_test3.5)
PICO_4.5pred <- predict(PICO_train.rf4.5, newdata=PICO_test4.5)
PICO_5.5pred <- predict(PICO_train.rf5.5, newdata=PICO_test5.5)

length(PICO_3.1pred)
length(PICO_test3.1$Status)

PICO.test.preds <-data.frame(Status = c(PICO_test1.1$Status,PICO_test2.1$Status,PICO_test3.1$Status,PICO_test4.1$Status,PICO_test5.1$Status,
                                        PICO_test1.2$Status,PICO_test2.2$Status,PICO_test3.2$Status,PICO_test4.2$Status,PICO_test5.2$Status,
                                        PICO_test1.3$Status,PICO_test2.3$Status,PICO_test3.3$Status,PICO_test4.3$Status,PICO_test5.3$Status,
                                        PICO_test1.4$Status,PICO_test2.4$Status,PICO_test3.4$Status,PICO_test4.4$Status,PICO_test5.4$Status,
                                        PICO_test1.5$Status,PICO_test2.5$Status,PICO_test3.5$Status,PICO_test4.5$Status,PICO_test5.5$Status),
                             preds = c(PICO_1.1pred,PICO_2.1pred,PICO_3.1pred,PICO_4.1pred,PICO_5.1pred,
                                       PICO_1.2pred,PICO_2.2pred,PICO_3.2pred,PICO_4.2pred,PICO_5.2pred,
                                       PICO_1.3pred,PICO_2.3pred,PICO_3.3pred,PICO_4.3pred,PICO_5.3pred,
                                       PICO_1.4pred,PICO_2.4pred,PICO_3.4pred,PICO_4.4pred,PICO_5.4pred,
                                       PICO_1.5pred,PICO_2.5pred,PICO_3.5pred,PICO_4.5pred,PICO_5.5pred),
                             model = c(rep(1,length(PICO_1.1pred)),rep(2,length(PICO_2.1pred)),rep(3,length(PICO_3.1pred)),rep(4,length(PICO_4.1pred)),rep(5,length(PICO_5.1pred)),
                                       rep(1,length(PICO_1.2pred)),rep(2,length(PICO_2.2pred)),rep(3,length(PICO_3.2pred)),rep(4,length(PICO_4.2pred)),rep(5,length(PICO_5.2pred)),
                                       rep(1,length(PICO_1.3pred)),rep(2,length(PICO_2.3pred)),rep(3,length(PICO_3.3pred)),rep(4,length(PICO_4.3pred)),rep(5,length(PICO_5.3pred)),
                                       rep(1,length(PICO_1.4pred)),rep(2,length(PICO_2.4pred)),rep(3,length(PICO_3.4pred)),rep(4,length(PICO_4.4pred)),rep(5,length(PICO_5.4pred)),
                                       rep(1,length(PICO_1.5pred)),rep(2,length(PICO_2.5pred)),rep(3,length(PICO_3.5pred)),rep(4,length(PICO_4.5pred)),rep(5,length(PICO_5.5pred))),
                             rep = c(rep(1,length(PICO_1.1pred)),rep(1,length(PICO_2.1pred)),rep(1,length(PICO_3.1pred)),rep(1,length(PICO_4.1pred)),rep(1,length(PICO_5.1pred)),
                                     rep(2,length(PICO_1.2pred)),rep(2,length(PICO_2.2pred)),rep(2,length(PICO_3.2pred)),rep(2,length(PICO_4.2pred)),rep(2,length(PICO_5.2pred)),
                                     rep(3,length(PICO_1.3pred)),rep(3,length(PICO_2.3pred)),rep(3,length(PICO_3.3pred)),rep(3,length(PICO_4.3pred)),rep(3,length(PICO_5.3pred)),
                                     rep(4,length(PICO_1.4pred)),rep(4,length(PICO_2.4pred)),rep(4,length(PICO_3.4pred)),rep(4,length(PICO_4.4pred)),rep(4,length(PICO_5.4pred)),
                                     rep(5,length(PICO_1.5pred)),rep(5,length(PICO_2.5pred)),rep(5,length(PICO_3.5pred)),rep(5,length(PICO_4.5pred)),rep(5,length(PICO_5.5pred))),
                            
                             dead_check = c(PICO_test1.1$dead_check, PICO_test2.1$dead_check, PICO_test3.1$dead_check, PICO_test4.1$dead_check, PICO_test5.1$dead_check,
                                            PICO_test1.2$dead_check, PICO_test2.2$dead_check, PICO_test3.2$dead_check, PICO_test4.2$dead_check, PICO_test5.2$dead_check,
                                            PICO_test1.3$dead_check, PICO_test2.3$dead_check, PICO_test3.3$dead_check, PICO_test4.3$dead_check, PICO_test5.3$dead_check,
                                            PICO_test1.4$dead_check, PICO_test2.4$dead_check, PICO_test3.4$dead_check, PICO_test4.4$dead_check, PICO_test5.4$dead_check,
                                            PICO_test1.5$dead_check, PICO_test2.5$dead_check, PICO_test3.5$dead_check, PICO_test4.5$dead_check, PICO_test5.5$dead_check))



PICO.test.preds$wrong <- ifelse(PICO.test.preds$Status == PICO.test.preds$preds, 0,1)

PICO.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> PICO.wrong.preds


PICO_RF1.1.tab <-table(factor(PICO_test1.1$Status, levels = 0:1), factor(PICO_1.1pred, levels = 0:1))
PICO_LiveAcc1.1<-PICO_RF1.1.tab[1,1]/sum(PICO_RF1.1.tab[1,])
PICO_DeadAcc1.1<-PICO_RF1.1.tab[2,2]/sum(PICO_RF1.1.tab[2,])
PICO_TotalAcc1.1<-(PICO_RF1.1.tab[1,1]+PICO_RF1.1.tab[2,2])/sum(PICO_RF1.1.tab)

PICO_RF2.1.tab <-table(factor(PICO_test2.1$Status, levels = 0:1), factor(PICO_2.1pred, levels = 0:1))
PICO_LiveAcc2.1<-PICO_RF2.1.tab[1,1]/sum(PICO_RF2.1.tab[1,])
PICO_DeadAcc2.1<-PICO_RF2.1.tab[2,2]/sum(PICO_RF2.1.tab[2,])
PICO_TotalAcc2.1<-(PICO_RF2.1.tab[1,1]+PICO_RF2.1.tab[2,2])/sum(PICO_RF2.1.tab)

PICO_RF3.1.tab <-table(factor(PICO_test3.1$Status, levels = 0:1), factor(PICO_3.1pred, levels = 0:1))
PICO_LiveAcc3.1<-PICO_RF3.1.tab[1,1]/sum(PICO_RF3.1.tab[1,])
PICO_DeadAcc3.1<-PICO_RF3.1.tab[2,2]/sum(PICO_RF3.1.tab[2,])
PICO_TotalAcc3.1<-(PICO_RF3.1.tab[1,1]+PICO_RF3.1.tab[2,2])/sum(PICO_RF3.1.tab)

PICO_RF4.1.tab <-table(factor(PICO_test4.1$Status, levels = 0:1), factor(PICO_4.1pred, levels = 0:1))
PICO_LiveAcc4.1<-PICO_RF4.1.tab[1,1]/sum(PICO_RF4.1.tab[1,])
PICO_DeadAcc4.1<-PICO_RF4.1.tab[2,2]/sum(PICO_RF4.1.tab[2,])
PICO_TotalAcc4.1<-(PICO_RF4.1.tab[1,1]+PICO_RF4.1.tab[2,2])/sum(PICO_RF4.1.tab)

PICO_RF5.1.tab <-table(factor(PICO_test5.1$Status, levels = 0:1), factor(PICO_5.1pred, levels = 0:1))
PICO_LiveAcc5.1<-PICO_RF5.1.tab[1,1]/sum(PICO_RF5.1.tab[1,])
PICO_DeadAcc5.1<-PICO_RF5.1.tab[2,2]/sum(PICO_RF5.1.tab[2,])
PICO_TotalAcc5.1<-(PICO_RF5.1.tab[1,1]+PICO_RF5.1.tab[2,2])/sum(PICO_RF5.1.tab)

PICO_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(PICO_LiveAcc1.1,
                                                                              PICO_LiveAcc2.1,
                                                                              PICO_LiveAcc3.1,
                                                                              PICO_LiveAcc4.1,
                                                                              PICO_LiveAcc5.1),
                              DeadAcc = c(PICO_DeadAcc1.1,
                                          PICO_DeadAcc2.1,
                                          PICO_DeadAcc3.1,
                                          PICO_DeadAcc4.1,
                                          PICO_DeadAcc5.1),
                              TotalAcc = c(PICO_TotalAcc1.1,
                                           PICO_TotalAcc2.1,
                                           PICO_TotalAcc3.1,
                                           PICO_TotalAcc4.1,
                                           PICO_TotalAcc5.1))


PICO_RF1.2.tab <-table(factor(PICO_test1.2$Status, levels = 0:1), factor(PICO_1.2pred, levels = 0:1))
PICO_LiveAcc1.2<-PICO_RF1.2.tab[1,1]/sum(PICO_RF1.2.tab[1,])
PICO_DeadAcc1.2<-PICO_RF1.2.tab[2,2]/sum(PICO_RF1.2.tab[2,])
PICO_TotalAcc1.2<-(PICO_RF1.2.tab[1,1]+PICO_RF1.2.tab[2,2])/sum(PICO_RF1.2.tab)

PICO_RF2.2.tab <-table(factor(PICO_test2.2$Status, levels = 0:1), factor(PICO_2.2pred, levels = 0:1))
PICO_LiveAcc2.2<-PICO_RF2.2.tab[1,1]/sum(PICO_RF2.2.tab[1,])
PICO_DeadAcc2.2<-PICO_RF2.2.tab[2,2]/sum(PICO_RF2.2.tab[2,])
PICO_TotalAcc2.2<-(PICO_RF2.2.tab[1,1]+PICO_RF2.2.tab[2,2])/sum(PICO_RF2.2.tab)

PICO_RF3.2.tab <-table(factor(PICO_test3.2$Status, levels = 0:1), factor(PICO_3.2pred, levels = 0:1))
PICO_LiveAcc3.2<-PICO_RF3.2.tab[1,1]/sum(PICO_RF3.2.tab[1,])
PICO_DeadAcc3.2<-PICO_RF3.2.tab[2,2]/sum(PICO_RF3.2.tab[2,])
PICO_TotalAcc3.2<-(PICO_RF3.2.tab[1,1]+PICO_RF3.2.tab[2,2])/sum(PICO_RF3.2.tab)

PICO_RF4.2.tab <-table(factor(PICO_test4.2$Status, levels = 0:1), factor(PICO_4.2pred, levels = 0:1))
PICO_LiveAcc4.2<-PICO_RF4.2.tab[1,1]/sum(PICO_RF4.2.tab[1,])
PICO_DeadAcc4.2<-PICO_RF4.2.tab[2,2]/sum(PICO_RF4.2.tab[2,])
PICO_TotalAcc4.2<-(PICO_RF4.2.tab[1,1]+PICO_RF4.2.tab[2,2])/sum(PICO_RF4.2.tab)

PICO_RF5.2.tab <-table(factor(PICO_test5.2$Status, levels = 0:1), factor(PICO_5.2pred, levels = 0:1))
PICO_LiveAcc5.2<-PICO_RF5.2.tab[1,1]/sum(PICO_RF5.2.tab[1,])
PICO_DeadAcc5.2<-PICO_RF5.2.tab[2,2]/sum(PICO_RF5.2.tab[2,])
PICO_TotalAcc5.2<-(PICO_RF5.2.tab[1,1]+PICO_RF5.2.tab[2,2])/sum(PICO_RF5.2.tab)

PICO_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(PICO_LiveAcc1.2,
                                                                              PICO_LiveAcc2.2,
                                                                              PICO_LiveAcc3.2,
                                                                              PICO_LiveAcc4.2,
                                                                              PICO_LiveAcc5.2),
                              DeadAcc = c(PICO_DeadAcc1.2,
                                          PICO_DeadAcc2.2,
                                          PICO_DeadAcc3.2,
                                          PICO_DeadAcc4.2,
                                          PICO_DeadAcc5.2),
                              TotalAcc = c(PICO_TotalAcc1.2,
                                           PICO_TotalAcc2.2,
                                           PICO_TotalAcc3.2,
                                           PICO_TotalAcc4.2,
                                           PICO_TotalAcc5.2))

PICO_RF1.3.tab <-table(factor(PICO_test1.3$Status, levels = 0:1), factor(PICO_1.3pred, levels = 0:1))
PICO_LiveAcc1.3<-PICO_RF1.3.tab[1,1]/sum(PICO_RF1.3.tab[1,])
PICO_DeadAcc1.3<-PICO_RF1.3.tab[2,2]/sum(PICO_RF1.3.tab[2,])
PICO_TotalAcc1.3<-(PICO_RF1.3.tab[1,1]+PICO_RF1.3.tab[2,2])/sum(PICO_RF1.3.tab)

PICO_RF2.3.tab <-table(factor(PICO_test2.3$Status, levels = 0:1), factor(PICO_2.3pred, levels = 0:1))
PICO_LiveAcc2.3<-PICO_RF2.3.tab[1,1]/sum(PICO_RF2.3.tab[1,])
PICO_DeadAcc2.3<-PICO_RF2.3.tab[2,2]/sum(PICO_RF2.3.tab[2,])
PICO_TotalAcc2.3<-(PICO_RF2.3.tab[1,1]+PICO_RF2.3.tab[2,2])/sum(PICO_RF2.3.tab)

PICO_RF3.3.tab <-table(factor(PICO_test3.3$Status, levels = 0:1), factor(PICO_3.3pred, levels = 0:1))
PICO_LiveAcc3.3<-PICO_RF3.3.tab[1,1]/sum(PICO_RF3.3.tab[1,])
PICO_DeadAcc3.3<-PICO_RF3.3.tab[2,2]/sum(PICO_RF3.3.tab[2,])
PICO_TotalAcc3.3<-(PICO_RF3.3.tab[1,1]+PICO_RF3.3.tab[2,2])/sum(PICO_RF3.3.tab)

PICO_RF4.3.tab <-table(factor(PICO_test4.3$Status, levels = 0:1), factor(PICO_4.3pred, levels = 0:1))
PICO_LiveAcc4.3<-PICO_RF4.3.tab[1,1]/sum(PICO_RF4.3.tab[1,])
PICO_DeadAcc4.3<-PICO_RF4.3.tab[2,2]/sum(PICO_RF4.3.tab[2,])
PICO_TotalAcc4.3<-(PICO_RF4.3.tab[1,1]+PICO_RF4.3.tab[2,2])/sum(PICO_RF4.3.tab)

PICO_RF5.3.tab <-table(factor(PICO_test5.3$Status, levels = 0:1), factor(PICO_5.3pred, levels = 0:1))
PICO_LiveAcc5.3<-PICO_RF5.3.tab[1,1]/sum(PICO_RF5.3.tab[1,])
PICO_DeadAcc5.3<-PICO_RF5.3.tab[2,2]/sum(PICO_RF5.3.tab[2,])
PICO_TotalAcc5.3<-(PICO_RF5.3.tab[1,1]+PICO_RF5.3.tab[2,2])/sum(PICO_RF5.3.tab)

PICO_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(PICO_LiveAcc1.3,
                                                                              PICO_LiveAcc2.3,
                                                                              PICO_LiveAcc3.3,
                                                                              PICO_LiveAcc4.3,
                                                                              PICO_LiveAcc5.3),
                              DeadAcc = c(PICO_DeadAcc1.3,
                                          PICO_DeadAcc2.3,
                                          PICO_DeadAcc3.3,
                                          PICO_DeadAcc4.3,
                                          PICO_DeadAcc5.3),
                              TotalAcc = c(PICO_TotalAcc1.3,
                                           PICO_TotalAcc2.3,
                                           PICO_TotalAcc3.3,
                                           PICO_TotalAcc4.3,
                                           PICO_TotalAcc5.3))

PICO_RF1.4.tab <-table(factor(PICO_test1.4$Status, levels = 0:1), factor(PICO_1.4pred, levels = 0:1))
PICO_LiveAcc1.4<-PICO_RF1.4.tab[1,1]/sum(PICO_RF1.4.tab[1,])
PICO_DeadAcc1.4<-PICO_RF1.4.tab[2,2]/sum(PICO_RF1.4.tab[2,])
PICO_TotalAcc1.4<-(PICO_RF1.4.tab[1,1]+PICO_RF1.4.tab[2,2])/sum(PICO_RF1.4.tab)

PICO_RF2.4.tab <-table(factor(PICO_test2.4$Status, levels = 0:1), factor(PICO_2.4pred, levels = 0:1))
PICO_LiveAcc2.4<-PICO_RF2.4.tab[1,1]/sum(PICO_RF2.4.tab[1,])
PICO_DeadAcc2.4<-PICO_RF2.4.tab[2,2]/sum(PICO_RF2.4.tab[2,])
PICO_TotalAcc2.4<-(PICO_RF2.4.tab[1,1]+PICO_RF2.4.tab[2,2])/sum(PICO_RF2.4.tab)

PICO_RF3.4.tab <-table(factor(PICO_test3.4$Status, levels = 0:1), factor(PICO_3.4pred, levels = 0:1))
PICO_LiveAcc3.4<-PICO_RF3.4.tab[1,1]/sum(PICO_RF3.4.tab[1,])
PICO_DeadAcc3.4<-PICO_RF3.4.tab[2,2]/sum(PICO_RF3.4.tab[2,])
PICO_TotalAcc3.4<-(PICO_RF3.4.tab[1,1]+PICO_RF3.4.tab[2,2])/sum(PICO_RF3.4.tab)

PICO_RF4.4.tab <-table(factor(PICO_test4.4$Status, levels = 0:1), factor(PICO_4.4pred, levels = 0:1))
PICO_LiveAcc4.4<-PICO_RF4.4.tab[1,1]/sum(PICO_RF4.4.tab[1,])
PICO_DeadAcc4.4<-PICO_RF4.4.tab[2,2]/sum(PICO_RF4.4.tab[2,])
PICO_TotalAcc4.4<-(PICO_RF4.4.tab[1,1]+PICO_RF4.4.tab[2,2])/sum(PICO_RF4.4.tab)

PICO_RF5.4.tab <-table(factor(PICO_test5.4$Status, levels = 0:1), factor(PICO_5.4pred, levels = 0:1))
PICO_LiveAcc5.4<-PICO_RF5.4.tab[1,1]/sum(PICO_RF5.4.tab[1,])
PICO_DeadAcc5.4<-PICO_RF5.4.tab[2,2]/sum(PICO_RF5.4.tab[2,])
PICO_TotalAcc5.4<-(PICO_RF5.4.tab[1,1]+PICO_RF5.4.tab[2,2])/sum(PICO_RF5.4.tab)

PICO_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(PICO_LiveAcc1.4,
                                                                              PICO_LiveAcc2.4,
                                                                              PICO_LiveAcc3.4,
                                                                              PICO_LiveAcc4.4,
                                                                              PICO_LiveAcc5.4),
                              DeadAcc = c(PICO_DeadAcc1.4,
                                          PICO_DeadAcc2.4,
                                          PICO_DeadAcc3.4,
                                          PICO_DeadAcc4.4,
                                          PICO_DeadAcc5.4),
                              TotalAcc = c(PICO_TotalAcc1.4,
                                           PICO_TotalAcc2.4,
                                           PICO_TotalAcc3.4,
                                           PICO_TotalAcc4.4,
                                           PICO_TotalAcc5.4))

PICO_RF1.5.tab <-table(factor(PICO_test1.5$Status, levels = 0:1), factor(PICO_1.5pred, levels = 0:1))
PICO_LiveAcc1.5<-PICO_RF1.5.tab[1,1]/sum(PICO_RF1.5.tab[1,])
PICO_DeadAcc1.5<-PICO_RF1.5.tab[2,2]/sum(PICO_RF1.5.tab[2,])
PICO_TotalAcc1.5<-(PICO_RF1.5.tab[1,1]+PICO_RF1.5.tab[2,2])/sum(PICO_RF1.5.tab)

PICO_RF2.5.tab <-table(factor(PICO_test2.5$Status, levels = 0:1), factor(PICO_2.5pred, levels = 0:1))
PICO_LiveAcc2.5<-PICO_RF2.5.tab[1,1]/sum(PICO_RF2.5.tab[1,])
PICO_DeadAcc2.5<-PICO_RF2.5.tab[2,2]/sum(PICO_RF2.5.tab[2,])
PICO_TotalAcc2.5<-(PICO_RF2.5.tab[1,1]+PICO_RF2.5.tab[2,2])/sum(PICO_RF2.5.tab)

PICO_RF3.5.tab <-table(factor(PICO_test3.5$Status, levels = 0:1), factor(PICO_3.5pred, levels = 0:1))
PICO_LiveAcc3.5<-PICO_RF3.5.tab[1,1]/sum(PICO_RF3.5.tab[1,])
PICO_DeadAcc3.5<-PICO_RF3.5.tab[2,2]/sum(PICO_RF3.5.tab[2,])
PICO_TotalAcc3.5<-(PICO_RF3.5.tab[1,1]+PICO_RF3.5.tab[2,2])/sum(PICO_RF3.5.tab)

PICO_RF4.5.tab <-table(factor(PICO_test4.5$Status, levels = 0:1), factor(PICO_4.5pred, levels = 0:1))
PICO_LiveAcc4.5<-PICO_RF4.5.tab[1,1]/sum(PICO_RF4.5.tab[1,])
PICO_DeadAcc4.5<-PICO_RF4.5.tab[2,2]/sum(PICO_RF4.5.tab[2,])
PICO_TotalAcc4.5<-(PICO_RF4.5.tab[1,1]+PICO_RF4.5.tab[2,2])/sum(PICO_RF4.5.tab)

PICO_RF5.5.tab <-table(factor(PICO_test5.5$Status, levels = 0:1), factor(PICO_5.5pred, levels = 0:1))
PICO_LiveAcc5.5<-PICO_RF5.5.tab[1,1]/sum(PICO_RF5.5.tab[1,])
PICO_DeadAcc5.5<-PICO_RF5.5.tab[2,2]/sum(PICO_RF5.5.tab[2,])
PICO_TotalAcc5.5<-(PICO_RF5.5.tab[1,1]+PICO_RF5.5.tab[2,2])/sum(PICO_RF5.5.tab)

PICO_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(PICO_LiveAcc1.5,
                                                                              PICO_LiveAcc2.5,
                                                                              PICO_LiveAcc3.5,
                                                                              PICO_LiveAcc4.5,
                                                                              PICO_LiveAcc5.5),
                              DeadAcc = c(PICO_DeadAcc1.5,
                                          PICO_DeadAcc2.5,
                                          PICO_DeadAcc3.5,
                                          PICO_DeadAcc4.5,
                                          PICO_DeadAcc5.5),
                              TotalAcc = c(PICO_TotalAcc1.5,
                                           PICO_TotalAcc2.5,
                                           PICO_TotalAcc3.5,
                                           PICO_TotalAcc4.5,
                                           PICO_TotalAcc5.5))

PICO_pred.results <-rbind(PICO_Pre.results1,PICO_Pre.results2,PICO_Pre.results3,PICO_Pre.results4,PICO_Pre.results5)


PICO_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "PICO",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> PICO_RF_preds.sum





#############PILA######################
##############subset species###########

PILA <- subset(Tree_data, Species == 'PILA')
PILA$OBS_ID <- 1:nrow(PILA)

###Get complete cases##########
PILA_yr1 <- select(PILA, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)

PILA_yr2 <- select(PILA, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

PILA_yr3 <- select(PILA, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

PILA_yr4 <- select(PILA, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

PILA_yr5 <- select(PILA, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
PILA_flds1<-createFolds(PILA_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
PILA_flds2<-createFolds(PILA_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PILA_flds3<-createFolds(PILA_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PILA_flds4<-createFolds(PILA_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PILA_flds5<-createFolds(PILA_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 




##1
set.seed(1980)
samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[1]],]$Status))))
PILA_train.rf1.1 <- randomForest(data = PILA_yr1[-PILA_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr2[-PILA_flds2[[1]],]$Status))))
PILA_train.rf2.1 <- randomForest(data = PILA_yr2[-PILA_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PILA_yr3[-PILA_flds3[[1]],]$Status))))
PILA_train.rf3.1 <- randomForest(data = PILA_yr3[-PILA_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr4[-PILA_flds4[[1]],]$Status))))
PILA_train.rf4.1 <- randomForest(data = PILA_yr4[-PILA_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr5[-PILA_flds5[[1]],]$Status))))
PILA_train.rf5.1 <- randomForest(data = PILA_yr5[-PILA_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(PILA_train.rf5.1)

##2

samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[2]],]$Status))))
PILA_train.rf1.2 <- randomForest(data = PILA_yr1[-PILA_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr2[-PILA_flds2[[2]],]$Status))))
PILA_train.rf2.2 <- randomForest(data = PILA_yr2[-PILA_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PILA_yr3[-PILA_flds3[[2]],]$Status))))
PILA_train.rf3.2 <- randomForest(data = PILA_yr3[-PILA_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr4[-PILA_flds4[[2]],]$Status))))
PILA_train.rf4.2 <- randomForest(data = PILA_yr4[-PILA_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr5[-PILA_flds5[[2]],]$Status))))
PILA_train.rf5.2 <- randomForest(data = PILA_yr5[-PILA_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[3]],]$Status))))
PILA_train.rf1.3 <- randomForest(data = PILA_yr1[-PILA_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr2[-PILA_flds2[[3]],]$Status))))
PILA_train.rf2.3 <- randomForest(data = PILA_yr2[-PILA_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PILA_yr3[-PILA_flds3[[3]],]$Status))))
PILA_train.rf3.3 <- randomForest(data = PILA_yr3[-PILA_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr4[-PILA_flds4[[3]],]$Status))))
PILA_train.rf4.3 <- randomForest(data = PILA_yr4[-PILA_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr5[-PILA_flds5[[3]],]$Status))))
PILA_train.rf5.3 <- randomForest(data = PILA_yr5[-PILA_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[4]],]$Status))))
PILA_train.rf1.4 <- randomForest(data = PILA_yr1[-PILA_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr2[-PILA_flds2[[4]],]$Status))))
PILA_train.rf2.4 <- randomForest(data = PILA_yr2[-PILA_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PILA_yr3[-PILA_flds3[[4]],]$Status))))
PILA_train.rf3.4 <- randomForest(data = PILA_yr3[-PILA_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr4[-PILA_flds4[[4]],]$Status))))
PILA_train.rf4.4 <- randomForest(data = PILA_yr4[-PILA_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr5[-PILA_flds5[[4]],]$Status))))
PILA_train.rf5.4 <- randomForest(data = PILA_yr5[-PILA_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PILA_yr1[-PILA_flds1[[5]],]$Status))))
PILA_train.rf1.5 <- randomForest(data = PILA_yr1[-PILA_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr2[-PILA_flds2[[5]],]$Status))))
PILA_train.rf2.5 <- randomForest(data = PILA_yr2[-PILA_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PILA_yr3[-PILA_flds3[[5]],]$Status))))
PILA_train.rf3.5 <- randomForest(data = PILA_yr3[-PILA_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr4[-PILA_flds4[[5]],]$Status))))
PILA_train.rf4.5 <- randomForest(data = PILA_yr4[-PILA_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PILA_yr5[-PILA_flds5[[5]],]$Status))))
PILA_train.rf5.5 <- randomForest(data = PILA_yr5[-PILA_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)


##################Create test datasets#####################
##1
PILA_test1.1 <- PILA_yr1[PILA_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(PILA_yr1[PILA_flds1[[1]],] $  exclude=NULL)
table(PILA_test1.1$  exclude=NULL)

PILA_test2.1 <- PILA_yr2[PILA_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test3.1 <- PILA_yr3[PILA_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test4.1 <- PILA_yr4[PILA_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test5.1 <- PILA_yr5[PILA_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
PILA_test1.2 <- PILA_yr1[PILA_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test2.2 <- PILA_yr2[PILA_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test3.2 <- PILA_yr3[PILA_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test4.2 <- PILA_yr4[PILA_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test5.2 <- PILA_yr5[PILA_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
PILA_test1.3 <- PILA_yr1[PILA_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PILA_test2.3 <- PILA_yr2[PILA_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PILA_test3.3 <- PILA_yr3[PILA_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test4.3 <- PILA_yr4[PILA_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test5.3 <- PILA_yr5[PILA_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##4
PILA_test1.4 <- PILA_yr1[PILA_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test2.4 <- PILA_yr2[PILA_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test3.4 <- PILA_yr3[PILA_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test4.4 <- PILA_yr4[PILA_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test5.4 <- PILA_yr5[PILA_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##5
PILA_test1.5 <- PILA_yr1[PILA_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test2.5 <- PILA_yr2[PILA_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test3.5 <- PILA_yr3[PILA_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test4.5 <- PILA_yr4[PILA_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PILA_test5.5 <- PILA_yr5[PILA_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 




#######################Predicting###############################
PILA_pred.results <-data.frame() 

PILA_1.1pred <- predict(PILA_train.rf1.1, newdata=PILA_test1.1)
PILA_2.1pred <- predict(PILA_train.rf2.1, newdata=PILA_test2.1)
PILA_3.1pred <- predict(PILA_train.rf3.1, newdata=PILA_test3.1)
PILA_4.1pred <- predict(PILA_train.rf4.1, newdata=PILA_test4.1)
PILA_5.1pred <- predict(PILA_train.rf5.1, newdata=PILA_test5.1)

PILA_1.2pred <- predict(PILA_train.rf1.2, newdata=PILA_test1.2)
PILA_2.2pred <- predict(PILA_train.rf2.2, newdata=PILA_test2.2)
PILA_3.2pred <- predict(PILA_train.rf3.2, newdata=PILA_test3.2)
PILA_4.2pred <- predict(PILA_train.rf4.2, newdata=PILA_test4.2)
PILA_5.2pred <- predict(PILA_train.rf5.2, newdata=PILA_test5.2)

PILA_1.3pred <- predict(PILA_train.rf1.3, newdata=PILA_test1.3)
PILA_2.3pred <- predict(PILA_train.rf2.3, newdata=PILA_test2.3)
PILA_3.3pred <- predict(PILA_train.rf3.3, newdata=PILA_test3.3)
PILA_4.3pred <- predict(PILA_train.rf4.3, newdata=PILA_test4.3)
PILA_5.3pred <- predict(PILA_train.rf5.3, newdata=PILA_test5.3)

PILA_1.4pred <- predict(PILA_train.rf1.4, newdata=PILA_test1.4)
PILA_2.4pred <- predict(PILA_train.rf2.4, newdata=PILA_test2.4)
PILA_3.4pred <- predict(PILA_train.rf3.4, newdata=PILA_test3.4)
PILA_4.4pred <- predict(PILA_train.rf4.4, newdata=PILA_test4.4)
PILA_5.4pred <- predict(PILA_train.rf5.4, newdata=PILA_test5.4)

PILA_1.5pred <- predict(PILA_train.rf1.5, newdata=PILA_test1.5)
PILA_2.5pred <- predict(PILA_train.rf2.5, newdata=PILA_test2.5)
PILA_3.5pred <- predict(PILA_train.rf3.5, newdata=PILA_test3.5)
PILA_4.5pred <- predict(PILA_train.rf4.5, newdata=PILA_test4.5)
PILA_5.5pred <- predict(PILA_train.rf5.5, newdata=PILA_test5.5)



PILA.test.preds <-data.frame(Status = c(PILA_test1.1$Status,PILA_test2.1$Status,PILA_test3.1$Status,PILA_test4.1$Status,PILA_test5.1$Status,
                                        PILA_test1.2$Status,PILA_test2.2$Status,PILA_test3.2$Status,PILA_test4.2$Status,PILA_test5.2$Status,
                                        PILA_test1.3$Status,PILA_test2.3$Status,PILA_test3.3$Status,PILA_test4.3$Status,PILA_test5.3$Status,
                                        PILA_test1.4$Status,PILA_test2.4$Status,PILA_test3.4$Status,PILA_test4.4$Status,PILA_test5.4$Status,
                                        PILA_test1.5$Status,PILA_test2.5$Status,PILA_test3.5$Status,PILA_test4.5$Status,PILA_test5.5$Status),
                             preds = c(PILA_1.1pred,PILA_2.1pred,PILA_3.1pred,PILA_4.1pred,PILA_5.1pred,
                                       PILA_1.2pred,PILA_2.2pred,PILA_3.2pred,PILA_4.2pred,PILA_5.2pred,
                                       PILA_1.3pred,PILA_2.3pred,PILA_3.3pred,PILA_4.3pred,PILA_5.3pred,
                                       PILA_1.4pred,PILA_2.4pred,PILA_3.4pred,PILA_4.4pred,PILA_5.4pred,
                                       PILA_1.5pred,PILA_2.5pred,PILA_3.5pred,PILA_4.5pred,PILA_5.5pred),
                             model = c(rep(1,length(PILA_1.1pred)),rep(2,length(PILA_2.1pred)),rep(3,length(PILA_3.1pred)),rep(4,length(PILA_4.1pred)),rep(5,length(PILA_5.1pred)),
                                       rep(1,length(PILA_1.2pred)),rep(2,length(PILA_2.2pred)),rep(3,length(PILA_3.2pred)),rep(4,length(PILA_4.2pred)),rep(5,length(PILA_5.2pred)),
                                       rep(1,length(PILA_1.3pred)),rep(2,length(PILA_2.3pred)),rep(3,length(PILA_3.3pred)),rep(4,length(PILA_4.3pred)),rep(5,length(PILA_5.3pred)),
                                       rep(1,length(PILA_1.4pred)),rep(2,length(PILA_2.4pred)),rep(3,length(PILA_3.4pred)),rep(4,length(PILA_4.4pred)),rep(5,length(PILA_5.4pred)),
                                       rep(1,length(PILA_1.5pred)),rep(2,length(PILA_2.5pred)),rep(3,length(PILA_3.5pred)),rep(4,length(PILA_4.5pred)),rep(5,length(PILA_5.5pred))),
                             rep = c(rep(1,length(PILA_1.1pred)),rep(1,length(PILA_2.1pred)),rep(1,length(PILA_3.1pred)),rep(1,length(PILA_4.1pred)),rep(1,length(PILA_5.1pred)),
                                     rep(2,length(PILA_1.2pred)),rep(2,length(PILA_2.2pred)),rep(2,length(PILA_3.2pred)),rep(2,length(PILA_4.2pred)),rep(2,length(PILA_5.2pred)),
                                     rep(3,length(PILA_1.3pred)),rep(3,length(PILA_2.3pred)),rep(3,length(PILA_3.3pred)),rep(3,length(PILA_4.3pred)),rep(3,length(PILA_5.3pred)),
                                     rep(4,length(PILA_1.4pred)),rep(4,length(PILA_2.4pred)),rep(4,length(PILA_3.4pred)),rep(4,length(PILA_4.4pred)),rep(4,length(PILA_5.4pred)),
                                     rep(5,length(PILA_1.5pred)),rep(5,length(PILA_2.5pred)),rep(5,length(PILA_3.5pred)),rep(5,length(PILA_4.5pred)),rep(5,length(PILA_5.5pred))),
                            
                             dead_check = c(PILA_test1.1$dead_check, PILA_test2.1$dead_check, PILA_test3.1$dead_check, PILA_test4.1$dead_check, PILA_test5.1$dead_check,
                                            PILA_test1.2$dead_check, PILA_test2.2$dead_check, PILA_test3.2$dead_check, PILA_test4.2$dead_check, PILA_test5.2$dead_check,
                                            PILA_test1.3$dead_check, PILA_test2.3$dead_check, PILA_test3.3$dead_check, PILA_test4.3$dead_check, PILA_test5.3$dead_check,
                                            PILA_test1.4$dead_check, PILA_test2.4$dead_check, PILA_test3.4$dead_check, PILA_test4.4$dead_check, PILA_test5.4$dead_check,
                                            PILA_test1.5$dead_check, PILA_test2.5$dead_check, PILA_test3.5$dead_check, PILA_test4.5$dead_check, PILA_test5.5$dead_check))



PILA.test.preds$wrong <- ifelse(PILA.test.preds$Status == PILA.test.preds$preds, 0,1)

PILA.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> PILA.wrong.preds


PILA_RF1.1.tab <-table(factor(PILA_test1.1$Status, levels = 0:1), factor(PILA_1.1pred, levels = 0:1))
PILA_LiveAcc1.1<-PILA_RF1.1.tab[1,1]/sum(PILA_RF1.1.tab[1,])
PILA_DeadAcc1.1<-PILA_RF1.1.tab[2,2]/sum(PILA_RF1.1.tab[2,])
PILA_TotalAcc1.1<-(PILA_RF1.1.tab[1,1]+PILA_RF1.1.tab[2,2])/sum(PILA_RF1.1.tab)

PILA_RF2.1.tab <-table(factor(PILA_test2.1$Status, levels = 0:1), factor(PILA_2.1pred, levels = 0:1))
PILA_LiveAcc2.1<-PILA_RF2.1.tab[1,1]/sum(PILA_RF2.1.tab[1,])
PILA_DeadAcc2.1<-PILA_RF2.1.tab[2,2]/sum(PILA_RF2.1.tab[2,])
PILA_TotalAcc2.1<-(PILA_RF2.1.tab[1,1]+PILA_RF2.1.tab[2,2])/sum(PILA_RF2.1.tab)

PILA_RF3.1.tab <-table(factor(PILA_test3.1$Status, levels = 0:1), factor(PILA_3.1pred, levels = 0:1))
PILA_LiveAcc3.1<-PILA_RF3.1.tab[1,1]/sum(PILA_RF3.1.tab[1,])
PILA_DeadAcc3.1<-PILA_RF3.1.tab[2,2]/sum(PILA_RF3.1.tab[2,])
PILA_TotalAcc3.1<-(PILA_RF3.1.tab[1,1]+PILA_RF3.1.tab[2,2])/sum(PILA_RF3.1.tab)

PILA_RF4.1.tab <-table(factor(PILA_test4.1$Status, levels = 0:1), factor(PILA_4.1pred, levels = 0:1))
PILA_LiveAcc4.1<-PILA_RF4.1.tab[1,1]/sum(PILA_RF4.1.tab[1,])
PILA_DeadAcc4.1<-PILA_RF4.1.tab[2,2]/sum(PILA_RF4.1.tab[2,])
PILA_TotalAcc4.1<-(PILA_RF4.1.tab[1,1]+PILA_RF4.1.tab[2,2])/sum(PILA_RF4.1.tab)

PILA_RF5.1.tab <-table(factor(PILA_test5.1$Status, levels = 0:1), factor(PILA_5.1pred, levels = 0:1))
PILA_LiveAcc5.1<-PILA_RF5.1.tab[1,1]/sum(PILA_RF5.1.tab[1,])
PILA_DeadAcc5.1<-PILA_RF5.1.tab[2,2]/sum(PILA_RF5.1.tab[2,])
PILA_TotalAcc5.1<-(PILA_RF5.1.tab[1,1]+PILA_RF5.1.tab[2,2])/sum(PILA_RF5.1.tab)

PILA_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(PILA_LiveAcc1.1,
                                                                              PILA_LiveAcc2.1,
                                                                              PILA_LiveAcc3.1,
                                                                              PILA_LiveAcc4.1,
                                                                              PILA_LiveAcc5.1),
                              DeadAcc = c(PILA_DeadAcc1.1,
                                          PILA_DeadAcc2.1,
                                          PILA_DeadAcc3.1,
                                          PILA_DeadAcc4.1,
                                          PILA_DeadAcc5.1),
                              TotalAcc = c(PILA_TotalAcc1.1,
                                           PILA_TotalAcc2.1,
                                           PILA_TotalAcc3.1,
                                           PILA_TotalAcc4.1,
                                           PILA_TotalAcc5.1))


PILA_RF1.2.tab <-table(factor(PILA_test1.2$Status, levels = 0:1), factor(PILA_1.2pred, levels = 0:1))
PILA_LiveAcc1.2<-PILA_RF1.2.tab[1,1]/sum(PILA_RF1.2.tab[1,])
PILA_DeadAcc1.2<-PILA_RF1.2.tab[2,2]/sum(PILA_RF1.2.tab[2,])
PILA_TotalAcc1.2<-(PILA_RF1.2.tab[1,1]+PILA_RF1.2.tab[2,2])/sum(PILA_RF1.2.tab)

PILA_RF2.2.tab <-table(factor(PILA_test2.2$Status, levels = 0:1), factor(PILA_2.2pred, levels = 0:1))
PILA_LiveAcc2.2<-PILA_RF2.2.tab[1,1]/sum(PILA_RF2.2.tab[1,])
PILA_DeadAcc2.2<-PILA_RF2.2.tab[2,2]/sum(PILA_RF2.2.tab[2,])
PILA_TotalAcc2.2<-(PILA_RF2.2.tab[1,1]+PILA_RF2.2.tab[2,2])/sum(PILA_RF2.2.tab)

PILA_RF3.2.tab <-table(factor(PILA_test3.2$Status, levels = 0:1), factor(PILA_3.2pred, levels = 0:1))
PILA_LiveAcc3.2<-PILA_RF3.2.tab[1,1]/sum(PILA_RF3.2.tab[1,])
PILA_DeadAcc3.2<-PILA_RF3.2.tab[2,2]/sum(PILA_RF3.2.tab[2,])
PILA_TotalAcc3.2<-(PILA_RF3.2.tab[1,1]+PILA_RF3.2.tab[2,2])/sum(PILA_RF3.2.tab)

PILA_RF4.2.tab <-table(factor(PILA_test4.2$Status, levels = 0:1), factor(PILA_4.2pred, levels = 0:1))
PILA_LiveAcc4.2<-PILA_RF4.2.tab[1,1]/sum(PILA_RF4.2.tab[1,])
PILA_DeadAcc4.2<-PILA_RF4.2.tab[2,2]/sum(PILA_RF4.2.tab[2,])
PILA_TotalAcc4.2<-(PILA_RF4.2.tab[1,1]+PILA_RF4.2.tab[2,2])/sum(PILA_RF4.2.tab)

PILA_RF5.2.tab <-table(factor(PILA_test5.2$Status, levels = 0:1), factor(PILA_5.2pred, levels = 0:1))
PILA_LiveAcc5.2<-PILA_RF5.2.tab[1,1]/sum(PILA_RF5.2.tab[1,])
PILA_DeadAcc5.2<-PILA_RF5.2.tab[2,2]/sum(PILA_RF5.2.tab[2,])
PILA_TotalAcc5.2<-(PILA_RF5.2.tab[1,1]+PILA_RF5.2.tab[2,2])/sum(PILA_RF5.2.tab)

PILA_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(PILA_LiveAcc1.2,
                                                                              PILA_LiveAcc2.2,
                                                                              PILA_LiveAcc3.2,
                                                                              PILA_LiveAcc4.2,
                                                                              PILA_LiveAcc5.2),
                              DeadAcc = c(PILA_DeadAcc1.2,
                                          PILA_DeadAcc2.2,
                                          PILA_DeadAcc3.2,
                                          PILA_DeadAcc4.2,
                                          PILA_DeadAcc5.2),
                              TotalAcc = c(PILA_TotalAcc1.2,
                                           PILA_TotalAcc2.2,
                                           PILA_TotalAcc3.2,
                                           PILA_TotalAcc4.2,
                                           PILA_TotalAcc5.2))

PILA_RF1.3.tab <-table(factor(PILA_test1.3$Status, levels = 0:1), factor(PILA_1.3pred, levels = 0:1))
PILA_LiveAcc1.3<-PILA_RF1.3.tab[1,1]/sum(PILA_RF1.3.tab[1,])
PILA_DeadAcc1.3<-PILA_RF1.3.tab[2,2]/sum(PILA_RF1.3.tab[2,])
PILA_TotalAcc1.3<-(PILA_RF1.3.tab[1,1]+PILA_RF1.3.tab[2,2])/sum(PILA_RF1.3.tab)

PILA_RF2.3.tab <-table(factor(PILA_test2.3$Status, levels = 0:1), factor(PILA_2.3pred, levels = 0:1))
PILA_LiveAcc2.3<-PILA_RF2.3.tab[1,1]/sum(PILA_RF2.3.tab[1,])
PILA_DeadAcc2.3<-PILA_RF2.3.tab[2,2]/sum(PILA_RF2.3.tab[2,])
PILA_TotalAcc2.3<-(PILA_RF2.3.tab[1,1]+PILA_RF2.3.tab[2,2])/sum(PILA_RF2.3.tab)

PILA_RF3.3.tab <-table(factor(PILA_test3.3$Status, levels = 0:1), factor(PILA_3.3pred, levels = 0:1))
PILA_LiveAcc3.3<-PILA_RF3.3.tab[1,1]/sum(PILA_RF3.3.tab[1,])
PILA_DeadAcc3.3<-PILA_RF3.3.tab[2,2]/sum(PILA_RF3.3.tab[2,])
PILA_TotalAcc3.3<-(PILA_RF3.3.tab[1,1]+PILA_RF3.3.tab[2,2])/sum(PILA_RF3.3.tab)

PILA_RF4.3.tab <-table(factor(PILA_test4.3$Status, levels = 0:1), factor(PILA_4.3pred, levels = 0:1))
PILA_LiveAcc4.3<-PILA_RF4.3.tab[1,1]/sum(PILA_RF4.3.tab[1,])
PILA_DeadAcc4.3<-PILA_RF4.3.tab[2,2]/sum(PILA_RF4.3.tab[2,])
PILA_TotalAcc4.3<-(PILA_RF4.3.tab[1,1]+PILA_RF4.3.tab[2,2])/sum(PILA_RF4.3.tab)

PILA_RF5.3.tab <-table(factor(PILA_test5.3$Status, levels = 0:1), factor(PILA_5.3pred, levels = 0:1))
PILA_LiveAcc5.3<-PILA_RF5.3.tab[1,1]/sum(PILA_RF5.3.tab[1,])
PILA_DeadAcc5.3<-PILA_RF5.3.tab[2,2]/sum(PILA_RF5.3.tab[2,])
PILA_TotalAcc5.3<-(PILA_RF5.3.tab[1,1]+PILA_RF5.3.tab[2,2])/sum(PILA_RF5.3.tab)

PILA_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(PILA_LiveAcc1.3,
                                                                              PILA_LiveAcc2.3,
                                                                              PILA_LiveAcc3.3,
                                                                              PILA_LiveAcc4.3,
                                                                              PILA_LiveAcc5.3),
                              DeadAcc = c(PILA_DeadAcc1.3,
                                          PILA_DeadAcc2.3,
                                          PILA_DeadAcc3.3,
                                          PILA_DeadAcc4.3,
                                          PILA_DeadAcc5.3),
                              TotalAcc = c(PILA_TotalAcc1.3,
                                           PILA_TotalAcc2.3,
                                           PILA_TotalAcc3.3,
                                           PILA_TotalAcc4.3,
                                           PILA_TotalAcc5.3))

PILA_RF1.4.tab <-table(factor(PILA_test1.4$Status, levels = 0:1), factor(PILA_1.4pred, levels = 0:1))
PILA_LiveAcc1.4<-PILA_RF1.4.tab[1,1]/sum(PILA_RF1.4.tab[1,])
PILA_DeadAcc1.4<-PILA_RF1.4.tab[2,2]/sum(PILA_RF1.4.tab[2,])
PILA_TotalAcc1.4<-(PILA_RF1.4.tab[1,1]+PILA_RF1.4.tab[2,2])/sum(PILA_RF1.4.tab)

PILA_RF2.4.tab <-table(factor(PILA_test2.4$Status, levels = 0:1), factor(PILA_2.4pred, levels = 0:1))
PILA_LiveAcc2.4<-PILA_RF2.4.tab[1,1]/sum(PILA_RF2.4.tab[1,])
PILA_DeadAcc2.4<-PILA_RF2.4.tab[2,2]/sum(PILA_RF2.4.tab[2,])
PILA_TotalAcc2.4<-(PILA_RF2.4.tab[1,1]+PILA_RF2.4.tab[2,2])/sum(PILA_RF2.4.tab)

PILA_RF3.4.tab <-table(factor(PILA_test3.4$Status, levels = 0:1), factor(PILA_3.4pred, levels = 0:1))
PILA_LiveAcc3.4<-PILA_RF3.4.tab[1,1]/sum(PILA_RF3.4.tab[1,])
PILA_DeadAcc3.4<-PILA_RF3.4.tab[2,2]/sum(PILA_RF3.4.tab[2,])
PILA_TotalAcc3.4<-(PILA_RF3.4.tab[1,1]+PILA_RF3.4.tab[2,2])/sum(PILA_RF3.4.tab)

PILA_RF4.4.tab <-table(factor(PILA_test4.4$Status, levels = 0:1), factor(PILA_4.4pred, levels = 0:1))
PILA_LiveAcc4.4<-PILA_RF4.4.tab[1,1]/sum(PILA_RF4.4.tab[1,])
PILA_DeadAcc4.4<-PILA_RF4.4.tab[2,2]/sum(PILA_RF4.4.tab[2,])
PILA_TotalAcc4.4<-(PILA_RF4.4.tab[1,1]+PILA_RF4.4.tab[2,2])/sum(PILA_RF4.4.tab)

PILA_RF5.4.tab <-table(factor(PILA_test5.4$Status, levels = 0:1), factor(PILA_5.4pred, levels = 0:1))
PILA_LiveAcc5.4<-PILA_RF5.4.tab[1,1]/sum(PILA_RF5.4.tab[1,])
PILA_DeadAcc5.4<-PILA_RF5.4.tab[2,2]/sum(PILA_RF5.4.tab[2,])
PILA_TotalAcc5.4<-(PILA_RF5.4.tab[1,1]+PILA_RF5.4.tab[2,2])/sum(PILA_RF5.4.tab)

PILA_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(PILA_LiveAcc1.4,
                                                                              PILA_LiveAcc2.4,
                                                                              PILA_LiveAcc3.4,
                                                                              PILA_LiveAcc4.4,
                                                                              PILA_LiveAcc5.4),
                              DeadAcc = c(PILA_DeadAcc1.4,
                                          PILA_DeadAcc2.4,
                                          PILA_DeadAcc3.4,
                                          PILA_DeadAcc4.4,
                                          PILA_DeadAcc5.4),
                              TotalAcc = c(PILA_TotalAcc1.4,
                                           PILA_TotalAcc2.4,
                                           PILA_TotalAcc3.4,
                                           PILA_TotalAcc4.4,
                                           PILA_TotalAcc5.4))

PILA_RF1.5.tab <-table(factor(PILA_test1.5$Status, levels = 0:1), factor(PILA_1.5pred, levels = 0:1))
PILA_LiveAcc1.5<-PILA_RF1.5.tab[1,1]/sum(PILA_RF1.5.tab[1,])
PILA_DeadAcc1.5<-PILA_RF1.5.tab[2,2]/sum(PILA_RF1.5.tab[2,])
PILA_TotalAcc1.5<-(PILA_RF1.5.tab[1,1]+PILA_RF1.5.tab[2,2])/sum(PILA_RF1.5.tab)

PILA_RF2.5.tab <-table(factor(PILA_test2.5$Status, levels = 0:1), factor(PILA_2.5pred, levels = 0:1))
PILA_LiveAcc2.5<-PILA_RF2.5.tab[1,1]/sum(PILA_RF2.5.tab[1,])
PILA_DeadAcc2.5<-PILA_RF2.5.tab[2,2]/sum(PILA_RF2.5.tab[2,])
PILA_TotalAcc2.5<-(PILA_RF2.5.tab[1,1]+PILA_RF2.5.tab[2,2])/sum(PILA_RF2.5.tab)

PILA_RF3.5.tab <-table(factor(PILA_test3.5$Status, levels = 0:1), factor(PILA_3.5pred, levels = 0:1))
PILA_LiveAcc3.5<-PILA_RF3.5.tab[1,1]/sum(PILA_RF3.5.tab[1,])
PILA_DeadAcc3.5<-PILA_RF3.5.tab[2,2]/sum(PILA_RF3.5.tab[2,])
PILA_TotalAcc3.5<-(PILA_RF3.5.tab[1,1]+PILA_RF3.5.tab[2,2])/sum(PILA_RF3.5.tab)

PILA_RF4.5.tab <-table(factor(PILA_test4.5$Status, levels = 0:1), factor(PILA_4.5pred, levels = 0:1))
PILA_LiveAcc4.5<-PILA_RF4.5.tab[1,1]/sum(PILA_RF4.5.tab[1,])
PILA_DeadAcc4.5<-PILA_RF4.5.tab[2,2]/sum(PILA_RF4.5.tab[2,])
PILA_TotalAcc4.5<-(PILA_RF4.5.tab[1,1]+PILA_RF4.5.tab[2,2])/sum(PILA_RF4.5.tab)

PILA_RF5.5.tab <-table(factor(PILA_test5.5$Status, levels = 0:1), factor(PILA_5.5pred, levels = 0:1))
PILA_LiveAcc5.5<-PILA_RF5.5.tab[1,1]/sum(PILA_RF5.5.tab[1,])
PILA_DeadAcc5.5<-PILA_RF5.5.tab[2,2]/sum(PILA_RF5.5.tab[2,])
PILA_TotalAcc5.5<-(PILA_RF5.5.tab[1,1]+PILA_RF5.5.tab[2,2])/sum(PILA_RF5.5.tab)

PILA_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(PILA_LiveAcc1.5,
                                                                              PILA_LiveAcc2.5,
                                                                              PILA_LiveAcc3.5,
                                                                              PILA_LiveAcc4.5,
                                                                              PILA_LiveAcc5.5),
                              DeadAcc = c(PILA_DeadAcc1.5,
                                          PILA_DeadAcc2.5,
                                          PILA_DeadAcc3.5,
                                          PILA_DeadAcc4.5,
                                          PILA_DeadAcc5.5),
                              TotalAcc = c(PILA_TotalAcc1.5,
                                           PILA_TotalAcc2.5,
                                           PILA_TotalAcc3.5,
                                           PILA_TotalAcc4.5,
                                           PILA_TotalAcc5.5))

PILA_pred.results <-rbind(PILA_Pre.results1,PILA_Pre.results2,PILA_Pre.results3,PILA_Pre.results4,PILA_Pre.results5)


PILA_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "PILA",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> PILA_RF_preds.sum



#############PIPO######################
#############subset species###########

PIPO <- subset(Tree_data, Species == 'PIPO')
PIPO$OBS_ID <- 1:nrow(PIPO)

###Get complete cases##########
PIPO_yr1 <- select(PIPO, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)

PIPO_yr2 <- select(PIPO, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

PIPO_yr3 <- select(PIPO, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

PIPO_yr4 <- select(PIPO, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

PIPO_yr5 <- select(PIPO, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
PIPO_flds1<-createFolds(PIPO_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
PIPO_flds2<-createFolds(PIPO_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PIPO_flds3<-createFolds(PIPO_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PIPO_flds4<-createFolds(PIPO_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PIPO_flds5<-createFolds(PIPO_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 



##1
set.seed(1980)
samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[1]],]$Status))))
PIPO_train.rf1.1 <- randomForest(data = PIPO_yr1[-PIPO_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr2[-PIPO_flds2[[1]],]$Status))))
PIPO_train.rf2.1 <- randomForest(data = PIPO_yr2[-PIPO_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PIPO_yr3[-PIPO_flds3[[1]],]$Status))))
PIPO_train.rf3.1 <- randomForest(data = PIPO_yr3[-PIPO_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr4[-PIPO_flds4[[1]],]$Status))))
PIPO_train.rf4.1 <- randomForest(data = PIPO_yr4[-PIPO_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr5[-PIPO_flds5[[1]],]$Status))))
PIPO_train.rf5.1 <- randomForest(data = PIPO_yr5[-PIPO_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
varImpPlot(PIPO_train.rf5.1)

##2

samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[2]],]$Status))))
PIPO_train.rf1.2 <- randomForest(data = PIPO_yr1[-PIPO_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr2[-PIPO_flds2[[2]],]$Status))))
PIPO_train.rf2.2 <- randomForest(data = PIPO_yr2[-PIPO_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PIPO_yr3[-PIPO_flds3[[2]],]$Status))))
PIPO_train.rf3.2 <- randomForest(data = PIPO_yr3[-PIPO_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr4[-PIPO_flds4[[2]],]$Status))))
PIPO_train.rf4.2 <- randomForest(data = PIPO_yr4[-PIPO_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr5[-PIPO_flds5[[2]],]$Status))))
PIPO_train.rf5.2 <- randomForest(data = PIPO_yr5[-PIPO_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[3]],]$Status))))
PIPO_train.rf1.3 <- randomForest(data = PIPO_yr1[-PIPO_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr2[-PIPO_flds2[[3]],]$Status))))
PIPO_train.rf2.3 <- randomForest(data = PIPO_yr2[-PIPO_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PIPO_yr3[-PIPO_flds3[[3]],]$Status))))
PIPO_train.rf3.3 <- randomForest(data = PIPO_yr3[-PIPO_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr4[-PIPO_flds4[[3]],]$Status))))
PIPO_train.rf4.3 <- randomForest(data = PIPO_yr4[-PIPO_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr5[-PIPO_flds5[[3]],]$Status))))
PIPO_train.rf5.3 <- randomForest(data = PIPO_yr5[-PIPO_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[4]],]$Status))))
PIPO_train.rf1.4 <- randomForest(data = PIPO_yr1[-PIPO_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr2[-PIPO_flds2[[4]],]$Status))))
PIPO_train.rf2.4 <- randomForest(data = PIPO_yr2[-PIPO_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PIPO_yr3[-PIPO_flds3[[4]],]$Status))))
PIPO_train.rf3.4 <- randomForest(data = PIPO_yr3[-PIPO_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr4[-PIPO_flds4[[4]],]$Status))))
PIPO_train.rf4.4 <- randomForest(data = PIPO_yr4[-PIPO_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr5[-PIPO_flds5[[4]],]$Status))))
PIPO_train.rf5.4 <- randomForest(data = PIPO_yr5[-PIPO_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PIPO_yr1[-PIPO_flds1[[5]],]$Status))))
PIPO_train.rf1.5 <- randomForest(data = PIPO_yr1[-PIPO_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr2[-PIPO_flds2[[5]],]$Status))))
PIPO_train.rf2.5 <- randomForest(data = PIPO_yr2[-PIPO_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PIPO_yr3[-PIPO_flds3[[5]],]$Status))))
PIPO_train.rf3.5 <- randomForest(data = PIPO_yr3[-PIPO_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr4[-PIPO_flds4[[5]],]$Status))))
PIPO_train.rf4.5 <- randomForest(data = PIPO_yr4[-PIPO_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PIPO_yr5[-PIPO_flds5[[5]],]$Status))))
PIPO_train.rf5.5 <- randomForest(data = PIPO_yr5[-PIPO_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)



##################Create test datasets#####################
##1
PIPO_test1.1 <- PIPO_yr1[PIPO_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(PIPO_yr1[PIPO_flds1[[1]],] $  exclude=NULL)
table(PIPO_test1.1$  exclude=NULL)

PIPO_test2.1 <- PIPO_yr2[PIPO_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test3.1 <- PIPO_yr3[PIPO_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test4.1 <- PIPO_yr4[PIPO_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test5.1 <- PIPO_yr5[PIPO_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
PIPO_test1.2 <- PIPO_yr1[PIPO_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test2.2 <- PIPO_yr2[PIPO_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test3.2 <- PIPO_yr3[PIPO_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test4.2 <- PIPO_yr4[PIPO_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test5.2 <- PIPO_yr5[PIPO_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##3
PIPO_test1.3 <- PIPO_yr1[PIPO_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test2.3 <- PIPO_yr2[PIPO_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test3.3 <- PIPO_yr3[PIPO_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test4.3 <- PIPO_yr4[PIPO_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test5.3 <- PIPO_yr5[PIPO_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##4
PIPO_test1.4 <- PIPO_yr1[PIPO_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PIPO_test2.4 <- PIPO_yr2[PIPO_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test3.4 <- PIPO_yr3[PIPO_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test4.4 <- PIPO_yr4[PIPO_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PIPO_test5.4 <- PIPO_yr5[PIPO_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##5
PIPO_test1.5 <- PIPO_yr1[PIPO_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test2.5 <- PIPO_yr2[PIPO_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test3.5 <- PIPO_yr3[PIPO_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test4.5 <- PIPO_yr4[PIPO_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PIPO_test5.5 <- PIPO_yr5[PIPO_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



#######################Predicting###############################
PIPO_pred.results <-data.frame() 

PIPO_1.1pred <- predict(PIPO_train.rf1.1, newdata=PIPO_test1.1)
PIPO_2.1pred <- predict(PIPO_train.rf2.1, newdata=PIPO_test2.1)
PIPO_3.1pred <- predict(PIPO_train.rf3.1, newdata=PIPO_test3.1)
PIPO_4.1pred <- predict(PIPO_train.rf4.1, newdata=PIPO_test4.1)
PIPO_5.1pred <- predict(PIPO_train.rf5.1, newdata=PIPO_test5.1)

PIPO_1.2pred <- predict(PIPO_train.rf1.2, newdata=PIPO_test1.2)
PIPO_2.2pred <- predict(PIPO_train.rf2.2, newdata=PIPO_test2.2)
PIPO_3.2pred <- predict(PIPO_train.rf3.2, newdata=PIPO_test3.2)
PIPO_4.2pred <- predict(PIPO_train.rf4.2, newdata=PIPO_test4.2)
PIPO_5.2pred <- predict(PIPO_train.rf5.2, newdata=PIPO_test5.2)

PIPO_1.3pred <- predict(PIPO_train.rf1.3, newdata=PIPO_test1.3)
PIPO_2.3pred <- predict(PIPO_train.rf2.3, newdata=PIPO_test2.3)
PIPO_3.3pred <- predict(PIPO_train.rf3.3, newdata=PIPO_test3.3)
PIPO_4.3pred <- predict(PIPO_train.rf4.3, newdata=PIPO_test4.3)
PIPO_5.3pred <- predict(PIPO_train.rf5.3, newdata=PIPO_test5.3)

PIPO_1.4pred <- predict(PIPO_train.rf1.4, newdata=PIPO_test1.4)
PIPO_2.4pred <- predict(PIPO_train.rf2.4, newdata=PIPO_test2.4)
PIPO_3.4pred <- predict(PIPO_train.rf3.4, newdata=PIPO_test3.4)
PIPO_4.4pred <- predict(PIPO_train.rf4.4, newdata=PIPO_test4.4)
PIPO_5.4pred <- predict(PIPO_train.rf5.4, newdata=PIPO_test5.4)

PIPO_1.5pred <- predict(PIPO_train.rf1.5, newdata=PIPO_test1.5)
PIPO_2.5pred <- predict(PIPO_train.rf2.5, newdata=PIPO_test2.5)
PIPO_3.5pred <- predict(PIPO_train.rf3.5, newdata=PIPO_test3.5)
PIPO_4.5pred <- predict(PIPO_train.rf4.5, newdata=PIPO_test4.5)
PIPO_5.5pred <- predict(PIPO_train.rf5.5, newdata=PIPO_test5.5)


PIPO.test.preds <-data.frame(Status = c(PIPO_test1.1$Status,PIPO_test2.1$Status,PIPO_test3.1$Status,PIPO_test4.1$Status,PIPO_test5.1$Status,
                                        PIPO_test1.2$Status,PIPO_test2.2$Status,PIPO_test3.2$Status,PIPO_test4.2$Status,PIPO_test5.2$Status,
                                        PIPO_test1.3$Status,PIPO_test2.3$Status,PIPO_test3.3$Status,PIPO_test4.3$Status,PIPO_test5.3$Status,
                                        PIPO_test1.4$Status,PIPO_test2.4$Status,PIPO_test3.4$Status,PIPO_test4.4$Status,PIPO_test5.4$Status,
                                        PIPO_test1.5$Status,PIPO_test2.5$Status,PIPO_test3.5$Status,PIPO_test4.5$Status,PIPO_test5.5$Status),
                             preds = c(PIPO_1.1pred,PIPO_2.1pred,PIPO_3.1pred,PIPO_4.1pred,PIPO_5.1pred,
                                       PIPO_1.2pred,PIPO_2.2pred,PIPO_3.2pred,PIPO_4.2pred,PIPO_5.2pred,
                                       PIPO_1.3pred,PIPO_2.3pred,PIPO_3.3pred,PIPO_4.3pred,PIPO_5.3pred,
                                       PIPO_1.4pred,PIPO_2.4pred,PIPO_3.4pred,PIPO_4.4pred,PIPO_5.4pred,
                                       PIPO_1.5pred,PIPO_2.5pred,PIPO_3.5pred,PIPO_4.5pred,PIPO_5.5pred),
                             model = c(rep(1,length(PIPO_1.1pred)),rep(2,length(PIPO_2.1pred)),rep(3,length(PIPO_3.1pred)),rep(4,length(PIPO_4.1pred)),rep(5,length(PIPO_5.1pred)),
                                       rep(1,length(PIPO_1.2pred)),rep(2,length(PIPO_2.2pred)),rep(3,length(PIPO_3.2pred)),rep(4,length(PIPO_4.2pred)),rep(5,length(PIPO_5.2pred)),
                                       rep(1,length(PIPO_1.3pred)),rep(2,length(PIPO_2.3pred)),rep(3,length(PIPO_3.3pred)),rep(4,length(PIPO_4.3pred)),rep(5,length(PIPO_5.3pred)),
                                       rep(1,length(PIPO_1.4pred)),rep(2,length(PIPO_2.4pred)),rep(3,length(PIPO_3.4pred)),rep(4,length(PIPO_4.4pred)),rep(5,length(PIPO_5.4pred)),
                                       rep(1,length(PIPO_1.5pred)),rep(2,length(PIPO_2.5pred)),rep(3,length(PIPO_3.5pred)),rep(4,length(PIPO_4.5pred)),rep(5,length(PIPO_5.5pred))),
                             rep = c(rep(1,length(PIPO_1.1pred)),rep(1,length(PIPO_2.1pred)),rep(1,length(PIPO_3.1pred)),rep(1,length(PIPO_4.1pred)),rep(1,length(PIPO_5.1pred)),
                                     rep(2,length(PIPO_1.2pred)),rep(2,length(PIPO_2.2pred)),rep(2,length(PIPO_3.2pred)),rep(2,length(PIPO_4.2pred)),rep(2,length(PIPO_5.2pred)),
                                     rep(3,length(PIPO_1.3pred)),rep(3,length(PIPO_2.3pred)),rep(3,length(PIPO_3.3pred)),rep(3,length(PIPO_4.3pred)),rep(3,length(PIPO_5.3pred)),
                                     rep(4,length(PIPO_1.4pred)),rep(4,length(PIPO_2.4pred)),rep(4,length(PIPO_3.4pred)),rep(4,length(PIPO_4.4pred)),rep(4,length(PIPO_5.4pred)),
                                     rep(5,length(PIPO_1.5pred)),rep(5,length(PIPO_2.5pred)),rep(5,length(PIPO_3.5pred)),rep(5,length(PIPO_4.5pred)),rep(5,length(PIPO_5.5pred))),
                            
                             dead_check = c(PIPO_test1.1$dead_check, PIPO_test2.1$dead_check, PIPO_test3.1$dead_check, PIPO_test4.1$dead_check, PIPO_test5.1$dead_check,
                                            PIPO_test1.2$dead_check, PIPO_test2.2$dead_check, PIPO_test3.2$dead_check, PIPO_test4.2$dead_check, PIPO_test5.2$dead_check,
                                            PIPO_test1.3$dead_check, PIPO_test2.3$dead_check, PIPO_test3.3$dead_check, PIPO_test4.3$dead_check, PIPO_test5.3$dead_check,
                                            PIPO_test1.4$dead_check, PIPO_test2.4$dead_check, PIPO_test3.4$dead_check, PIPO_test4.4$dead_check, PIPO_test5.4$dead_check,
                                            PIPO_test1.5$dead_check, PIPO_test2.5$dead_check, PIPO_test3.5$dead_check, PIPO_test4.5$dead_check, PIPO_test5.5$dead_check))



PIPO.test.preds$wrong <- ifelse(PIPO.test.preds$Status == PIPO.test.preds$preds, 0,1)

PIPO.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> PIPO.wrong.preds



PIPO_RF1.1.tab <-table(factor(PIPO_test1.1$Status, levels = 0:1), factor(PIPO_1.1pred, levels = 0:1))
PIPO_LiveAcc1.1<-PIPO_RF1.1.tab[1,1]/sum(PIPO_RF1.1.tab[1,])
PIPO_DeadAcc1.1<-PIPO_RF1.1.tab[2,2]/sum(PIPO_RF1.1.tab[2,])
PIPO_TotalAcc1.1<-(PIPO_RF1.1.tab[1,1]+PIPO_RF1.1.tab[2,2])/sum(PIPO_RF1.1.tab)

PIPO_RF2.1.tab <-table(factor(PIPO_test2.1$Status, levels = 0:1), factor(PIPO_2.1pred, levels = 0:1))
PIPO_LiveAcc2.1<-PIPO_RF2.1.tab[1,1]/sum(PIPO_RF2.1.tab[1,])
PIPO_DeadAcc2.1<-PIPO_RF2.1.tab[2,2]/sum(PIPO_RF2.1.tab[2,])
PIPO_TotalAcc2.1<-(PIPO_RF2.1.tab[1,1]+PIPO_RF2.1.tab[2,2])/sum(PIPO_RF2.1.tab)

PIPO_RF3.1.tab <-table(factor(PIPO_test3.1$Status, levels = 0:1), factor(PIPO_3.1pred, levels = 0:1))
PIPO_LiveAcc3.1<-PIPO_RF3.1.tab[1,1]/sum(PIPO_RF3.1.tab[1,])
PIPO_DeadAcc3.1<-PIPO_RF3.1.tab[2,2]/sum(PIPO_RF3.1.tab[2,])
PIPO_TotalAcc3.1<-(PIPO_RF3.1.tab[1,1]+PIPO_RF3.1.tab[2,2])/sum(PIPO_RF3.1.tab)

PIPO_RF4.1.tab <-table(factor(PIPO_test4.1$Status, levels = 0:1), factor(PIPO_4.1pred, levels = 0:1))
PIPO_LiveAcc4.1<-PIPO_RF4.1.tab[1,1]/sum(PIPO_RF4.1.tab[1,])
PIPO_DeadAcc4.1<-PIPO_RF4.1.tab[2,2]/sum(PIPO_RF4.1.tab[2,])
PIPO_TotalAcc4.1<-(PIPO_RF4.1.tab[1,1]+PIPO_RF4.1.tab[2,2])/sum(PIPO_RF4.1.tab)

PIPO_RF5.1.tab <-table(factor(PIPO_test5.1$Status, levels = 0:1), factor(PIPO_5.1pred, levels = 0:1))
PIPO_LiveAcc5.1<-PIPO_RF5.1.tab[1,1]/sum(PIPO_RF5.1.tab[1,])
PIPO_DeadAcc5.1<-PIPO_RF5.1.tab[2,2]/sum(PIPO_RF5.1.tab[2,])
PIPO_TotalAcc5.1<-(PIPO_RF5.1.tab[1,1]+PIPO_RF5.1.tab[2,2])/sum(PIPO_RF5.1.tab)

PIPO_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(PIPO_LiveAcc1.1,
                                                                              PIPO_LiveAcc2.1,
                                                                              PIPO_LiveAcc3.1,
                                                                              PIPO_LiveAcc4.1,
                                                                              PIPO_LiveAcc5.1),
                              DeadAcc = c(PIPO_DeadAcc1.1,
                                          PIPO_DeadAcc2.1,
                                          PIPO_DeadAcc3.1,
                                          PIPO_DeadAcc4.1,
                                          PIPO_DeadAcc5.1),
                              TotalAcc = c(PIPO_TotalAcc1.1,
                                           PIPO_TotalAcc2.1,
                                           PIPO_TotalAcc3.1,
                                           PIPO_TotalAcc4.1,
                                           PIPO_TotalAcc5.1))


PIPO_RF1.2.tab <-table(factor(PIPO_test1.2$Status, levels = 0:1), factor(PIPO_1.2pred, levels = 0:1))
PIPO_LiveAcc1.2<-PIPO_RF1.2.tab[1,1]/sum(PIPO_RF1.2.tab[1,])
PIPO_DeadAcc1.2<-PIPO_RF1.2.tab[2,2]/sum(PIPO_RF1.2.tab[2,])
PIPO_TotalAcc1.2<-(PIPO_RF1.2.tab[1,1]+PIPO_RF1.2.tab[2,2])/sum(PIPO_RF1.2.tab)

PIPO_RF2.2.tab <-table(factor(PIPO_test2.2$Status, levels = 0:1), factor(PIPO_2.2pred, levels = 0:1))
PIPO_LiveAcc2.2<-PIPO_RF2.2.tab[1,1]/sum(PIPO_RF2.2.tab[1,])
PIPO_DeadAcc2.2<-PIPO_RF2.2.tab[2,2]/sum(PIPO_RF2.2.tab[2,])
PIPO_TotalAcc2.2<-(PIPO_RF2.2.tab[1,1]+PIPO_RF2.2.tab[2,2])/sum(PIPO_RF2.2.tab)

PIPO_RF3.2.tab <-table(factor(PIPO_test3.2$Status, levels = 0:1), factor(PIPO_3.2pred, levels = 0:1))
PIPO_LiveAcc3.2<-PIPO_RF3.2.tab[1,1]/sum(PIPO_RF3.2.tab[1,])
PIPO_DeadAcc3.2<-PIPO_RF3.2.tab[2,2]/sum(PIPO_RF3.2.tab[2,])
PIPO_TotalAcc3.2<-(PIPO_RF3.2.tab[1,1]+PIPO_RF3.2.tab[2,2])/sum(PIPO_RF3.2.tab)

PIPO_RF4.2.tab <-table(factor(PIPO_test4.2$Status, levels = 0:1), factor(PIPO_4.2pred, levels = 0:1))
PIPO_LiveAcc4.2<-PIPO_RF4.2.tab[1,1]/sum(PIPO_RF4.2.tab[1,])
PIPO_DeadAcc4.2<-PIPO_RF4.2.tab[2,2]/sum(PIPO_RF4.2.tab[2,])
PIPO_TotalAcc4.2<-(PIPO_RF4.2.tab[1,1]+PIPO_RF4.2.tab[2,2])/sum(PIPO_RF4.2.tab)

PIPO_RF5.2.tab <-table(factor(PIPO_test5.2$Status, levels = 0:1), factor(PIPO_5.2pred, levels = 0:1))
PIPO_LiveAcc5.2<-PIPO_RF5.2.tab[1,1]/sum(PIPO_RF5.2.tab[1,])
PIPO_DeadAcc5.2<-PIPO_RF5.2.tab[2,2]/sum(PIPO_RF5.2.tab[2,])
PIPO_TotalAcc5.2<-(PIPO_RF5.2.tab[1,1]+PIPO_RF5.2.tab[2,2])/sum(PIPO_RF5.2.tab)

PIPO_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(PIPO_LiveAcc1.2,
                                                                              PIPO_LiveAcc2.2,
                                                                              PIPO_LiveAcc3.2,
                                                                              PIPO_LiveAcc4.2,
                                                                              PIPO_LiveAcc5.2),
                              DeadAcc = c(PIPO_DeadAcc1.2,
                                          PIPO_DeadAcc2.2,
                                          PIPO_DeadAcc3.2,
                                          PIPO_DeadAcc4.2,
                                          PIPO_DeadAcc5.2),
                              TotalAcc = c(PIPO_TotalAcc1.2,
                                           PIPO_TotalAcc2.2,
                                           PIPO_TotalAcc3.2,
                                           PIPO_TotalAcc4.2,
                                           PIPO_TotalAcc5.2))

PIPO_RF1.3.tab <-table(factor(PIPO_test1.3$Status, levels = 0:1), factor(PIPO_1.3pred, levels = 0:1))
PIPO_LiveAcc1.3<-PIPO_RF1.3.tab[1,1]/sum(PIPO_RF1.3.tab[1,])
PIPO_DeadAcc1.3<-PIPO_RF1.3.tab[2,2]/sum(PIPO_RF1.3.tab[2,])
PIPO_TotalAcc1.3<-(PIPO_RF1.3.tab[1,1]+PIPO_RF1.3.tab[2,2])/sum(PIPO_RF1.3.tab)

PIPO_RF2.3.tab <-table(factor(PIPO_test2.3$Status, levels = 0:1), factor(PIPO_2.3pred, levels = 0:1))
PIPO_LiveAcc2.3<-PIPO_RF2.3.tab[1,1]/sum(PIPO_RF2.3.tab[1,])
PIPO_DeadAcc2.3<-PIPO_RF2.3.tab[2,2]/sum(PIPO_RF2.3.tab[2,])
PIPO_TotalAcc2.3<-(PIPO_RF2.3.tab[1,1]+PIPO_RF2.3.tab[2,2])/sum(PIPO_RF2.3.tab)

PIPO_RF3.3.tab <-table(factor(PIPO_test3.3$Status, levels = 0:1), factor(PIPO_3.3pred, levels = 0:1))
PIPO_LiveAcc3.3<-PIPO_RF3.3.tab[1,1]/sum(PIPO_RF3.3.tab[1,])
PIPO_DeadAcc3.3<-PIPO_RF3.3.tab[2,2]/sum(PIPO_RF3.3.tab[2,])
PIPO_TotalAcc3.3<-(PIPO_RF3.3.tab[1,1]+PIPO_RF3.3.tab[2,2])/sum(PIPO_RF3.3.tab)

PIPO_RF4.3.tab <-table(factor(PIPO_test4.3$Status, levels = 0:1), factor(PIPO_4.3pred, levels = 0:1))
PIPO_LiveAcc4.3<-PIPO_RF4.3.tab[1,1]/sum(PIPO_RF4.3.tab[1,])
PIPO_DeadAcc4.3<-PIPO_RF4.3.tab[2,2]/sum(PIPO_RF4.3.tab[2,])
PIPO_TotalAcc4.3<-(PIPO_RF4.3.tab[1,1]+PIPO_RF4.3.tab[2,2])/sum(PIPO_RF4.3.tab)

PIPO_RF5.3.tab <-table(factor(PIPO_test5.3$Status, levels = 0:1), factor(PIPO_5.3pred, levels = 0:1))
PIPO_LiveAcc5.3<-PIPO_RF5.3.tab[1,1]/sum(PIPO_RF5.3.tab[1,])
PIPO_DeadAcc5.3<-PIPO_RF5.3.tab[2,2]/sum(PIPO_RF5.3.tab[2,])
PIPO_TotalAcc5.3<-(PIPO_RF5.3.tab[1,1]+PIPO_RF5.3.tab[2,2])/sum(PIPO_RF5.3.tab)

PIPO_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(PIPO_LiveAcc1.3,
                                                                              PIPO_LiveAcc2.3,
                                                                              PIPO_LiveAcc3.3,
                                                                              PIPO_LiveAcc4.3,
                                                                              PIPO_LiveAcc5.3),
                              DeadAcc = c(PIPO_DeadAcc1.3,
                                          PIPO_DeadAcc2.3,
                                          PIPO_DeadAcc3.3,
                                          PIPO_DeadAcc4.3,
                                          PIPO_DeadAcc5.3),
                              TotalAcc = c(PIPO_TotalAcc1.3,
                                           PIPO_TotalAcc2.3,
                                           PIPO_TotalAcc3.3,
                                           PIPO_TotalAcc4.3,
                                           PIPO_TotalAcc5.3))

PIPO_RF1.4.tab <-table(factor(PIPO_test1.4$Status, levels = 0:1), factor(PIPO_1.4pred, levels = 0:1))
PIPO_LiveAcc1.4<-PIPO_RF1.4.tab[1,1]/sum(PIPO_RF1.4.tab[1,])
PIPO_DeadAcc1.4<-PIPO_RF1.4.tab[2,2]/sum(PIPO_RF1.4.tab[2,])
PIPO_TotalAcc1.4<-(PIPO_RF1.4.tab[1,1]+PIPO_RF1.4.tab[2,2])/sum(PIPO_RF1.4.tab)

PIPO_RF2.4.tab <-table(factor(PIPO_test2.4$Status, levels = 0:1), factor(PIPO_2.4pred, levels = 0:1))
PIPO_LiveAcc2.4<-PIPO_RF2.4.tab[1,1]/sum(PIPO_RF2.4.tab[1,])
PIPO_DeadAcc2.4<-PIPO_RF2.4.tab[2,2]/sum(PIPO_RF2.4.tab[2,])
PIPO_TotalAcc2.4<-(PIPO_RF2.4.tab[1,1]+PIPO_RF2.4.tab[2,2])/sum(PIPO_RF2.4.tab)

PIPO_RF3.4.tab <-table(factor(PIPO_test3.4$Status, levels = 0:1), factor(PIPO_3.4pred, levels = 0:1))
PIPO_LiveAcc3.4<-PIPO_RF3.4.tab[1,1]/sum(PIPO_RF3.4.tab[1,])
PIPO_DeadAcc3.4<-PIPO_RF3.4.tab[2,2]/sum(PIPO_RF3.4.tab[2,])
PIPO_TotalAcc3.4<-(PIPO_RF3.4.tab[1,1]+PIPO_RF3.4.tab[2,2])/sum(PIPO_RF3.4.tab)

PIPO_RF4.4.tab <-table(factor(PIPO_test4.4$Status, levels = 0:1), factor(PIPO_4.4pred, levels = 0:1))
PIPO_LiveAcc4.4<-PIPO_RF4.4.tab[1,1]/sum(PIPO_RF4.4.tab[1,])
PIPO_DeadAcc4.4<-PIPO_RF4.4.tab[2,2]/sum(PIPO_RF4.4.tab[2,])
PIPO_TotalAcc4.4<-(PIPO_RF4.4.tab[1,1]+PIPO_RF4.4.tab[2,2])/sum(PIPO_RF4.4.tab)

PIPO_RF5.4.tab <-table(factor(PIPO_test5.4$Status, levels = 0:1), factor(PIPO_5.4pred, levels = 0:1))
PIPO_LiveAcc5.4<-PIPO_RF5.4.tab[1,1]/sum(PIPO_RF5.4.tab[1,])
PIPO_DeadAcc5.4<-PIPO_RF5.4.tab[2,2]/sum(PIPO_RF5.4.tab[2,])
PIPO_TotalAcc5.4<-(PIPO_RF5.4.tab[1,1]+PIPO_RF5.4.tab[2,2])/sum(PIPO_RF5.4.tab)

PIPO_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(PIPO_LiveAcc1.4,
                                                                              PIPO_LiveAcc2.4,
                                                                              PIPO_LiveAcc3.4,
                                                                              PIPO_LiveAcc4.4,
                                                                              PIPO_LiveAcc5.4),
                              DeadAcc = c(PIPO_DeadAcc1.4,
                                          PIPO_DeadAcc2.4,
                                          PIPO_DeadAcc3.4,
                                          PIPO_DeadAcc4.4,
                                          PIPO_DeadAcc5.4),
                              TotalAcc = c(PIPO_TotalAcc1.4,
                                           PIPO_TotalAcc2.4,
                                           PIPO_TotalAcc3.4,
                                           PIPO_TotalAcc4.4,
                                           PIPO_TotalAcc5.4))

PIPO_RF1.5.tab <-table(factor(PIPO_test1.5$Status, levels = 0:1), factor(PIPO_1.5pred, levels = 0:1))
PIPO_LiveAcc1.5<-PIPO_RF1.5.tab[1,1]/sum(PIPO_RF1.5.tab[1,])
PIPO_DeadAcc1.5<-PIPO_RF1.5.tab[2,2]/sum(PIPO_RF1.5.tab[2,])
PIPO_TotalAcc1.5<-(PIPO_RF1.5.tab[1,1]+PIPO_RF1.5.tab[2,2])/sum(PIPO_RF1.5.tab)

PIPO_RF2.5.tab <-table(factor(PIPO_test2.5$Status, levels = 0:1), factor(PIPO_2.5pred, levels = 0:1))
PIPO_LiveAcc2.5<-PIPO_RF2.5.tab[1,1]/sum(PIPO_RF2.5.tab[1,])
PIPO_DeadAcc2.5<-PIPO_RF2.5.tab[2,2]/sum(PIPO_RF2.5.tab[2,])
PIPO_TotalAcc2.5<-(PIPO_RF2.5.tab[1,1]+PIPO_RF2.5.tab[2,2])/sum(PIPO_RF2.5.tab)

PIPO_RF3.5.tab <-table(factor(PIPO_test3.5$Status, levels = 0:1), factor(PIPO_3.5pred, levels = 0:1))
PIPO_LiveAcc3.5<-PIPO_RF3.5.tab[1,1]/sum(PIPO_RF3.5.tab[1,])
PIPO_DeadAcc3.5<-PIPO_RF3.5.tab[2,2]/sum(PIPO_RF3.5.tab[2,])
PIPO_TotalAcc3.5<-(PIPO_RF3.5.tab[1,1]+PIPO_RF3.5.tab[2,2])/sum(PIPO_RF3.5.tab)

PIPO_RF4.5.tab <-table(factor(PIPO_test4.5$Status, levels = 0:1), factor(PIPO_4.5pred, levels = 0:1))
PIPO_LiveAcc4.5<-PIPO_RF4.5.tab[1,1]/sum(PIPO_RF4.5.tab[1,])
PIPO_DeadAcc4.5<-PIPO_RF4.5.tab[2,2]/sum(PIPO_RF4.5.tab[2,])
PIPO_TotalAcc4.5<-(PIPO_RF4.5.tab[1,1]+PIPO_RF4.5.tab[2,2])/sum(PIPO_RF4.5.tab)

PIPO_RF5.5.tab <-table(factor(PIPO_test5.5$Status, levels = 0:1), factor(PIPO_5.5pred, levels = 0:1))
PIPO_LiveAcc5.5<-PIPO_RF5.5.tab[1,1]/sum(PIPO_RF5.5.tab[1,])
PIPO_DeadAcc5.5<-PIPO_RF5.5.tab[2,2]/sum(PIPO_RF5.5.tab[2,])
PIPO_TotalAcc5.5<-(PIPO_RF5.5.tab[1,1]+PIPO_RF5.5.tab[2,2])/sum(PIPO_RF5.5.tab)

PIPO_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(PIPO_LiveAcc1.5,
                                                                              PIPO_LiveAcc2.5,
                                                                              PIPO_LiveAcc3.5,
                                                                              PIPO_LiveAcc4.5,
                                                                              PIPO_LiveAcc5.5),
                              DeadAcc = c(PIPO_DeadAcc1.5,
                                          PIPO_DeadAcc2.5,
                                          PIPO_DeadAcc3.5,
                                          PIPO_DeadAcc4.5,
                                          PIPO_DeadAcc5.5),
                              TotalAcc = c(PIPO_TotalAcc1.5,
                                           PIPO_TotalAcc2.5,
                                           PIPO_TotalAcc3.5,
                                           PIPO_TotalAcc4.5,
                                           PIPO_TotalAcc5.5))

PIPO_pred.results <-rbind(PIPO_Pre.results1,PIPO_Pre.results2,PIPO_Pre.results3,PIPO_Pre.results4,PIPO_Pre.results5)



PIPO_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "PIPO",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> PIPO_RF_preds.sum



#############PSME######################
#############subset species###########

PSME <- subset(Tree_data, Species == 'PSME')
PSME$OBS_ID <- 1:nrow(PSME)

###Get complete cases##########
PSME_yr1 <- select(PSME, yr1status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr1status)


PSME_yr2 <- select(PSME, yr2status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr2status)

PSME_yr3 <- select(PSME, yr3status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr3status)

PSME_yr4 <- select(PSME, yr4status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr4status)

PSME_yr5 <- select(PSME, yr5status, CVS_percent, DBH_cm, BCH_percent, dead_check, OBS_ID) %>% na.omit %>% rename(Status = yr5status)


##########5-fold cross validation
set.seed(1980)
PSME_flds1<-createFolds(PSME_yr1$Status, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data
PSME_flds2<-createFolds(PSME_yr2$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PSME_flds3<-createFolds(PSME_yr3$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PSME_flds4<-createFolds(PSME_yr4$Status, k = 5, list = TRUE, returnTrain = FALSE)  
PSME_flds5<-createFolds(PSME_yr5$Status, k = 5, list = TRUE, returnTrain = FALSE) 


##1
set.seed(1980)
samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[1]],]$Status))))
samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[1]],]$Status))))
PSME_train.rf1.1 <- randomForest(data = PSME_yr1[-PSME_flds1[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr2[-PSME_flds2[[1]],]$Status))))
PSME_train.rf2.1 <- randomForest(data = PSME_yr2[-PSME_flds2[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PSME_yr3[-PSME_flds3[[1]],]$Status))))
PSME_train.rf3.1 <- randomForest(data = PSME_yr3[-PSME_flds3[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr4[-PSME_flds4[[1]],]$Status))))
PSME_train.rf4.1 <- randomForest(data = PSME_yr4[-PSME_flds4[[1]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr5[-PSME_flds5[[1]],]$Status))))
PSME_train.rf5.1 <- randomForest(data = PSME_yr5[-PSME_flds5[[1]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)


##2

samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[2]],]$Status))))
PSME_train.rf1.2 <- randomForest(data = PSME_yr1[-PSME_flds1[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr2[-PSME_flds2[[2]],]$Status))))
PSME_train.rf2.2 <- randomForest(data = PSME_yr2[-PSME_flds2[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PSME_yr3[-PSME_flds3[[2]],]$Status))))
PSME_train.rf3.2 <- randomForest(data = PSME_yr3[-PSME_flds3[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr4[-PSME_flds4[[2]],]$Status))))
PSME_train.rf4.2 <- randomForest(data = PSME_yr4[-PSME_flds4[[2]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr5[-PSME_flds5[[2]],]$Status))))
PSME_train.rf5.2 <- randomForest(data = PSME_yr5[-PSME_flds5[[2]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[3]],]$Status))))
PSME_train.rf1.3 <- randomForest(data = PSME_yr1[-PSME_flds1[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr2[-PSME_flds2[[3]],]$Status))))
PSME_train.rf2.3 <- randomForest(data = PSME_yr2[-PSME_flds2[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PSME_yr3[-PSME_flds3[[3]],]$Status))))
PSME_train.rf3.3 <- randomForest(data = PSME_yr3[-PSME_flds3[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr4[-PSME_flds4[[3]],]$Status))))
PSME_train.rf4.3 <- randomForest(data = PSME_yr4[-PSME_flds4[[3]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr5[-PSME_flds5[[3]],]$Status))))
PSME_train.rf5.3 <- randomForest(data = PSME_yr5[-PSME_flds5[[3]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

##4

samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[4]],]$Status))))
PSME_train.rf1.4 <- randomForest(data = PSME_yr1[-PSME_flds1[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr2[-PSME_flds2[[4]],]$Status))))
PSME_train.rf2.4 <- randomForest(data = PSME_yr2[-PSME_flds2[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PSME_yr3[-PSME_flds3[[4]],]$Status))))
PSME_train.rf3.4 <- randomForest(data = PSME_yr3[-PSME_flds3[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr4[-PSME_flds4[[4]],]$Status))))
PSME_train.rf4.4 <- randomForest(data = PSME_yr4[-PSME_flds4[[4]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, mtry=1,sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr5[-PSME_flds5[[4]],]$Status))))
PSME_train.rf5.4 <- randomForest(data = PSME_yr5[-PSME_flds5[[4]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PSME_yr1[-PSME_flds1[[5]],]$Status))))
PSME_train.rf1.5 <- randomForest(data = PSME_yr1[-PSME_flds1[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr2[-PSME_flds2[[5]],]$Status))))
PSME_train.rf2.5 <- randomForest(data = PSME_yr2[-PSME_flds2[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)
samp <-trunc((0.2*min(table(PSME_yr3[-PSME_flds3[[5]],]$Status))))
PSME_train.rf3.5 <- randomForest(data = PSME_yr3[-PSME_flds3[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr4[-PSME_flds4[[5]],]$Status))))
PSME_train.rf4.5 <- randomForest(data = PSME_yr4[-PSME_flds4[[5]],], factor(Status)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)

samp <-trunc((0.2*min(table(PSME_yr5[-PSME_flds5[[5]],]$Status))))
PSME_train.rf5.5 <- randomForest(data = PSME_yr5[-PSME_flds5[[5]],], factor(Status)~CVS_percent+DBH_cm +  
                                   BCH_percent, 
                                 importance=TRUE, ntree=10000,  mtry=1, sampsize=c(samp,samp),
                                 na.action=na.omit)


##################Create test datasets#####################
##1
PSME_test1.1 <- PSME_yr1[PSME_flds1[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


table(PSME_yr1[PSME_flds1[[1]],] $  exclude=NULL)
table(PSME_test1.1$  exclude=NULL)

PSME_test2.1 <- PSME_yr2[PSME_flds2[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test3.1 <- PSME_yr3[PSME_flds3[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test4.1 <- PSME_yr4[PSME_flds4[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test5.1 <- PSME_yr5[PSME_flds5[[1]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##2
PSME_test1.2 <- PSME_yr1[PSME_flds1[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test2.2 <- PSME_yr2[PSME_flds2[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test3.2 <- PSME_yr3[PSME_flds3[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test4.2 <- PSME_yr4[PSME_flds4[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



PSME_test5.2 <- PSME_yr5[PSME_flds5[[2]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

##3
PSME_test1.3 <- PSME_yr1[PSME_flds1[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test2.3 <- PSME_yr2[PSME_flds2[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test3.3 <- PSME_yr3[PSME_flds3[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test4.3 <- PSME_yr4[PSME_flds4[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test5.3 <- PSME_yr5[PSME_flds5[[3]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##4
PSME_test1.4 <- PSME_yr1[PSME_flds1[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test2.4 <- PSME_yr2[PSME_flds2[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test3.4 <- PSME_yr3[PSME_flds3[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test4.4 <- PSME_yr4[PSME_flds4[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test5.4 <- PSME_yr5[PSME_flds5[[4]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


##5
PSME_test1.5 <- PSME_yr1[PSME_flds1[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PSME_test2.5 <- PSME_yr2[PSME_flds2[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PSME_test3.5 <- PSME_yr3[PSME_flds3[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 

PSME_test4.5 <- PSME_yr4[PSME_flds4[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 


PSME_test5.5 <- PSME_yr5[PSME_flds5[[5]],] %>% 
  select(Status, CVS_percent, DBH_cm,BCH_percent,   dead_check) 



#######################Predicting###############################
PSME_pred.results <-data.frame() 

PSME_1.1pred <- predict(PSME_train.rf1.1, newdata=PSME_test1.1)
PSME_2.1pred <- predict(PSME_train.rf2.1, newdata=PSME_test2.1)
PSME_3.1pred <- predict(PSME_train.rf3.1, newdata=PSME_test3.1)
PSME_4.1pred <- predict(PSME_train.rf4.1, newdata=PSME_test4.1)
PSME_5.1pred <- predict(PSME_train.rf5.1, newdata=PSME_test5.1)

PSME_1.2pred <- predict(PSME_train.rf1.2, newdata=PSME_test1.2)
PSME_2.2pred <- predict(PSME_train.rf2.2, newdata=PSME_test2.2)
PSME_3.2pred <- predict(PSME_train.rf3.2, newdata=PSME_test3.2)
PSME_4.2pred <- predict(PSME_train.rf4.2, newdata=PSME_test4.2)
PSME_5.2pred <- predict(PSME_train.rf5.2, newdata=PSME_test5.2)

PSME_1.3pred <- predict(PSME_train.rf1.3, newdata=PSME_test1.3)
PSME_2.3pred <- predict(PSME_train.rf2.3, newdata=PSME_test2.3)
PSME_3.3pred <- predict(PSME_train.rf3.3, newdata=PSME_test3.3)
PSME_4.3pred <- predict(PSME_train.rf4.3, newdata=PSME_test4.3)
PSME_5.3pred <- predict(PSME_train.rf5.3, newdata=PSME_test5.3)

PSME_1.4pred <- predict(PSME_train.rf1.4, newdata=PSME_test1.4)
PSME_2.4pred <- predict(PSME_train.rf2.4, newdata=PSME_test2.4)
PSME_3.4pred <- predict(PSME_train.rf3.4, newdata=PSME_test3.4)
PSME_4.4pred <- predict(PSME_train.rf4.4, newdata=PSME_test4.4)
PSME_5.4pred <- predict(PSME_train.rf5.4, newdata=PSME_test5.4)

PSME_1.5pred <- predict(PSME_train.rf1.5, newdata=PSME_test1.5)
PSME_2.5pred <- predict(PSME_train.rf2.5, newdata=PSME_test2.5)
PSME_3.5pred <- predict(PSME_train.rf3.5, newdata=PSME_test3.5)
PSME_4.5pred <- predict(PSME_train.rf4.5, newdata=PSME_test4.5)
PSME_5.5pred <- predict(PSME_train.rf5.5, newdata=PSME_test5.5)

PSME.test.preds <-data.frame(Status = c(PSME_test1.1$Status,PSME_test2.1$Status,PSME_test3.1$Status,PSME_test4.1$Status,PSME_test5.1$Status,
                                        PSME_test1.2$Status,PSME_test2.2$Status,PSME_test3.2$Status,PSME_test4.2$Status,PSME_test5.2$Status,
                                        PSME_test1.3$Status,PSME_test2.3$Status,PSME_test3.3$Status,PSME_test4.3$Status,PSME_test5.3$Status,
                                        PSME_test1.4$Status,PSME_test2.4$Status,PSME_test3.4$Status,PSME_test4.4$Status,PSME_test5.4$Status,
                                        PSME_test1.5$Status,PSME_test2.5$Status,PSME_test3.5$Status,PSME_test4.5$Status,PSME_test5.5$Status),
                             preds = c(PSME_1.1pred,PSME_2.1pred,PSME_3.1pred,PSME_4.1pred,PSME_5.1pred,
                                       PSME_1.2pred,PSME_2.2pred,PSME_3.2pred,PSME_4.2pred,PSME_5.2pred,
                                       PSME_1.3pred,PSME_2.3pred,PSME_3.3pred,PSME_4.3pred,PSME_5.3pred,
                                       PSME_1.4pred,PSME_2.4pred,PSME_3.4pred,PSME_4.4pred,PSME_5.4pred,
                                       PSME_1.5pred,PSME_2.5pred,PSME_3.5pred,PSME_4.5pred,PSME_5.5pred),
                             model = c(rep(1,length(PSME_1.1pred)),rep(2,length(PSME_2.1pred)),rep(3,length(PSME_3.1pred)),rep(4,length(PSME_4.1pred)),rep(5,length(PSME_5.1pred)),
                                       rep(1,length(PSME_1.2pred)),rep(2,length(PSME_2.2pred)),rep(3,length(PSME_3.2pred)),rep(4,length(PSME_4.2pred)),rep(5,length(PSME_5.2pred)),
                                       rep(1,length(PSME_1.3pred)),rep(2,length(PSME_2.3pred)),rep(3,length(PSME_3.3pred)),rep(4,length(PSME_4.3pred)),rep(5,length(PSME_5.3pred)),
                                       rep(1,length(PSME_1.4pred)),rep(2,length(PSME_2.4pred)),rep(3,length(PSME_3.4pred)),rep(4,length(PSME_4.4pred)),rep(5,length(PSME_5.4pred)),
                                       rep(1,length(PSME_1.5pred)),rep(2,length(PSME_2.5pred)),rep(3,length(PSME_3.5pred)),rep(4,length(PSME_4.5pred)),rep(5,length(PSME_5.5pred))),
                             rep = c(rep(1,length(PSME_1.1pred)),rep(1,length(PSME_2.1pred)),rep(1,length(PSME_3.1pred)),rep(1,length(PSME_4.1pred)),rep(1,length(PSME_5.1pred)),
                                     rep(2,length(PSME_1.2pred)),rep(2,length(PSME_2.2pred)),rep(2,length(PSME_3.2pred)),rep(2,length(PSME_4.2pred)),rep(2,length(PSME_5.2pred)),
                                     rep(3,length(PSME_1.3pred)),rep(3,length(PSME_2.3pred)),rep(3,length(PSME_3.3pred)),rep(3,length(PSME_4.3pred)),rep(3,length(PSME_5.3pred)),
                                     rep(4,length(PSME_1.4pred)),rep(4,length(PSME_2.4pred)),rep(4,length(PSME_3.4pred)),rep(4,length(PSME_4.4pred)),rep(4,length(PSME_5.4pred)),
                                     rep(5,length(PSME_1.5pred)),rep(5,length(PSME_2.5pred)),rep(5,length(PSME_3.5pred)),rep(5,length(PSME_4.5pred)),rep(5,length(PSME_5.5pred))),
                            
                             dead_check = c(PSME_test1.1$dead_check, PSME_test2.1$dead_check, PSME_test3.1$dead_check, PSME_test4.1$dead_check, PSME_test5.1$dead_check,
                                            PSME_test1.2$dead_check, PSME_test2.2$dead_check, PSME_test3.2$dead_check, PSME_test4.2$dead_check, PSME_test5.2$dead_check,
                                            PSME_test1.3$dead_check, PSME_test2.3$dead_check, PSME_test3.3$dead_check, PSME_test4.3$dead_check, PSME_test5.3$dead_check,
                                            PSME_test1.4$dead_check, PSME_test2.4$dead_check, PSME_test3.4$dead_check, PSME_test4.4$dead_check, PSME_test5.4$dead_check,
                                            PSME_test1.5$dead_check, PSME_test2.5$dead_check, PSME_test3.5$dead_check, PSME_test4.5$dead_check, PSME_test5.5$dead_check))



PSME.test.preds$wrong <- ifelse(PSME.test.preds$Status == PSME.test.preds$preds, 0,1)

PSME.test.preds %>%
  filter(Status==1) %>%
  group_by(dead_check, model, rep) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100) %>%
  drop_na(dead_check)-> PSME.wrong.preds



PSME_RF1.1.tab <-table(factor(PSME_test1.1$Status, levels = 0:1), factor(PSME_1.1pred, levels = 0:1))
PSME_LiveAcc1.1<-PSME_RF1.1.tab[1,1]/sum(PSME_RF1.1.tab[1,])
PSME_DeadAcc1.1<-PSME_RF1.1.tab[2,2]/sum(PSME_RF1.1.tab[2,])
PSME_TotalAcc1.1<-(PSME_RF1.1.tab[1,1]+PSME_RF1.1.tab[2,2])/sum(PSME_RF1.1.tab)

PSME_RF2.1.tab <-table(factor(PSME_test2.1$Status, levels = 0:1), factor(PSME_2.1pred, levels = 0:1))
PSME_LiveAcc2.1<-PSME_RF2.1.tab[1,1]/sum(PSME_RF2.1.tab[1,])
PSME_DeadAcc2.1<-PSME_RF2.1.tab[2,2]/sum(PSME_RF2.1.tab[2,])
PSME_TotalAcc2.1<-(PSME_RF2.1.tab[1,1]+PSME_RF2.1.tab[2,2])/sum(PSME_RF2.1.tab)

PSME_RF3.1.tab <-table(factor(PSME_test3.1$Status, levels = 0:1), factor(PSME_3.1pred, levels = 0:1))
PSME_LiveAcc3.1<-PSME_RF3.1.tab[1,1]/sum(PSME_RF3.1.tab[1,])
PSME_DeadAcc3.1<-PSME_RF3.1.tab[2,2]/sum(PSME_RF3.1.tab[2,])
PSME_TotalAcc3.1<-(PSME_RF3.1.tab[1,1]+PSME_RF3.1.tab[2,2])/sum(PSME_RF3.1.tab)

PSME_RF4.1.tab <-table(factor(PSME_test4.1$Status, levels = 0:1), factor(PSME_4.1pred, levels = 0:1))
PSME_LiveAcc4.1<-PSME_RF4.1.tab[1,1]/sum(PSME_RF4.1.tab[1,])
PSME_DeadAcc4.1<-PSME_RF4.1.tab[2,2]/sum(PSME_RF4.1.tab[2,])
PSME_TotalAcc4.1<-(PSME_RF4.1.tab[1,1]+PSME_RF4.1.tab[2,2])/sum(PSME_RF4.1.tab)

PSME_RF5.1.tab <-table(factor(PSME_test5.1$Status, levels = 0:1), factor(PSME_5.1pred, levels = 0:1))
PSME_LiveAcc5.1<-PSME_RF5.1.tab[1,1]/sum(PSME_RF5.1.tab[1,])
PSME_DeadAcc5.1<-PSME_RF5.1.tab[2,2]/sum(PSME_RF5.1.tab[2,])
PSME_TotalAcc5.1<-(PSME_RF5.1.tab[1,1]+PSME_RF5.1.tab[2,2])/sum(PSME_RF5.1.tab)

PSME_Pre.results1<-data.frame(Model = c(1:5), rep = c(1,1,1,1,1), LiveAcc = c(PSME_LiveAcc1.1,
                                                                              PSME_LiveAcc2.1,
                                                                              PSME_LiveAcc3.1,
                                                                              PSME_LiveAcc4.1,
                                                                              PSME_LiveAcc5.1),
                              DeadAcc = c(PSME_DeadAcc1.1,
                                          PSME_DeadAcc2.1,
                                          PSME_DeadAcc3.1,
                                          PSME_DeadAcc4.1,
                                          PSME_DeadAcc5.1),
                              TotalAcc = c(PSME_TotalAcc1.1,
                                           PSME_TotalAcc2.1,
                                           PSME_TotalAcc3.1,
                                           PSME_TotalAcc4.1,
                                           PSME_TotalAcc5.1))


PSME_RF1.2.tab <-table(factor(PSME_test1.2$Status, levels = 0:1), factor(PSME_1.2pred, levels = 0:1))
PSME_LiveAcc1.2<-PSME_RF1.2.tab[1,1]/sum(PSME_RF1.2.tab[1,])
PSME_DeadAcc1.2<-PSME_RF1.2.tab[2,2]/sum(PSME_RF1.2.tab[2,])
PSME_TotalAcc1.2<-(PSME_RF1.2.tab[1,1]+PSME_RF1.2.tab[2,2])/sum(PSME_RF1.2.tab)

PSME_RF2.2.tab <-table(factor(PSME_test2.2$Status, levels = 0:1), factor(PSME_2.2pred, levels = 0:1))
PSME_LiveAcc2.2<-PSME_RF2.2.tab[1,1]/sum(PSME_RF2.2.tab[1,])
PSME_DeadAcc2.2<-PSME_RF2.2.tab[2,2]/sum(PSME_RF2.2.tab[2,])
PSME_TotalAcc2.2<-(PSME_RF2.2.tab[1,1]+PSME_RF2.2.tab[2,2])/sum(PSME_RF2.2.tab)

PSME_RF3.2.tab <-table(factor(PSME_test3.2$Status, levels = 0:1), factor(PSME_3.2pred, levels = 0:1))
PSME_LiveAcc3.2<-PSME_RF3.2.tab[1,1]/sum(PSME_RF3.2.tab[1,])
PSME_DeadAcc3.2<-PSME_RF3.2.tab[2,2]/sum(PSME_RF3.2.tab[2,])
PSME_TotalAcc3.2<-(PSME_RF3.2.tab[1,1]+PSME_RF3.2.tab[2,2])/sum(PSME_RF3.2.tab)

PSME_RF4.2.tab <-table(factor(PSME_test4.2$Status, levels = 0:1), factor(PSME_4.2pred, levels = 0:1))
PSME_LiveAcc4.2<-PSME_RF4.2.tab[1,1]/sum(PSME_RF4.2.tab[1,])
PSME_DeadAcc4.2<-PSME_RF4.2.tab[2,2]/sum(PSME_RF4.2.tab[2,])
PSME_TotalAcc4.2<-(PSME_RF4.2.tab[1,1]+PSME_RF4.2.tab[2,2])/sum(PSME_RF4.2.tab)

PSME_RF5.2.tab <-table(factor(PSME_test5.2$Status, levels = 0:1), factor(PSME_5.2pred, levels = 0:1))
PSME_LiveAcc5.2<-PSME_RF5.2.tab[1,1]/sum(PSME_RF5.2.tab[1,])
PSME_DeadAcc5.2<-PSME_RF5.2.tab[2,2]/sum(PSME_RF5.2.tab[2,])
PSME_TotalAcc5.2<-(PSME_RF5.2.tab[1,1]+PSME_RF5.2.tab[2,2])/sum(PSME_RF5.2.tab)

PSME_Pre.results2<-data.frame(Model = c(1:5), rep = c(2,2,2,2,2), LiveAcc = c(PSME_LiveAcc1.2,
                                                                              PSME_LiveAcc2.2,
                                                                              PSME_LiveAcc3.2,
                                                                              PSME_LiveAcc4.2,
                                                                              PSME_LiveAcc5.2),
                              DeadAcc = c(PSME_DeadAcc1.2,
                                          PSME_DeadAcc2.2,
                                          PSME_DeadAcc3.2,
                                          PSME_DeadAcc4.2,
                                          PSME_DeadAcc5.2),
                              TotalAcc = c(PSME_TotalAcc1.2,
                                           PSME_TotalAcc2.2,
                                           PSME_TotalAcc3.2,
                                           PSME_TotalAcc4.2,
                                           PSME_TotalAcc5.2))

PSME_RF1.3.tab <-table(factor(PSME_test1.3$Status, levels = 0:1), factor(PSME_1.3pred, levels = 0:1))
PSME_LiveAcc1.3<-PSME_RF1.3.tab[1,1]/sum(PSME_RF1.3.tab[1,])
PSME_DeadAcc1.3<-PSME_RF1.3.tab[2,2]/sum(PSME_RF1.3.tab[2,])
PSME_TotalAcc1.3<-(PSME_RF1.3.tab[1,1]+PSME_RF1.3.tab[2,2])/sum(PSME_RF1.3.tab)

PSME_RF2.3.tab <-table(factor(PSME_test2.3$Status, levels = 0:1), factor(PSME_2.3pred, levels = 0:1))
PSME_LiveAcc2.3<-PSME_RF2.3.tab[1,1]/sum(PSME_RF2.3.tab[1,])
PSME_DeadAcc2.3<-PSME_RF2.3.tab[2,2]/sum(PSME_RF2.3.tab[2,])
PSME_TotalAcc2.3<-(PSME_RF2.3.tab[1,1]+PSME_RF2.3.tab[2,2])/sum(PSME_RF2.3.tab)

PSME_RF3.3.tab <-table(factor(PSME_test3.3$Status, levels = 0:1), factor(PSME_3.3pred, levels = 0:1))
PSME_LiveAcc3.3<-PSME_RF3.3.tab[1,1]/sum(PSME_RF3.3.tab[1,])
PSME_DeadAcc3.3<-PSME_RF3.3.tab[2,2]/sum(PSME_RF3.3.tab[2,])
PSME_TotalAcc3.3<-(PSME_RF3.3.tab[1,1]+PSME_RF3.3.tab[2,2])/sum(PSME_RF3.3.tab)

PSME_RF4.3.tab <-table(factor(PSME_test4.3$Status, levels = 0:1), factor(PSME_4.3pred, levels = 0:1))
PSME_LiveAcc4.3<-PSME_RF4.3.tab[1,1]/sum(PSME_RF4.3.tab[1,])
PSME_DeadAcc4.3<-PSME_RF4.3.tab[2,2]/sum(PSME_RF4.3.tab[2,])
PSME_TotalAcc4.3<-(PSME_RF4.3.tab[1,1]+PSME_RF4.3.tab[2,2])/sum(PSME_RF4.3.tab)

PSME_RF5.3.tab <-table(factor(PSME_test5.3$Status, levels = 0:1), factor(PSME_5.3pred, levels = 0:1))
PSME_LiveAcc5.3<-PSME_RF5.3.tab[1,1]/sum(PSME_RF5.3.tab[1,])
PSME_DeadAcc5.3<-PSME_RF5.3.tab[2,2]/sum(PSME_RF5.3.tab[2,])
PSME_TotalAcc5.3<-(PSME_RF5.3.tab[1,1]+PSME_RF5.3.tab[2,2])/sum(PSME_RF5.3.tab)

PSME_Pre.results3<-data.frame(Model = c(1:5), rep = c(3,3,3,3,3), LiveAcc = c(PSME_LiveAcc1.3,
                                                                              PSME_LiveAcc2.3,
                                                                              PSME_LiveAcc3.3,
                                                                              PSME_LiveAcc4.3,
                                                                              PSME_LiveAcc5.3),
                              DeadAcc = c(PSME_DeadAcc1.3,
                                          PSME_DeadAcc2.3,
                                          PSME_DeadAcc3.3,
                                          PSME_DeadAcc4.3,
                                          PSME_DeadAcc5.3),
                              TotalAcc = c(PSME_TotalAcc1.3,
                                           PSME_TotalAcc2.3,
                                           PSME_TotalAcc3.3,
                                           PSME_TotalAcc4.3,
                                           PSME_TotalAcc5.3))

PSME_RF1.4.tab <-table(factor(PSME_test1.4$Status, levels = 0:1), factor(PSME_1.4pred, levels = 0:1))
PSME_LiveAcc1.4<-PSME_RF1.4.tab[1,1]/sum(PSME_RF1.4.tab[1,])
PSME_DeadAcc1.4<-PSME_RF1.4.tab[2,2]/sum(PSME_RF1.4.tab[2,])
PSME_TotalAcc1.4<-(PSME_RF1.4.tab[1,1]+PSME_RF1.4.tab[2,2])/sum(PSME_RF1.4.tab)

PSME_RF2.4.tab <-table(factor(PSME_test2.4$Status, levels = 0:1), factor(PSME_2.4pred, levels = 0:1))
PSME_LiveAcc2.4<-PSME_RF2.4.tab[1,1]/sum(PSME_RF2.4.tab[1,])
PSME_DeadAcc2.4<-PSME_RF2.4.tab[2,2]/sum(PSME_RF2.4.tab[2,])
PSME_TotalAcc2.4<-(PSME_RF2.4.tab[1,1]+PSME_RF2.4.tab[2,2])/sum(PSME_RF2.4.tab)

PSME_RF3.4.tab <-table(factor(PSME_test3.4$Status, levels = 0:1), factor(PSME_3.4pred, levels = 0:1))
PSME_LiveAcc3.4<-PSME_RF3.4.tab[1,1]/sum(PSME_RF3.4.tab[1,])
PSME_DeadAcc3.4<-PSME_RF3.4.tab[2,2]/sum(PSME_RF3.4.tab[2,])
PSME_TotalAcc3.4<-(PSME_RF3.4.tab[1,1]+PSME_RF3.4.tab[2,2])/sum(PSME_RF3.4.tab)

PSME_RF4.4.tab <-table(factor(PSME_test4.4$Status, levels = 0:1), factor(PSME_4.4pred, levels = 0:1))
PSME_LiveAcc4.4<-PSME_RF4.4.tab[1,1]/sum(PSME_RF4.4.tab[1,])
PSME_DeadAcc4.4<-PSME_RF4.4.tab[2,2]/sum(PSME_RF4.4.tab[2,])
PSME_TotalAcc4.4<-(PSME_RF4.4.tab[1,1]+PSME_RF4.4.tab[2,2])/sum(PSME_RF4.4.tab)

PSME_RF5.4.tab <-table(factor(PSME_test5.4$Status, levels = 0:1), factor(PSME_5.4pred, levels = 0:1))
PSME_LiveAcc5.4<-PSME_RF5.4.tab[1,1]/sum(PSME_RF5.4.tab[1,])
PSME_DeadAcc5.4<-PSME_RF5.4.tab[2,2]/sum(PSME_RF5.4.tab[2,])
PSME_TotalAcc5.4<-(PSME_RF5.4.tab[1,1]+PSME_RF5.4.tab[2,2])/sum(PSME_RF5.4.tab)

PSME_Pre.results4<-data.frame(Model = c(1:5), rep = c(4,4,4,4,4), LiveAcc = c(PSME_LiveAcc1.4,
                                                                              PSME_LiveAcc2.4,
                                                                              PSME_LiveAcc3.4,
                                                                              PSME_LiveAcc4.4,
                                                                              PSME_LiveAcc5.4),
                              DeadAcc = c(PSME_DeadAcc1.4,
                                          PSME_DeadAcc2.4,
                                          PSME_DeadAcc3.4,
                                          PSME_DeadAcc4.4,
                                          PSME_DeadAcc5.4),
                              TotalAcc = c(PSME_TotalAcc1.4,
                                           PSME_TotalAcc2.4,
                                           PSME_TotalAcc3.4,
                                           PSME_TotalAcc4.4,
                                           PSME_TotalAcc5.4))

PSME_RF1.5.tab <-table(factor(PSME_test1.5$Status, levels = 0:1), factor(PSME_1.5pred, levels = 0:1))
PSME_LiveAcc1.5<-PSME_RF1.5.tab[1,1]/sum(PSME_RF1.5.tab[1,])
PSME_DeadAcc1.5<-PSME_RF1.5.tab[2,2]/sum(PSME_RF1.5.tab[2,])
PSME_TotalAcc1.5<-(PSME_RF1.5.tab[1,1]+PSME_RF1.5.tab[2,2])/sum(PSME_RF1.5.tab)

PSME_RF2.5.tab <-table(factor(PSME_test2.5$Status, levels = 0:1), factor(PSME_2.5pred, levels = 0:1))
PSME_LiveAcc2.5<-PSME_RF2.5.tab[1,1]/sum(PSME_RF2.5.tab[1,])
PSME_DeadAcc2.5<-PSME_RF2.5.tab[2,2]/sum(PSME_RF2.5.tab[2,])
PSME_TotalAcc2.5<-(PSME_RF2.5.tab[1,1]+PSME_RF2.5.tab[2,2])/sum(PSME_RF2.5.tab)

PSME_RF3.5.tab <-table(factor(PSME_test3.5$Status, levels = 0:1), factor(PSME_3.5pred, levels = 0:1))
PSME_LiveAcc3.5<-PSME_RF3.5.tab[1,1]/sum(PSME_RF3.5.tab[1,])
PSME_DeadAcc3.5<-PSME_RF3.5.tab[2,2]/sum(PSME_RF3.5.tab[2,])
PSME_TotalAcc3.5<-(PSME_RF3.5.tab[1,1]+PSME_RF3.5.tab[2,2])/sum(PSME_RF3.5.tab)

PSME_RF4.5.tab <-table(factor(PSME_test4.5$Status, levels = 0:1), factor(PSME_4.5pred, levels = 0:1))
PSME_LiveAcc4.5<-PSME_RF4.5.tab[1,1]/sum(PSME_RF4.5.tab[1,])
PSME_DeadAcc4.5<-PSME_RF4.5.tab[2,2]/sum(PSME_RF4.5.tab[2,])
PSME_TotalAcc4.5<-(PSME_RF4.5.tab[1,1]+PSME_RF4.5.tab[2,2])/sum(PSME_RF4.5.tab)

PSME_RF5.5.tab <-table(factor(PSME_test5.5$Status, levels = 0:1), factor(PSME_5.5pred, levels = 0:1))
PSME_LiveAcc5.5<-PSME_RF5.5.tab[1,1]/sum(PSME_RF5.5.tab[1,])
PSME_DeadAcc5.5<-PSME_RF5.5.tab[2,2]/sum(PSME_RF5.5.tab[2,])
PSME_TotalAcc5.5<-(PSME_RF5.5.tab[1,1]+PSME_RF5.5.tab[2,2])/sum(PSME_RF5.5.tab)

PSME_Pre.results5<-data.frame(Model = c(1:5), rep = c(5,5,5,5,5), LiveAcc = c(PSME_LiveAcc1.5,
                                                                              PSME_LiveAcc2.5,
                                                                              PSME_LiveAcc3.5,
                                                                              PSME_LiveAcc4.5,
                                                                              PSME_LiveAcc5.5),
                              DeadAcc = c(PSME_DeadAcc1.5,
                                          PSME_DeadAcc2.5,
                                          PSME_DeadAcc3.5,
                                          PSME_DeadAcc4.5,
                                          PSME_DeadAcc5.5),
                              TotalAcc = c(PSME_TotalAcc1.5,
                                           PSME_TotalAcc2.5,
                                           PSME_TotalAcc3.5,
                                           PSME_TotalAcc4.5,
                                           PSME_TotalAcc5.5))

PSME_pred.results <-rbind(PSME_Pre.results1,PSME_Pre.results2,PSME_Pre.results3,PSME_Pre.results4,PSME_Pre.results5)



PSME_pred.results %>%
  group_by(Model) %>%
  summarise(Species = "PSME",
            MeanLive = mean(LiveAcc),
            sdLive = sd(LiveAcc),
            MeanDead = mean(DeadAcc),
            sdDead = sd(DeadAcc),
            MeanTotal = mean(TotalAcc),
            sdTotal = sd(TotalAcc)) -> PSME_RF_preds.sum

###############Plotting false negatives#################

ABCO.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> ABCO.wrong.preds


ABCO.wrong.p <- ggplot(data=ABCO.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")

ABGR.test.preds$wrong <- ifelse(ABGR.test.preds$Status == ABGR.test.preds$preds, 0,1)
ABGR.test.preds$correct <- ifelse(ABGR.test.preds$Status == ABGR.test.preds$preds, 1,0)
ABGR.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> ABGR.wrong.preds


ABGR.wrong.p <- ggplot(data=ABGR.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")


CADE27.test.preds$wrong <- ifelse(CADE27.test.preds$Status == CADE27.test.preds$preds, 0,1)
CADE27.test.preds$correct <- ifelse(CADE27.test.preds$Status == CADE27.test.preds$preds, 1,0)
CADE27.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> CADE27.wrong.preds


CADE27.wrong.p <- ggplot(data=CADE27.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")


LAOC.test.preds$wrong <- ifelse(LAOC.test.preds$Status == LAOC.test.preds$preds, 0,1)
LAOC.test.preds$correct <- ifelse(LAOC.test.preds$Status == LAOC.test.preds$preds, 1,0)
LAOC.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> LAOC.wrong.preds


LAOC.wrong.p <- ggplot(data=LAOC.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")


PICO.test.preds$wrong <- ifelse(PICO.test.preds$Status == PICO.test.preds$preds, 0,1)
PICO.test.preds$correct <- ifelse(PICO.test.preds$Status == PICO.test.preds$preds, 1,0)
PICO.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> PICO.wrong.preds


PICO.wrong.p <- ggplot(data=PICO.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")


PILA.test.preds$wrong <- ifelse(PILA.test.preds$Status == PILA.test.preds$preds, 0,1)
PILA.test.preds$correct <- ifelse(PILA.test.preds$Status == PILA.test.preds$preds, 1,0)
PILA.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> PILA.wrong.preds


PILA.wrong.p <- ggplot(data=PILA.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")


PIPO.test.preds$wrong <- ifelse(PIPO.test.preds$Status == PIPO.test.preds$preds, 0,1)
PIPO.test.preds$correct <- ifelse(PIPO.test.preds$Status == PIPO.test.preds$preds, 1,0)
PIPO.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> PIPO.wrong.preds


PIPO.wrong.p <- ggplot(data=PIPO.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")

PSME.test.preds$wrong <- ifelse(PSME.test.preds$Status == PSME.test.preds$preds, 0,1)
PSME.test.preds$correct <- ifelse(PSME.test.preds$Status == PSME.test.preds$preds, 1,0)
PSME.test.preds %>%
  filter(Status==1, dead_check != 9) %>%
  group_by(dead_check,model, rep, Status) %>%
  summarize(pct_wrong = sum(wrong)/length(wrong) * 100,
            nwrong = sum(wrong),
            ncorrect = sum(correct),
            lwrong = length(wrong),
            lcorr = length(correct),
            modelrep = paste(model, "_", rep),
            pct_correct = sum(correct)/length(correct) * 100) %>%
  drop_na(dead_check) -> PSME.wrong.preds


PSME.wrong.p <- ggplot(data=PSME.wrong.preds, aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  scale_color_viridis(discrete=TRUE, option = "turbo")

#Facet plots so they all fit in one figure
p1 <- ABCO.wrong.p + ggtitle("ABCO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p2 <- ABGR.wrong.p + ggtitle("ABGR")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p3 <- CADE27.wrong.p + ggtitle("CADE27")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p4 <- LAOC.wrong.p + ggtitle("LAOC")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p5 <- PICO.wrong.p + ggtitle("PICO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p6 <- PILA.wrong.p + ggtitle("PILA")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p7 <- PIPO.wrong.p + ggtitle("PIPO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p8 <- PSME.wrong.p + ggtitle("PSME")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


ggarrange(p1,p2,
          p3,p4,
          p5,p6,
          p7,p8,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")

##From here the plots are exported to Inkscape for some finishing cosmetic touches
##These post-process touches (fonts etc.) can probably be done here in R. 
##In the future I will try to update the code so it is all done here.

#create a file with the cumulative model accuracy
cumulative_acc <- rbind(ABCO_RF_preds.sum,ABGR_RF_preds.sum,CADE27_RF_preds.sum,LAOC_RF_preds.sum,PICO_RF_preds.sum,PILA_RF_preds.sum,PIPO_RF_preds.sum, PSME_RF_preds.sum)
write.csv(cumulative_acc, "cummulative_acc.csv")

spp_list5
temp <- lm(data=CADE27_RF_preds.sum, MeanLive~Model)
summary(temp)
plot(temp)

spp.preds<-c(ABCO_RF_preds.sum,
             ABGR_RF_preds.sum,
             CADE27_RF_preds.sum,
             LAOC_RF_preds.sum,
             PICO_RF_preds.sum,
             PILA_RF_preds.sum,
             PIPO_RF_preds.sum, 
             PSME_RF_preds.sum)


ABCO.live.lm <- lm(data=ABCO_RF_preds.sum,  MeanLive~Model)
ABCO.dead.lm <- lm(data=ABCO_RF_preds.sum,  MeanDead~Model)
ABCO.total.lm <- lm(data=ABCO_RF_preds.sum,  MeanTotal~Model)

ABGR.live.lm <- lm(data=ABGR_RF_preds.sum,  MeanLive~Model)
ABGR.dead.lm <- lm(data=ABGR_RF_preds.sum,  MeanDead~Model)
ABGR.total.lm <- lm(data=ABGR_RF_preds.sum,  MeanTotal~Model)

CADE27.live.lm <- lm(data=CADE27_RF_preds.sum,  MeanLive~Model)
CADE27.dead.lm <- lm(data=CADE27_RF_preds.sum,  MeanDead~Model)
CADE27.total.lm <- lm(data=CADE27_RF_preds.sum,  MeanTotal~Model)

LAOC.live.lm <- lm(data=LAOC_RF_preds.sum,  MeanLive~Model)
LAOC.dead.lm <- lm(data=LAOC_RF_preds.sum,  MeanDead~Model)
LAOC.total.lm <- lm(data=LAOC_RF_preds.sum,  MeanTotal~Model)

PICO.live.lm <- lm(data=PICO_RF_preds.sum,  MeanLive~Model)
PICO.dead.lm <- lm(data=PICO_RF_preds.sum,  MeanDead~Model)
PICO.total.lm <- lm(data=PICO_RF_preds.sum,  MeanTotal~Model)

PILA.live.lm <- lm(data=PILA_RF_preds.sum,  MeanLive~Model)
summary(PILA.live.lm)
PILA.dead.lm <- lm(data=PILA_RF_preds.sum,  MeanDead~Model)
summary(PILA.dead.lm)

PILA.total.lm <- lm(data=PILA_RF_preds.sum,  MeanTotal~Model)

PIPO.live.lm <- lm(data=PIPO_RF_preds.sum,  MeanLive~Model)
PIPO.dead.lm <- lm(data=PIPO_RF_preds.sum,  MeanDead~Model)
PIPO.total.lm <- lm(data=PIPO_RF_preds.sum,  MeanTotal~Model)

PSME.live.lm <- lm(data=PSME_RF_preds.sum,  MeanLive~Model)
PSME.dead.lm <- lm(data=PSME_RF_preds.sum,  MeanDead~Model)
PSME.total.lm <- lm(data=PSME_RF_preds.sum,  MeanTotal~Model)

summary(ABCO.live.lm)

plot(ABCO.total.lm)

