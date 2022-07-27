##############################
##Here, we will make models for each species in which the response variable
##is either live, dead in first year, or dead in years 2-5. Hopefully this will
##shed some more light on our data. We already created a variable for the response
##in the Best_code_cumulative.R file so if you haven't run that scrip, you will need to at
##least run the data wrangling part in the beginning for the rest of this to work.

#####Subset species

library(randomForest)
#############ABCO###########

ABCO.cc <- select(ABCO, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
ABCO.cc_flds<-createFolds(ABCO.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(ABCO.cc[-ABCO.cc_flds[[1]],]$dead_check))))
ABCO.cc_train.1 <- randomForest(data = ABCO.cc[-ABCO.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                 na.action=na.omit)

##2

samp <-trunc((0.2*min(table(ABCO.cc[-ABCO.cc_flds[[2]],]$dead_check))))
ABCO.cc_train.2 <- randomForest(data = ABCO.cc[-ABCO.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                   BCH_percent,
                                 importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                 na.action=na.omit)

##3

samp <-trunc((0.2*min(table(ABCO.cc[-ABCO.cc_flds[[3]],]$dead_check))))
ABCO.cc_train.3 <- randomForest(data = ABCO.cc[-ABCO.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(ABCO.cc[-ABCO.cc_flds[[4]],]$dead_check))))
ABCO.cc_train.4 <- randomForest(data = ABCO.cc[-ABCO.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(ABCO.cc[-ABCO.cc_flds[[5]],]$dead_check))))
ABCO.cc_train.5 <- randomForest(data = ABCO.cc[-ABCO.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


varImpPlot(ABCO.cc_train.1)


ABCO.cc_results1 <- data.frame(Feature = row.names(ABCO.cc_train.1$importance), 
                                 Rep=c(1,1,1), 
                                 LiveImp = ABCO.cc_train.1$importance[,1],
                                 Dead1Imp = ABCO.cc_train.1$importance[,2],
                                 Dead2_5Imp = ABCO.cc_train.1$importance[,3])

ABCO.cc_results2 <- data.frame(Feature = row.names(ABCO.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = ABCO.cc_train.2$importance[,1],
                               Dead1Imp = ABCO.cc_train.2$importance[,2],
                               Dead2_5Imp = ABCO.cc_train.2$importance[,3])

ABCO.cc_results3 <- data.frame(Feature = row.names(ABCO.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = ABCO.cc_train.3$importance[,1],
                               Dead1Imp = ABCO.cc_train.3$importance[,2],
                               Dead2_5Imp = ABCO.cc_train.3$importance[,3])

ABCO.cc_results4 <- data.frame(Feature = row.names(ABCO.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = ABCO.cc_train.4$importance[,1],
                               Dead1Imp = ABCO.cc_train.4$importance[,2],
                               Dead2_5Imp = ABCO.cc_train.4$importance[,3])

ABCO.cc_results5 <- data.frame(Feature = row.names(ABCO.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = ABCO.cc_train.5$importance[,1],
                               Dead1Imp = ABCO.cc_train.5$importance[,2],
                               Dead2_5Imp = ABCO.cc_train.5$importance[,3])

ABCO.cc_results<-rbind(ABCO.cc_results1,
                       ABCO.cc_results2,
                       ABCO.cc_results3,
                       ABCO.cc_results4,
                       ABCO.cc_results5)

ABCO.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> ABCO.cc.long

ABCO.cc.long$feature_rep = paste(ABCO.cc.long$Feature,"_", ABCO.cc.long$Rep)



ABCO.cc.imp.p <- ggplot(ABCO.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    # remove x-axis labels
    axis.title.y = element_text(size = 18),
    # remove y-axis labels
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "ABCO") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )






##################Create test datasets#####################
##1
ABCO.cc_test1 <- ABCO.cc[ABCO.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABCO.cc_test2 <- ABCO.cc[ABCO.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABCO.cc_test3 <- ABCO.cc[ABCO.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABCO.cc_test4 <- ABCO.cc[ABCO.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABCO.cc_test5 <- ABCO.cc[ABCO.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
ABCO.cc_1pred <- predict(ABCO.cc_train.1, newdata=ABCO.cc_test1)
ABCO.cc_2pred <- predict(ABCO.cc_train.2, newdata=ABCO.cc_test2)
ABCO.cc_3pred <- predict(ABCO.cc_train.3, newdata=ABCO.cc_test3)
ABCO.cc_4pred <- predict(ABCO.cc_train.4, newdata=ABCO.cc_test4)
ABCO.cc_5pred <- predict(ABCO.cc_train.5, newdata=ABCO.cc_test5)

test <- data.frame(DBH_cm = c(100,100,100,100),
                   BCH_percent =c(0,10,25,50),
                   CVS_percent = c(74,74,74,74))
predict(CADE27.cc_train.1, newdata=test)

ABCO.cc.test.preds <- data.frame(Status = c(ABCO.cc_test1$dead_check,
                                            ABCO.cc_test2$dead_check,
                                            ABCO.cc_test3$dead_check,
                                            ABCO.cc_test4$dead_check,
                                            ABCO.cc_test5$dead_check),
                                 preds = c(ABCO.cc_1pred,
                                           ABCO.cc_2pred,
                                           ABCO.cc_3pred,
                                           ABCO.cc_4pred,
                                           ABCO.cc_5pred),
                                 rep = c(rep(1,length(ABCO.cc_1pred)),
                                         rep(2,length(ABCO.cc_2pred)),
                                         rep(3,length(ABCO.cc_3pred)),
                                         rep(4,length(ABCO.cc_4pred)),
                                         rep(5,length(ABCO.cc_5pred))
                                 ))
ABCO.cc.test.preds$wrong <- ifelse(ABCO.cc.test.preds$Status == ABCO.cc.test.preds$preds, 0,1)
ABCO.cc.test.preds$correct <- ifelse(ABCO.cc.test.preds$Status == ABCO.cc.test.preds$preds, 1,0)

ABCO.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> ABCO.cc.sum

table(ABCO.cc.test.preds$Status ,ABCO.cc.test.preds$preds)
ABCO.cc.sum$StatusCD <- ifelse(ABCO.cc.sum$Status == 0, "Live", 
                               ifelse(ABCO.cc.sum$Status == 1, "Dead_yr1",
                                                           "Dead_yr2_5"))
ABCO.cc.acc <- ggplot(ABCO.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())









#############ABGR###########

ABGR.cc <- select(ABGR, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
ABGR.cc_flds<-createFolds(ABGR.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)                          #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(ABGR.cc[-ABGR.cc_flds[[1]],]$dead_check))))
ABGR.cc_train.1 <- randomForest(data = ABGR.cc[-ABGR.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(ABGR.cc[-ABGR.cc_flds[[2]],]$dead_check))))
ABGR.cc_train.2 <- randomForest(data = ABGR.cc[-ABGR.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(ABGR.cc[-ABGR.cc_flds[[3]],]$dead_check))))
ABGR.cc_train.3 <- randomForest(data = ABGR.cc[-ABGR.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(ABGR.cc[-ABGR.cc_flds[[4]],]$dead_check))))
ABGR.cc_train.4 <- randomForest(data = ABGR.cc[-ABGR.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(ABGR.cc[-ABGR.cc_flds[[5]],]$dead_check))))
ABGR.cc_train.5 <- randomForest(data = ABGR.cc[-ABGR.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


varImpPlot(ABGR.cc_train.1)


ABGR.cc_results1 <- data.frame(Feature = row.names(ABGR.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = ABGR.cc_train.1$importance[,1],
                               Dead1Imp = ABGR.cc_train.1$importance[,2],
                               Dead2_5Imp = ABGR.cc_train.1$importance[,3])

ABGR.cc_results2 <- data.frame(Feature = row.names(ABGR.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = ABGR.cc_train.2$importance[,1],
                               Dead1Imp = ABGR.cc_train.2$importance[,2],
                               Dead2_5Imp = ABGR.cc_train.2$importance[,3])

ABGR.cc_results3 <- data.frame(Feature = row.names(ABGR.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = ABGR.cc_train.3$importance[,1],
                               Dead1Imp = ABGR.cc_train.3$importance[,2],
                               Dead2_5Imp = ABGR.cc_train.3$importance[,3])

ABGR.cc_results4 <- data.frame(Feature = row.names(ABGR.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = ABGR.cc_train.4$importance[,1],
                               Dead1Imp = ABGR.cc_train.4$importance[,2],
                               Dead2_5Imp = ABGR.cc_train.4$importance[,3])

ABGR.cc_results5 <- data.frame(Feature = row.names(ABGR.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = ABGR.cc_train.5$importance[,1],
                               Dead1Imp = ABGR.cc_train.5$importance[,2],
                               Dead2_5Imp = ABGR.cc_train.5$importance[,3])



ABGR.cc_results<-rbind(ABGR.cc_results1,
                       ABGR.cc_results2,
                       ABGR.cc_results3,
                       ABGR.cc_results4,
                       ABGR.cc_results5)

ABGR.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> ABGR.cc.long

ABGR.cc.long$feature_rep = paste(ABGR.cc.long$Feature,"_", ABGR.cc.long$Rep)



ABGR.cc.imp.p <- ggplot(ABGR.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    # remove x-axis labels
    axis.title.y = element_text(size = 18),
    # remove y-axis labels
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "ABGR") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )



##################Create test datasets#####################
##1
ABGR.cc_test1 <- ABGR.cc[ABGR.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABGR.cc_test2 <- ABGR.cc[ABGR.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABGR.cc_test3 <- ABGR.cc[ABGR.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABGR.cc_test4 <- ABGR.cc[ABGR.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

ABGR.cc_test5 <- ABGR.cc[ABGR.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
ABGR.cc_1pred <- predict(ABGR.cc_train.1, newdata=ABGR.cc_test1)
ABGR.cc_2pred <- predict(ABGR.cc_train.2, newdata=ABGR.cc_test2)
ABGR.cc_3pred <- predict(ABGR.cc_train.3, newdata=ABGR.cc_test3)
ABGR.cc_4pred <- predict(ABGR.cc_train.4, newdata=ABGR.cc_test4)
ABGR.cc_5pred <- predict(ABGR.cc_train.5, newdata=ABGR.cc_test5)

ABGR.cc.test.preds <- data.frame(Status = c(ABGR.cc_test1$dead_check,
                                            ABGR.cc_test2$dead_check,
                                            ABGR.cc_test3$dead_check,
                                            ABGR.cc_test4$dead_check,
                                            ABGR.cc_test5$dead_check),
                                 preds = c(ABGR.cc_1pred,
                                           ABGR.cc_2pred,
                                           ABGR.cc_3pred,
                                           ABGR.cc_4pred,
                                           ABGR.cc_5pred),
                                 rep = c(rep(1,length(ABGR.cc_1pred)),
                                         rep(2,length(ABGR.cc_2pred)),
                                         rep(3,length(ABGR.cc_3pred)),
                                         rep(4,length(ABGR.cc_4pred)),
                                         rep(5,length(ABGR.cc_5pred))
                                 ))
ABGR.cc.test.preds$wrong <- ifelse(ABGR.cc.test.preds$Status == ABGR.cc.test.preds$preds, 0,1)
ABGR.cc.test.preds$correct <- ifelse(ABGR.cc.test.preds$Status == ABGR.cc.test.preds$preds, 1,0)

ABGR.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> ABGR.cc.sum

table(ABGR.cc.test.preds$Status ,ABGR.cc.test.preds$preds)
ABGR.cc.sum$StatusCD <- ifelse(ABGR.cc.sum$Status == 0, "Live", 
                               ifelse(ABGR.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
ABGR.cc.acc <- ggplot(ABGR.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())



#####CADE27######

CADE27.cc <- select(CADE27, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
CADE27.cc_flds<-createFolds(CADE27.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)     #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(CADE27.cc[-CADE27.cc_flds[[1]],]$dead_check))))
CADE27.cc_train.1 <- randomForest(data = CADE27.cc[-CADE27.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(CADE27.cc[-CADE27.cc_flds[[2]],]$dead_check))))
CADE27.cc_train.2 <- randomForest(data = CADE27.cc[-CADE27.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(CADE27.cc[-CADE27.cc_flds[[3]],]$dead_check))))
CADE27.cc_train.3 <- randomForest(data = CADE27.cc[-CADE27.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(CADE27.cc[-CADE27.cc_flds[[4]],]$dead_check))))
CADE27.cc_train.4 <- randomForest(data = CADE27.cc[-CADE27.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(CADE27.cc[-CADE27.cc_flds[[5]],]$dead_check))))
CADE27.cc_train.5 <- randomForest(data = CADE27.cc[-CADE27.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)



CADE27.cc_results1 <- data.frame(Feature = row.names(CADE27.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = CADE27.cc_train.1$importance[,1],
                               Dead1Imp = CADE27.cc_train.1$importance[,2],
                               Dead2_5Imp = CADE27.cc_train.1$importance[,3])

CADE27.cc_results2 <- data.frame(Feature = row.names(CADE27.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = CADE27.cc_train.2$importance[,1],
                               Dead1Imp = CADE27.cc_train.2$importance[,2],
                               Dead2_5Imp = CADE27.cc_train.2$importance[,3])

CADE27.cc_results3 <- data.frame(Feature = row.names(CADE27.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = CADE27.cc_train.3$importance[,1],
                               Dead1Imp = CADE27.cc_train.3$importance[,2],
                               Dead2_5Imp = CADE27.cc_train.3$importance[,3])

CADE27.cc_results4 <- data.frame(Feature = row.names(CADE27.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = CADE27.cc_train.4$importance[,1],
                               Dead1Imp = CADE27.cc_train.4$importance[,2],
                               Dead2_5Imp = CADE27.cc_train.4$importance[,3])

CADE27.cc_results5 <- data.frame(Feature = row.names(CADE27.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = CADE27.cc_train.5$importance[,1],
                               Dead1Imp = CADE27.cc_train.5$importance[,2],
                               Dead2_5Imp = CADE27.cc_train.5$importance[,3])


CADE27.cc_results<-rbind(CADE27.cc_results1,
                       CADE27.cc_results2,
                       CADE27.cc_results3,
                       CADE27.cc_results4,
                       CADE27.cc_results5)

CADE27.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> CADE27.cc.long

CADE27.cc.long$feature_rep = paste(CADE27.cc.long$Feature,"_", CADE27.cc.long$Rep)



CADE27.cc.imp.p <- ggplot(CADE27.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    # remove x-axis labels
    axis.title.y = element_text(size = 18),
    # remove y-axis labels
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "CADE27") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )


##################Create test datasets#####################
##1
CADE27.cc_test1 <- CADE27.cc[CADE27.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

CADE27.cc_test2 <- CADE27.cc[CADE27.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

CADE27.cc_test3 <- CADE27.cc[CADE27.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

CADE27.cc_test4 <- CADE27.cc[CADE27.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

CADE27.cc_test5 <- CADE27.cc[CADE27.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
CADE27.cc_1pred <- predict(CADE27.cc_train.1, newdata=CADE27.cc_test1)
CADE27.cc_2pred <- predict(CADE27.cc_train.2, newdata=CADE27.cc_test2)
CADE27.cc_3pred <- predict(CADE27.cc_train.3, newdata=CADE27.cc_test3)
CADE27.cc_4pred <- predict(CADE27.cc_train.4, newdata=CADE27.cc_test4)
CADE27.cc_5pred <- predict(CADE27.cc_train.5, newdata=CADE27.cc_test5)

CADE27.cc.test.preds <- data.frame(Status = c(CADE27.cc_test1$dead_check,
                                            CADE27.cc_test2$dead_check,
                                            CADE27.cc_test3$dead_check,
                                            CADE27.cc_test4$dead_check,
                                            CADE27.cc_test5$dead_check),
                                 preds = c(CADE27.cc_1pred,
                                           CADE27.cc_2pred,
                                           CADE27.cc_3pred,
                                           CADE27.cc_4pred,
                                           CADE27.cc_5pred),
                                 rep = c(rep(1,length(CADE27.cc_1pred)),
                                         rep(2,length(CADE27.cc_2pred)),
                                         rep(3,length(CADE27.cc_3pred)),
                                         rep(4,length(CADE27.cc_4pred)),
                                         rep(5,length(CADE27.cc_5pred))
                                 ))
CADE27.cc.test.preds$wrong <- ifelse(CADE27.cc.test.preds$Status == CADE27.cc.test.preds$preds, 0,1)
CADE27.cc.test.preds$correct <- ifelse(CADE27.cc.test.preds$Status == CADE27.cc.test.preds$preds, 1,0)

CADE27.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> CADE27.cc.sum

table(CADE27.cc.test.preds$Status ,CADE27.cc.test.preds$preds)
CADE27.cc.sum$StatusCD <- ifelse(CADE27.cc.sum$Status == 0, "Live", 
                               ifelse(CADE27.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
CADE27.cc.acc <- ggplot(CADE27.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())



#############LAOC###########

LAOC.cc <- select(LAOC, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
LAOC.cc_flds<-createFolds(LAOC.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)         #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(LAOC.cc[-LAOC.cc_flds[[1]],]$dead_check))))
LAOC.cc_train.1 <- randomForest(data = LAOC.cc[-LAOC.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(LAOC.cc[-LAOC.cc_flds[[2]],]$dead_check))))
LAOC.cc_train.2 <- randomForest(data = LAOC.cc[-LAOC.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(LAOC.cc[-LAOC.cc_flds[[3]],]$dead_check))))
LAOC.cc_train.3 <- randomForest(data = LAOC.cc[-LAOC.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(LAOC.cc[-LAOC.cc_flds[[4]],]$dead_check))))
LAOC.cc_train.4 <- randomForest(data = LAOC.cc[-LAOC.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(LAOC.cc[-LAOC.cc_flds[[5]],]$dead_check))))
LAOC.cc_train.5 <- randomForest(data = LAOC.cc[-LAOC.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)



LAOC.cc_results1 <- data.frame(Feature = row.names(LAOC.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = LAOC.cc_train.1$importance[,1],
                               Dead1Imp = LAOC.cc_train.1$importance[,2],
                               Dead2_5Imp = LAOC.cc_train.1$importance[,3])

LAOC.cc_results2 <- data.frame(Feature = row.names(LAOC.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = LAOC.cc_train.2$importance[,1],
                               Dead1Imp = LAOC.cc_train.2$importance[,2],
                               Dead2_5Imp = LAOC.cc_train.2$importance[,3])

LAOC.cc_results3 <- data.frame(Feature = row.names(LAOC.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = LAOC.cc_train.3$importance[,1],
                               Dead1Imp = LAOC.cc_train.3$importance[,2],
                               Dead2_5Imp = LAOC.cc_train.3$importance[,3])

LAOC.cc_results4 <- data.frame(Feature = row.names(LAOC.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = LAOC.cc_train.4$importance[,1],
                               Dead1Imp = LAOC.cc_train.4$importance[,2],
                               Dead2_5Imp = LAOC.cc_train.4$importance[,3])

LAOC.cc_results5 <- data.frame(Feature = row.names(LAOC.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = LAOC.cc_train.5$importance[,1],
                               Dead1Imp = LAOC.cc_train.5$importance[,2],
                               Dead2_5Imp = LAOC.cc_train.5$importance[,3])

LAOC.cc_results<-rbind(LAOC.cc_results1,
                       LAOC.cc_results2,
                       LAOC.cc_results3,
                       LAOC.cc_results4,
                       LAOC.cc_results5)

LAOC.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> LAOC.cc.long

LAOC.cc.long$feature_rep = paste(LAOC.cc.long$Feature,"_", LAOC.cc.long$Rep)



LAOC.cc.imp.p <- ggplot(LAOC.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "LAOC") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )
##################Create test datasets#####################
##1
LAOC.cc_test1 <- LAOC.cc[LAOC.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

LAOC.cc_test2 <- LAOC.cc[LAOC.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

LAOC.cc_test3 <- LAOC.cc[LAOC.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

LAOC.cc_test4 <- LAOC.cc[LAOC.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

LAOC.cc_test5 <- LAOC.cc[LAOC.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
LAOC.cc_1pred <- predict(LAOC.cc_train.1, newdata=LAOC.cc_test1)
LAOC.cc_2pred <- predict(LAOC.cc_train.2, newdata=LAOC.cc_test2)
LAOC.cc_3pred <- predict(LAOC.cc_train.3, newdata=LAOC.cc_test3)
LAOC.cc_4pred <- predict(LAOC.cc_train.4, newdata=LAOC.cc_test4)
LAOC.cc_5pred <- predict(LAOC.cc_train.5, newdata=LAOC.cc_test5)

LAOC.cc.test.preds <- data.frame(Status = c(LAOC.cc_test1$dead_check,
                                            LAOC.cc_test2$dead_check,
                                            LAOC.cc_test3$dead_check,
                                            LAOC.cc_test4$dead_check,
                                            LAOC.cc_test5$dead_check),
                                 preds = c(LAOC.cc_1pred,
                                           LAOC.cc_2pred,
                                           LAOC.cc_3pred,
                                           LAOC.cc_4pred,
                                           LAOC.cc_5pred),
                                 rep = c(rep(1,length(LAOC.cc_1pred)),
                                         rep(2,length(LAOC.cc_2pred)),
                                         rep(3,length(LAOC.cc_3pred)),
                                         rep(4,length(LAOC.cc_4pred)),
                                         rep(5,length(LAOC.cc_5pred))
                                 ))
LAOC.cc.test.preds$wrong <- ifelse(LAOC.cc.test.preds$Status == LAOC.cc.test.preds$preds, 0,1)
LAOC.cc.test.preds$correct <- ifelse(LAOC.cc.test.preds$Status == LAOC.cc.test.preds$preds, 1,0)

LAOC.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> LAOC.cc.sum

table(LAOC.cc.test.preds$Status ,LAOC.cc.test.preds$preds)
LAOC.cc.sum$StatusCD <- ifelse(LAOC.cc.sum$Status == 0, "Live", 
                               ifelse(LAOC.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
LAOC.cc.acc <- ggplot(LAOC.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



#######PICO##############
PICO.cc <- select(PICO, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
PICO.cc_flds<-createFolds(PICO.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)         #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(PICO.cc[-PICO.cc_flds[[1]],]$dead_check))))
PICO.cc_train.1 <- randomForest(data = PICO.cc[-PICO.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(PICO.cc[-PICO.cc_flds[[2]],]$dead_check))))
PICO.cc_train.2 <- randomForest(data = PICO.cc[-PICO.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PICO.cc[-PICO.cc_flds[[3]],]$dead_check))))
PICO.cc_train.3 <- randomForest(data = PICO.cc[-PICO.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(PICO.cc[-PICO.cc_flds[[4]],]$dead_check))))
PICO.cc_train.4 <- randomForest(data = PICO.cc[-PICO.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PICO.cc[-PICO.cc_flds[[5]],]$dead_check))))
PICO.cc_train.5 <- randomForest(data = PICO.cc[-PICO.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)





PICO.cc_results1 <- data.frame(Feature = row.names(PICO.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = PICO.cc_train.1$importance[,1],
                               Dead1Imp = PICO.cc_train.1$importance[,2],
                               Dead2_5Imp = PICO.cc_train.1$importance[,3])

PICO.cc_results2 <- data.frame(Feature = row.names(PICO.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = PICO.cc_train.2$importance[,1],
                               Dead1Imp = PICO.cc_train.2$importance[,2],
                               Dead2_5Imp = PICO.cc_train.2$importance[,3])

PICO.cc_results3 <- data.frame(Feature = row.names(PICO.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = PICO.cc_train.3$importance[,1],
                               Dead1Imp = PICO.cc_train.3$importance[,2],
                               Dead2_5Imp = PICO.cc_train.3$importance[,3])

PICO.cc_results4 <- data.frame(Feature = row.names(PICO.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = PICO.cc_train.4$importance[,1],
                               Dead1Imp = PICO.cc_train.4$importance[,2],
                               Dead2_5Imp = PICO.cc_train.4$importance[,3])

PICO.cc_results5 <- data.frame(Feature = row.names(PICO.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = PICO.cc_train.5$importance[,1],
                               Dead1Imp = PICO.cc_train.5$importance[,2],
                               Dead2_5Imp = PICO.cc_train.5$importance[,3])

PICO.cc_results<-rbind(PICO.cc_results1,
                       PICO.cc_results2,
                       PICO.cc_results3,
                       PICO.cc_results4,
                       PICO.cc_results5)

PICO.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> PICO.cc.long

PICO.cc.long$feature_rep = paste(PICO.cc.long$Feature,"_", PICO.cc.long$Rep)



PICO.cc.imp.p <- ggplot(PICO.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "PICO") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )
##################Create test datasets#####################
##1
PICO.cc_test1 <- PICO.cc[PICO.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PICO.cc_test2 <- PICO.cc[PICO.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PICO.cc_test3 <- PICO.cc[PICO.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PICO.cc_test4 <- PICO.cc[PICO.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PICO.cc_test5 <- PICO.cc[PICO.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
PICO.cc_1pred <- predict(PICO.cc_train.1, newdata=PICO.cc_test1)
PICO.cc_2pred <- predict(PICO.cc_train.2, newdata=PICO.cc_test2)
PICO.cc_3pred <- predict(PICO.cc_train.3, newdata=PICO.cc_test3)
PICO.cc_4pred <- predict(PICO.cc_train.4, newdata=PICO.cc_test4)
PICO.cc_5pred <- predict(PICO.cc_train.5, newdata=PICO.cc_test5)

PICO.cc.test.preds <- data.frame(Status = c(PICO.cc_test1$dead_check,
                                            PICO.cc_test2$dead_check,
                                            PICO.cc_test3$dead_check,
                                            PICO.cc_test4$dead_check,
                                            PICO.cc_test5$dead_check),
                                 preds = c(PICO.cc_1pred,
                                           PICO.cc_2pred,
                                           PICO.cc_3pred,
                                           PICO.cc_4pred,
                                           PICO.cc_5pred),
                                 rep = c(rep(1,length(PICO.cc_1pred)),
                                         rep(2,length(PICO.cc_2pred)),
                                         rep(3,length(PICO.cc_3pred)),
                                         rep(4,length(PICO.cc_4pred)),
                                         rep(5,length(PICO.cc_5pred))
                                 ))
PICO.cc.test.preds$wrong <- ifelse(PICO.cc.test.preds$Status == PICO.cc.test.preds$preds, 0,1)
PICO.cc.test.preds$correct <- ifelse(PICO.cc.test.preds$Status == PICO.cc.test.preds$preds, 1,0)

PICO.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> PICO.cc.sum

table(PICO.cc.test.preds$Status ,PICO.cc.test.preds$preds)
PICO.cc.sum$StatusCD <- ifelse(PICO.cc.sum$Status == 0, "Live", 
                               ifelse(PICO.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
PICO.cc.acc <- ggplot(PICO.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#

######PILA#####

PILA.cc <- select(PILA, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
PILA.cc_flds<-createFolds(PILA.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)        #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(PILA.cc[-PILA.cc_flds[[1]],]$dead_check))))
PILA.cc_train.1 <- randomForest(data = PILA.cc[-PILA.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(PILA.cc[-PILA.cc_flds[[2]],]$dead_check))))
PILA.cc_train.2 <- randomForest(data = PILA.cc[-PILA.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PILA.cc[-PILA.cc_flds[[3]],]$dead_check))))
PILA.cc_train.3 <- randomForest(data = PILA.cc[-PILA.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(PILA.cc[-PILA.cc_flds[[4]],]$dead_check))))
PILA.cc_train.4 <- randomForest(data = PILA.cc[-PILA.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PILA.cc[-PILA.cc_flds[[5]],]$dead_check))))
PILA.cc_train.5 <- randomForest(data = PILA.cc[-PILA.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)




PILA.cc_results1 <- data.frame(Feature = row.names(PILA.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = PILA.cc_train.1$importance[,1],
                               Dead1Imp = PILA.cc_train.1$importance[,2],
                               Dead2_5Imp = PILA.cc_train.1$importance[,3])

PILA.cc_results2 <- data.frame(Feature = row.names(PILA.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = PILA.cc_train.2$importance[,1],
                               Dead1Imp = PILA.cc_train.2$importance[,2],
                               Dead2_5Imp = PILA.cc_train.2$importance[,3])

PILA.cc_results3 <- data.frame(Feature = row.names(PILA.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = PILA.cc_train.3$importance[,1],
                               Dead1Imp = PILA.cc_train.3$importance[,2],
                               Dead2_5Imp = PILA.cc_train.3$importance[,3])

PILA.cc_results4 <- data.frame(Feature = row.names(PILA.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = PILA.cc_train.4$importance[,1],
                               Dead1Imp = PILA.cc_train.4$importance[,2],
                               Dead2_5Imp = PILA.cc_train.4$importance[,3])

PILA.cc_results5 <- data.frame(Feature = row.names(PILA.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = PILA.cc_train.5$importance[,1],
                               Dead1Imp = PILA.cc_train.5$importance[,2],
                               Dead2_5Imp = PILA.cc_train.5$importance[,3])

PILA.cc_results<-rbind(PILA.cc_results1,
                       PILA.cc_results2,
                       PILA.cc_results3,
                       PILA.cc_results4,
                       PILA.cc_results5)

PILA.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> PILA.cc.long

PILA.cc.long$feature_rep = paste(PILA.cc.long$Feature,"_", PILA.cc.long$Rep)



PILA.cc.imp.p <- ggplot(PILA.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "PILA") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )

##################Create test datasets#####################
##1
PILA.cc_test1 <- PILA.cc[PILA.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PILA.cc_test2 <- PILA.cc[PILA.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PILA.cc_test3 <- PILA.cc[PILA.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PILA.cc_test4 <- PILA.cc[PILA.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PILA.cc_test5 <- PILA.cc[PILA.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
PILA.cc_1pred <- predict(PILA.cc_train.1, newdata=PILA.cc_test1)
PILA.cc_2pred <- predict(PILA.cc_train.2, newdata=PILA.cc_test2)
PILA.cc_3pred <- predict(PILA.cc_train.3, newdata=PILA.cc_test3)
PILA.cc_4pred <- predict(PILA.cc_train.4, newdata=PILA.cc_test4)
PILA.cc_5pred <- predict(PILA.cc_train.5, newdata=PILA.cc_test5)

PILA.cc.test.preds <- data.frame(Status = c(PILA.cc_test1$dead_check,
                                            PILA.cc_test2$dead_check,
                                            PILA.cc_test3$dead_check,
                                            PILA.cc_test4$dead_check,
                                            PILA.cc_test5$dead_check),
                                 preds = c(PILA.cc_1pred,
                                           PILA.cc_2pred,
                                           PILA.cc_3pred,
                                           PILA.cc_4pred,
                                           PILA.cc_5pred),
                                 rep = c(rep(1,length(PILA.cc_1pred)),
                                         rep(2,length(PILA.cc_2pred)),
                                         rep(3,length(PILA.cc_3pred)),
                                         rep(4,length(PILA.cc_4pred)),
                                         rep(5,length(PILA.cc_5pred))
                                 ))
PILA.cc.test.preds$wrong <- ifelse(PILA.cc.test.preds$Status == PILA.cc.test.preds$preds, 0,1)
PILA.cc.test.preds$correct <- ifelse(PILA.cc.test.preds$Status == PILA.cc.test.preds$preds, 1,0)

PILA.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> PILA.cc.sum
table(PILA.cc.test.preds$Status ,PILA.cc.test.preds$preds)
PILA.cc.sum$StatusCD <- ifelse(PILA.cc.sum$Status == 0, "Live", 
                               ifelse(PILA.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
PILA.cc.acc <- ggplot(PILA.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())




#####PIPO#######

PIPO.cc <- select(PIPO, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
PIPO.cc_flds<-createFolds(PIPO.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)          #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(PIPO.cc[-PIPO.cc_flds[[1]],]$dead_check))))
PIPO.cc_train.1 <- randomForest(data = PIPO.cc[-PIPO.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(PIPO.cc[-PIPO.cc_flds[[2]],]$dead_check))))
PIPO.cc_train.2 <- randomForest(data = PIPO.cc[-PIPO.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PIPO.cc[-PIPO.cc_flds[[3]],]$dead_check))))
PIPO.cc_train.3 <- randomForest(data = PIPO.cc[-PIPO.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(PIPO.cc[-PIPO.cc_flds[[4]],]$dead_check))))
PIPO.cc_train.4 <- randomForest(data = PIPO.cc[-PIPO.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PIPO.cc[-PIPO.cc_flds[[5]],]$dead_check))))
PIPO.cc_train.5 <- randomForest(data = PIPO.cc[-PIPO.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)



PIPO.cc_results1 <- data.frame(Feature = row.names(PIPO.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = PIPO.cc_train.1$importance[,1],
                               Dead1Imp = PIPO.cc_train.1$importance[,2],
                               Dead2_5Imp = PIPO.cc_train.1$importance[,3])

PIPO.cc_results2 <- data.frame(Feature = row.names(PIPO.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = PIPO.cc_train.2$importance[,1],
                               Dead1Imp = PIPO.cc_train.2$importance[,2],
                               Dead2_5Imp = PIPO.cc_train.2$importance[,3])

PIPO.cc_results3 <- data.frame(Feature = row.names(PIPO.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = PIPO.cc_train.3$importance[,1],
                               Dead1Imp = PIPO.cc_train.3$importance[,2],
                               Dead2_5Imp = PIPO.cc_train.3$importance[,3])

PIPO.cc_results4 <- data.frame(Feature = row.names(PIPO.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = PIPO.cc_train.4$importance[,1],
                               Dead1Imp = PIPO.cc_train.4$importance[,2],
                               Dead2_5Imp = PIPO.cc_train.4$importance[,3])

PIPO.cc_results5 <- data.frame(Feature = row.names(PIPO.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = PIPO.cc_train.5$importance[,1],
                               Dead1Imp = PIPO.cc_train.5$importance[,2],
                               Dead2_5Imp = PIPO.cc_train.5$importance[,3])


PIPO.cc_results<-rbind(PIPO.cc_results1,
                       PIPO.cc_results2,
                       PIPO.cc_results3,
                       PIPO.cc_results4,
                       PIPO.cc_results5)

PIPO.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> PIPO.cc.long

PIPO.cc.long$feature_rep = paste(PIPO.cc.long$Feature,"_", PIPO.cc.long$Rep)



PIPO.cc.imp.p <- ggplot(PIPO.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "PIPO") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )
table(PIPO.cc.test.preds$Status ,PIPO.cc.test.preds$preds)
PIPO.cc.sum$StatusCD <- ifelse(PIPO.cc.sum$Status == 0, "Live", 
                               ifelse(PIPO.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
PIPO.cc.acc <- ggplot(PIPO.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
##################Create test datasets#####################
##1
PIPO.cc_test1 <- PIPO.cc[PIPO.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PIPO.cc_test2 <- PIPO.cc[PIPO.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PIPO.cc_test3 <- PIPO.cc[PIPO.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PIPO.cc_test4 <- PIPO.cc[PIPO.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PIPO.cc_test5 <- PIPO.cc[PIPO.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
PIPO.cc_1pred <- predict(PIPO.cc_train.1, newdata=PIPO.cc_test1)
PIPO.cc_2pred <- predict(PIPO.cc_train.2, newdata=PIPO.cc_test2)
PIPO.cc_3pred <- predict(PIPO.cc_train.3, newdata=PIPO.cc_test3)
PIPO.cc_4pred <- predict(PIPO.cc_train.4, newdata=PIPO.cc_test4)
PIPO.cc_5pred <- predict(PIPO.cc_train.5, newdata=PIPO.cc_test5)

PIPO.cc.test.preds <- data.frame(Status = c(PIPO.cc_test1$dead_check,
                                            PIPO.cc_test2$dead_check,
                                            PIPO.cc_test3$dead_check,
                                            PIPO.cc_test4$dead_check,
                                            PIPO.cc_test5$dead_check),
                                 preds = c(PIPO.cc_1pred,
                                           PIPO.cc_2pred,
                                           PIPO.cc_3pred,
                                           PIPO.cc_4pred,
                                           PIPO.cc_5pred),
                                 rep = c(rep(1,length(PIPO.cc_1pred)),
                                         rep(2,length(PIPO.cc_2pred)),
                                         rep(3,length(PIPO.cc_3pred)),
                                         rep(4,length(PIPO.cc_4pred)),
                                         rep(5,length(PIPO.cc_5pred))
                                 ))
PIPO.cc.test.preds$wrong <- ifelse(PIPO.cc.test.preds$Status == PIPO.cc.test.preds$preds, 0,1)
PIPO.cc.test.preds$correct <- ifelse(PIPO.cc.test.preds$Status == PIPO.cc.test.preds$preds, 1,0)

PIPO.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
  ) -> PIPO.cc.sum



PIPO.cc.sum$StatusCD <- ifelse(PIPO.cc.sum$Status == 0, "Live", 
                               ifelse(PIPO.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))

PIPO.cc.acc <- ggplot(PIPO.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

######PSME#####

PSME.cc <- select(PSME, OBS_ID, CVS_percent, DBH_cm, BCH_percent, dead_check) %>% na.omit

##########5-fold cross validation
set.seed(1980)
PSME.cc_flds<-createFolds(PSME.cc$dead_check, k = 5, list = TRUE, returnTrain = FALSE)        #Create 5 subsets of the data


#####Fit RF models
##1
set.seed(1980)
samp <-trunc((0.2*min(table(PSME.cc[-PSME.cc_flds[[1]],]$dead_check))))
PSME.cc_train.1 <- randomForest(data = PSME.cc[-PSME.cc_flds[[1]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##2

samp <-trunc((0.2*min(table(PSME.cc[-PSME.cc_flds[[2]],]$dead_check))))
PSME.cc_train.2 <- randomForest(data = PSME.cc[-PSME.cc_flds[[2]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##3

samp <-trunc((0.2*min(table(PSME.cc[-PSME.cc_flds[[3]],]$dead_check))))
PSME.cc_train.3 <- randomForest(data = PSME.cc[-PSME.cc_flds[[3]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


##4

samp <-trunc((0.2*min(table(PSME.cc[-PSME.cc_flds[[4]],]$dead_check))))
PSME.cc_train.4 <- randomForest(data = PSME.cc[-PSME.cc_flds[[4]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)

##5

samp <-trunc((0.2*min(table(PSME.cc[-PSME.cc_flds[[5]],]$dead_check))))
PSME.cc_train.5 <- randomForest(data = PSME.cc[-PSME.cc_flds[[5]],], factor(dead_check)~CVS_percent+DBH_cm + 
                                  BCH_percent,
                                importance=TRUE, ntree=10000, sampsize=c(samp,samp,samp),
                                na.action=na.omit)


varImpPlot(PSME.cc_train.1)


PSME.cc_results1 <- data.frame(Feature = row.names(PSME.cc_train.1$importance), 
                               Rep=c(1,1,1), 
                               LiveImp = PSME.cc_train.1$importance[,1],
                               Dead1Imp = PSME.cc_train.1$importance[,2],
                               Dead2_5Imp = PSME.cc_train.1$importance[,3])

PSME.cc_results2 <- data.frame(Feature = row.names(PSME.cc_train.2$importance), 
                               Rep=c(2,2,2), 
                               LiveImp = PSME.cc_train.2$importance[,1],
                               Dead1Imp = PSME.cc_train.2$importance[,2],
                               Dead2_5Imp = PSME.cc_train.2$importance[,3])

PSME.cc_results3 <- data.frame(Feature = row.names(PSME.cc_train.3$importance), 
                               Rep=c(3,3,3), 
                               LiveImp = PSME.cc_train.3$importance[,1],
                               Dead1Imp = PSME.cc_train.3$importance[,2],
                               Dead2_5Imp = PSME.cc_train.3$importance[,3])

PSME.cc_results4 <- data.frame(Feature = row.names(PSME.cc_train.4$importance), 
                               Rep=c(4,4,4), 
                               LiveImp = PSME.cc_train.4$importance[,1],
                               Dead1Imp = PSME.cc_train.4$importance[,2],
                               Dead2_5Imp = PSME.cc_train.4$importance[,3])

PSME.cc_results5 <- data.frame(Feature = row.names(PSME.cc_train.5$importance), 
                               Rep=c(5,5,5), 
                               LiveImp = PSME.cc_train.5$importance[,1],
                               Dead1Imp = PSME.cc_train.5$importance[,2],
                               Dead2_5Imp = PSME.cc_train.5$importance[,3])

PSME.cc_results<-rbind(PSME.cc_results1,
                       PSME.cc_results2,
                       PSME.cc_results3,
                       PSME.cc_results4,
                       PSME.cc_results5)

PSME.cc_results %>%
  pivot_longer(cols = ends_with("Imp"),
               names_to = "Status",
               values_to = "Importance") -> PSME.cc.long

PSME.cc.long$feature_rep = paste(PSME.cc.long$Feature,"_", PSME.cc.long$Rep)



PSME.cc.imp.p <- ggplot(PSME.cc.long,
                        aes(
                          x = Status,
                          y = Importance,
                          color = Feature,
                          group = feature_rep
                        )) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 18),
    # remove x-axis labels
    axis.title.y = element_text(size = 18),
    # remove y-axis labels
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    #remove major-grid labels
    panel.grid.minor = element_blank(),
    #remove minor-grid labels
    plot.background = element_blank()
  ) +
  ggtitle(label = "PSME") +
  scale_color_viridis(
    discrete = TRUE,
    option = "viridis",
    end = 1,
    direction = 1
  )
##################Create test datasets#####################
##1
PSME.cc_test1 <- PSME.cc[PSME.cc_flds[[1]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PSME.cc_test2 <- PSME.cc[PSME.cc_flds[[2]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PSME.cc_test3 <- PSME.cc[PSME.cc_flds[[3]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PSME.cc_test4 <- PSME.cc[PSME.cc_flds[[4]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 

PSME.cc_test5 <- PSME.cc[PSME.cc_flds[[5]],] %>% 
  select(CVS_percent, DBH_cm,BCH_percent, dead_check) 


#######################Predicting###############################
PSME.cc_1pred <- predict(PSME.cc_train.1, newdata=PSME.cc_test1)
PSME.cc_2pred <- predict(PSME.cc_train.2, newdata=PSME.cc_test2)
PSME.cc_3pred <- predict(PSME.cc_train.3, newdata=PSME.cc_test3)
PSME.cc_4pred <- predict(PSME.cc_train.4, newdata=PSME.cc_test4)
PSME.cc_5pred <- predict(PSME.cc_train.5, newdata=PSME.cc_test5)

PSME.cc.test.preds <- data.frame(Status = c(PSME.cc_test1$dead_check,
                                            PSME.cc_test2$dead_check,
                                            PSME.cc_test3$dead_check,
                                            PSME.cc_test4$dead_check,
                                            PSME.cc_test5$dead_check),
                                 preds = c(PSME.cc_1pred,
                                           PSME.cc_2pred,
                                           PSME.cc_3pred,
                                           PSME.cc_4pred,
                                           PSME.cc_5pred),
                                 rep = c(rep(1,length(PSME.cc_1pred)),
                                         rep(2,length(PSME.cc_2pred)),
                                         rep(3,length(PSME.cc_3pred)),
                                         rep(4,length(PSME.cc_4pred)),
                                         rep(5,length(PSME.cc_5pred))
                                         ))
PSME.cc.test.preds$wrong <- ifelse(PSME.cc.test.preds$Status == PSME.cc.test.preds$preds, 0,1)
PSME.cc.test.preds$correct <- ifelse(PSME.cc.test.preds$Status == PSME.cc.test.preds$preds, 1,0)

PSME.cc.test.preds %>%
  group_by(Status, rep) %>%
  summarize(correct = sum(correct)/length(correct) * 100,
            ) -> PSME.cc.sum

table(PSME.cc.test.preds$Status ,PSME.cc.test.preds$preds)
PSME.cc.sum$StatusCD <- ifelse(PSME.cc.sum$Status == 0, "Live", 
                               ifelse(PSME.cc.sum$Status == 1, "Dead_yr1",
                                      "Dead_yr2_5"))
PSME.cc.acc <- ggplot(PSME.cc.sum, aes(x=StatusCD, y=correct, group = rep)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



####plotting importance values####

imp1 <- ABCO.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp2 <- ABGR.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp3 <- CADE27.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp4 <- LAOC.cc.imp.p + ylim(c(-.1,0.7))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp5 <- PICO.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp6 <- PILA.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp7 <- PIPO.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
imp8 <- PSME.cc.imp.p + ylim(c(-.1,0.7))+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

ggarrange(imp1,imp2,
          imp3,imp4,
          imp5,imp6,
          imp7,imp8,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")
#The code below arranges species based on their bark thickness coefficient
ggarrange(imp5,imp2,
          imp1,imp3,
          imp4,imp8,
          imp7,imp6,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")
 
####plotting accuracy####

acc1 <- ABCO.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("ABCO") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc2 <- ABGR.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("ABGR") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc3 <- CADE27.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("CADE27") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc4 <- LAOC.cc.acc + ylim(c(10,100)) +
  ggtitle("LAOC") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc5 <- PICO.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("PICO") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc6 <- PILA.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("PILA") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc7 <- PIPO.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("PIPO") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

acc8 <- PSME.cc.acc + ylim(c(10,100)) +
  theme(legend.position = "none") +
  ggtitle("PSME") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

ggarrange(acc1,acc2,
          acc3,acc4,
          acc5,acc6,
          acc7,acc8,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")

#arrange by BT

ggarrange(acc5,acc2,
          acc1,acc3,
          acc4,acc8,
          acc7,acc6,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")
                           