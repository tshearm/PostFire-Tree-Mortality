library(randomForest)
library(pdp)
library(doParallel)
#This code is for the partial dependence plots for the random forest models. 
#It uses parallel processing because they take an absurd amount of time to run.

cl <- makeCluster(15)  # use 15 workers. You will need to change this depending on how many CPU cores your computer has available.
registerDoParallel(cl)  # register the parallel backend


ABCO.cc.pdp.CVSxBCH_0<-ABCO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)

ABCO.cc.pdp.CVSxBCH_1<-ABCO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)

ABCO.cc.pdp.CVSxBCH_2<-ABCO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend



ABGR.cc.pdp.CVSxBCH_0<-ABGR.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

ABGR.cc.pdp.CVSxBCH_1<-ABGR.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

ABGR.cc.pdp.CVSxBCH_2<-ABGR.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


CADE27.cc.pdp.CVSxBCH_0<-CADE27.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

CADE27.cc.pdp.CVSxBCH_1<-CADE27.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

CADE27.cc.pdp.CVSxBCH_2<-CADE27.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend



LAOC.cc.pdp.CVSxBCH_0<-LAOC.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

LAOC.cc.pdp.CVSxBCH_1<-LAOC.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

LAOC.cc.pdp.CVSxBCH_2<-LAOC.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PICO.cc.pdp.CVSxBCH_0<-PICO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)

PICO.cc.pdp.CVSxBCH_1<-PICO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)

PICO.cc.pdp.CVSxBCH_2<-PICO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PILA.cc.pdp.CVSxBCH_0<-PILA.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)

PILA.cc.pdp.CVSxBCH_1<-PILA.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)

PILA.cc.pdp.CVSxBCH_2<-PILA.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PIPO.cc.pdp.CVSxBCH_0<-PIPO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)

PIPO.cc.pdp.CVSxBCH_1<-PIPO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)

PIPO.cc.pdp.CVSxBCH_2<-PIPO.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PSME.cc.pdp.CVSxBCH_0<-PSME.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)

PSME.cc.pdp.CVSxBCH_1<-PSME.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)

PSME.cc.pdp.CVSxBCH_2<-PSME.cc_train.1 %>%
  partial(pred.var = c("CVS_percent","BCH_percent"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


ABCO.cc.pdp.DBHxBCH_0<-ABCO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)

ABCO.cc.pdp.DBHxBCH_1<-ABCO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)

ABCO.cc.pdp.DBHxBCH_2<-ABCO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABCO.cc_train.1)


stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend



ABGR.cc.pdp.DBHxBCH_0<-ABGR.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

ABGR.cc.pdp.DBHxBCH_1<-ABGR.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

ABGR.cc.pdp.DBHxBCH_2<-ABGR.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=ABGR.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


CADE27.cc.pdp.DBHxBCH_0<-CADE27.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

CADE27.cc.pdp.DBHxBCH_1<-CADE27.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

CADE27.cc.pdp.DBHxBCH_2<-CADE27.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=CADE27.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


LAOC.cc.pdp.DBHxBCH_0<-LAOC.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

LAOC.cc.pdp.DBHxBCH_1<-LAOC.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

LAOC.cc.pdp.DBHxBCH_2<-LAOC.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=LAOC.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PICO.cc.pdp.DBHxBCH_0<-PICO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)

PICO.cc.pdp.DBHxBCH_1<-PICO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)

PICO.cc.pdp.DBHxBCH_2<-PICO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PICO.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend



PILA.cc.pdp.DBHxBCH_0<-PILA.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)

PILA.cc.pdp.DBHxBCH_1<-PILA.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)

PILA.cc.pdp.DBHxBCH_2<-PILA.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PILA.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend


PIPO.cc.pdp.DBHxBCH_0<-PIPO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)

PIPO.cc.pdp.DBHxBCH_1<-PIPO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)

PIPO.cc.pdp.DBHxBCH_2<-PIPO.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PIPO.cc_train.1)

stopCluster(cl)  # good practice
cl <- makeCluster(15)  # use 15 workers
registerDoParallel(cl)  # register the parallel backend



PSME.cc.pdp.DBHxBCH_0<-PSME.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=1, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)

PSME.cc.pdp.DBHxBCH_1<-PSME.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=2, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)

PSME.cc.pdp.DBHxBCH_2<-PSME.cc_train.1 %>%
  partial(pred.var = c("BCH_percent","DBH_cm"), which.class=3, prob=TRUE, ice=FALSE, chull=TRUE,grid.resolution=101,
          parallel = TRUE, paropts = list(.packages = "randomForest")) %>%
  plotPartial(train=PSME.cc_train.1)

stopCluster(cl)  # good practice



#####Save partial plots#####
#Change the file path as needed
saveRDS(ABCO.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxBCH_10DBH_0.rds')
saveRDS(ABCO.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxBCH_10DBH_1.rds')
saveRDS(ABCO.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxBCH_10DBH_2.rds')

saveRDS(ABCO.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxDBH_10DBH_0.rds')
saveRDS(ABCO.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxDBH_10DBH_1.rds')
saveRDS(ABCO.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxDBH_10DBH_2.rds')

saveRDS(ABCO.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_BCHxDBH_10DBH_0.rds')
saveRDS(ABCO.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_BCHxDBH_10DBH_1.rds')
saveRDS(ABCO.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_BCHxDBH_10DBH_2.rds')


saveRDS(ABGR.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxBCH_10DBH_0.rds')
saveRDS(ABGR.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxBCH_10DBH_1.rds')
saveRDS(ABGR.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxBCH_10DBH_2.rds')

saveRDS(ABGR.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxDBH_10DBH_0.rds')
saveRDS(ABGR.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxDBH_10DBH_1.rds')
saveRDS(ABGR.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxDBH_10DBH_2.rds')

saveRDS(ABGR.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_BCHxDBH_10DBH_0.rds')
saveRDS(ABGR.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_BCHxDBH_10DBH_1.rds')
saveRDS(ABGR.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_BCHxDBH_10DBH_2.rds')


saveRDS(CADE27.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxBCH_10DBH_0.rds')
saveRDS(CADE27.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxBCH_10DBH_1.rds')
saveRDS(CADE27.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxBCH_10DBH_2.rds')

saveRDS(CADE27.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxDBH_10DBH_0.rds')
saveRDS(CADE27.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxDBH_10DBH_1.rds')
saveRDS(CADE27.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxDBH_10DBH_2.rds')

saveRDS(CADE27.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_BCHxDBH_10DBH_0.rds')
saveRDS(CADE27.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_BCHxDBH_10DBH_1.rds')
saveRDS(CADE27.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_BCHxDBH_10DBH_2.rds')


saveRDS(LAOC.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxBCH_10DBH_0.rds')
saveRDS(LAOC.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxBCH_10DBH_1.rds')
saveRDS(LAOC.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxBCH_10DBH_2.rds')

saveRDS(LAOC.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxDBH_10DBH_0.rds')
saveRDS(LAOC.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxDBH_10DBH_1.rds')
saveRDS(LAOC.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxDBH_10DBH_2.rds')

saveRDS(LAOC.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_BCHxDBH_10DBH_0.rds')
saveRDS(LAOC.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_BCHxDBH_10DBH_1.rds')
saveRDS(LAOC.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_BCHxDBH_10DBH_2.rds')


saveRDS(PICO.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxBCH_10DBH_0.rds')
saveRDS(PICO.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxBCH_10DBH_1.rds')
saveRDS(PICO.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxBCH_10DBH_2.rds')

saveRDS(PICO.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxDBH_10DBH_0.rds')
saveRDS(PICO.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxDBH_10DBH_1.rds')
saveRDS(PICO.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxDBH_10DBH_2.rds')

saveRDS(PICO.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_BCHxDBH_10DBH_0.rds')
saveRDS(PICO.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_BCHxDBH_10DBH_1.rds')
saveRDS(PICO.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_BCHxDBH_10DBH_2.rds')


saveRDS(PILA.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxBCH_10DBH_0.rds')
saveRDS(PILA.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxBCH_10DBH_1.rds')
saveRDS(PILA.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxBCH_10DBH_2.rds')

saveRDS(PILA.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxDBH_10DBH_0.rds')
saveRDS(PILA.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxDBH_10DBH_1.rds')
saveRDS(PILA.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxDBH_10DBH_2.rds')

saveRDS(PILA.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_BCHxDBH_10DBH_0.rds')
saveRDS(PILA.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_BCHxDBH_10DBH_1.rds')
saveRDS(PILA.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_BCHxDBH_10DBH_2.rds')


saveRDS(PIPO.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxBCH_10DBH_0.rds')
saveRDS(PIPO.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxBCH_10DBH_1.rds')
saveRDS(PIPO.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxBCH_10DBH_2.rds')

saveRDS(PIPO.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxDBH_10DBH_0.rds')
saveRDS(PIPO.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxDBH_10DBH_1.rds')
saveRDS(PIPO.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxDBH_10DBH_2.rds')

saveRDS(PIPO.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_BCHxDBH_10DBH_0.rds')
saveRDS(PIPO.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_BCHxDBH_10DBH_1.rds')
saveRDS(PIPO.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_BCHxDBH_10DBH_2.rds')


saveRDS(PSME.cc.pdp.CVSxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxBCH_10DBH_0.rds')
saveRDS(PSME.cc.pdp.CVSxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxBCH_10DBH_1.rds')
saveRDS(PSME.cc.pdp.CVSxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxBCH_10DBH_2.rds')

saveRDS(PSME.cc.pdp.CVSxDBH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxDBH_10DBH_0.rds')
saveRDS(PSME.cc.pdp.CVSxDBH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxDBH_10DBH_1.rds')
saveRDS(PSME.cc.pdp.CVSxDBH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxDBH_10DBH_2.rds')

saveRDS(PSME.cc.pdp.DBHxBCH_0, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_BCHxDBH_10DBH_0.rds')
saveRDS(PSME.cc.pdp.DBHxBCH_1, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_BCHxDBH_10DBH_1.rds')
saveRDS(PSME.cc.pdp.DBHxBCH_2, file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_BCHxDBH_10DBH_2.rds')


