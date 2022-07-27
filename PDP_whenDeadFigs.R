#####Load partial plots#####
ABCOp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxDBH.rds')
ABCOp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_CVSxBCH.rds')
ABCOp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABCO_cc_DBHxBCH.rds')

ABGRp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxDBH.rds')
ABGRp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_CVSxBCH.rds')
ABGRp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/ABGR_cc_DBHxBCH.rds')

CADE27p.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxDBH.rds')
CADE27p.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_CVSxBCH.rds')
CADE27p.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/CADE27_cc_DBHxBCH.rds')

LAOCp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxDBH.rds')
LAOCp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_CVSxBCH.rds')
LAOCp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/LAOC_cc_DBHxBCH.rds')

PICOp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxDBH.rds')
PICOp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_CVSxBCH.rds')
PICOp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PICO_cc_DBHxBCH.rds')

PILAp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxDBH.rds')
PILAp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_CVSxBCH.rds')
PILAp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PILA_cc_DBHxBCH.rds')

PIPOp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxDBH.rds')
PIPOp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_CVSxBCH.rds')
PIPOp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PIPO_cc_DBHxBCH.rds')

PSMEp.cc_CVSxDBH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxDBH.rds')
PSMEp.cc_CVSxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_CVSxBCH.rds')
PSMEp.cc_DBHxBCH <- readRDS(file='C:/projects/FTM_Scorch/FTM_Scorch_home/BestCode/PDP_rds/PSME_cc_DBHxBCH.rds')


#######ABCO#######
ABCOp.cc_CVSxDBH <- data.frame(CVS = ABCO.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = ABCO.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =ABCO.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = ABCO.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = ABCO.cc.pdp.CVSxDBH_2$panel.args.common$z)



ABCOp.cc_CVSxDBH$status <- colnames(ABCOp.cc_CVSxDBH[,3:5])[apply(ABCOp.cc_CVSxDBH[,3:5],1,which.max)]
ABCOp.cc_CVSxDBH$Pstatus <- apply(ABCOp.cc_CVSxDBH[,3:5],1,max)


ABCOp.cc_CVSxBCH <- data.frame(CVS = ABCO.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = ABCO.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =ABCO.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = ABCO.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = ABCO.cc.pdp.CVSxBCH_2$panel.args.common$z)

ABCOp.cc_CVSxBCH$status <- colnames(ABCOp.cc_CVSxBCH[,3:5])[apply(ABCOp.cc_CVSxBCH[,3:5],1,which.max)]
ABCOp.cc_CVSxBCH$Pstatus <- apply(ABCOp.cc_CVSxBCH[,3:5],1,max)


ABCOp.cc_DBHxBCH <- data.frame(BCH = ABCO.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = ABCO.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =ABCO.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = ABCO.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = ABCO.cc.pdp.DBHxBCH_2$panel.args.common$z)



ABCOp.cc_DBHxBCH$status <- colnames(ABCOp.cc_DBHxBCH[,3:5])[apply(ABCOp.cc_DBHxBCH[,3:5],1,which.max)]
ABCOp.cc_DBHxBCH$Pstatus <- apply(ABCOp.cc_DBHxBCH[,3:5],1,max)


pal <- c("#D81B1B", "#FFC107","#1D5B84" )


ABCO.cc_CVSxDBH_plot <- ggplot(data=ABCOp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
ABCO.cc_CVSxDBH_plot


ABCO.cc_CVSxBCH_plot <- ggplot(data=ABCOp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
ABCO.cc_CVSxBCH_plot


ABCO.cc_DBHxBCH_plot <- ggplot(data=ABCOp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
ABCO.cc_DBHxBCH_plot



ggsave("ABCO_cc_CVSxDBH.png", device = "png")

#####ABGR#####
ABGRp.cc_CVSxDBH <- data.frame(CVS = ABGR.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = ABGR.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =ABGR.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = ABGR.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = ABGR.cc.pdp.CVSxDBH_2$panel.args.common$z)

ABGRp.cc_CVSxDBH$status <- colnames(ABGRp.cc_CVSxDBH[,3:5])[apply(ABGRp.cc_CVSxDBH[,3:5],1,which.max)]
ABGRp.cc_CVSxDBH$Pstatus <- apply(ABGRp.cc_CVSxDBH[,3:5],1,max)


ABGRp.cc_CVSxBCH <- data.frame(CVS = ABGR.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = ABGR.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =ABGR.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = ABGR.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = ABGR.cc.pdp.CVSxBCH_2$panel.args.common$z)

ABGRp.cc_CVSxBCH$status <- colnames(ABGRp.cc_CVSxBCH[,3:5])[apply(ABGRp.cc_CVSxBCH[,3:5],1,which.max)]
ABGRp.cc_CVSxBCH$Pstatus <- apply(ABGRp.cc_CVSxBCH[,3:5],1,max)


ABGRp.cc_DBHxBCH <- data.frame(BCH = ABGR.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = ABGR.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =ABGR.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = ABGR.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = ABGR.cc.pdp.DBHxBCH_2$panel.args.common$z)



ABGRp.cc_DBHxBCH$status <- colnames(ABGRp.cc_DBHxBCH[,3:5])[apply(ABGRp.cc_DBHxBCH[,3:5],1,which.max)]
ABGRp.cc_DBHxBCH$Pstatus <- apply(ABGRp.cc_DBHxBCH[,3:5],1,max)

pal <- c("red", "goldenrod1","green4" )

ABGR.cc_CVSxDBH_plot <- ggplot(data=ABGRp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
ABGR.cc_CVSxDBH_plot


ABGR.cc_CVSxBCH_plot <- ggplot(data=ABGRp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
ABGR.cc_CVSxBCH_plot


ABGR.cc_DBHxBCH_plot <- ggplot(data=ABGRp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
ABGR.cc_DBHxBCH_plot





#####CADE27####

CADE27p.cc_CVSxDBH <- data.frame(CVS = CADE27.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = CADE27.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =CADE27.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = CADE27.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = CADE27.cc.pdp.CVSxDBH_2$panel.args.common$z)

CADE27p.cc_CVSxDBH$status <- colnames(CADE27p.cc_CVSxDBH[,3:5])[apply(CADE27p.cc_CVSxDBH[,3:5],1,which.max)]
CADE27p.cc_CVSxDBH$Pstatus <- apply(CADE27p.cc_CVSxDBH[,3:5],1,max)


CADE27p.cc_CVSxBCH <- data.frame(CVS = CADE27.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = CADE27.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =CADE27.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = CADE27.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = CADE27.cc.pdp.CVSxBCH_2$panel.args.common$z)

CADE27p.cc_CVSxBCH$status <- colnames(CADE27p.cc_CVSxBCH[,3:5])[apply(CADE27p.cc_CVSxBCH[,3:5],1,which.max)]
CADE27p.cc_CVSxBCH$Pstatus <- apply(CADE27p.cc_CVSxBCH[,3:5],1,max)


CADE27p.cc_DBHxBCH <- data.frame(BCH = CADE27.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = CADE27.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =CADE27.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = CADE27.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = CADE27.cc.pdp.DBHxBCH_2$panel.args.common$z)



CADE27p.cc_DBHxBCH$status <- colnames(CADE27p.cc_DBHxBCH[,3:5])[apply(CADE27p.cc_DBHxBCH[,3:5],1,which.max)]
CADE27p.cc_DBHxBCH$Pstatus <- apply(CADE27p.cc_DBHxBCH[,3:5],1,max)

pal <- c("red", "goldenrod1","green4" )

CADE27.cc_CVSxDBH_plot <- ggplot(data=CADE27p.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
CADE27.cc_CVSxDBH_plot


CADE27.cc_CVSxBCH_plot <- ggplot(data=CADE27p.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
CADE27.cc_CVSxBCH_plot


CADE27.cc_DBHxBCH_plot <- ggplot(data=CADE27p.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
CADE27.cc_DBHxBCH_plot





####LAOC####

LAOCp.cc_CVSxDBH <- data.frame(CVS = LAOC.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = LAOC.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =LAOC.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = LAOC.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = LAOC.cc.pdp.CVSxDBH_2$panel.args.common$z)

LAOCp.cc_CVSxDBH$status <- colnames(LAOCp.cc_CVSxDBH[,3:5])[apply(LAOCp.cc_CVSxDBH[,3:5],1,which.max)]
LAOCp.cc_CVSxDBH$Pstatus <- apply(LAOCp.cc_CVSxDBH[,3:5],1,max)


LAOCp.cc_CVSxBCH <- data.frame(CVS = LAOC.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = LAOC.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =LAOC.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = LAOC.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = LAOC.cc.pdp.CVSxBCH_2$panel.args.common$z)

LAOCp.cc_CVSxBCH$status <- colnames(LAOCp.cc_CVSxBCH[,3:5])[apply(LAOCp.cc_CVSxBCH[,3:5],1,which.max)]
LAOCp.cc_CVSxBCH$Pstatus <- apply(LAOCp.cc_CVSxBCH[,3:5],1,max)


LAOCp.cc_DBHxBCH <- data.frame(BCH = LAOC.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = LAOC.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =LAOC.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = LAOC.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = LAOC.cc.pdp.DBHxBCH_2$panel.args.common$z)



LAOCp.cc_DBHxBCH$status <- colnames(LAOCp.cc_DBHxBCH[,3:5])[apply(LAOCp.cc_DBHxBCH[,3:5],1,which.max)]
LAOCp.cc_DBHxBCH$Pstatus <- apply(LAOCp.cc_DBHxBCH[,3:5],1,max)


LAOC.cc_CVSxDBH_plot <- ggplot(data=LAOCp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
LAOC.cc_CVSxDBH_plot


LAOC.cc_CVSxBCH_plot <- ggplot(data=LAOCp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
LAOC.cc_CVSxBCH_plot


LAOC.cc_DBHxBCH_plot <- ggplot(data=LAOCp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "Bole Char Height (%)") +
  ylab(label = "DBH (cm)")
LAOC.cc_DBHxBCH_plot





####PICO#####

PICOp.cc_CVSxDBH <- data.frame(CVS = PICO.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = PICO.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =PICO.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = PICO.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = PICO.cc.pdp.CVSxDBH_2$panel.args.common$z)

PICOp.cc_CVSxDBH$status <- colnames(PICOp.cc_CVSxDBH[,3:5])[apply(PICOp.cc_CVSxDBH[,3:5],1,which.max)]
PICOp.cc_CVSxDBH$Pstatus <- apply(PICOp.cc_CVSxDBH[,3:5],1,max)


PICOp.cc_CVSxBCH <- data.frame(CVS = PICO.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = PICO.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =PICO.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = PICO.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = PICO.cc.pdp.CVSxBCH_2$panel.args.common$z)

PICOp.cc_CVSxBCH$status <- colnames(PICOp.cc_CVSxBCH[,3:5])[apply(PICOp.cc_CVSxBCH[,3:5],1,which.max)]
PICOp.cc_CVSxBCH$Pstatus <- apply(PICOp.cc_CVSxBCH[,3:5],1,max)


PICOp.cc_DBHxBCH <- data.frame(BCH = PICO.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = PICO.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =PICO.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = PICO.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = PICO.cc.pdp.DBHxBCH_2$panel.args.common$z)



PICOp.cc_DBHxBCH$status <- colnames(PICOp.cc_DBHxBCH[,3:5])[apply(PICOp.cc_DBHxBCH[,3:5],1,which.max)]
PICOp.cc_DBHxBCH$Pstatus <- apply(PICOp.cc_DBHxBCH[,3:5],1,max)


PICO.cc_CVSxDBH_plot <- ggplot(data=PICOp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
PICO.cc_CVSxDBH_plot


PICO.cc_CVSxBCH_plot <- ggplot(data=PICOp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
PICO.cc_CVSxBCH_plot


PICO.cc_DBHxBCH_plot <- ggplot(data=PICOp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "Bole Char Height (%)") +
  ylab(label = "DBH (cm)")
PICO.cc_DBHxBCH_plot





#####PILA####

PILAp.cc_CVSxDBH <- data.frame(CVS = PILA.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = PILA.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =PILA.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = PILA.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = PILA.cc.pdp.CVSxDBH_2$panel.args.common$z)

PILAp.cc_CVSxDBH$status <- colnames(PILAp.cc_CVSxDBH[,3:5])[apply(PILAp.cc_CVSxDBH[,3:5],1,which.max)]
PILAp.cc_CVSxDBH$Pstatus <- apply(PILAp.cc_CVSxDBH[,3:5],1,max)


PILAp.cc_CVSxBCH <- data.frame(CVS = PILA.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = PILA.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =PILA.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = PILA.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = PILA.cc.pdp.CVSxBCH_2$panel.args.common$z)

PILAp.cc_CVSxBCH$status <- colnames(PILAp.cc_CVSxBCH[,3:5])[apply(PILAp.cc_CVSxBCH[,3:5],1,which.max)]
PILAp.cc_CVSxBCH$Pstatus <- apply(PILAp.cc_CVSxBCH[,3:5],1,max)


PILAp.cc_DBHxBCH <- data.frame(BCH = PILA.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = PILA.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =PILA.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = PILA.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = PILA.cc.pdp.DBHxBCH_2$panel.args.common$z)



PILAp.cc_DBHxBCH$status <- colnames(PILAp.cc_DBHxBCH[,3:5])[apply(PILAp.cc_DBHxBCH[,3:5],1,which.max)]
PILAp.cc_DBHxBCH$Pstatus <- apply(PILAp.cc_DBHxBCH[,3:5],1,max)


PILA.cc_CVSxDBH_plot <- ggplot(data=PILAp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
PILA.cc_CVSxDBH_plot


PILA.cc_CVSxBCH_plot <- ggplot(data=PILAp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
PILA.cc_CVSxBCH_plot


PILA.cc_DBHxBCH_plot <- ggplot(data=PILAp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
PILA.cc_DBHxBCH_plot





#######PIPO#######
PIPOp.cc_CVSxDBH <- data.frame(CVS = PIPO.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = PIPO.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =PIPO.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = PIPO.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = PIPO.cc.pdp.CVSxDBH_2$panel.args.common$z)

PIPOp.cc_CVSxDBH$status <- colnames(PIPOp.cc_CVSxDBH[,3:5])[apply(PIPOp.cc_CVSxDBH[,3:5],1,which.max)]
PIPOp.cc_CVSxDBH$Pstatus <- apply(PIPOp.cc_CVSxDBH[,3:5],1,max)


PIPOp.cc_CVSxBCH <- data.frame(CVS = PIPO.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = PIPO.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =PIPO.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = PIPO.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = PIPO.cc.pdp.CVSxBCH_2$panel.args.common$z)

PIPOp.cc_CVSxBCH$status <- colnames(PIPOp.cc_CVSxBCH[,3:5])[apply(PIPOp.cc_CVSxBCH[,3:5],1,which.max)]
PIPOp.cc_CVSxBCH$Pstatus <- apply(PIPOp.cc_CVSxBCH[,3:5],1,max)


PIPOp.cc_DBHxBCH <- data.frame(BCH = PIPO.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = PIPO.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =PIPO.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = PIPO.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = PIPO.cc.pdp.DBHxBCH_2$panel.args.common$z)



PIPOp.cc_DBHxBCH$status <- colnames(PIPOp.cc_DBHxBCH[,3:5])[apply(PIPOp.cc_DBHxBCH[,3:5],1,which.max)]
PIPOp.cc_DBHxBCH$Pstatus <- apply(PIPOp.cc_DBHxBCH[,3:5],1,max)


PIPO.cc_CVSxDBH_plot <- ggplot(data=PIPOp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
PIPO.cc_CVSxDBH_plot


PIPO.cc_CVSxBCH_plot <- ggplot(data=PIPOp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
PIPO.cc_CVSxBCH_plot


PIPO.cc_DBHxBCH_plot <- ggplot(data=PIPOp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
PIPO.cc_DBHxBCH_plot




#####PSME#####

PSMEp.cc_CVSxDBH <- data.frame(CVS = PSME.cc.pdp.CVSxDBH_0$panel.args.common$x,
                               DBH = PSME.cc.pdp.CVSxDBH_0$panel.args.common$y,
                               PLive =PSME.cc.pdp.CVSxDBH_0$panel.args.common$z,
                               PDead1 = PSME.cc.pdp.CVSxDBH_1$panel.args.common$z,
                               PDead2_5 = PSME.cc.pdp.CVSxDBH_2$panel.args.common$z)

PSMEp.cc_CVSxDBH$status <- colnames(PSMEp.cc_CVSxDBH[,3:5])[apply(PSMEp.cc_CVSxDBH[,3:5],1,which.max)]
PSMEp.cc_CVSxDBH$Pstatus <- apply(PSMEp.cc_CVSxDBH[,3:5],1,max)


PSMEp.cc_CVSxBCH <- data.frame(CVS = PSME.cc.pdp.CVSxBCH_0$panel.args.common$x,
                               BCH = PSME.cc.pdp.CVSxBCH_0$panel.args.common$y,
                               PLive =PSME.cc.pdp.CVSxBCH_0$panel.args.common$z,
                               PDead1 = PSME.cc.pdp.CVSxBCH_1$panel.args.common$z,
                               PDead2_5 = PSME.cc.pdp.CVSxBCH_2$panel.args.common$z)

PSMEp.cc_CVSxBCH$status <- colnames(PSMEp.cc_CVSxBCH[,3:5])[apply(PSMEp.cc_CVSxBCH[,3:5],1,which.max)]
PSMEp.cc_CVSxBCH$Pstatus <- apply(PSMEp.cc_CVSxBCH[,3:5],1,max)


PSMEp.cc_DBHxBCH <- data.frame(BCH = PSME.cc.pdp.DBHxBCH_0$panel.args.common$x,
                               DBH = PSME.cc.pdp.DBHxBCH_0$panel.args.common$y,
                               PLive =PSME.cc.pdp.DBHxBCH_0$panel.args.common$z,
                               PDead1 = PSME.cc.pdp.DBHxBCH_1$panel.args.common$z,
                               PDead2_5 = PSME.cc.pdp.DBHxBCH_2$panel.args.common$z)



PSMEp.cc_DBHxBCH$status <- colnames(PSMEp.cc_DBHxBCH[,3:5])[apply(PSMEp.cc_DBHxBCH[,3:5],1,which.max)]
PSMEp.cc_DBHxBCH$Pstatus <- apply(PSMEp.cc_DBHxBCH[,3:5],1,max)


PSME.cc_CVSxDBH_plot <- ggplot(data=PSMEp.cc_CVSxDBH, aes(x=CVS, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "DBH (cm)")
PSME.cc_CVSxDBH_plot


PSME.cc_CVSxBCH_plot <- ggplot(data=PSMEp.cc_CVSxBCH, aes(x=CVS, y=BCH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "Crown Volume Scorch (%)") +
  ylab(label = "Bark Char Height (%)")
PSME.cc_CVSxBCH_plot


PSME.cc_DBHxBCH_plot <- ggplot(data=PSMEp.cc_DBHxBCH, aes(x=BCH, y=DBH, fill=status, alpha =Pstatus)) +
  geom_raster() +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "Bark Char Height (%)") +
  ylab(label = "DBH (cm)")
PSME.cc_DBHxBCH_plot




####Plotting Everything####

library(grid)
library(gridExtra)
library(ggpubr)


p1<-ABCO.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
  
p2<-ABCO.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p3<-ABCO.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p4<-ABGR.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p5<-ABGR.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p6<-ABGR.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
 
p7<-CADE27.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p8<-CADE27.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p9<-CADE27.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p10<-LAOC.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p11<-LAOC.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p12<-LAOC.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p13<-PICO.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p14<-PICO.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p15<-PICO.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p16<-PILA.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p17<-PILA.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p18<-PILA.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p19<-PIPO.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p20<-PIPO.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p21<-PIPO.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p22<-PSME.cc_CVSxDBH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none")  +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p23<-PSME.cc_CVSxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

p24<-PSME.cc_DBHxBCH_plot + scale_alpha(limits=c(0.3,1)) +
  ylim(c(0,200)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


grid.arrange(p1,p2,p3,
             p4,p5,p6,
             p7,p8,p9,
             p10,p11,p12,
             p13,p14,p15,
             p16,p17,p18,
             p19,p20,p21,
             p22,p23,p24,
             ncol=3)
#CVSxDBH
labBCH <- textGrob("Bark Char Height (%)", rot=90)
labDBH <- textGrob("DBH (cm)", rot=90)
labCVS <- textGrob("Crown Volume Scorch (%)")


grid.arrange(p1,p4,p7,p10,
             p13,p16,p19,p22,
             ncol=2, left=labDBH, bottom = labCVS)

ggarrange(p1,p4,
          p7,p10,
          p13,p16,
          p19,p22,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")

#Arrange by BT
ggarrange(p13,p4,
          p1,p7,
          p10,p22,
          p19,p16,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")



#CVSxBCH

grid.arrange(p2,p5,p8,p11,
             p14,p17,p20,p23,
             ncol=4, left=labBCH, bottom = labCVS)

ggarrange(p2,p5,
          p8,p11,
          p14,p17,
          p20,p23,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")
#Arrange by BT
ggarrange(p14,p5,
          p2,p8,
          p11,p23,
          p20,p17,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")


#BCHxDBH

labBCH <- textGrob("Bole Char Height (%)")
labDBH <- textGrob("DBH (cm)", rot=90)

grid.arrange(p3,p6,p9,p12,
             p15,p18,p21,p24,
             ncol=4, left=labDBH, bottom = labBCH)

ggarrange(p3,p6,
          p9,p12,
          p15,p18,
          p21,p24,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")


#Arrange by BT

ggarrange(p15,p6,
          p3,p9,
          p12,p24,
          p21,p18,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")

##Some final post processing was done (Fonts, etc) in Inkscape