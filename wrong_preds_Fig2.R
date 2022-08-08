library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(viridis)



p.ABCO.wrong <- ggplot(data=filter(ABCO.wrong.preds, dead_check!=9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("ABCO") +
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.ABGR.wrong <- ggplot(data=filter(ABGR.wrong.preds, dead_check !=9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("ABGR")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.CADE27.wrong <- ggplot(data=filter(CADE27.wrong.preds, dead_check !=9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("CADE27")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.LAOC.wrong <- ggplot(data=filter(LAOC.wrong.preds, dead_check != 9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("LAOC")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.PICO.wrong <- ggplot(data=filter(PICO.wrong.preds, dead_check != 9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("PICO")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.PILA.wrong <- ggplot(data=filter(PILA.wrong.preds, dead_check != 9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("PILA")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.PIPO.wrong <- ggplot(data=filter(PIPO.wrong.preds, dead_check !=9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("PIPO")+
  scale_color_viridis(discrete=TRUE, option = "turbo")

p.PSME.wrong <- ggplot(data=filter(PSME.wrong.preds, dead_check != 9), aes(x=factor(dead_check), y= pct_wrong, color=factor(model))) +
  geom_boxplot() +
  theme_classic() +
  ylim(c(0,100)) +
  ggtitle("PSME")+
  scale_color_viridis(discrete=TRUE, option = "turbo")




#Facet plots so they all fit in one figure
p1 <-p.ABCO.wrong + ggtitle("ABCO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p2 <- p.ABGR.wrong + ggtitle("ABGR")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p3 <- p.CADE27.wrong + ggtitle("CADE27")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p4 <- p.LAOC.wrong + ggtitle("LAOC")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p5 <- p.PICO.wrong + ggtitle("PICO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p6 <- p.PILA.wrong + ggtitle("PILA")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p7 <- p.PIPO.wrong + ggtitle("PIPO")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


p8 <- p.PSME.wrong + ggtitle("PSME")+
  ylim(c(0,100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())


ggarrange(p5,p2,
          p1,p3,
          p4,p8,
          p7,p6,
          ncol=2,
          nrow = 4,
          common.legend = TRUE,
          legend = "right")