library(dplyr)
library(tidyr)
library(viridis)
library(ggpubr)

ABCO.cc$SPP <- "ABCO"
ABGR.cc$SPP <- "ABGR"
CADE27.cc$SPP <- "CADE27"
LAOC.cc$SPP <- "LAOC"
PICO.cc$SPP <- "PICO"
PILA.cc$SPP <- "PILA"
PIPO.cc$SPP <- "PIPO"
PSME.cc$SPP <- "PSME"

all.cc <- rbind(ABCO.cc, ABGR.cc, CADE27.cc, LAOC.cc, PICO.cc, PILA.cc,  PIPO.cc, PSME.cc)
all.cc$Status <- "Live > 5 Years"
all.cc$Status <- ifelse(all.cc$dead_check == 0, "Live > 5 Years", 
                        ifelse(all.cc$dead_check ==1, "Dead Year-1", "Dead Years-2-5"))


p1 <-ggplot(data= subset(all.cc, SPP=="PICO"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  xlim(c(-5,105)) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")



p2 <-ggplot(data= subset(all.cc, SPP=="PICO"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p3 <-ggplot(data= subset(all.cc, SPP=="PICO"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PICO") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")



p4 <-ggplot(data= subset(all.cc, SPP=="ABGR"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p5 <-ggplot(data= subset(all.cc, SPP=="ABGR"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p6 <-ggplot(data= subset(all.cc, SPP=="ABGR"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABGR") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p7 <-ggplot(data= subset(all.cc, SPP=="ABCO"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p8 <-ggplot(data= subset(all.cc, SPP=="ABCO"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p9 <-ggplot(data= subset(all.cc, SPP=="ABCO"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="ABCO") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p10 <-ggplot(data= subset(all.cc, SPP=="CADE27"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p11 <-ggplot(data= subset(all.cc, SPP=="CADE27"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p12 <-ggplot(data= subset(all.cc, SPP=="CADE27"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="CADE27") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p13 <-ggplot(data= subset(all.cc, SPP=="LAOC"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p14 <-ggplot(data= subset(all.cc, SPP=="LAOC"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p15 <-ggplot(data= subset(all.cc, SPP=="LAOC"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="LAOC") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p16 <-ggplot(data= subset(all.cc, SPP=="PSME"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p17 <-ggplot(data= subset(all.cc, SPP=="PSME"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p18 <-ggplot(data= subset(all.cc, SPP=="PSME"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PSME") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p19 <-ggplot(data= subset(all.cc, SPP=="PIPO"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p20 <-ggplot(data= subset(all.cc, SPP=="PIPO"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p21 <-ggplot(data= subset(all.cc, SPP=="PIPO"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PIPO") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


p22 <-ggplot(data= subset(all.cc, SPP=="PILA"), aes(x=CVS_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "Crown Scorch (%)") +
  ylab(label = "count")

p23 <-ggplot(data= subset(all.cc, SPP=="PILA"), aes(x=DBH_cm, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "DBH (cm)") +
  ylab(label = "count")

p24 <-ggplot(data= subset(all.cc, SPP=="PILA"), aes(x=BCH_percent, color=Status)) +
  geom_freqpoly(binwidth = 5) +
  scale_y_log10() +
  xlim(c(-5,105)) +
  scale_color_manual(values = pal) +
  theme_bw() +
  ggtitle(label="PILA") +
  xlab(label = "Bark Char (% of height)") +
  ylab(label = "count")


ggarrange(p1,p2,p3,
          p4,p5,p6,
          p7,p8,p9,
          p10,p11,p12,
          p13,p14,p15,
          p16,p17,p18,
          p19,p20,p21,
          p22,p23,p24,
          ncol=3,
          nrow = 8,
          common.legend = TRUE,
          legend = "right")
