###Paired error results

t1<- t.test(subset(ABCO.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(ABCO.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t2<- t.test(subset(ABCO.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(ABCO.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t3<- t.test(subset(ABCO.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(ABCO.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t4<- t.test(subset(ABCO.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(ABCO.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")


t5<- t.test(subset(ABGR.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(ABGR.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t6<- t.test(subset(ABGR.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(ABGR.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t7<- t.test(subset(ABGR.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(ABGR.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t8<- t.test(subset(ABGR.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(ABGR.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t9<- t.test(subset(CADE27.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(CADE27.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t10<- t.test(subset(CADE27.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(CADE27.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t11<- t.test(subset(CADE27.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(CADE27.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t12<- t.test(subset(CADE27.wrong.preds, model == 5 & dead_check == 1 & rep !=4)$pct_wrong, subset(CADE27.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t13<- t.test(subset(LAOC.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(LAOC.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t14<- t.test(subset(LAOC.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(LAOC.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t15<- t.test(subset(LAOC.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(LAOC.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t16<- t.test(subset(LAOC.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(LAOC.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t17<- t.test(subset(PICO.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(PICO.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t18<- t.test(subset(PICO.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(PICO.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t19<- t.test(subset(PICO.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(PICO.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t20<- t.test(subset(PICO.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(PICO.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t21<- t.test(subset(PILA.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(PILA.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t22<- t.test(subset(PILA.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(PILA.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t23<- t.test(subset(PILA.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(PILA.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t24<- t.test(subset(PILA.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(PILA.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t25<- t.test(subset(PIPO.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(PIPO.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t26<- t.test(subset(PIPO.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(PIPO.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t27<- t.test(subset(PIPO.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(PIPO.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t28<- t.test(subset(PIPO.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(PIPO.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")

t29<- t.test(subset(PSME.wrong.preds, model == 2 & dead_check == 1 )$pct_wrong, subset(PSME.wrong.preds, model == 2 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t30<- t.test(subset(PSME.wrong.preds, model == 3 & dead_check == 1 )$pct_wrong, subset(PSME.wrong.preds, model == 3 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t31<- t.test(subset(PSME.wrong.preds, model == 4 & dead_check == 1 )$pct_wrong, subset(PSME.wrong.preds, model == 4 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")
t32<- t.test(subset(PSME.wrong.preds, model == 5 & dead_check == 1 )$pct_wrong, subset(PSME.wrong.preds, model == 5 & dead_check == 2 )$pct_wrong, paired = TRUE, alternative = "less")


ttable <- data.frame(Species=c(rep("ABCO",4), rep("ABGR",4), rep("CADE27",4), rep("LAOC", 4), rep("PICO", 4), rep("PILA", 4), rep("PIPO",4), rep("PSME",4)),
                     Model = c(2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5),
                     Difference = c(t1$estimate, t2$estimate, t3$estimate,t4$estimate, t5$estimate, t6$estimate, t7$estimate,t8$estimate,t9$estimate,t10$estimate,
                                    t11$estimate, t12$estimate, t13$estimate,t14$estimate, t15$estimate, t16$estimate, t17$estimate,t18$estimate,t19$estimate,t20$estimate,
                                    t21$estimate, t22$estimate, t23$estimate,t24$estimate, t25$estimate, t26$estimate, t27$estimate,t28$estimate,t29$estimate,t30$estimate,t31$estimate,t32$estimate),
                     t = c(t1$statistic, t2$statistic, t3$statistic,t4$statistic, t5$statistic, t6$statistic, t7$statistic,t8$statistic,t9$statistic,t10$statistic,
                           t11$statistic, t12$statistic, t13$statistic,t14$statistic, t15$statistic, t16$statistic, t17$statistic,t18$statistic,t19$statistic,t20$statistic,
                           t21$statistic, t22$statistic, t23$statistic,t24$statistic, t25$statistic, t26$statistic, t27$statistic,t28$statistic,t29$statistic,t30$statistic,t31$statistic,t32$statistic),
                     Pvalue =c(t1$p.value, t2$p.value, t3$p.value,t4$p.value, t5$p.value, t6$p.value, t7$p.value,t8$p.value,t9$p.value,t10$p.value,
                               t11$p.value, t12$p.value, t13$p.value,t14$p.value, t15$p.value, t16$p.value, t17$p.value,t18$p.value,t19$p.value,t20$p.value,
                               t21$p.value, t22$p.value, t23$p.value,t24$p.value, t25$p.value, t26$p.value, t27$p.value,t28$p.value,t29$p.value,t30$p.value,t31$p.value,t32$p.value))

write.csv(ttable,"ttable.csv")



