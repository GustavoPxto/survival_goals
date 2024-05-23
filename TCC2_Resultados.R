library(dplyr)
library(survival)
library(tidyr)
library(ggplot2)
library(survminer)
library(npcure)
library(mixcure)

dadoscomp = read.csv("home_matches.csv")
dadoscomp2 = read.csv("away_matches.csv")
dadosmult = read.csv("recurring_matches.csv")

# Argentina - Kaplan-Meier Estimate
argcomp = dadoscomp[dadoscomp$league=="Argentina",]
argcomp2 = dadoscomp2[dadoscomp2$league=="Argentina",]

argcomp$group = "Home"
argcomp2$group = "Away"
argcomp = rbind(argcomp, argcomp2)

argcompsurv = Surv(time = argcomp$time, event = argcomp$cens)
argcompekm = survfit(argcompsurv ~ argcomp$group)

ggsurvplot(argcompekm, data = argcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Brazil - Kaplan-Meier Estimate
bracomp = dadoscomp[dadoscomp$league=="Brasil",]
bracomp2 = dadoscomp2[dadoscomp2$league=="Brasil",]

bracomp$group = "Home"
bracomp2$group = "Away"
bracomp = rbind(bracomp, bracomp2)

bracompsurv = Surv(time = bracomp$time, event = bracomp$cens)
bracompekm = survfit(bracompsurv ~ bracomp$group)

ggsurvplot(bracompekm, data = bracomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Chile - Kaplan-Meier Estimate
chicomp = dadoscomp[dadoscomp$league=="Chile",]
chicomp2 = dadoscomp2[dadoscomp2$league=="Chile",]

chicomp$group = "Home"
chicomp2$group = "Away"
chicomp = rbind(chicomp, chicomp2)

chicompsurv = Surv(time = chicomp$time, event = chicomp$cens)
chicompekm = survfit(chicompsurv ~ chicomp$group)

ggsurvplot(chicompekm, data = chicomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Colombia - Kaplan-Meier Estimate
colcomp = dadoscomp[dadoscomp$league=="Colombia",]
colcomp2 = dadoscomp2[dadoscomp2$league=="Colombia",]

colcomp$group = "Home"
colcomp2$group = "Away"
colcomp = rbind(colcomp, colcomp2)

colcompsurv = Surv(time = colcomp$time, event = colcomp$cens)
colcompekm = survfit(colcompsurv ~ colcomp$group)

ggsurvplot(colcompekm, data = colcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Paraguay - Kaplan-Meier Estimate
parcomp = dadoscomp[dadoscomp$league=="Paraguai",]
parcomp2 = dadoscomp2[dadoscomp2$league=="Paraguai",]

parcomp$group = "Home"
parcomp2$group = "Away"
parcomp = rbind(parcomp, parcomp2)

parcompsurv = Surv(time = parcomp$time, event = parcomp$cens)
parcompekm = survfit(parcompsurv ~ parcomp$group)

ggsurvplot(parcompekm, data = parcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Uruguay - Kaplan-Meier Estimate
urucomp = dadoscomp[dadoscomp$league=="Uruguai",]
urucomp2 = dadoscomp2[dadoscomp2$league=="Uruguai",]

urucomp$group = "Home"
urucomp2$group = "Away"
urucomp = rbind(urucomp, urucomp2)

urucompsurv = Surv(time = urucomp$time, event = urucomp$cens)
urucompekm = survfit(urucompsurv ~ urucomp$group)

ggsurvplot(urucompekm, data = urucomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Home team scores first - Kaplan-Meier estimate

#Ligas
dadoscompsurv = Surv(time = dadoscomp$time, event = dadoscomp$cens)

ekmcomp = survfit(dadoscompsurv ~ dadoscomp$league)

ggsurvplot(ekmcomp, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Argentina", "Brazil", "Chile", "Colombia", "Paraguay", "Uruguay"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcompcovid = survfit(dadoscompsurv ~ dadoscomp$covid)

ggsurvplot(ekmcompcovid, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Before", "After"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Quartiles
quantile(ekmcomp)

#Away team scores first - Kaplan-Meier Estimate

#Leagues
dadoscomp2surv = Surv(time = dadoscomp2$time, event = dadoscomp2$cens)

ekmcomp2 = survfit(dadoscomp2surv ~ dadoscomp2$league)

ggsurvplot(ekmcomp2, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Argentina", "Brazil", "Chile", "Colombia", "Paraguay", "Uruguay"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcomp2covid = survfit(dadoscomp2surv ~ dadoscomp2$covid)

ggsurvplot(ekmcomp2covid, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Before", "After"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Multiple comparison
pairwise_survdiff(Surv(time, cens) ~ league ,p.adjust.method = "bonferroni", rho=0,  
                  data = dadoscomp2)

#Quartiles
quantile(ekmcomp2)

#Cox model for the home team scoring first

#Adjusting the model
cox_regalt = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp)
summary(cox_regalt)

#Schoenfeld residuals
testph = cox.zph(cox_regalt)
ggcoxzph(testph)


#Cox model for the away team scoring first

#Adjusting the model
cox_regalt2 = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp2)
summary(cox_regalt2)

#Schoenfeld residuals
testph2 = cox.zph(cox_regalt2)
ggcoxzph(testph2)

#Cure fraction model for the home team scoring first

#Maller-Zhou test
testmz(t = time, d = cens, dataset = dadoscomp)

#Cure Probabilities

#Leagues
probcure = c(min(ekmcomp[1]$surv), min(ekmcomp[2]$surv), min(ekmcomp[3]$surv), min(ekmcomp[4]$surv), min(ekmcomp[5]$surv), min(ekmcomp[6]$surv))

#Covid
probcurecovid = c(min(ekmcompcovid[1]$surv), min(ekmcompcovid[2]$surv))

#Adjusting the model
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp)

#Cure fraction model for the away team scoring first

#Maller-Zhou test
testmz(t = time, d = cens, dataset = dadoscomp2)

#Cure probabilities

#Leagues
probcure2 = c(min(ekmcomp2[1]$surv), min(ekmcomp2[2]$surv), min(ekmcomp2[3]$surv), min(ekmcomp2[4]$surv), min(ekmcomp2[5]$surv), min(ekmcomp2[6]$surv))
#Covid
probcurecovid2 = c(min(ekmcomp2covid[1]$surv), min(ekmcomp2covid[2]$surv))

#Adjusting the model
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp2)

#AG model

#Adjusting the model
agfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game), data = dadosmult)
summary(agfitalt)

#Schoenfeld residuals
agtestph = cox.zph(agfitalt)
ggcoxzph(agtestph)

#PWP model

#Adjusting the model
pwpfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game) + strata(stratum), data = dadosmult)
summary(pwpfitalt)

#Schoenfeld residuals
pwptestph = cox.zph(pwpfitalt)
ggcoxzph(pwptestph)





