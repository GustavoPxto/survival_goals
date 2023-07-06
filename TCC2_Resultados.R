library(dplyr)
library(survival)
library(tidyr)
library(survminer)
library(npcure)
library(mixcure)

dadoscomp = read.csv("partidas_mandante.csv")
dadoscomp2 = read.csv("partidas_visitante.csv")
dadosmult = read.csv("partidas_recorrente.csv")

# Argentina - Estimativa de Kaplan-Meier
argcomp = dadoscomp[dadoscomp$league=="Argentina",]
argcomp2 = dadoscomp2[dadoscomp2$league=="Argentina",]

argcomp$group = "Mandante"
argcomp2$group = "Visitante"
argcomp = rbind(argcomp, argcomp2)

argcompsurv = Surv(time = argcomp$time, event = argcomp$cens)
argcompekm = survfit(argcompsurv ~ argcomp$group)

ggsurvplot(argcompekm, data = argcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Brasil - Estimativa de Kaplan-Meier
bracomp = dadoscomp[dadoscomp$league=="Brasil",]
bracomp2 = dadoscomp2[dadoscomp2$league=="Brasil",]

bracomp$group = "Mandante"
bracomp2$group = "Visitante"
bracomp = rbind(bracomp, bracomp2)

bracompsurv = Surv(time = bracomp$time, event = bracomp$cens)
bracompekm = survfit(bracompsurv ~ bracomp$group)

ggsurvplot(bracompekm, data = bracomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Chile - Estimativa de Kaplan-Meier
chicomp = dadoscomp[dadoscomp$league=="Chile",]
chicomp2 = dadoscomp2[dadoscomp2$league=="Chile",]

chicomp$group = "Mandante"
chicomp2$group = "Visitante"
chicomp = rbind(chicomp, chicomp2)

chicompsurv = Surv(time = chicomp$time, event = chicomp$cens)
chicompekm = survfit(chicompsurv ~ chicomp$group)

ggsurvplot(chicompekm, data = chicomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Colombia - Estimativa de Kaplan-Meier
colcomp = dadoscomp[dadoscomp$league=="Colombia",]
colcomp2 = dadoscomp2[dadoscomp2$league=="Colombia",]

colcomp$group = "Mandante"
colcomp2$group = "Visitante"
colcomp = rbind(colcomp, colcomp2)

colcompsurv = Surv(time = colcomp$time, event = colcomp$cens)
colcompekm = survfit(colcompsurv ~ colcomp$group)

ggsurvplot(colcompekm, data = colcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Paraguai - Estimativa de Kaplan-Meier
parcomp = dadoscomp[dadoscomp$league=="Paraguai",]
parcomp2 = dadoscomp2[dadoscomp2$league=="Paraguai",]

parcomp$group = "Mandante"
parcomp2$group = "Visitante"
parcomp = rbind(parcomp, parcomp2)

parcompsurv = Surv(time = parcomp$time, event = parcomp$cens)
parcompekm = survfit(parcompsurv ~ parcomp$group)

ggsurvplot(parcompekm, data = parcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Uruguai - Estimativa de Kaplan-Meier
urucomp = dadoscomp[dadoscomp$league=="Uruguai",]
urucomp2 = dadoscomp2[dadoscomp2$league=="Uruguai",]

urucomp$group = "Mandante"
urucomp2$group = "Visitante"
urucomp = rbind(urucomp, urucomp2)

urucompsurv = Surv(time = urucomp$time, event = urucomp$cens)
urucompekm = survfit(urucompsurv ~ urucomp$group)

ggsurvplot(urucompekm, data = urucomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de SobrevivÃªncia", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Primeiro gol do mandante - Estimativa de Kaplan-Meier

#Ligas
dadoscompsurv = Surv(time = dadoscomp$time, event = dadoscomp$cens)

ekmcomp = survfit(dadoscompsurv ~ dadoscomp$league)

ggsurvplot(ekmcomp, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcompcovid = survfit(dadoscompsurv ~ dadoscomp$covid)

ggsurvplot(ekmcompcovid, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Antes", "Depois"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Quartis
quantile(ekmcomp)

#Primeiro gol do visitante - Estimativa de Kaplan-Meier

#Ligas
dadoscomp2surv = Surv(time = dadoscomp2$time, event = dadoscomp2$cens)

ekmcomp2 = survfit(dadoscomp2surv ~ dadoscomp2$league)

ggsurvplot(ekmcomp2, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcomp2covid = survfit(dadoscomp2surv ~ dadoscomp2$covid)

ggsurvplot(ekmcomp2covid, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Antes", "Depois"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Comparações múltiplas
pairwise_survdiff(Surv(time, cens) ~ league ,p.adjust.method = "bonferroni", rho=0,  
                  data = dadoscomp2)

#Quartis
quantile(ekmcomp2)

#Modelo de Cox para a marcação do primeiro gol pelo mandante

#Ajuste
cox_regalt = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp)
summary(cox_regalt)

#Resíduos Schoenfeld
testph = cox.zph(cox_regalt)
ggcoxzph(testph)


#Modelo de Cox para a marcação do primeiro gol pelo visitante

#Ajuste
cox_regalt2 = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp2)
summary(cox_regalt2)

#Resíduos Schoenfeld
testph2 = cox.zph(cox_regalt2)
ggcoxzph(testph2)

#Modelo de fração de cura para a marcação do primeiro gol pelo mandante

#Teste de Maller-Zhou
testmz(t = time, d = cens, dataset = dadoscomp)

#Probabilidades de cura

#Ligas
probcure = c(min(ekmcomp[1]$surv), min(ekmcomp[2]$surv), min(ekmcomp[3]$surv), min(ekmcomp[4]$surv), min(ekmcomp[5]$surv), min(ekmcomp[6]$surv))

#Covid
probcurecovid = c(min(ekmcompcovid[1]$surv), min(ekmcompcovid[2]$surv))

#Ajuste
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp)

#Modelo de fração de cura para a marcação do primeiro gol pelo visitante

#Teste de Maller-Zhou
testmz(t = time, d = cens, dataset = dadoscomp2)

#Probabilidades de cura

#Ligas
probcure2 = c(min(ekmcomp2[1]$surv), min(ekmcomp2[2]$surv), min(ekmcomp2[3]$surv), min(ekmcomp2[4]$surv), min(ekmcomp2[5]$surv), min(ekmcomp2[6]$surv))
#Covid
probcurecovid2 = c(min(ekmcomp2covid[1]$surv), min(ekmcomp2covid[2]$surv))

#Ajuste
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp2)

#Modelo AG

#Ajuste
agfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game), data = dadosmult)
summary(agfitalt)

#Resíduos Schoenfeld
agtestph = cox.zph(agfitalt)
ggcoxzph(agtestph)

#Modelo PWP

#Ajuste
pwpfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game) + strata(stratum), data = dadosmult)
summary(pwpfitalt)

#Resíduos Schoenfeld
pwptestph = cox.zph(pwpfitalt)
ggcoxzph(pwptestph)





