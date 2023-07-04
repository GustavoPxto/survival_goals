arg2018 <- read.csv("C:/Users/gugap/Downloads/sagoals/arg2018.csv")
arg2019 <- read.csv("C:/Users/gugap/Downloads/sagoals/arg2019.csv")
arg2020 <- read.csv("C:/Users/gugap/Downloads/sagoals/arg2020.csv")
bra2018 <- read.csv("C:/Users/gugap/Downloads/sagoals/bra2018.csv")
bra2019 <- read.csv("C:/Users/gugap/Downloads/sagoals/bra2019.csv")
bra2020 <- read.csv("C:/Users/gugap/Downloads/sagoals/bra2020.csv")
chi2018 = read.csv("C:/Users/gugap/Downloads/sagoals/chi2018.csv")
chi2019 = read.csv("C:/Users/gugap/Downloads/sagoals/chi2019.csv")
chi2020 = read.csv("C:/Users/gugap/Downloads/sagoals/chi2020.csv")
col2018apertura = read.csv("C:/Users/gugap/Downloads/sagoals/col2018apertura.csv")
col2019apertura = read.csv("C:/Users/gugap/Downloads/sagoals/col2019apertura.csv")
col2018clausura = read.csv("C:/Users/gugap/Downloads/sagoals/col2018clausura.csv")
col2019clausura = read.csv("C:/Users/gugap/Downloads/sagoals/col2019clausura.csv")
col2020 = read.csv("C:/Users/gugap/Downloads/sagoals/col2020.csv")
par2018apertura = read.csv("C:/Users/gugap/Downloads/sagoals/par2018apertura.csv")
par2019apertura = read.csv("C:/Users/gugap/Downloads/sagoals/par2019apertura.csv")
par2020apertura = read.csv("C:/Users/gugap/Downloads/sagoals/par2020apertura.csv")
par2018clausura = read.csv("C:/Users/gugap/Downloads/sagoals/par2018clausura.csv")
par2019clausura = read.csv("C:/Users/gugap/Downloads/sagoals/par2019clausura.csv")
par2020clausura = read.csv("C:/Users/gugap/Downloads/sagoals/par2020clausura.csv")
uru2018apertura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2018apertura.csv")
uru2019apertura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2019apertura.csv")
uru2020apertura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2020apertura.csv")
uru2018clausura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2018clausura.csv")
uru2019clausura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2019clausura.csv")
uru2020clausura = read.csv("C:/Users/gugap/Downloads/sagoals/uru2020clausura.csv")
uru2018intermedio = read.csv("C:/Users/gugap/Downloads/sagoals/uru2018intermedio.csv")
uru2019intermedio = read.csv("C:/Users/gugap/Downloads/sagoals/uru2019intermedio.csv")
uru2020intermedio = read.csv("C:/Users/gugap/Downloads/sagoals/uru2020intermedio.csv")

arg2018$league = "Argentina"
arg2019$league = "Argentina"
arg2020$league = "Argentina"
arg$league = "Argentina"
bra2018$league = "Brasil"
bra2019$league = "Brasil"
bra2020$league = "Brasil"
bra$league = "Brasil"
chi2018$league = "Chile"
chi2019$league = "Chile"
chi2020$league = "Chile"
chi$league = "Chile"
col2018apertura$league = "Colombia"
col2019apertura$league = "Colombia"
col_apertura$league = "Colombia"
col2018clausura$league = "Colombia"
col2019clausura$league = "Colombia"
col_clausura$league = "Colombia"
col2020$league = "Colombia"
par2018apertura$league = "Paraguai"
par2019apertura$league = "Paraguai"
par2020apertura$league = "Paraguai"
par_apertura$league = "Paraguai"
par2018clausura$league = "Paraguai"
par2019clausura$league = "Paraguai"
par2020clausura$league = "Paraguai"
par_clausura$league = "Paraguai"
uru2018apertura$league = "Uruguai"
uru2019apertura$league = "Uruguai"
uru2020apertura$league = "Uruguai"
uru_apertura$league = "Uruguai"
uru2018clausura$league = "Uruguai"
uru2019clausura$league = "Uruguai"
uru2020clausura$league = "Uruguai"
uru_clausura$league = "Uruguai"
uru2018intermedio$league = "Uruguai"
uru2019intermedio$league = "Uruguai"
uru2020intermedio$league = "Uruguai"

arg2018$covid = "Antes"
arg2019$covid = "Antes"
arg2020$covid = "Antes"
arg$covid = "Depois"
bra2018$covid = "Antes"
bra2019$covid = "Antes"
bra2020$covid = "Depois"
bra$covid = "Depois"
chi2018$covid = "Antes"
chi2019$covid = "Antes"
chi2020$covid = "Depois"
chi$covid = "Depois"
col2018apertura$covid = "Antes"
col2019apertura$covid = "Antes"
col_apertura$covid ="Depois"
col2018clausura$covid = "Antes"
col2019clausura$covid = "Antes"
col_clausura$covid = "Depois"
col2020$covid = "Depois"
par2018apertura$covid = "Antes"
par2019apertura$covid = "Antes"
par2020apertura$covid = "Depois"
par_apertura$covid = "Depois"
par2018clausura$covid = "Antes"
par2019clausura$covid = "Antes"
par2020clausura$covid = "Depois"
par_clausura$covid = "Depois"
uru2018apertura$covid = "Antes"
uru2019apertura$covid = "Antes"
uru2020apertura$covid = "Depois"
uru_apertura$covid = "Depois"
uru2018clausura$covid = "Antes"
uru2019clausura$covid = "Antes"
uru2020clausura$covid = "Depois"
uru_clausura$covid = "Depois"
uru2018intermedio$covid = "Antes"
uru2019intermedio$covid = "Antes"
uru2020intermedio$covid = "Depois"

chi2019 = chi2019[c(1:523, 533:537),]

chi2020[1:182,]$covid = "Antes"
chi2020[168,]$covid = "Depois"
col2020[1:196,]$covid = "Antes"
par2020apertura[1:142,]$covid = "Antes"
uru2020apertura[1:63,]$covid = "Antes"

arg2018 = arg2018[,-1]
arg2019 = arg2019[,-1]
arg2020 = arg2020[,-1]
bra2018 = bra2018[,-1]
bra2019 = bra2019[,-1]
bra2020 = bra2020[,-1]
chi2018 = chi2018[,-1]
chi2019 = chi2019[,-1]
chi2020 = chi2020[,-1]
col2018apertura = col2018apertura[,-1]
col2019apertura = col2019apertura[,-1]
col2018clausura = col2018clausura[,-1]
col2019clausura = col2019clausura[,-1]
col2020 = col2020[,-1]
par2018apertura = par2018apertura[,-1]
par2019apertura = par2019apertura[,-1]
par2020apertura = par2020apertura[,-1]
par2018clausura = par2018clausura[,-1]
par2019clausura = par2019clausura[,-1]
par2020clausura = par2020clausura[,-1]
uru2018apertura = uru2018apertura[,-1]
uru2019apertura = uru2019apertura[,-1]
uru2020apertura = uru2020apertura[,-1]
uru2018clausura = uru2018clausura[,-1]
uru2019clausura = uru2019clausura[,-1]
uru2020clausura = uru2020clausura[,-1]
uru2018intermedio = uru2018intermedio[,-1]
uru2019intermedio = uru2019intermedio[,-1]
uru2020intermedio = uru2020intermedio[,-1]


dadoscomp = rbind(arg2018, arg2019, arg2020, arg, bra2018, bra2019, bra2020, bra, chi2018, chi2019, chi2020, chi, col2018apertura, col2019apertura, col_apertura, col2018clausura, col2019clausura, col_clausura, col2020, par2018apertura, par2019apertura, par2020apertura, par_apertura, par2018clausura, par2019clausura, par2020clausura, par_clausura, uru2018apertura, uru2019apertura, uru2020apertura, uru_apertura, uru2018clausura, uru2019clausura, uru2020clausura, uru_clausura, uru2018intermedio, uru2019intermedio, uru2020intermedio)

library(dplyr)
library(survival)
library(tidyr)

#primeiro gol do jogo - equipe da casa
dadoscomp = dadoscomp %>% 
  mutate(time = replace(time, is.na(time), 90))

dadoscomp$cens <- with(dadoscomp, ifelse(is.na(GH)==T, 0,
                                 ifelse(GH == 1 & GA == 0, 1, 0)))

dadosmult = dadoscomp
dadosmult$start = dadosmult$time
dadosmult$stop = dadosmult$time
dadosmult$start[2:nrow(dadosmult)] = dadosmult$start[1:(nrow(dadosmult)-1)]

dadosmult[!duplicated(dadosmult$game),]$start=0

dadosna = dadosmult[!complete.cases(dadosmult),]
dadosmult = dadosmult[complete.cases(dadosmult),]

dadosmult = dadosmult %>% 
  group_by(game) %>% 
  slice(c(1:n(),n()))

dadosmult = rbind(dadosmult, dadosna)

dadosmult[!duplicated(dadosmult$game, fromLast = T),]$start = dadosmult[!duplicated(dadosmult$game, fromLast = T),]$stop
dadosmult[!duplicated(dadosmult$game, fromLast = T),]$stop = 90

dadosmult[dadosmult$start>dadosmult$stop,]$start=0
dadosmult$cens = 1
dadosmult[!duplicated(dadosmult$game, fromLast = T),]$cens = 0

dadosmult[is.na(dadosmult$GH)==T,]$start=0
dadosmult[is.na(dadosmult$GH)==T,]$cens=0

dadosmult$stratum = with(dadosmult, ave(rep(1, nrow(dadosmult)), game, FUN = seq_along))

dadosmult = dadosmult %>%
  dplyr::group_by(game) %>%
  filter(!any(stratum>5))
  
dadoscomp <- dadoscomp[!duplicated(dadoscomp$game),]

cox_reg1 = coxph(Surv(time, cens) ~ league + covid + league*covid, data = dadoscomp)
summary(cox_reg1)

cox_regalt = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp)
summary(cox_regalt)

anova(cox_regalt, cox_reg1)

library(survminer)

testph = cox.zph(cox_reg1)
ggcoxdiagnostics(cox_reg1, type = "schoenfeld")
ggcoxzph(testph)

dadoscompsurv = Surv(time = dadoscomp$time, event = dadoscomp$cens)

ekmcomp = survfit(dadoscompsurv ~ dadoscomp$league)

ggsurvplot(ekmcomp, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

ekmcompcovid = survfit(dadoscompsurv ~ dadoscomp$covid)

ggsurvplot(ekmcompcovid, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Antes", "Depois"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)


#primeiro gol do jogo - equipe de fora
dadoscomp2 = rbind(arg2018, arg2019, arg2020, arg, bra2018, bra2019, bra2020, bra, chi2018, chi2019, chi2020, chi, col2018apertura, col2019apertura, col_apertura, col2018clausura, col2019clausura, col_clausura, col2020, par2018apertura, par2019apertura, par2020apertura, par_apertura, par2018clausura, par2019clausura, par2020clausura, par_clausura, uru2018apertura, uru2019apertura, uru2020apertura, uru_apertura, uru2018clausura, uru2019clausura, uru2020clausura, uru_clausura, uru2018intermedio, uru2019intermedio, uru2020intermedio)

dadoscomp2 = dadoscomp2 %>% 
  mutate(time = replace(time, is.na(time), 90))

dadoscomp2$cens <- with(dadoscomp2, ifelse(is.na(GH)==T, 0,
                                   ifelse(GH == 0 & GA == 1, 1, 0)))

dadoscomp2 <- dadoscomp2[!duplicated(dadoscomp2$game),]

cox_reg2 = coxph(Surv(time, cens) ~ league, data = dadoscomp2)
cox_regalt2 = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp2)

anova(cox_reg2, cox_regalt2)

testph2 = cox.zph(cox_reg2)
ggcoxzph(testph2)

dadoscomp2surv = Surv(time = dadoscomp2$time, event = dadoscomp2$cens)
ekmcomp2 = survfit(dadoscomp2surv ~ dadoscomp2$league)

ggsurvplot(ekmcomp2, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

ekmcomp2covid = survfit(dadoscomp2surv ~ dadoscomp2$covid)

ggsurvplot(ekmcomp2covid, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Antes", "Depois"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#primeiro gol do jogo -riscos competitivos

dadoscrisk = dadoscomp 

dadoscrisk$cens <- with(dadoscrisk, ifelse(is.na(GH)==T, 0,
                                         ifelse(GH == 1 & GA == 0, 1, 2)))

dadoscrisk$cens = factor(dadoscrisk$cens, 0:2, c("Sem gols", "Mandante marca", "Visitante marca"))

crsurv = Surv(time = dadoscrisk$time, event = dadoscrisk$cens)
crfit = survfit(crsurv ~ dadoscrisk$league)

ggcompetingrisks(crfit, title = "", legend.title = c("Evento"), 
                 ylab = "Probabilidade de um evento", xlab = "Tempo") 
                 

crfitcovid = survfit(crsurv ~ dadoscrisk$covid)
ggcompetingrisks(crfitcovid, title = "", legend.title = c("Evento"), 
                 ylab = "Probabilidade de um evento", xlab = "Tempo")

crcox = coxph(Surv(time, cens) ~ league, id = game, data = dadoscrisk)
crcoxalt = coxph(Surv(time, cens=="Visitante marca") ~ league, id = game, data = dadoscrisk)

testph3 = cox.zph(crcox)
ggcoxzph(testph3)

#primeiro gol do jogo - equipe da casa - fração de cura
library(npcure)
library(smcure)
library(mixcure)

testmz(t = time, d = cens, dataset = dadoscomp)

probcure = c(min(ekmcomp[1]$surv), min(ekmcomp[2]$surv), min(ekmcomp[3]$surv), min(ekmcomp[4]$surv), min(ekmcomp[5]$surv), min(ekmcomp[6]$surv))
probcurecovid = c(min(ekmcompcovid[1]$surv), min(ekmcompcovid[2]$surv))

mixcure(Surv(time, cens) ~ league + covid + league*covid, ~ league + covid + league*covid, data = dadoscomp)

#primeiro gol do jogo - equipe de fora - fração de cura

testmz(t = time, d = cens, dataset = dadoscomp2)

probcure2 = c(min(ekmcomp2[1]$surv), min(ekmcomp2[2]$surv), min(ekmcomp2[3]$surv), min(ekmcomp2[4]$surv), min(ekmcomp2[5]$surv), min(ekmcomp2[6]$surv))
probcurecovid2 = c(min(ekmcomp2covid[1]$surv), min(ekmcomp2covid[2]$surv))

mixreg = mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp2)

#eventos recorrentes

agfit = coxph(Surv(start, stop, cens) ~ league + cluster(game), data = dadosmult)
agfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game), data = dadosmult)
summary(agfit)

agtestph = cox.zph(agfit)
ggcoxzph(agtestph)

phpfit = coxph(Surv(start, stop, cens) ~ league + cluster(game) + strata(stratum), data = dadosmult)
summary(phpfit)
phptestph = cox.zph(phpfit)
ggcoxzph(phptestph)


