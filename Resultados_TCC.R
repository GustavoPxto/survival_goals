arg <- read.csv("C:/Users/gugap/Downloads/goals/arg.csv")
bra <- read.csv("C:/Users/gugap/Downloads/goals/bra.csv")
chi = read.csv("C:/Users/gugap/Downloads/goals/chi.csv")
col_apertura = read.csv("C:/Users/gugap/Downloads/goals/col_apertura.csv")
col_clausura = read.csv("C:/Users/gugap/Downloads/goals/col_clausura.csv")
par_apertura = read.csv("C:/Users/gugap/Downloads/goals/parl_apertura.csv")
par_clausura = read.csv("C:/Users/gugap/Downloads/goals/par_clausura.csv")
uru_apertura = read.csv("C:/Users/gugap/Downloads/goals/uru_apertura.csv")
uru_clausura = read.csv("C:/Users/gugap/Downloads/goals/uru_clausura.csv")

arg$league = "Argentina"
bra$league = "Brasil"
chi$league = "Chile"
col_apertura$league = "Colombia"
col_clausura$league = "Colombia"
par_apertura$league = "Paraguai"
par_clausura$league = "Paraguai"
uru_apertura$league = "Uruguai"
uru_clausura$league = "Uruguai"

#dados = rbind(arg2018, arg2019, arg2020, arg, bra2018, bra2019, bra2020, bra, chi2018, chi2019, chi2020, chi, col2018apertura, col2019apertura, col_apertura, col2018clausura, col2019clausura, col_clausura, col2020, par2018apertura, par2019apertura, par2020apertura, par_apertura, par2018clausura, par2019clausura, par2020clausura, par_clausura, uru2018apertura, uru2019apertura, uru2020apertura, uru_apertura, uru2018clausura, uru2019clausura, uru2020clausura, uru_clausura, uru2018intermedio, uru2019intermedio, uru2020intermedio)
  
library(survival)
library(dplyr)

#primeiro gol do jogo - equipe da casa
dados = dados %>% 
  mutate(time = replace(time, is.na(time), 90))
#brasurv = Surv(time = bra$time, event = bra$evento)

dados$cens <- with(dados, ifelse(is.na(GH)==T, 0,
                                 ifelse(GH == 1 & GA == 0, 1, 0)))

dados <- dados[!duplicated(dados$game),]
dadossurv = Surv(time = dados$time, event = dados$cens)

ekm = survfit(dadossurv ~ dados$league)
summary(ekm)

library(survminer)

ggsurvplot(ekm, data = dados, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

# Teste logrank: 
survdiff(dadossurv~dados$league, rho=0)

# Teste de Peto:
survdiff(dadossurv~dados$league, rho=1)


#primeiro gol do jogo - equipe de fora
dados2 = rbind(arg, bra, chi, col_apertura, col_clausura, par_apertura, par_clausura, uru_apertura, uru_clausura)
dados2 = dados2 %>% 
  mutate(time = replace(time, is.na(time), 90))

dados2$cens <- with(dados2, ifelse(is.na(GH)==T, 0,
                                 ifelse(GH == 0 & GA == 1, 1, 0)))

dados2 <- dados2[!duplicated(dados2$game),]
dados2surv = Surv(time = dados2$time, event = dados2$cens)

ekm2 = survfit(dados2surv ~ dados2$league)
summary(ekm2)

ggsurvplot(ekm2, data = dados2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Probabilidade de Sobrevivência", xlab = "Tempo", 
           legend.labs = c("Argentina", "Brasil", "Chile", "Colômbia", "Paraguai", "Uruguai"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

# Teste logrank: 
survdiff(dados2surv~dados2$league, rho=0)

# Teste de Peto:
survdiff(dados2surv~dados2$league, rho=1)

#Comparação múltipla

pairwise_survdiff(Surv(time, cens) ~ league ,p.adjust.method = "bonferroni", rho=0,  
                  data = dados2)

#Argentina

arg = arg %>% 
  mutate(time = replace(time, is.na(time), 90))

arg2 = arg
arg$cens <- with(arg, ifelse(is.na(GH)==T, 0,
                                 ifelse(GH == 1 & GA == 0, 1, 0)))

arg <- arg[!duplicated(arg$game),]

#argsurv = Surv(time = arg$time, event = arg$cens)
#argekm = survfit(argsurv ~ 1)

arg2$cens <- with(arg2, ifelse(is.na(GH)==T, 0,
                                   ifelse(GH == 0 & GA == 1, 1, 0)))

arg2 <- arg2[!duplicated(arg2$game),]

#arg$cens2 = arg2$cens
arg$group = "Mandante"
arg2$group = "Visitante"
arg = rbind(arg, arg2)

#arg2surv = Surv(time = arg$time, event = arg$cens2)
#arg2ekm = survfit(arg2surv ~ 1)
#fit <- list(Casa = argekm, Fora = arg2ekm)

argsurv = Surv(time = arg$time, event = arg$cens)
argekm = survfit(argsurv ~ arg$group)

ggsurvplot(argekm, data = arg, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)
           
#Brasil

bra = bra %>% 
  mutate(time = replace(time, is.na(time), 90))

bra2 = bra
bra$cens <- with(bra, ifelse(is.na(GH)==T, 0,
                             ifelse(GH == 1 & GA == 0, 1, 0)))

bra <- bra[!duplicated(bra$game),]

#brasurv = Surv(time = bra$time, event = bra$cens)
#braekm = survfit(brasurv ~ 1)

bra2$cens <- with(bra2, ifelse(is.na(GH)==T, 0,
                               ifelse(GH == 0 & GA == 1, 1, 0)))

bra2 <- bra2[!duplicated(bra2$game),]

#bra$cens2 = bra2$cens
#bra2surv = Surv(time = bra$time, event = bra$cens2)
#bra2ekm = survfit(bra2surv ~ 1)
#fit <- list(Casa = braekm, Fora = bra2ekm)

bra$group = "Mandante"
bra2$group = "Visitante" 
bra = rbind(bra, bra2)

brasurv = Surv(time = bra$time, event = bra$cens)
braekm = survfit(brasurv ~ bra$group)

ggsurvplot(braekm, data = bra, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Chile

chi = chi %>% 
  mutate(time = replace(time, is.na(time), 90))

chi2 = chi
chi$cens <- with(chi, ifelse(is.na(GH)==T, 0,
                             ifelse(GH == 1 & GA == 0, 1, 0)))

chi <- chi[!duplicated(chi$game),]

#chisurv = Surv(time = chi$time, event = chi$cens)
#chiekm = survfit(chisurv ~ 1)

chi2$cens <- with(chi2, ifelse(is.na(GH)==T, 0,
                               ifelse(GH == 0 & GA == 1, 1, 0)))

chi2 <- chi2[!duplicated(chi2$game),]

#chi$cens2 = chi2$cens
#chi2surv = Surv(time = chi$time, event = chi$cens2)
#chi2ekm = survfit(chi2surv ~ 1)
#fit <- list(Casa = chiekm, Fora = chi2ekm)

chi$group = "Mandante"
chi2$group = "Visitante" 
chi = rbind(chi, chi2)

chisurv = Surv(time = chi$time, event = chi$cens)
chiekm = survfit(chisurv ~ chi$group)

ggsurvplot(chiekm, data = chi, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Colombia

col = rbind(col_apertura, col_clausura)

col = col %>% 
  mutate(time = replace(time, is.na(time), 90))

col2 = col
col$cens <- with(col, ifelse(is.na(GH)==T, 0,
                             ifelse(GH == 1 & GA == 0, 1, 0)))

col <- col[!duplicated(col$game),]

#colsurv = Surv(time = col$time, event = col$cens)
#colekm = survfit(colsurv ~ 1)

col2$cens <- with(col2, ifelse(is.na(GH)==T, 0,
                               ifelse(GH == 0 & GA == 1, 1, 0)))

col2 <- col2[!duplicated(col2$game),]

#col$cens2 = col2$cens
#col2surv = Surv(time = col$time, event = col$cens2)
#col2ekm = survfit(col2surv ~ 1)
#fit <- list(Casa = colekm, Fora = col2ekm)

col$group = "Mandante"
col2$group = "Visitante" 
col2 = rbind(col, col2)

col2surv = Surv(time = col2$time, event = col2$cens)
col2ekm = survfit(col2surv ~ col2$group)

ggsurvplot(col2ekm, data = col2, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Paraguai

par = rbind(par_apertura, par_clausura)

par = par %>% 
  mutate(time = replace(time, is.na(time), 90))

par2 = par
par$cens <- with(par, ifelse(is.na(GH)==T, 0,
                             ifelse(GH == 1 & GA == 0, 1, 0)))

par <- par[!duplicated(par$game),]

#parsurv = Surv(time = par$time, event = par$cens)
#parekm = survfit(parsurv ~ 1)

par2$cens <- with(par2, ifelse(is.na(GH)==T, 0,
                               ifelse(GH == 0 & GA == 1, 1, 0)))

par2 <- par2[!duplicated(par2$game),]

#par$cens2 = par2$cens
#par2surv = Surv(time = par$time, event = par$cens2)
#par2ekm = survfit(par2surv ~ 1)
#fit <- list(Casa = parekm, Fora = par2ekm)

par$group = "Mandante"
par2$group = "Visitante" 
par = rbind(par, par2)

parsurv = Surv(time = par$time, event = par$cens)
parekm = survfit(parsurv ~ par$group)

ggsurvplot(parekm, data = par, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#Uruguai

uru = rbind(uru_apertura, uru_clausura)

uru = uru %>% 
  mutate(time = replace(time, is.na(time), 90))

uru2 = uru
uru$cens <- with(uru, ifelse(is.na(GH)==T, 0,
                             ifelse(GH == 1 & GA == 0, 1, 0)))

uru <- uru[!duplicated(uru$game),]

#urusurv = Surv(time = uru$time, event = uru$cens)
#uruekm = survfit(urusurv ~ 1)

uru2$cens <- with(uru2, ifelse(is.na(GH)==T, 0,
                               ifelse(GH == 0 & GA == 1, 1, 0)))

uru2 <- uru2[!duplicated(uru2$game),]

#uru$cens2 = uru2$cens
#uru2surv = Surv(time = uru$time, event = uru$cens2)
#uru2ekm = survfit(uru2surv ~ 1)
#fit <- list(Casa = uruekm, Fora = uru2ekm)

uru$group = "Mandante"
uru2$group = "Visitante" 
uru = rbind(uru, uru2)

urusurv = Surv(time = uru$time, event = uru$cens)
uruekm = survfit(urusurv ~ uru$group)

ggsurvplot(uruekm, data = uru, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Tempo", legend.labs = c("Mandante", "Visitante"),
           ylab = "Probabilidade de Sobrevivência", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)
