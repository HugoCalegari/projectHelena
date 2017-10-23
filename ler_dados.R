library(readxl)
library(dplyr)
library(ggplot2)
library(plyr)
library(plotrix)
setwd("~/Documentos/ME810/HELENA/projectHelena/")

###################################################################### 1ª forma
controle <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "controle"))
t1 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.006"))
t2 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.0125"))
t3 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.025"))
t4 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.05"))
t5 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.1"))

#Substitui os caracteres "Morta" por "NA". Estes comando é para o caso dos dados não organizados como em data_compact
controle$`8` <- gsub("Morta", "NA", controle$`8`)
t2[3,] <- gsub("Morta", "NA", t2[3,])
t3[9,] <- gsub("Morta", "NA", t3[9,])
t5$`7` <- gsub("Morta", "NA", t5$`7`)
t5$`8` <- gsub("Morta", "NA", t5$`8`)

dados <- list(controle, t1, t2, t3, t4, t5)

dados[[1]]$`8` <- as.numeric(dados[[1]]$`8`)

dados[[3]]$`7` <- as.numeric(dados[[3]]$`7`)
dados[[3]]$`8` <- as.numeric(dados[[3]]$`8`)

dados[[4]]$`4` <- as.numeric(dados[[4]]$`4`)
dados[[4]]$`5` <- as.numeric(dados[[4]]$`5`)
dados[[4]]$`6` <- as.numeric(dados[[4]]$`6`)
dados[[4]]$`7` <- as.numeric(dados[[4]]$`7`)
dados[[4]]$`8` <- as.numeric(dados[[4]]$`8`)

dados[[6]]$`7` <- as.numeric(dados[[6]]$`7`)
dados[[6]]$`8` <- as.numeric(dados[[6]]$`8`)

###################################################################### 2º forma
# 0 = controle, 1 = t0.006, 2 = t0.0125, 3 = t0.025, 4 = t0.05 e 5 = t0.1
data_compact_nano <- as.data.frame(read_excel("./dados/nanoparticulas_dadosR.xlsx"))
data_compact_nano$Tratamento <- as.factor(data_compact_nano$Tratamento)
data_compact_nano$Comprimento <- as.numeric(data_compact_nano$Comprimento)
data_compact_nano$Dia <- as.factor(data_compact_nano$Dia)
data_compact_nano$Repeticao <- as.factor(data_compact_nano$Repeticao)

prop.table(table(is.na(data_compact_nano))) #quantidade de NA's geral

ggplot(data_compact_nano, aes(Dia, Comprimento, color = Tratamento)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)


#Tentativa de fazer o perfil médio por dia
lista1 <- list(0,0,0,0,0,0)
lista2 <- list(0,0,0,0,0,0)
#which(is.na(data_compact_nano$Comprimento))
data_compact_nano <- na.omit(data_compact_nano)

for(i in 1:6){
  lista1[[i]] <- rep(0, 9)
  for(j in 1:9){
   lista1[[i]][j] <- mean( as.data.frame(data_compact_nano %>% filter(Tratamento == paste0(i-1) & Dia==paste0(j-1)))$Comprimento )
   lista2[[i]][j] <- sd( as.data.frame(data_compact_nano %>% filter(Tratamento == paste0(i-1) & Dia==paste0(j-1)))$Comprimento )
  }
}


suporte <- as.data.frame(cbind(rep(0:8, 6), c(lista1[[1]], lista1[[2]], lista1[[3]], lista1[[4]], lista1[[5]], lista1[[6]]),
                         c(lista2[[1]], lista2[[2]], lista2[[3]], lista2[[4]], lista2[[5]], lista2[[6]])))
suporte$V1 <- as.factor(suporte$V1)


plot(suporte$V2[1:9],axes=FALSE,ylim=c(0,0.9),cex.lab=1,xlab="Tempo",ylab="Comprimento médio", main = "Gráfico de perfil médio do comprimento")

axis(2,cex.axis=1)

axis(1,1:9,c("1","2","3","4", "5", "6", "7", "8", "9"),cex.axis=1)


plotCI(suporte$V2[1:9],liw=1.96*suporte$V3[1:9]/sqrt(10),uiw=1.96*suporte$V3[1:9]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 1, cex=1)

lines(suporte$V2[1:9],lwd=1)

plotCI(suporte$V2[10:18],liw=1.96*suporte$V3[10:18]/sqrt(10),uiw=1.96*suporte$V3[10:18]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 2,cex=1)

lines(suporte$V2[10:18],lwd=1, col = 2)

plotCI(suporte$V2[19:27],liw=1.96*suporte$V3[19:27]/sqrt(10),uiw=1.96*suporte$V3[19:27]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 3, cex=1)

lines(suporte$V2[19:27],lwd=1, col = 3)

plotCI(suporte$V2[28:36],liw=1.96*suporte$V3[28:36]/sqrt(10),uiw=1.96*suporte$V3[28:36]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 4,cex=1)

lines(suporte$V2[28:36],lwd=1, col = 4)

plotCI(suporte$V2[37:45],liw=1.96*suporte$V3[37:45]/sqrt(10),uiw=1.96*suporte$V3[37:45]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 5,cex=1)

lines(suporte$V2[37:45],lwd=1, col = 5)

plotCI(suporte$V2[46:54],liw=1.96*suporte$V3[46:54]/sqrt(10),uiw=1.96*suporte$V3[46:54]/sqrt(10),pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 6,cex=1)

lines(suporte$V2[46:54],lwd=1, col = 6)


data_compact_nano <- as.data.frame(read_excel("./dados/nanoparticulas_dadosR.xlsx"))
data_compact_nano$Tratamento <- as.factor(data_compact_nano$Tratamento)
data_compact_nano$Comprimento <- as.numeric(data_compact_nano$Comprimento)
data_compact_nano$Dia <- as.factor(data_compact_nano$Dia)
data_compact_nano$Repeticao <- as.factor(data_compact_nano$Repeticao)

#o ajuste "lme" não funciona quando se tem NA's. O que foi feito? Usado a função "na.omit()" para retirar as linhas
#que tenham os NA's. No total foram retiradas 16 linhas.

data_compact_nano <- na.omit(data_compact_nano)

library(nlme)
#Para o primeiro modelo considera-se que o crescimento é independente dos dias, mas isto de fato não ocorre
fit1 <- lme(Comprimento ~ Tratamento , data = data_compact_nano, random = ~ 1| Repeticao) 
plot(fit1)
qqnorm(fit1$residuals)
anova(fit1)
intervals(fit1, which = "fixed")

#############################################################################################

#Pelo gráfico de perfil nota-se que existe interação entre o dia e comprimento, algo esperado, pois pelos próprios
#dados nota-se o crescimento dos indivíduos

interaction.plot(data_compact_nano$Dia, data_compact_nano$Tratamento, data_compact_nano$Comprimento, las = 1)
fit2 <- lme(Comprimento ~ Tratamento*Dia , data = data_compact_nano, random = ~ 1| Repeticao) 
plot(fit2)
qqnorm(fit2)

#Arrumar a variância dos resíduos para o ajuste fit2; transformação da variável comprimento com o uso do log na
#base 10

data_compact_nano$Comprimento = log(data_compact_nano$Comprimento, base = 10)
fit2_2 <- lme(Comprimento ~ Tratamento*Dia , data = data_compact_nano, random = ~ 1| Repeticao) 
plot(fit2_2) #Os resíduos estão melhores que a variável resposta sem transformação
hist(residuals(fit2_2), freq = F)

#padronizar resíduos
teste <- (residuals(fit2_2) - mean(residuals(fit2_2)))/sqrt(var(residuals(fit2_2)))
hist(teste, freq = F)
qqnorm(teste)
qqline(teste, col = 2, lwd=2)


#Sem padronização dos resíduos
teste2 <- residuals(fit2_2)

hist(teste2, freq = F)
qqnorm(teste2)
qqline(teste2, col = 2, lwd=2)

#Identificação da posição do menor resíduo que se distancia da reta de referência nos gráficos de qqnorm e no histograma
which(teste2 == min(teste2))
teste2[which(teste2 == min(teste2))]

#Verificar como ficam os resíduos ao se retirar o resíduo que distancia da reta de referência do Q-Q plot normal
teste2 <- teste2[-which(teste2 == min(teste2))]

hist(teste2, freq = F)
qqnorm(teste2)
qqline(teste2, col = 2, lwd=2)

#A posição do menor resíduo que mais se distancia da reta de referência no Q-Q plot normal é o mesma para os valores
#da variável resposta do log do comprimento e do próprio comprimento. VERIFICAR COMO FICA O AJUSTE SEM ESSE DADO

#############################################################################################
library(geepack)
fit3 <- geeglm(Comprimento ~ Tratamento*Dia, id = Repeticao, data = data_compact_nano)
#############################################################################################

#Com a retirada dos NA's tem dados balanceados ou não ? Observar tabela abaixo. A parir dela observa-se que
#com a retirada dos NA's tem-se dados não balanceados, ou seja, dentro de cada tratamento tem-se diferentes
#valores para as réplicas.

table(data_compact_nano$Tratamento, data_compact_nano$Repeticao)

#############################################################################################
#A variável resposta, que é o comprimento dos microcrustáceos é estritamente positiva;
require(car)
require(MASS)

par(mfrow=c(2,2))
qqp(data_compact_nano$Comprimento, "norm")
qqp(data_compact_nano$Comprimento, "lnorm")

gama <- fitdistr(data_compact_nano$Comprimento, "gamma")
qqp(data_compact_nano$Comprimento, "gamma", shape = gama$estimate[[1]], rate = gama$estimate[[2]])

beta <- fitdistr(data_compact_nano$Comprimento, "beta", start = list(shape1=1,shape2=1))
qqp(data_compact_nano$Comprimento, "beta", shape1 = beta$estimate[[1]], shape2 = beta$estimate[[2]])
