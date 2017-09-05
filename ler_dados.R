library(readxl)
library(dplyr)
library(ggplot2)
library(plyr)
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
data_compact <- as.data.frame(read_excel("./dados/nanoparticulas_dadosR.xlsx"))
data_compact$Tratamento <- as.factor(data_compact$Tratamento)
data_compact$Comprimento <- as.numeric(data_compact$Comprimento)
data_compact$Dia <- as.factor(data_compact$Dia)
data_compact$Repeticao <- as.factor(data_compact$Repeticao)

prop.table(table(is.na(data_compact))) #quantidade de NA's geral

#
teste <- as.factor(c(rep(1:9, 60)))
data_compact <- cbind(teste, data_compact)

ggplot(data_compact, aes(Dia, Comprimento, color = Tratamento)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

#
