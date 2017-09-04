library(readxl)
library(dplyr)
library(ggplot2)
setwd("~/Documentos/ME810/HELENA/projectHelena/")

source("funcoes.R")

controle <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "controle"))
t1 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.006"))
t2 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.0125"))
t3 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.025"))
t4 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.05"))
t5 <- as.data.frame(read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.1"))

#Substitui os caracteres "Morta" por "NA".
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

#Obter um vetor de médias para cada dia, em controle e tratamentos
media_controle <- sapply(na.omit(dados[[1]][,2:10]), mean)
media_t1 <- sapply(na.omit(dados[[2]][,2:10]), mean)
media_t2 <- sapply(na.omit(dados[[3]][,2:10]), mean)
media_t3 <- sapply(na.omit(dados[[4]][,2:10]), mean)
media_t4 <- sapply(na.omit(dados[[5]][,2:10]), mean)
media_t5 <- sapply(na.omit(dados[[6]][,2:10]), mean)

medias <- list(media_controle, media_t1, media_t2, media_t3, media_t4, media_t5)
c(media_controle, media_t1, media_t2, media_t3, media_t4, media_t5)

#Obter um veto de variância para cada indivíduo
var_controle <- sapply(na.omit(dados[[1]][1:10,2:10]), var)