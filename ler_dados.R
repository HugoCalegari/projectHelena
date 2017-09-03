library(readxl)
library(dplyr)
library(ggplot2)
setwd("~/Documentos/ME810/HELENA/projectHelena/")

source("funcoes.R")

controle <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "controle")
t1 <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.006")
t2 <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.0125")
t3 <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.025")
t4 <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.05")
t5 <- read_excel("./dados/DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.1")

controle <- as.data.frame(controle)
t1 <- as.data.frame(t1)
t2 <- as.data.frame(t2)
t3 <- as.data.frame(t3)
t4 <- as.data.frame(t4)
t5 <- as.data.frame(t5)

#Substitui os caracteres "Morta" por zero. Pretende-se assim, diminuir a perda de informação.

for(i in 2:10){
  for(j in 1:10){
    if(controle[j,i] == "Morta"){
      controle[j,i] = NA
    }
  }
}

for(i in 2:10){
  for(j in 1:10){
    if(t1[j,i] == "Morta"){
      t1[j,i] = NA
    }
  }
}

for(i in 2:10){
  for(j in 1:10){
    if(t2[j,i] == "Morta"){
      t2[j,i] = NA
    }
  }
}

for(i in 2:10){
  for(j in 1:10){
    if(t3[j,i] == "Morta"){
      t3[j,i] = NA
    }
  }
}

for(i in 2:10){
  for(j in 1:10){
    if(t4[j,i] == "Morta"){
      t4[j,i] = NA
    }
  }
}

for(i in 2:10){
  for(j in 1:10){
    if(t5[j,i] == "Morta"){
      t5[j,i] = NA
    }
  }
}

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