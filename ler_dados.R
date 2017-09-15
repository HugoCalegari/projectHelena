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
data_compact_nano$Comprimento[which(is.na(data_compact_nano$Comprimento))] <- 0

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


plot(suporte$V2[1:8],axes=FALSE,ylim=c(0,0.9),cex.lab=1,xlab="Tempo",ylab="Comprimento médio")

axis(2,cex.axis=1)

axis(1,1:9,c("0","1","2","3","4", "5", "6", "7", "8"),cex.axis=1)



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
