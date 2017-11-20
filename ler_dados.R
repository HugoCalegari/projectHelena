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
data_compact_nano <- as.data.frame(read_excel("/home/hugo/Documentos/ME810/HELENA/projectHelena/dados/nanoparticulas_dadosR.xlsx"))
data_compact_nano$Tratamento <- as.factor(data_compact_nano$Tratamento)
data_compact_nano$Comprimento <- as.numeric(data_compact_nano$Comprimento)
data_compact_nano$Dia <- as.factor(data_compact_nano$Dia)
data_compact_nano$Repeticao <- as.factor(data_compact_nano$Repeticao)

prop.table(table(is.na(data_compact_nano))) #quantidade de NA's geral

data_compact_nano <- na.omit(data_compact_nano)

ggplot(data_compact_nano, aes(Dia, Comprimento)) + 
  geom_boxplot() + geom_point() + theme_classic() +
  ggtitle("Boxplot do comprimento para cada dia", subtitle = "Nanopartículas") 


ggplot(data_compact_nano, aes(Dia, Comprimento, group = Dia)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_point(aes(y=Comprimento, group=Tratamento), position = position_dodge(width=0.75))

ggplot(data_compact_nano, aes(Dia, Comprimento, color = Tratamento)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  geom_point(aes(y=Comprimento, group=Tratamento), position = position_dodge(width=0.75))+
  geom_smooth(aes(x = Dia , y = Comprimento), se = F)


#Tentativa de fazer o perfil médio por dia
lista1 <- list(0,0,0,0,0,0)

for(i in 1:6){
  lista1[[i]] <- rep(0, 9)
  for(j in 1:9){
   lista1[[i]][j] <- mean( as.data.frame(data_compact_nano %>% filter(Tratamento == paste0(i-1) & Dia==paste0(j-1)))$Comprimento )
  }
}


suporte <- as.data.frame(cbind(rep(0:8, 6), c(lista1[[1]], lista1[[2]], lista1[[3]], lista1[[4]], lista1[[5]], lista1[[6]])))
suporte$V1 <- as.factor(suporte$V1)


plot(suporte$V2[1:9],axes=FALSE,ylim=c(0,0.9),cex.lab=1,xlab="Tempo",ylab="Comprimento médio", main = "Gráfico de perfil médio do comprimento")

axis(2,cex.axis=1)

axis(1,1:9,c("1","2","3","4", "5", "6", "7", "8", "9"),cex.axis=1)


plotCI(suporte$V2[1:9],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 1, cex=1)

lines(suporte$V2[1:9],lwd=1)

plotCI(suporte$V2[10:18],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 2,cex=1)

lines(suporte$V2[10:18],lwd=1, col = 2)

plotCI(suporte$V2[19:27],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 3, cex=1)

lines(suporte$V2[19:27],lwd=1, col = 3)

plotCI(suporte$V2[28:36],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 4,cex=1)

lines(suporte$V2[28:36],lwd=1, col = 4)

plotCI(suporte$V2[37:45],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 5,cex=1)

lines(suporte$V2[37:45],lwd=1, col = 5)

plotCI(suporte$V2[46:54],liw=0,uiw=0,pch=19,add=TRUE,cex.lab=1,slty=1,lwd=2, col = 6,cex=1)

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
library(plotly)
fit <- geeglm(Comprimento ~ Tratamento*Dia, id = Repeticao, data = data_compact_nano)

fit3 <- geeglm(Comprimento ~ Tratamento*Dia, family = Gamma("identity"), id = Repeticao, data = data_compact_nano)

Indice <- c(rep(0, length(fit3$fitted.values)), rep(1, length(fit3$fitted.values)))
sup <- c(data_compact_nano$Comprimento, fit3$fitted.values)
res_fit_true <- as.data.frame(cbind(Indice, sup))
res_fit_true$Indice <- as.factor(res_fit_true$Indice)

# 0 = valores verdadeiros de comprimento e 1 = valores ajustados de comprimento
ggplot(res_fit_true, aes(x = Indice, y = sup, fill = Indice)) + geom_boxplot() + theme_classic() + 
  scale_y_continuous(name = "Valores dos comprimentos") + scale_x_discrete(name = "Indices") + 
  ggtitle("Boxplot dos comprimentos observados e ajustados") + scale_color_discrete(name = "Índice") +
  scale_fill_discrete(name="Indice", breaks=c("0", "1"), labels=c("0 = Observado", "1 = Ajustado"))

envelgama(fit.model = fit3, ligacao = "", 3)


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



#Trabalhar com o número de ovos somados para cada indivíduo
#Deve-se carregar os dados de novo, poque as linhas com NA's foram retiradas
data_compact_nano <- as.data.frame(read_excel("./dados/nanoparticulas_dadosR.xlsx"))
data_compact_nano$Tratamento <- as.factor(data_compact_nano$Tratamento)
data_compact_nano$Comprimento <- as.numeric(data_compact_nano$Comprimento)
data_compact_nano$Dia <- as.factor(data_compact_nano$Dia)
data_compact_nano$Repeticao <- as.factor(data_compact_nano$Repeticao)

#A soma do número de ovos está localizada no dia 8
num_sum_ovo <- as.data.frame(data_compact_nano %>% filter(Dia == 8))
ggplot(num_sum_ovo, aes(x = Repeticao, y = Numero_ovos_somados, group = Tratamento)) + geom_line(aes(linetype = Tratamento))+
 geom_point(aes(shape = Tratamento))

#par(mfrow=c(1,2))
Ps <- fitdistr(num_sum_ovo$Numero_ovos_somados, "Poisson")
qqp(num_sum_ovo$Numero_ovos_somados, "pois", Ps$estimate)

BN <- fitdistr(num_sum_ovo$Numero_ovos_somados, "Negative Binomial")
qqp(num_sum_ovo$Numero_ovos_somados, "nbinom", size = BN$estimate[[1]], mu = BN$estimate[[2]])

#A soma do número de neonatos está localizada no dia 8
#Usar a variável num_sum_ovo no campo Num_neo_somados

plot(num_sum_ovo$Num_neo_somados)

par(mfrow=c(2,1))
Ps_neo <- fitdistr(num_sum_ovo$Num_neo_somados, "Poisson")
qqp(num_sum_ovo$Num_neo_somados, "pois", Ps_neo$estimate)

BN_neo <- fitdistr(num_sum_ovo$Num_neo_somados, "Negative Binomial")
qqp(num_sum_ovo$Num_neo_somados, "nbinom", size = BN_neo$estimate[[1]], mu = BN_neo$estimate[[2]])
#######################################################################################################
#Determinação dos coeficientes angulares para nanopartículas, ao se considerar os valores de comprimentos médios por dia

O objetivo de se obter as diferenças entre os comprimentos médios é encontrar os coeficientes angulares para cada dois pontos consecutivos, e isto implica para dias consecutivos. Com isso, pode-se ter uma ideia de como é a taxa de variação do crescimento para os diferentes tratamentos no decorrer de dias consecutivos.

De fato, matematicamente considerou-se a seguinte ideia: dada uma reta com coeficientes \underline{a} e \underline{b}, $y = a + bx$, foi calculado os diferentes valores de \underline{b} como $b = \frac{\Delta y}{\Delta x}$, dado que os possíveis valores de $y$ são os comprimentos médios e $x$ os dias de 0 até 8. Dessa forma, como leva-se em conta dias consecutivos, tem-se que o valor assumido por $\Delta x = x_{i+1} - x_{i} = 1$ para todos os valores de $i \in \{0, 1, ..., 7\}$, com $x_{0} = 0$, $x_{1} = 1$, ..., $x_{8} = 8$. Logo, temos que $b = \Delta y$.

```{r diferenca_entre_comprimentos_medios,fig.cap="Gráfico que representa as diferenças consecutivas entre os valores dos comprimentos médios. Observe que o eixo x (Dia) representa o dia no qual a diferença foi obtida. Por exemplo, o valor 1 indica a diferença entre os valores dos comprimentos médios do dia 1 em relação ao dia 0; o valor 2 refere-se a diferença entre os valores dos comprimentos médios do dia 2 em relação ao dia 1 e assim para os outros valores. Assim como mencionado anteriormente, essas diferenças representam os coeficientes angulares das retas que passam por dois diferentes pontos do comprimento médio, em dias consecutivos. É possível perceber uma tendência decrescente dos coeficientes para os diferentes tratamentos o que sugere uma possível estabilização do comprimento. Note que somente para o grupo controle o último valor é menor que zero e isto revela, pelo seu valor, que houve dminuição do comprimento médio do dia 7 para o dia 8.", fig.pos='H', fig.align='center', echo=FALSE}
dif_lista1 <- list(0,0,0,0,0,0)

for(i in 1:length(dif_lista1)){
  dif_lista1[[i]] <- diff(lista1[[i]])
}

suport_dif <- as.data.frame(cbind(rep(1:8, 6), c(dif_lista1[[1]], dif_lista1[[2]], dif_lista1[[3]], dif_lista1[[4]], dif_lista1[[5]], dif_lista1[[6]])))

suport_dif$V1 <- as.factor(suport_dif$V1)

contr_dif <- t(t(rep(0, 8)))
t1_dif <- t(t(rep(1, 8)))
t2_dif <- t(t(rep(2, 8)))
t3_dif <- t(t(rep(3, 8)))
t4_dif <- t(t(rep(4, 8)))
t5_dif <- t(t(rep(5, 8)))

t_dif <- as.factor(rbind(contr_dif, t1_dif, t2_dif, t3_dif, t4_dif, t5_dif))

suport_dif <- cbind(t_dif, suport_dif)
colnames(suport_dif) <- c("Tratamento", "Dia", "Diferença consecutiva entre os comprimentos médios")

ggplot(suport_dif, aes(Dia, `Diferença consecutiva entre os comprimentos médios`, group = Tratamento)) + geom_line(aes(linetype = Tratamento)) + geom_point(aes(shape = Tratamento)) + ggtitle("Gráfico da diferença consecutiva entre os comprimentos médios", subtitle = "Nanopartículas") + scale_y_continuous("Coeficiente angular para comprimento médio") + theme_classic()
```

#####################################################################################################
ajuste <- fit$fitted.values
data_compact_nano <- cbind(data_compact_nano, ajuste)
data_compact_nano <- data_compact_nano %>% mutate(dif = Comprimento - ajuste)

ajuste_log <- fit_log$fitted.values
data_compact_nano <- cbind(data_compact_nano, ajuste_log)
data_compact_nano <- data_compact_nano %>% mutate(dif_log = Comprimento - ajuste_log)


f <- rbind(t(t(data_compact_nano$dif)), t(t(data_compact_nano$dif_log)))
t <- rbind(t(t(rep(0, 524))), t(t(rep(1, 524))))

f <- cbind(t, f)
f <- as.data.frame(f)
f$V1 <- as.factor(f$V1)

ggplot(f, aes(V1, V2)) + geom_boxplot(outlier.colour = NA) 


data_compact_nano$dif[which(data_compact_nano$dif != data_compact_nano$dif_log)]
data_compact_nano$dif_log[which(data_compact_nano$dif != data_compact_nano$dif_log)]

f <- rbind(t(t(data_compact_nano$dif[which(data_compact_nano$dif != data_compact_nano$dif_log)])), t(t(data_compact_nano$dif_log[which(data_compact_nano$dif != data_compact_nano$dif_log)])))
t <- rbind(t(t(rep(0, 284))), t(t(rep(1, 284))))

f <- cbind(t, f)
f <- as.data.frame(f)
f$V1 <- as.factor(f$V1)

ggplot(f, aes(V1, V2)) + geom_boxplot() 


lista1 <- list(0,0,0,0,0,0) #media

for(i in 1:6){
  lista1[[i]] <- rep(0, 9)
  for(j in 1:9){
    lista1[[i]][j] <- round(mean( as.data.frame(data_compact_nano %>% filter(Tratamento == paste0(i-1) & Dia==paste0(j-1)))$ajuste), 4)
  }
}


suporte <- as.data.frame(cbind(rep(0:8, 6), c(lista1[[1]], lista1[[2]], lista1[[3]], lista1[[4]], lista1[[5]], lista1[[6]])))
suporte$V1 <- as.factor(suporte$V1)
contr <- t(t(rep(0, 9)))
t1 <- t(t(rep(1, 9)))
t2 <- t(t(rep(2, 9)))
t3 <- t(t(rep(3, 9)))
t4 <- t(t(rep(4, 9)))
t5 <- t(t(rep(5, 9)))

t <- as.factor(rbind(contr, t1, t2, t3, t4, t5))

suporte <- cbind(t, suporte)
colnames(suporte) <- c("Tratamento", "Dia", "Comprimento médio por dia")

#c("0 = controle", "1 = 0.006mg/L", "2 = 0.0125mg/L", "3 = 0.025mg/L", "4 = 0.05mg/L", "5 = 0.1mg/L")
ggplot(suporte, aes(x = Dia, y = `Comprimento médio por dia`, group = Tratamento)) + geom_line(aes(linetype = Tratamento)) + geom_point(aes(shape = Tratamento)) + ggtitle("Gráfico de perfil médio do comprimento", subtitle = "Nanopartículas") + scale_y_continuous("Comprimento médio (mm)") +theme_classic()
