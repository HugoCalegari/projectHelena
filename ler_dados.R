library(readxl)
library(dplyr)
setwd("Documentos/ME810/HELENA")

source("funcoes.R")
controle <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "controle")
t1 <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.006")
t2 <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.0125")
t3 <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.025")
t4 <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.05")
t5 <- read_excel("DADOS COMPRIMENTO NANO UNICAMP.xlsx", sheet = "t0.1")

teste <- 
  