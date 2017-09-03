muda_titulo <- function(dados){
  colnames(dados) <- c("Réplicas", "Dia 0", "Dia 1", "Dia 2", "Dia 3", "Dia 4", "Dia 5", "Dia 6", "Dia 7", "Dia 8")
}

muda_df <- function(dados){
  dados <- as.data.frame(dados)
}

morta_zero <- function(dados){
  for(i in 2:10){
    for(j in 1:10){
      if(dados[j,i] == "Morta"){
        dados[j,i] = 0
      }
    }
  }
}
