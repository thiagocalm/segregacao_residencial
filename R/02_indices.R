options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)

# 2000 ---------------------------------------------------------
# Importacao de funcoes externas
source("./R/X_funcao_indices.R")

## Importacao dos dados

ano <- 2000

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_tratado_",ano,"_",RM,".RData")))

    # exportacao
    assign(paste0("censo_",ano,"_",RM),censo)

    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM,"!!!"))
    rm(censo)
    gc()
  }
}

## Calculo dos indices

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]

    print(paste0("Começando a RM: ",RM,"!!!"))

    # Importacao dos dados
    censo <- get(glue::glue("censo_{ano}_{RM}"))

    # Calcula D
    D <- func_calcula_dissimilaridade(censo)
    assign(paste0("D_",ano,"_",RM),D)

    print(paste0("Finalizamos o cáculo do D para RM: ",RM,"!!!"))

    # Calcula QL
    QL <- func_calcula_quociente_locacional(censo)
    assign(paste0("QL_",ano,"_",RM),QL)

    print(paste0("Finalizamos o cáculo do QL para RM: ",RM,"!!!"))


    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM,"!!!"))
    rm(censo, D, QL)
    gc()
  }
}
rm(censo_2000_RMSalvador,censo_2000_RMFortaleza,censo_2000_RMBH,censo_2000_RMRecife,
   censo_2000_RMCuritiba,censo_2000_RMRJ,censo_2000_RMPortoAlegre,censo_2000_RMCampinas,
   censo_2000_RMSP)

# 2010 ---------------------------------------------------------

## Importacao dos dados
source("./R/X_funcao_indices.R")

ano <- 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_tratado_",ano,"_",RM,".RData")))

    # exportacao
    assign(paste0("censo_",ano,"_",RM),censo)

    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM,"!!!"))
    rm(censo)
    gc()
  }
}

## Calculo dos indices

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]

    print(paste0("Começando a RM: ",RM,"!!!"))

    # Importacao dos dados
    censo <- get(glue::glue("censo_{ano}_{RM}"))

    # Calcula D
    D <- func_calcula_dissimilaridade(censo)
    assign(paste0("D_",ano,"_",RM),D)

    print(paste0("Finalizamos o cáculo do D para RM: ",RM,"!!!"))

    # Calcula QL
    QL <- func_calcula_quociente_locacional(censo)
    assign(paste0("QL_",ano,"_",RM),QL)

    print(paste0("Finalizamos o cáculo do QL para RM: ",RM,"!!!"))


    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM,"!!!"))
    rm(censo, D, QL)
    gc()
  }
}

rm(censo_2010_RMSalvador,censo_2010_RMFortaleza,censo_2010_RMBH,censo_2010_RMRecife,
   censo_2010_RMCuritiba,censo_2010_RMRJ,censo_2010_RMPortoAlegre,censo_2010_RMCampinas,
   censo_2010_RMSP)


# Salvando outputs --------------------------------------------------------

resultados_D_index_2000 <- list(
  D_2000_RMBH,D_2000_RMCampinas,D_2000_RMCuritiba, D_2000_RMFortaleza,
  D_2000_RMPortoAlegre, D_2000_RMRecife, D_2000_RMRJ, D_2000_RMSalvador,D_2000_RMSP
)

resultados_D_index_2010 <- list(
  D_2010_RMBH,D_2010_RMCampinas,D_2010_RMCuritiba, D_2010_RMFortaleza,
  D_2010_RMPortoAlegre, D_2010_RMRecife, D_2010_RMRJ, D_2010_RMSalvador,D_2010_RMSP
)

resultados_QL_index_2000 <- list(
  QL_2000_RMBH,QL_2000_RMCampinas,QL_2000_RMCuritiba, QL_2000_RMFortaleza,
  QL_2000_RMPortoAlegre, QL_2000_RMRecife, QL_2000_RMRJ, QL_2000_RMSalvador,QL_2000_RMSP
)

resultados_QL_index_2010 <- list(
  QL_2010_RMBH,QL_2010_RMCampinas,QL_2010_RMCuritiba, QL_2010_RMFortaleza,
  QL_2010_RMPortoAlegre, QL_2010_RMRecife, QL_2010_RMRJ, QL_2010_RMSalvador,QL_2010_RMSP
)

save(resultados_D_index_2000, file = "./output/resultados_D_indice_2000.RData")
save(resultados_D_index_2010, file = "./output/resultados_D_indice_2010.RData")
save(resultados_QL_index_2000, file = "./output/resultados_QL_indice_2000.RData")
save(resultados_QL_index_2010, file = "./output/resultados_QL_indice_2010.RData")
