options(scipen = 99999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(SAScii,tidyverse, srvyr, readr)

# Importar funcoes auxiliares ---------------------------------------------

source("./R/X_funcao_importacao.R")

# 2000 -----------------------------------------------------------------

# Importacao e manipulacao dos dados

ano = "2000"
UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    censo <- func_import_handl_data(UF = uf, ano = ano)
    save(censo, file = file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))
    rm(censo)
    gc()
    print(paste0("Finalizamos importação/exportação para: ",uf,"!!"))
  }
}

# 2010 ------------------------------------------------------------------

# Importacao e manipulacao dos dados

ano = "2010"
UF = c(
  # "CE", "PE","BA",
  "PR","RS",
  "RJ","MG","SP2_RM")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    censo <- func_import_handl_data(UF = uf, ano = ano)
    save(censo, file = file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))
    rm(censo)
    gc()
    print(paste0("Finalizamos importação/exportação para: ",uf,"!!"))
  }
}

# for(i in 1: length(ano)){
#   ano = ano[i]
#   for(k in 1: length(UF)){
#     uf = UF[k]
#     censo <- func_import_handl_data(UF = uf, ano = ano)
#     assign(paste0("censo_",ano,"_",uf), censo)
#     rm(censo)
#   }
# }
#
# # Exportacao dos dados
#
# ano = "2010"
# UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP2_RM")
#
# for(i in 1: length(ano)){
#   ano = ano[i]
#   for(k in 1: length(UF)){
#     uf = UF[k]
#     censo <- get(glue::glue("censo_{ano}_{uf}"))
#     save(censo, file = file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))
#   }
#   rm(censo)
# }
