options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)


# Importacao de dados -----------------------------------------------------

RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

## Importacao dos dados

ano = 2010
UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP", "SP1")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_tratado_",ano,"_",uf,".RData")))

    # exportacao
    assign(paste0("censo_",ano,"_",uf),censo)

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

## Restrição dos dados para cada RM

censo_2010_RMSalvador <- censo_2010_BA |> filter(rm == 15)
censo_2010_RMFortaleza <- censo_2010_CE |> filter(rm == 7)
censo_2010_RMBH <- censo_2010_MG |> filter(rm == 16)
censo_2010_RMRecife <- censo_2010_PE |> filter(rm == 11)
censo_2010_RMCuritiba <- censo_2010_PR |> filter(rm == 23)
censo_2010_RMRJ <- censo_2010_RJ |> filter(rm == 19)
censo_2010_RMPortoAlegre <- censo_2010_RS |> filter(rm == 34)
censo_2010_RMCampinas <- censo_2010_SP1 |> filter(rm == 22)
censo_2010_RMSP <- censo_2010_SP |> filter(rm == 20)

rm(censo_2010_BA, censo_2010_CE, censo_2010_MG, censo_2010_PE, censo_2010_PR,
   censo_2010_RJ, censo_2010_RS, censo_2010_SP, censo_2010_SP1)

## Todas as RM juntas

for(i in seq_along(RMs)){
  # parametro
  RM = RMs[i]
  # criando dataset
  df <- get(glue::glue("censo_2010_{RM}")) |>
    mutate(RM = RM, ano = 2010) |>
    select(ano, RM, everything())
  # atribuindo a objeto agregando censos
  if(i == 1){
    censo_2010_RMs <- df
  } else{
    censo_2010_RMs <- censo_2010_RMs |>
      bind_rows(df)
  }
  # next loop
  rm(df)
}

rm(list = ls() |> as_tibble() |> filter(!value %in% c("censo_2010_RMs")) |> pull())
invisible(gc())

