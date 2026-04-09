options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)

# Importacao de dados -----------------------------------------------------

## Definicao de parametros de importacao

# anos <- c(2000,2010)
anos <- 2010
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(i in 1: length(anos)){
  ANO = anos[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_tratado_",ANO,"_",RM,".RData")))
    # criando dataset
    censo <- censo |>
      mutate(rm = RM, ano = ANO) |>
      select(ano, rm, everything())
    # atribuindo a objeto agregando censos
    if(ANO == 2000){
      if(k == 1){
        censo_2000_RMs <- censo
      } else{
        censo_2000_RMs <- censo_2000_RMs |>
          bind_rows(censo)
      }
    } else{
      if(k == 1){
        censo_2010_RMs <- censo
      } else{
        censo_2010_RMs <- censo_2010_RMs |>
          bind_rows(censo)
      }
    }
    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM," para o ano ",ANO,"!!!"))
    rm(censo)
    gc()
  }
}

invisible(gc())

# Variaveis derivadas -----------------------------------------------------

###
# Faixas de salario minimo
###

censo_2010_RMs <- censo_2010_RMs |>
  mutate(
    SM_desagregado = case_when(
      renda_pc < 510 * 0.25 ~ 1,
      renda_pc >= 510* 0.25 & renda_pc < 510 * 0.5 ~ 2,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 0.75 ~ 3,
      renda_pc >= 510 * 0.75 & renda_pc < 510 * 1 ~ 4,
      renda_pc >= 510 & renda_pc < 510 * 1.25 ~ 5,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 1.5 ~ 6,
      renda_pc >= 510 * 1.5 & renda_pc < 510 * 1.75 ~ 7,
      renda_pc >= 510 * 1.75 & renda_pc < 510 * 2 ~ 8,
      renda_pc >= 510 * 2 & renda_pc < 510 * 2.25 ~ 9,
      renda_pc >= 510 * 2.25 & renda_pc < 510 * 2.5 ~ 10,
      renda_pc >= 510 * 2.5 & renda_pc < 510 * 2.75 ~ 11,
      renda_pc >= 510 * 2.75 & renda_pc < 510 * 3 ~ 12,
      renda_pc >= 510 * 3 & renda_pc < 510 * 3.25 ~ 13,
      renda_pc >= 510 * 3.25 & renda_pc < 510 * 3.5 ~ 14,
      renda_pc >= 510 * 3.5 & renda_pc < 510 * 3.75 ~ 15,
      renda_pc >= 510 * 3.75 & renda_pc < 510 * 4 ~ 16,
      TRUE ~ 17
    ),
    SM_NE = case_when(
      renda_pc < 510 * 0.25 ~ 1,
      renda_pc >= 510 * 0.25 & renda_pc < 510 * 0.5 ~ 2,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 0.75 ~ 3,
      renda_pc >= 510 * 0.75 & renda_pc < 510 * 1.25 ~ 4,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 4 ~ 5,
      TRUE ~ 6
    ),
    SM_SUL = case_when(
      renda_pc < 510 * 0.5 ~ 1,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 1.25 ~ 2,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 2 ~ 3,
      renda_pc >= 510 * 2 & renda_pc < 510 * 4 ~ 4,
      TRUE ~ 5
    )
  )


# Selecao de RMs ----------------------------------------------------------

censo_2010_RMs <- censo_2010_RMs |>
  filter(
    rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")
  )


# Table 1 - distribuicao da categoria criada por par de rms ---------------

###
# Nordeste
###

t1_NE <- censo_2010_RMs |>
  filter(
    rm %in% c("RMFortaleza","RMRecife")
  ) |>
  summarise(
    raca = 0,
    n = sum(peso),
    .by = c(ano, rm, SM_NE)
  ) |>
  mutate(
    perc = round(n / sum(n) * 100, 2),
    .by = c(ano, rm, raca)
  ) |>
  select(ano, rm, raca, everything()) |>
  bind_rows(
    censo_2010_RMs |>
      filter(
        rm %in% c("RMFortaleza","RMRecife")
      ) |>
      filter(cor_raca_d != 0) |>
      summarise(
        n = sum(peso),
        .by = c(ano, rm, cor_raca_d, SM_NE)
      ) |>
      mutate(
        perc = round(n / sum(n) * 100, 2),
        .by = c(ano, rm, cor_raca_d)
      ) |>
      select(ano, rm, "raca" = cor_raca_d, everything())
  ) |>
  arrange(ano, rm, raca, SM_NE)

# ajustes de tabela

t1_NE <- t1_NE |>
  mutate(
    raca = factor(
      raca,
      levels = c(0,1,2,4),
      labels = c("Total","Branco","Preto","Pardo")
    ),
    SM_NE = factor(
      SM_NE,
      levels = seq(1L,6L),
      labels = c(
        "[0 a 0,25 SM)", "[0,25 a 0,5 SM)",  "[0,5 a 0,75 SM)",
        "[0,75 a 1,25 SM)", "[1,25 a 4 SM)","[4 SM+"
      )
    )
  ) |>
  select(-n, -ano) |>
  pivot_wider(
    names_from = raca,
    values_from = perc
  )

###
# Sul
###

t1_SUL <- censo_2010_RMs |>
  filter(
    rm %in% c("RMCuritiba","RMPortoAlegre")
  ) |>
  summarise(
    raca = 0,
    n = sum(peso),
    .by = c(ano, rm, SM_SUL)
  ) |>
  mutate(
    perc = round(n / sum(n) * 100, 2),
    .by = c(ano, rm, raca)
  ) |>
  select(ano, rm, raca, everything()) |>
  bind_rows(
    censo_2010_RMs |>
      filter(
        rm %in% c("RMCuritiba","RMPortoAlegre")
      ) |>
      filter(cor_raca_d != 0) |>
      summarise(
        n = sum(peso),
        .by = c(ano, rm, cor_raca_d, SM_SUL)
      ) |>
      mutate(
        perc = round(n / sum(n) * 100, 2),
        .by = c(ano, rm, cor_raca_d)
      ) |>
      select(ano, rm, "raca" = cor_raca_d, everything())
  ) |>
  arrange(ano, rm, raca, SM_SUL)

# ajustes de tabela

t1_SUL <- t1_SUL |>
  mutate(
    raca = factor(
      raca,
      levels = c(0,1,2,4),
      labels = c("Total","Branco","Preto","Pardo")
    ),
    SM_SUL = factor(
      SM_SUL,
      levels = seq(1L,5L),
      labels = c(
        "[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 2 SM)",
        "[2 a 4 SM)", "[4 SM+"
      )
    )
  ) |>
  select(-n, -ano) |>
  pivot_wider(
    names_from = raca,
    values_from = perc
  )


# Calculo do D ------------------------------------------------------------


