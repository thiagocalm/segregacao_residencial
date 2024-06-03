options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)

# 2000 --------------------------------------------------------------------

## Importacao dos dados

ano = "2000"
UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP")

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

censo_2000_RMSalvador <- censo_2000_BA |> filter(rm == 7)
censo_2000_RMFortaleza <- censo_2000_CE |> filter(rm == 3)
censo_2000_RMBH <- censo_2000_MG |> filter(rm == 8)
censo_2000_RMRecife <- censo_2000_PE |> filter(rm == 5)
censo_2000_RMCuritiba <- censo_2000_PR |> filter(rm == 17)
censo_2000_RMRJ <- censo_2000_RJ |> filter(rm == 13)
censo_2000_RMPortoAlegre <- censo_2000_RS |> filter(rm == 26)
censo_2000_RMCampinas <- censo_2000_SP |> filter(rm == 16)
censo_2000_RMSP <- censo_2000_SP |> filter(rm == 14)

rm(censo_2000_BA, censo_2000_CE, censo_2000_MG, censo_2000_PE, censo_2000_PR,
   censo_2000_RJ, censo_2000_RS, censo_2000_SP)


# 0 - Distribuicao por cor ou raca ----------------------------------------

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_egp_rm <- df_rm |>
      filter(idade >= 10) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        estratos_sociais = 3,
        n = survey_total()
      ) |>
      bind_rows(
        df_rm |>
          filter(idade >= 10 & PO == 1) |>
          summarise(
            ano = ano,
            cor_raca = 0,
            estratos_sociais = 4,
            n = survey_total()
          )
      ) |>
      bind_rows(
        df_rm |>
          filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
          mutate(
            estratos_sociais = case_when(
              EGP11 %in% c(5, 1) ~ 2,
              EGP11 %in% c(2, 8, 3) ~ 1,
              EGP11 %in% c(4, 9, 10) ~ 0)
          ) |>
          group_by(estratos_sociais) |>
          summarise(
            ano = ano,
            cor_raca = 0,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10) |>
          group_by(cor_raca) |>
          summarise(
            ano = ano,
            estratos_sociais = 3,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10 & PO == 1) |>
          group_by(cor_raca) |>
          summarise(
            ano = ano,
            estratos_sociais = 4,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
          mutate(
            estratos_sociais = case_when(
              EGP11 %in% c(5, 1) ~ 2,
              EGP11 %in% c(2, 8, 3) ~ 1,
              EGP11 %in% c(4, 9, 10) ~ 0)
          ) |>
          group_by(cor_raca, estratos_sociais) |>
          summarise(
            ano = ano,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      select(-ends_with("_se")) |>
      filter(!is.na(estratos_sociais)) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             ),
             estratos_sociais = factor(
               estratos_sociais,
               levels = c(3,4,0,1,2),
               labels = c("Total","Ocupados acima de 10 anos","Baixo","Intermediário","Superior")
             ))

    # juncao de RMs
    if(k == 1){
      tabela_00 <- tabela_egp_rm
    } else{
      tabela_00 <- tabela_00 |>
        bind_rows(tabela_egp_rm)
    }
    rm(tabela_egp_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 1 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## 1 - classes EGP (ordenadas via renda de todas as fontes)

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_egp_rm <- df_rm |>
      group_by(EGP11) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        prop = round(survey_mean(na.rm = T)*100,2),
        inc = survey_mean(v4614_defl, na.rm = T)
      ) |>
      select(-ends_with("_se")) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, EGP11) |>
          summarise(
            ano = ano,
            prop = round(survey_mean(na.rm = T)*100,2),
            inc = 0
          ) |>
          select(-ends_with("_se")) |>
          select(EGP11, ano, cor_raca, prop, inc)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             )) |>
      pivot_wider(names_from = cor_raca, values_from = prop) |>
      mutate(Total = if_else(is.na(Total), 0 , Total),
             Brancos = if_else(is.na(Brancos), 0 , Brancos),
             Negros = if_else(is.na(Negros), 0 , Negros)) |>
      reframe(inc = sum(inc),
              Total = sum(Total),
              Negros = sum(Negros),
              Brancos = sum(Brancos),
              .by = c(ano, RM, EGP11)) |>
      arrange(desc(inc))

    # juncao de RMs
    if(k == 1){
      tabela_01 <- tabela_egp_rm
    } else{
      tabela_01 <- tabela_01 |>
        bind_rows(tabela_egp_rm)
    }
    rm(tabela_egp_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 1 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## 2 - Estratos de renda ordenados

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_inc_rm <- df_rm |>
      group_by(estrato_renda) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        prop = round(survey_mean(na.rm = T)*100,2),
        inc_pc_def = survey_mean(renda_pc_def, na.rm = T)
      ) |>
      select(-ends_with("_se")) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estrato_renda) |>
          summarise(
            ano = ano,
            prop = round(survey_mean(na.rm = T)*100,2),
            inc_pc_def = survey_mean(renda_pc_def, na.rm = T)
          ) |>
          select(-ends_with("_se")) |>
          select(estrato_renda, ano, cor_raca, prop, inc_pc_def)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             ))


    # juncao de RMs
    if(k == 1){
      tabela_02 <- tabela_inc_rm
    } else{
      tabela_02 <- tabela_02 |>
        bind_rows(tabela_inc_rm)
    }
    rm(tabela_inc_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 2 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## Estratos sociais via renda e EGP

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

ano = 2000

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    censo <- get(glue::glue("censo_{ano}_{RM}")) |>
      mutate(
        estratos_sociais_renda = case_when(
          estrato_renda <= 2 ~ 0,
          estrato_renda <= 4 ~ 1,
          estrato_renda == 5 ~ 2),
        estratos_sociais_egp = case_when(
          EGP11 %in% c(5, 1) ~ 2,
          EGP11 %in% c(2, 8, 3) ~ 1,
          EGP11 %in% c(4, 9, 10) ~ 0))

    assign(paste0("censo_",ano,"_",RM),censo)
    rm(censo)
    print(paste0("Finalizamos criacao de estratos para o ano ",ano," e RM ",RM,"!!!"))
  }
}

## 3 - Distribuicao das caracteristicas do estrato social segundo as definicoes

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

ano = 2000

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_renda <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    df_egp <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp)) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_estratos_rm <- df_egp |>
      group_by(estratos_sociais_egp) |>
      summarise(
        tipo = "EGP",
        ano = ano,
        cor_raca = 0,
        n = round(survey_total(na.rm = T)*100,0),
        prop = round(survey_mean(na.rm = T)*100,2)
      ) |>
      select(-ends_with("_se")) |>
      select(tipo, estratos_sociais = estratos_sociais_egp, ano, cor_raca, n, prop) |>
      bind_rows(
        df_egp |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_egp) |>
          summarise(
            tipo = "EGP",
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_egp, ano, cor_raca, n, prop)
      ) |>
      bind_rows(
        df_renda |>
          group_by(estratos_sociais_renda) |>
          summarise(
            tipo = "Renda per capita",
            ano = ano,
            cor_raca = 0,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_renda, ano, cor_raca, n, prop)
      ) |>
      bind_rows(
        df_renda |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_renda) |>
          summarise(
            tipo = "Renda per capita",
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_renda, ano, cor_raca, n, prop)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")),
             estratos_sociais = factor(
               estratos_sociais,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")))

    # juncao de RMs
    if(k == 1){
      tabela_03 <- tabela_estratos_rm
    } else{
      tabela_03 <- tabela_03 |>
        bind_rows(tabela_estratos_rm)
    }
    rm(tabela_estratos_rm, df_renda, df_egp)
    gc()
    print(paste0("Finalizamos a Tabela 3 para o ano ",ano," e RM ",RM,"!!!"))
  }
}


## 4 - Tabela cruzada estratos sociais

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

ano = 2000

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_estratos_rm <- df_rm |>
      group_by(estratos_sociais_egp, estratos_sociais_renda) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        n = round(survey_total(na.rm = T)*100,0)
      ) |>
      select(-ends_with("_se")) |>
      select(ano, cor_raca, estratos_sociais_egp, estratos_sociais_renda, n) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_egp, estratos_sociais_renda) |>
          summarise(
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0)
          ) |>
          select(-ends_with("_se")) |>
          select(ano, cor_raca, estratos_sociais_egp, estratos_sociais_renda, n)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")),
             estratos_sociais_egp = factor(
               estratos_sociais_egp,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")),
             estratos_sociais_renda = factor(
               estratos_sociais_renda,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")))

    # juncao de RMs
    if(k == 1){
      tabela_04 <- tabela_estratos_rm
    } else{
      tabela_04 <- tabela_04 |>
        bind_rows(tabela_estratos_rm)
    }
    rm(tabela_estratos_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 4 para o ano ",ano," e RM ",RM,"!!!"))
  }
}

rm(censo_2000_RMBH, censo_2000_RMCampinas, censo_2000_RMCuritiba, censo_2000_RMFortaleza,
   censo_2000_RMPortoAlegre, censo_2000_RMRecife, censo_2000_RMRJ, censo_2000_RMSalvador,
   censo_2000_RMSP)

# 2010 --------------------------------------------------------------------

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

# 0 - Distribuicao por cor ou raca ----------------------------------------

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_egp_rm <- df_rm |>
      filter(idade >= 10) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        estratos_sociais = 3,
        n = survey_total()
      ) |>
      bind_rows(
        df_rm |>
          filter(idade >= 10 & PO == 1) |>
          summarise(
            ano = ano,
            cor_raca = 0,
            estratos_sociais = 4,
            n = survey_total()
          )
      ) |>
      bind_rows(
        df_rm |>
          filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
          mutate(
            estratos_sociais = case_when(
              EGP11 %in% c(5, 1) ~ 2,
              EGP11 %in% c(2, 8, 3) ~ 1,
              EGP11 %in% c(4, 9, 10) ~ 0)
          ) |>
          group_by(estratos_sociais) |>
          summarise(
            ano = ano,
            cor_raca = 0,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10) |>
          group_by(cor_raca) |>
          summarise(
            ano = ano,
            estratos_sociais = 3,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10 & PO == 1) |>
          group_by(cor_raca) |>
          summarise(
            ano = ano,
            estratos_sociais = 4,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
          mutate(
            estratos_sociais = case_when(
              EGP11 %in% c(5, 1) ~ 2,
              EGP11 %in% c(2, 8, 3) ~ 1,
              EGP11 %in% c(4, 9, 10) ~ 0)
          ) |>
          group_by(cor_raca, estratos_sociais) |>
          summarise(
            ano = ano,
            n = survey_total()
          ) |>
          ungroup() |>
          select(ano, cor_raca, estratos_sociais, everything())
      ) |>
      select(-ends_with("_se")) |>
      filter(!is.na(estratos_sociais)) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             ),
             estratos_sociais = factor(
               estratos_sociais,
               levels = c(3,4,0,1,2),
               labels = c("Total","Ocupados acima de 10 anos","Baixo","Intermediário","Superior")
             ))

    # juncao de RMs
    if(k == 1){
      tabela_00_2010 <- tabela_egp_rm
    } else{
      tabela_00_2010 <- tabela_00_2010 |>
        bind_rows(tabela_egp_rm)
    }
    rm(tabela_egp_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 1 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## 1 - classes EGP (ordenadas via renda de todas as fontes)

ano = 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10 & PO == 1 & !is.na(EGP11)) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_egp_rm <- df_rm |>
      group_by(EGP11) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        prop = round(survey_mean(na.rm = T)*100,2),
        inc = survey_mean(v6527_defl, na.rm = T)
      ) |>
      select(-ends_with("_se")) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, EGP11) |>
          summarise(
            ano = ano,
            prop = round(survey_mean(na.rm = T)*100,2),
            inc = 0
          ) |>
          select(-ends_with("_se")) |>
          select(EGP11, ano, cor_raca, prop, inc)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             )) |>
      pivot_wider(names_from = cor_raca, values_from = prop) |>
      mutate(Total = if_else(is.na(Total), 0 , Total),
             Brancos = if_else(is.na(Brancos), 0 , Brancos),
             Negros = if_else(is.na(Negros), 0 , Negros)) |>
      reframe(inc = sum(inc),
              Total = sum(Total),
              Negros = sum(Negros),
              Brancos = sum(Brancos),
              .by = c(ano, RM, EGP11)) |>
      arrange(desc(inc))

    # juncao de RMs
    if(k == 1){
      tabela_01_2010 <- tabela_egp_rm
    } else{
      tabela_01_2010 <- tabela_01_2010 |>
        bind_rows(tabela_egp_rm)
    }
    rm(tabela_egp_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 1 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## 2 - Estratos de renda ordenados

ano = 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_inc_rm <- df_rm |>
      group_by(estrato_renda) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        prop = round(survey_mean(na.rm = T)*100,2),
        inc_pc_def = survey_mean(renda_pc_def, na.rm = T)
      ) |>
      select(-ends_with("_se")) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estrato_renda) |>
          summarise(
            ano = ano,
            prop = round(survey_mean(na.rm = T)*100,2),
            inc_pc_def = survey_mean(renda_pc_def, na.rm = T)
          ) |>
          select(-ends_with("_se")) |>
          select(estrato_renda, ano, cor_raca, prop, inc_pc_def)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")
             ))


    # juncao de RMs
    if(k == 1){
      tabela_02_2010 <- tabela_inc_rm
    } else{
      tabela_02_2010 <- tabela_02_2010 |>
        bind_rows(tabela_inc_rm)
    }
    rm(tabela_inc_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 2 para o ano ",ano,"e RM ",RM,"!!!"))
  }
}

## Estratos sociais via renda e EGP

ano = 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    censo <- get(glue::glue("censo_{ano}_{RM}")) |>
      mutate(
        estratos_sociais_renda = case_when(
          estrato_renda <= 2 ~ 0,
          estrato_renda <= 4 ~ 1,
          estrato_renda == 5 ~ 2),
        estratos_sociais_egp = case_when(
          EGP11 %in% c(5, 1) ~ 2,
          EGP11 %in% c(2, 8, 3) ~ 1,
          EGP11 %in% c(4, 9, 10) ~ 0))

    assign(paste0("censo_",ano,"_",RM),censo)
    rm(censo)
    print(paste0("Finalizamos criacao de estratos para o ano ",ano," e RM ",RM,"!!!"))
  }
}

## 3 - Distribuicao das caracteristicas do estrato social segundo as definicoes

ano = 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_renda <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    df_egp <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp)) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_estratos_rm <- df_egp |>
      group_by(estratos_sociais_egp) |>
      summarise(
        tipo = "EGP",
        ano = ano,
        cor_raca = 0,
        n = round(survey_total(na.rm = T)*100,0),
        prop = round(survey_mean(na.rm = T)*100,2)
      ) |>
      select(-ends_with("_se")) |>
      select(tipo, estratos_sociais = estratos_sociais_egp, ano, cor_raca, n, prop) |>
      bind_rows(
        df_egp |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_egp) |>
          summarise(
            tipo = "EGP",
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_egp, ano, cor_raca, n, prop)
      ) |>
      bind_rows(
        df_renda |>
          group_by(estratos_sociais_renda) |>
          summarise(
            tipo = "Renda per capita",
            ano = ano,
            cor_raca = 0,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_renda, ano, cor_raca, n, prop)
      ) |>
      bind_rows(
        df_renda |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_renda) |>
          summarise(
            tipo = "Renda per capita",
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0),
            prop = round(survey_mean(na.rm = T)*100,2)
          ) |>
          select(-ends_with("_se")) |>
          select(tipo, estratos_sociais = estratos_sociais_renda, ano, cor_raca, n, prop)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")),
             estratos_sociais = factor(
               estratos_sociais,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")))

    # juncao de RMs
    if(k == 1){
      tabela_03_2010 <- tabela_estratos_rm
    } else{
      tabela_03_2010 <- tabela_03_2010 |>
        bind_rows(tabela_estratos_rm)
    }
    rm(tabela_estratos_rm, df_renda, df_egp)
    gc()
    print(paste0("Finalizamos a Tabela 3 para o ano ",ano," e RM ",RM,"!!!"))
  }
}


## 4 - Tabela cruzada estratos sociais

ano = 2010

RMs <- c("RMSalvador","RMFortaleza","RMBH","RMRecife","RMCuritiba","RMRJ",
         "RMPortoAlegre","RMCampinas","RMSP")

for(i in seq_along(ano)){
  ano = ano[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # definindo dados
    df_rm <- get(glue::glue("censo_{ano}_{RM}")) |>
      filter(idade >= 10) |>
      as_survey_design(ids = id_pes, weights = peso)

    # tabela de cada rm
    tabela_estratos_rm <- df_rm |>
      group_by(estratos_sociais_egp, estratos_sociais_renda) |>
      summarise(
        ano = ano,
        cor_raca = 0,
        n = round(survey_total(na.rm = T)*100,0)
      ) |>
      select(-ends_with("_se")) |>
      select(ano, cor_raca, estratos_sociais_egp, estratos_sociais_renda, n) |>
      bind_rows(
        df_rm |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca, estratos_sociais_egp, estratos_sociais_renda) |>
          summarise(
            ano = ano,
            n = round(survey_total(na.rm = T)*100,0)
          ) |>
          select(-ends_with("_se")) |>
          select(ano, cor_raca, estratos_sociais_egp, estratos_sociais_renda, n)
      ) |>
      mutate(RM = RM,
             cor_raca = factor(
               cor_raca,
               levels = c(0,1,2),
               labels = c("Total","Brancos","Negros")),
             estratos_sociais_egp = factor(
               estratos_sociais_egp,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")),
             estratos_sociais_renda = factor(
               estratos_sociais_renda,
               levels = c(0,1,2),
               labels = c("Baixa","Intermediárias","Superiores")))

    # juncao de RMs
    if(k == 1){
      tabela_04_2010 <- tabela_estratos_rm
    } else{
      tabela_04_2010 <- tabela_04_2010 |>
        bind_rows(tabela_estratos_rm)
    }
    rm(tabela_estratos_rm, df_rm)
    gc()
    print(paste0("Finalizamos a Tabela 4 para o ano ",ano," e RM ",RM,"!!!"))
  }
}

# Juncao e exportacao dos dados -------------------------------------------

## Tabela 0

tabela_00_10 <- tabela_00 |>
  mutate(ano = as.double(ano)) |>
  bind_rows(tabela_00_2010)

tabela0 <- tabela_00 |>
  mutate(ano = as.numeric(ano)) |>
  bind_rows(tabela_00_2010) |>
  filter(!is.na(estratos_sociais)) |>
  arrange(ano,cor_raca) |>
  mutate(across(n, ~ round(.,0))) #|>
  # pivot_longer(Total:Brancos, names_to = "cor_raca", values_to = "prop") |>
  # mutate(
  #   EGP11 = factor(
  #     EGP11,
  #     levels = c(1:11),
  #     labels = c("I. Profissionais, nivel alto","II. Profissionais, nivel baixo",
  #                "IIIa. Não manuais, de rotina, nivel alto","IIIb. Não manuais, de rotina, nivel baixo",
  #                "IVa2. Proprietários e empregadores","IVc1. Empregadores rurais","IVc2. Agricultores para consumo",
  #                "V. Técnicos e supervisores do trabalho manual","VI. Manuais qualificados",
  #                "VIIa. Manuais semi ou não qualificados","VIIb. Trabalhadores da agricultura")
  #   )) |>
  # arrange(desc(inc)) |>
  # select(-inc)

tabela0 <- ftable(xtabs(prop ~ ano + RM + EGP11 + cor_raca,
                        tabela0),
                  row.vars = c("ano","EGP11"),
                  col.vars = c("RM","cor_raca")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Montagem da tabela

titulo <- matrix(ncol = dim(tabela1)[2], nrow = 2)
titulo[1,1] <- "Tabela: População urbana residente em RMs selecionadas classificada via EGP11, segundo cor ou raça - Brasil, 2000-2010"

nota <- matrix(ncol = dim(tabela1)[2], nrow = 7)
nota[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
nota[4,1] <- "Nota:"
nota[5,1] <- "1. Foi considerada somente a população residente em área urbana."
nota[6,1] <- "2. Por população negra, entende-se aquelas pessoas autodeclaradas pretas ou pardas."

tabela_export <- rbind(titulo,tabela1, nota)

# Salvando arquivo

write.xlsx(
  tabela_export,
  file = "./output/Tabela - classe e raca.xlsx",
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "tabela 00 - Descritivo geral",
  append = TRUE,
  showNA = FALSE
)

## Tabela 1

tabela1 <- tabela_01 |> mutate(ano = as.numeric(ano)) |>
  bind_rows(tabela_01_2010) |>
  pivot_longer(Total:Brancos, names_to = "cor_raca", values_to = "prop") |>
  mutate(
    EGP11 = factor(
      EGP11,
      levels = c(1:11),
      labels = c("I. Profissionais, nivel alto","II. Profissionais, nivel baixo",
                 "IIIa. Não manuais, de rotina, nivel alto","IIIb. Não manuais, de rotina, nivel baixo",
                 "IVa2. Proprietários e empregadores","IVc1. Empregadores rurais","IVc2. Agricultores para consumo",
                 "V. Técnicos e supervisores do trabalho manual","VI. Manuais qualificados",
                 "VIIa. Manuais semi ou não qualificados","VIIb. Trabalhadores da agricultura")
    )) |>
  arrange(desc(inc)) |>
  select(-inc)

tabela1 <- ftable(xtabs(prop ~ ano + RM + EGP11 + cor_raca,
                             tabela1),
                       row.vars = c("ano","EGP11"),
                       col.vars = c("RM","cor_raca")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Montagem da tabela

titulo <- matrix(ncol = dim(tabela1)[2], nrow = 2)
titulo[1,1] <- "Tabela: População urbana residente em RMs selecionadas classificada via EGP11, segundo cor ou raça - Brasil, 2000-2010"

nota <- matrix(ncol = dim(tabela1)[2], nrow = 7)
nota[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
nota[4,1] <- "Nota:"
nota[5,1] <- "1. Foi considerada somente a população residente em área urbana."
nota[6,1] <- "2. Por população negra, entende-se aquelas pessoas autodeclaradas pretas ou pardas."

tabela_export <- rbind(titulo,tabela1, nota)

# Salvando arquivo

write.xlsx(
  tabela_export,
  file = "./output/Tabela - classe e raca.xlsx",
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "tabela 01 - EGP",
  append = TRUE,
  showNA = FALSE
)

## Tabela 2

tabela2 <- tabela_02 |> mutate(ano = as.numeric(ano)) |>
  bind_rows(tabela_02_2010) |>
  pivot_longer(prop:inc_pc_def, names_to = "medida", values_to = "valores") |>
  mutate(
    medida = factor(
      medida,
      levels = c("prop","inc_pc_def"),
      labels = c("%","Renda per capita")),
    estrato_renda = factor(
      estrato_renda,
      levels = c(1,2,3,4,5),
      labels = c("P20","P40","P60","P80","P100")))

tabela2 <- ftable(xtabs(valores ~ ano + RM + estrato_renda + cor_raca + medida,
                        tabela2),
                  row.vars = c("ano","estrato_renda"),
                  col.vars = c("RM","cor_raca","medida")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Montagem da tabela

titulo <- matrix(ncol = dim(tabela2)[2], nrow = 2)
titulo[1,1] <- "Tabela: População urbana residente em RMs selecionadas classificada via estratos de renda per capita domiciliar de todas as fontes, segundo cor ou raça - Brasil, 2000-2010"

nota <- matrix(ncol = dim(tabela2)[2], nrow = 9)
nota[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
nota[4,1] <- "Nota:"
nota[5,1] <- "1. Foi considerada somente a população residente em área urbana."
nota[6,1] <- "2. Por população negra, entende-se aquelas pessoas autodeclaradas pretas ou pardas."
nota[7,1] <- "3. Para a classificação da renda, utilizou-se a renda domiciliar per capita oriunda de todas as fontes."
nota[8,1] <- "4. A renda foi deflacionada para 01/08/2022 com base na data de referência de cada recenseamento."

tabela_export <- rbind(titulo,tabela2, nota)

# Salvando arquivo

write.xlsx(
  tabela_export,
  file = "./output/Tabela - classe e raca.xlsx",
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "tabela 02 - classes via renda",
  append = TRUE,
  showNA = FALSE
)

## Tabela 3

tabela3 <- tabela_03 |> mutate(ano = as.numeric(ano)) |>
  bind_rows(tabela_03_2010) |>
  pivot_longer(n:prop, names_to = "medida", values_to = "valores") |>
  mutate(
    medida = factor(
      medida,
      levels = c("n","prop"),
      labels = c("N","%")))

tabela3 <- ftable(xtabs(valores ~ ano + RM + estratos_sociais + cor_raca + tipo + medida,
                        tabela3),
                  row.vars = c("ano","tipo","estratos_sociais"),
                  col.vars = c("RM","cor_raca","medida")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Montagem da tabela

titulo <- matrix(ncol = dim(tabela3)[2], nrow = 2)
titulo[1,1] <- "Tabela: Distribuição absoluta e relativa das classes sociais da população residente em RMs selecionadas, segundo classificação via EGP e distribuição da renda domiciliar per capita, segundo cor ou raça - Brasil, 2000-2010"

nota <- matrix(ncol = dim(tabela3)[2], nrow = 11)
nota[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
nota[4,1] <- "Nota:"
nota[5,1] <- "1. Foi considerada somente a população residente em área urbana."
nota[6,1] <- "2. Por população negra, entende-se aquelas pessoas autodeclaradas pretas ou pardas."
nota[7,1] <- "3. Para a classificação da renda, utilizou-se a renda domiciliar per capita oriunda de todas as fontes."
nota[8,1] <- "4. A renda foi deflacionada para 01/08/2022 com base na data de referência de cada recenseamento."
nota[9,1] <- "5. As classes categorizadas via EGP seguiram a classificação feita por você, Danilo, em sua tese."
nota[10,1] <- "6. As classes categorizadas via distribuição da renda seguiram: (i) baixa, até percentil 40; (ii) intermediária, acima do 40 e até 80; (iii) acima do percentil 80."


tabela_export <- rbind(titulo,tabela3, nota)

# Salvando arquivo

write.xlsx(
  tabela_export,
  file = "./output/Tabela - classe e raca.xlsx",
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "tabela 03 - classes egp e renda",
  append = TRUE,
  showNA = FALSE
)

## Tabela 4

tabela4 <- tabela_04 |> mutate(ano = as.numeric(ano)) |>
  bind_rows(tabela_04_2010) |>
  mutate(
    estratos_sociais_egp = case_when(is.na(estratos_sociais_egp) ~ "Valores ausentes",
                                     TRUE ~ estratos_sociais_egp))

tabela4 <- ftable(xtabs(n ~ ano + RM + estratos_sociais_egp + estratos_sociais_renda +
                          cor_raca,
                        tabela4),
                  row.vars = c("ano","estratos_sociais_renda"),
                  col.vars = c("RM","cor_raca","estratos_sociais_egp")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Montagem da tabela

titulo <- matrix(ncol = dim(tabela4)[2], nrow = 2)
titulo[1,1] <- "Tabela: Distribuição absoluta e relativa das classes sociais da população residente em RMs selecionadas via EGP, segundo classificação via distribuição da renda domiciliar per capita, por cor ou raça - Brasil, 2000-2010"

nota <- matrix(ncol = dim(tabela4)[2], nrow = 11)
nota[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
nota[4,1] <- "Nota:"
nota[5,1] <- "1. Foi considerada somente a população residente em área urbana."
nota[6,1] <- "2. Por população negra, entende-se aquelas pessoas autodeclaradas pretas ou pardas."
nota[7,1] <- "3. Para a classificação da renda, utilizou-se a renda domiciliar per capita oriunda de todas as fontes."
nota[8,1] <- "4. A renda foi deflacionada para 01/08/2022 com base na data de referência de cada recenseamento."
nota[9,1] <- "5. As classes categorizadas via EGP seguiram a classificação feita por você, Danilo, em sua tese."
nota[10,1] <- "6. As classes categorizadas via distribuição da renda seguiram: (i) baixa, até percentil 40; (ii) intermediária, acima do 40 e até 80; (iii) acima do percentil 80."


tabela_export <- rbind(titulo,tabela4, nota)

# Salvando arquivo

write.xlsx(
  tabela_export,
  file = "./output/Tabela - classe e raca.xlsx",
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "tabela 04 - classes egp por renda",
  append = TRUE,
  showNA = FALSE
)
