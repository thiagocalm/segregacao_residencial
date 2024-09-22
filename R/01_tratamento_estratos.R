options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr)

# Importacao de funcoes externas ------------------------------------------

source("./R/X_funcao_egp.R")

# 2000 --------------------------------------------------------------------

# Transformacao EGP

ano = "2000"
UF = c("SP","CE", "PE","BA","MG","RJ","PR","RS")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))

    # aplicacao da funcao para conversao
    censo <- func_tratamento_classes_egp(dados = censo)

    ## Tratamento quintil de renda pc
    # Rendimento geral
    censo <- censo |>
      mutate(renda_pc_def = case_when(is.na(v4614_defl) ~ 0, TRUE ~ v4614_defl)) |>
      group_by(id_dom) |>
      mutate(renda_pc_def = sum(renda_pc_def)) |>
      ungroup() |>
      mutate(renda_pc_def = renda_pc_def/n_pes_dom) |>
      mutate(
        estrato_renda = ntile(renda_pc_def, 5),
        decimos_renda_br = ntile(renda_pc_def, 10)
      ) |>
      mutate(peso = p001/10^8) |>
      # Faixas de rendimento em salario minimo
      # OBS: utilizou-se o SM como sendo de 1212, referente a 2022
      mutate(
        estrato_renda_sm = case_when(
          renda_pc_def <= 606 ~ 1,
          renda_pc_def > 606 & renda_pc_def <= 1212 ~ 2,
          renda_pc_def > 1212 & renda_pc_def <= 3636 ~ 3,
          renda_pc_def > 3636 ~ 4
        )
      ) |>
      select(id_dom, id_pes, peso, rm = v1004, municipio = v0103, area_ponderacao = areap,
             idade = v4572, sexo = v0401, anos_estudo = v4300, especie_dom = v0201, situacao_dom = v1006,
             cor_raca, v4614_defl, PEA, PO, PosicaoOcupacao, ISIC, ISCO, EGP11, renda_pc_def,
             estrato_renda,decimos_renda_br, v4513, estrato_renda_sm)

    # Rendimento urbano-rural

    censo <- censo |>
      group_by(situacao_dom) |>
      mutate(
        decimos_renda_situacao = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    # Rendimento metropolitano

    censo <- censo |>
      group_by(rm) |>
      mutate(
        decimos_renda_rm = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    # Rendimento urbano e metropolitano

    censo <- censo |>
      group_by(rm, situacao_dom) |>
      mutate(
        decimos_renda_rm_situacao = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    ## Exportacao
    save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_",uf,".RData")))

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

# 2010 --------------------------------------------------------------------
invisible(gc())

# Transformacao EGP

ano = "2010"
UF = c("SP2_RM","SP1","CE", "PE","BA","MG","RJ","PR","RS")

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))

    # aplicacao da funcao para conversao
    censo <- func_tratamento_classes_egp(
      dados = censo,
      var_cnae_censo = "v6471",
      var_trabalhando = "v6910",
      var_afastado = "v0642",
      var_aprendiz = "v0643",
      var_trabalho_cultivo = "v0652",
      var_trabalho_consumo = "v0644",
      var_buscou_emprego = "v0654",
      var_posicao_ocupacao = "v0648",
      var_codigo_ocupacao = "v6461"
    )

    ## Tratamento quintil de renda pc
    censo <- censo |>
      mutate(renda_pc_def = case_when(is.na(v6527_defl) ~ 0, TRUE ~ v6527_defl)) |>
      group_by(id_dom) |>
      mutate(renda_pc_def = sum(renda_pc_def)) |>
      ungroup() |>
      mutate(renda_pc_def = renda_pc_def/n_pes_dom) |>
      mutate(
        estrato_renda = ntile(renda_pc_def, 5),
        decimos_renda_br = ntile(renda_pc_def, 10)
      ) |>
      mutate(peso = v0010/10^13) |>
      # Faixas de rendimento em salario minimo
      # OBS: utilizou-se o SM como sendo de 1212, referente a 2022
      mutate(
        estrato_renda_sm = case_when(
          renda_pc_def <= 606 ~ 1,
          renda_pc_def > 606 & renda_pc_def <= 1212 ~ 2,
          renda_pc_def > 1212 & renda_pc_def <= 3636 ~ 3,
          renda_pc_def > 3636 ~ 4
        )
      ) |>
      select(id_dom, id_pes, peso, rm = v1004, municipio = v0002, area_ponderacao = v0011,
             idade = v6036, sexo = v0601, anos_estudo = v6400, especie_dom = v4001, situacao_dom = v1006,
             cor_raca, v6527_defl, PEA, PO, PosicaoOcupacao, ISIC, ISCO, EGP11, renda_pc_def,
             estrato_renda,decimos_renda_br, v6513, estrato_renda_sm)

    # Rendimento urbano-rural

    censo <- censo |>
      group_by(situacao_dom) |>
      mutate(
        decimos_renda_situacao = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    # Rendimento metropolitano

    censo <- censo |>
      group_by(rm) |>
      mutate(
        decimos_renda_rm = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    # Rendimento urbano e metropolitano

    censo <- censo |>
      group_by(rm, situacao_dom) |>
      mutate(
        decimos_renda_rm_situacao = ntile(renda_pc_def, 10)
      ) |>
      ungroup()

    ## Exportacao
    if(uf == "SP2_RM"){
      save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_SP.RData")))
    }else{
      save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_",uf,".RData")))
    }

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}


# Exportacao dos dados para as RMs ----------------------------------------

## 2000
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

## 2010
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

# Exportacao
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(i in seq_along(anos)){
  ano = anos[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]
    censo <- get(glue::glue("censo_{ano}_{RM}"))
    save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_",RM,".RData")))
    # Proximo loop
    rm(censo)
    print(paste0("Finalizamos a UF ",RM," para ", ano,"..."))
    invisible(gc())
  }
}
