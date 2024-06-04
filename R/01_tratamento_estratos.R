options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr)

# Importacao de funcoes externas ------------------------------------------

source("./R/X_funcao_egp.R")

# Importacao dos dados ----------------------------------------------------

# Relacao de classificacao CNAE
tabela_cnae <- readxl::read_excel("./docs/tabela_conversao_cbo_cnae.xlsx", 7)
tabela_cnae_ajuste <- readxl::read_excel("./docs/tabela_ajustes_cbo_cnae_2010.xlsx", 1)

# Relacao de classificacao CBO
tabela_cbo <- readxl::read_excel("./docs/tabela_conversao_cbo_cnae.xlsx", 3)
tabela_cbo_ajuste <- readxl::read_excel("./docs/tabela_ajustes_cbo_cnae_2010.xlsx", 2)

# 2000 --------------------------------------------------------------------

# Transformacao EGP

ano = "2000"
UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP")
k = 1
for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))

    # aplicacao da funcao para conversao
    db <- func_tratamento_classes_egp(dados = censo)

    # base com os devidos tratamentos
    censo <- db[[1]]

    # criando arquivo de controle do processo de compatibilizacao CNAE/CBO
    if(k == 1){
      tabela <- db[[2]] |>
        mutate(
          uf = uf,
          ano = ano,
          .before = everything()
        )
    } else{
      tabela <- tabela |>
        bind_rows(
          db[[2]] |>
            mutate(
              uf = uf,
              ano = ano,
              .before = everything()
            )
        )
    }

    # exportacao
    assign(paste0("censo_",ano,"_",uf),censo)

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

# Tratamento quintil de renda pc

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    censo <- get(glue::glue("censo_{ano}_{uf}")) |>
      mutate(renda_pc_def = case_when(is.na(v4614_defl) ~ 0, TRUE ~ v4614_defl)) |>
      group_by(id_dom) |>
      mutate(renda_pc_def = sum(renda_pc_def)) |>
      ungroup() |>
      mutate(renda_pc_def = renda_pc_def/n_pes_dom) |>
      mutate(estrato_renda = ntile(renda_pc_def, 5)) |>
      mutate(peso = p001/10^8) |>
      select(id_dom, id_pes, peso, rm = v1004, municipio = v0103, area_ponderacao = areap,
             idade = v4572, sexo = v0401, anos_estudo = v4300, especie_dom = v0201,
             cor_raca, v4614_defl, PEA, PO, PosicaoOcupacao, ISIC, ISCO, EGP11, renda_pc_def,
             estrato_renda, v4513)

    # exportacao
    assign(paste0("censo_",ano,"_",uf),censo)

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

## Exportacao

# exportacao da base

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    censo <- get(glue::glue("censo_{ano}_{uf}"))
    save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_",uf,".RData")))
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
  }
  rm(censo)
}

# exportacao da tabela de controle de compatibilizacao

write_csv2(tabela, file.path("./output","tabelas","Tabela - controle da compatibilizacao CNAE_CBO.csv"))

# 2010 --------------------------------------------------------------------

# Transformacao EGP

ano = "2010"
UF = c("CE", "PE","BA","MG","RJ","PR","RS","SP2_RM","SP1")
k = 1
for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_",ano,"_",uf,".RData")))

    # aplicacao da funcao para conversao
    db <- func_tratamento_classes_egp(
      dados = censo,
      dados_cnae = tabela_cnae,
      var_cnae_censo = "v6471",
      var_cnae_compatibilizacao = "CNAE_Dom2",
      compatibilizacao_cnae = TRUE,
      var_cbo_compatibilizacao = "CBO2002_4d",
      compatibilizacao_cbo = TRUE,
      construir_var_pea = TRUE,
      var_trabalhando = "v6910",
      var_afastado = "v0652",
      var_aprendiz = "v0643",
      var_trabalho_consumo = "v0644",
      var_buscou_emprego = "v0654",
      var_posicao_ocupacao = "v0648",
      var_codigo_ocupacao = "v6461"
    )

    # base com os devidos tratamentos
    censo <- db[[1]]

    # criando arquivo de controle do processo de compatibilizacao CNAE/CBO
    if(k == 1){
      tabela <- db[[2]] |>
        mutate(
          uf = uf,
          ano = ano,
          .before = everything()
        )
    } else{
      tabela <- tabela |>
        bind_rows(
          db[[2]] |>
            mutate(
              uf = uf,
              ano = ano,
              .before = everything()
            )
        )
    }

    # exportacao
    assign(paste0("censo_",ano,"_",uf),censo)

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

# Tratamento quintil de renda pc

for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    # Importacao dos dados
    censo <- get(glue::glue("censo_{ano}_{uf}")) |>
      mutate(renda_pc_def = case_when(is.na(v6527_defl) ~ 0, TRUE ~ v6527_defl)) |>
      group_by(id_dom) |>
      mutate(renda_pc_def = sum(renda_pc_def)) |>
      ungroup() |>
      mutate(renda_pc_def = renda_pc_def/n_pes_dom) |>
      mutate(estrato_renda = ntile(renda_pc_def, 5)) |>
      mutate(peso = v0010/10^13) |>
      select(id_dom, id_pes, peso, rm = v1004, municipio = v0002, area_ponderacao = v0011,
             idade = v6036, sexo = v0601, anos_estudo = v6400, especie_dom = v4001,
             cor_raca, v6527_defl, PEA, PO, PosicaoOcupacao, ISIC, ISCO, EGP11, renda_pc_def,
             estrato_renda, v6513)

    # exportacao
    assign(paste0("censo_",ano,"_",uf),censo)

    # Proximo loop
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
    rm(censo)
    gc()
  }
}

## Exportacao

# exportacao da base
for(i in 1: length(ano)){
  ano = ano[i]
  for(k in 1: length(UF)){
    uf = UF[k]
    censo <- get(glue::glue("censo_{ano}_{uf}"))
    if(uf == "SP2_RM"){
      save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_SP.RData")))
    }else{
      save(censo, file = file.path("./dados",paste0("censo_tratado_",ano,"_",uf,".RData")))
    }
    print(paste0("Finalizamos a UF: ",uf,"!!!"))
  }
  rm(censo)
}

# exportacao da tabela de controle de compatibilizacao
if(file.exists(file.path("./output","tabelas","Tabela - controle da compatibilizacao CNAE_CBO.csv"))){
  tabela <- read_csv2(
    file.path("./output","tabelas","Tabela - controle da compatibilizacao CNAE_CBO.csv")
  ) |>
    bind_rows(
      tabela |>
        mutate(ano = as.numeric(ano))
    )
  write_csv2(tabela, file.path("./output","tabelas","Tabela - controle da compatibilizacao CNAE_CBO.csv"))
} else{
  write_csv2(tabela, file.path("./output","tabelas","Tabela - controle da compatibilizacao CNAE_CBO.csv"))
}

