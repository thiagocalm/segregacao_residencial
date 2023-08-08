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

# Indice de dissimilaridade -----------------------------------------------

# # Preparo da base de dados
#
# df <- censo_2000_RMPortoAlegre |>
#   mutate(classe_raca = case_when(
#     estratos_sociais_egp == 0 & cor_raca == 1 ~ "Brancos baixo",
#     estratos_sociais_egp == 1 & cor_raca == 1 ~ "Brancos intermediário",
#     estratos_sociais_egp == 2 & cor_raca == 1 ~ "Brancos alto",
#     estratos_sociais_egp == 0 & cor_raca == 2 ~ "Negros baixo",
#     estratos_sociais_egp == 1 & cor_raca == 2 ~ "Negros intermediário",
#     estratos_sociais_egp == 2 & cor_raca == 2 ~ "Negros alto",
#     TRUE ~ "Resto da população"
#   )) |>
#   filter(idade >= 10 & PO == 1) |>
#   as_survey_design(ids = id_pes, weights = peso)
#
# tabela <- df |>
#   summarise(cor_raca = 0,
#             area_ponderacao = 0,
#             n = survey_total(na.rm = T)) |>
#   bind_rows(
#     df |>
#       filter(cor_raca %in% c(1,2)) |>
#       group_by(cor_raca) |>
#       summarise(
#         area_ponderacao = 0,
#         n = survey_total(na.rm = T))
#       ) |>
#   bind_rows(
#     df |>
#       group_by(area_ponderacao) |>
#       summarise(
#         cor_raca = 0,
#         n = survey_total(na.rm = T))
#   ) |>
#   bind_rows(
#     df |>
#       filter(cor_raca %in% c(1,2)) |>
#       group_by(cor_raca,area_ponderacao) |>
#       summarise(
#         n = survey_total(na.rm = T))
#   )
#
# # Calculo do indice
#
# tabela |>
#   select(-n_se) |>
#   mutate(pop_branca = n[cor_raca == 1 & area_ponderacao == 0],
#          pop_negra = n[cor_raca == 2 & area_ponderacao == 0]) |>
#   filter(cor_raca != 0 & area_ponderacao != 0) |>
#   pivot_wider(names_from = cor_raca, values_from = n) |>
#   mutate(ratio_branca = `1`/pop_branca,
#          ratio_negra = `2`/pop_negra,
#          ratio_dif = abs(ratio_branca - ratio_negra)) |>
#   mutate(across(starts_with("ratio_"), ~ replace_na(.x, 0))) |>
#   summarise(D = sum(ratio_dif)*.5)
#
# ## Calculo do Indice de dissimilaridade por grupo
#
# tabela <- df |>
#   group_by(classe_raca) |>
#   summarise(area_ponderacao = 0,
#             n = survey_total(na.rm = T)) |>
#   bind_rows(
#     df |>
#       group_by(classe_raca, area_ponderacao) |>
#       summarise(
#         n = survey_total(na.rm = T))
#   )
#
# # Calculo do indice
#
# tabela_indice_classe <- tabela |>
#   select(-n_se) |>
#   mutate(pop_branca_baixo = n[classe_raca == "Brancos baixo" & area_ponderacao == 0],
#          pop_branca_intermediario = n[classe_raca == "Brancos intermediário" & area_ponderacao == 0],
#          pop_branca_alto = n[classe_raca == "Brancos alto" & area_ponderacao == 0],
#          pop_negra_baixo = n[classe_raca == "Negros baixo" & area_ponderacao == 0],
#          pop_negra_intermediario = n[classe_raca == "Negros intermediário" & area_ponderacao == 0],
#          pop_negra_alto = n[classe_raca == "Negros alto" & area_ponderacao == 0],
#          pop_resto = n[classe_raca == "Resto da população" & area_ponderacao == 0]) |>
#   pivot_wider(names_from = classe_raca, values_from = n) |>
#   filter(area_ponderacao != 0) |>
#   mutate(
#     # Razoes de cada grupo
#     ratio_Brancos_Alto = `Brancos alto`/pop_branca_alto,
#     ratio_Brancos_Intermediario = `Brancos intermediário`/pop_branca_intermediario,
#     ratio_Brancos_Baixo = `Brancos baixo`/pop_branca_baixo,
#     ratio_Negros_Alto = `Negros alto`/pop_negra_alto,
#     ratio_Negros_Intermediario = `Negros intermediário`/pop_negra_intermediario,
#     ratio_Negros_Baixo = `Negros baixo`/pop_negra_baixo,
#     ratio_Resto_Pop = `Resto da população`/pop_resto,
#     ## Ratio diff
#
#     # Brancos
#     dif_branco_baixo_branco_alto = abs(ratio_Brancos_Baixo - ratio_Brancos_Alto),
#     dif_branco_intermediario_branco_alto = abs(ratio_Brancos_Intermediario - ratio_Brancos_Alto),
#     dif_negro_baixo_branco_alto = abs(ratio_Negros_Baixo - ratio_Brancos_Alto),
#     dif_negro_intermediario_branco_alto = abs(ratio_Negros_Intermediario - ratio_Brancos_Alto),
#     dif_negro_alto_branco_alto = abs(ratio_Negros_Alto - ratio_Brancos_Alto),
#     dif_resto_branco_alto = abs(ratio_Resto_Pop - ratio_Brancos_Alto),
#
#     dif_branco_intermediario_branco_baixo = abs(ratio_Brancos_Intermediario - ratio_Brancos_Baixo),
#     dif_negro_baixo_branco_baixo = abs(ratio_Negros_Baixo - ratio_Brancos_Baixo),
#     dif_negro_intermediario_branco_baixo = abs(ratio_Negros_Intermediario - ratio_Brancos_Baixo),
#     dif_negro_alto_branco_baixo = abs(ratio_Negros_Alto - ratio_Brancos_Baixo),
#     dif_resto_branco_baixo = abs(ratio_Resto_Pop - ratio_Brancos_Baixo),
#
#     dif_negro_baixo_branco_intermediario = abs(ratio_Negros_Baixo - ratio_Brancos_Intermediario),
#     dif_negro_intermediario_branco_intermediario = abs(ratio_Negros_Intermediario - ratio_Brancos_Intermediario),
#     dif_negro_alto_branco_intermediario = abs(ratio_Negros_Alto - ratio_Brancos_Intermediario),
#     dif_resto_branco_intermediario = abs(ratio_Resto_Pop - ratio_Brancos_Intermediario),
#
#     # Negros
#     dif_negro_intermediario_negro_baixo = abs(ratio_Negros_Intermediario - ratio_Negros_Baixo),
#     dif_negro_alto_negro_baixo = abs(ratio_Negros_Alto - ratio_Negros_Baixo),
#     dif_resto_negro_baixo = abs(ratio_Resto_Pop - ratio_Negros_Baixo),
#     dif_negro_alto_negro_intermediario = abs(ratio_Negros_Alto - ratio_Negros_Intermediario),
#     dif_resto_negro_intermediario = abs(ratio_Resto_Pop - ratio_Negros_Intermediario),
#     dif_resto_negro_alto = abs(ratio_Resto_Pop - ratio_Brancos_Intermediario)
#   ) |>
#   mutate(across(starts_with("dif_"), ~ replace_na(.x, 0))) |>
#   summarise(
#     D_negro_alto_branco_alto = sum(dif_negro_alto_branco_alto)*.5,
#     D_negro_alto_branco_intermediario = sum(dif_negro_alto_branco_intermediario)*.5,
#     D_negro_alto_branco_baixo = sum(dif_negro_alto_branco_baixo)*.5,
#     D_negro_intermediario_branco_alto = sum(dif_negro_intermediario_branco_alto)*.5,
#     D_negro_intermediario_branco_intermediario = sum(dif_negro_intermediario_branco_intermediario)*.5,
#     D_negro_intermediario_branco_baixo = sum(dif_negro_intermediario_branco_baixo)*.5,
#     D_negro_baixo_branco_alto = sum(dif_negro_baixo_branco_alto)*.5,
#     D_negro_baixo_branco_intermediario = sum(dif_negro_baixo_branco_intermediario)*.5,
#     D_negro_baixo_branco_baixo = sum(dif_negro_baixo_branco_baixo)*.5,
#     D_negro_baixo_negro_alto = sum(dif_negro_alto_negro_baixo)*.5,
#     D_negro_baixo_negro_intermediario = sum(dif_negro_intermediario_negro_baixo)*.5,
#     D_negro_alto_negro_intermediario = sum(dif_negro_alto_negro_intermediario)*.5,
#     D_branco_baixo_branco_intermediario = sum(dif_branco_intermediario_branco_baixo)*.5,
#     D_branco_baixo_branco_alto = sum(dif_branco_baixo_branco_alto)*.5,
#     D_branco_alto_branco_intermediario = sum(dif_branco_intermediario_branco_alto)*.5,
#     D_negro_alto_resto = sum(dif_resto_negro_alto)*.5,
#     D_negro_intermediario_resto = sum(dif_resto_negro_intermediario)*.5,
#     D_negro_baixo_resto = sum(dif_resto_negro_baixo)*.5,
#     D_branco_alto_resto = sum(dif_resto_branco_alto)*.5,
#     D_branco_intermediario_resto = sum(dif_resto_branco_intermediario)*.5,
#     D_branco_baixo_resto = sum(dif_resto_branco_baixo)*.5
#     ) |>
#   pivot_longer(D_negro_alto_branco_alto:D_branco_baixo_resto, names_to = "grupo", values_to = "D") |>
#   mutate(
#     cor_classe1 = c(rep("Negros alto",3),rep("Negros intermediário",3),rep("Negros baixo",5),
#                     rep("Negros alto",1),rep("Brancos baixo",2),rep("Brancos alto",1),
#                     "Negros alto","Negros intermediário","Negro baixo","Branco alto",
#                     "Branco intermediário","Branco baixo"),
#     cor_classe2 = c(rep(c("Branco alto","Branco intermediário","Branco baixo"),3), "Negro alto",
#                     rep("Negro intermediário",2),"Branco intermediário","Branco alto",
#                     "Branco intermediário",rep("Resto da população",6))
#     )
#
# tabela_indice_classe_summary <- ftable(xtabs(D ~ cor_classe1 + cor_classe2,
#              tabela_indice_classe),
#        col.vars = c("cor_classe1"),
#        row.vars = c("cor_classe2")) |>
#   stats:::format.ftable(quote = FALSE, dec = ",") |>
#   trimws() |>
#   as.data.frame()


# Indice Quiciente locacional ---------------------------------------------
#
# # Geral
#
# tabela <- df |>
#   group_by(cor_raca) |>
#   summarise(area_ponderacao = 0,
#             prop = survey_mean(na.rm = T)) |>
#   bind_rows(
#     df |>
#       group_by(area_ponderacao, cor_raca) |>
#       summarise(prop = survey_mean(na.rm = T))
#   ) |>
#   select(-prop_se)
#
#
# # Calculo do indice
#
# tabela_QL_raca <- tabela |>
#   mutate(prop_branca = prop[cor_raca == 1 & area_ponderacao == 0],
#          prop_negra = prop[cor_raca == 2 & area_ponderacao == 0],
#          prop_resto = prop[cor_raca == 0 & area_ponderacao == 0]) |>
#   filter(area_ponderacao != 0) |>
#   pivot_wider(names_from = cor_raca, values_from = prop) |>
#   mutate(across(c(`0`,`1`,`2`), ~ replace_na(.x, 0))) |>
#   mutate(QL_branca = `1`/prop_branca,
#          QL_negra = `2`/prop_negra,
#          QL_resto = `0`/prop_resto)
#
# tabela_QL_raca_sintese <- tabela_QL_raca |>
#   summarise(
#     mean_QL_branca = mean(QL_branca),
#     mean_QL_negra = mean(QL_negra),
#     mean_QL_resto = mean(QL_resto),
#     desv_pad_QL_branca = sd(QL_branca),
#     desv_pad_QL_negra = sd(QL_negra),
#     desv_pad_QL_resto = sd(QL_resto),
#   )
#
# ## Por classe e raca
#
# tabela <- df |>
#   select(classe_raca) |>
#   group_by(classe_raca) |>
#   summarise(area_ponderacao = 0,
#             prop = survey_mean(na.rm = T)) |>
#   bind_rows(
#     df |>
#       select(classe_raca, area_ponderacao) |>
#       group_by(area_ponderacao, classe_raca) |>
#       summarise(prop = survey_mean(na.rm = T))
#   ) |>
#   select(-prop_se)
#
# # Calculo do indice
#
# tabela_QL_classe <- tabela |>
#   mutate(prop_branca_baixo = prop[classe_raca == "Brancos baixo" & area_ponderacao == 0],
#          pop_branca_intermediario = prop[classe_raca == "Brancos intermediário" & area_ponderacao == 0],
#          pop_branca_alto = prop[classe_raca == "Brancos alto" & area_ponderacao == 0],
#          pop_negra_baixo = prop[classe_raca == "Negros baixo" & area_ponderacao == 0],
#          pop_negra_intermediario = prop[classe_raca == "Negros intermediário" & area_ponderacao == 0],
#          pop_negra_alto = prop[classe_raca == "Negros alto" & area_ponderacao == 0],
#          pop_resto = prop[classe_raca == "Resto da população" & area_ponderacao == 0]) |>
#   pivot_wider(names_from = classe_raca, values_from = prop) |>
#   mutate(across(c(`Resto da população`,`Negros baixo`,`Negros intermediário`,
#                   `Negros alto`,`Brancos baixo`,`Brancos intermediário`,
#                   `Brancos alto`), ~ replace_na(.x, 0))) |>
#   mutate(
#     # Razoes de cada grupo
#     QL_Brancos_Alto = `Brancos alto`/pop_branca_alto,
#     QL_Brancos_Intermediario = `Brancos intermediário`/pop_branca_intermediario,
#     QL_Brancos_Baixo = `Brancos baixo`/pop_branca_baixo,
#     QL_Negros_Alto = `Negros alto`/pop_negra_alto,
#     QL_Negros_Intermediario = `Negros intermediário`/pop_negra_intermediario,
#     QL_Negros_Baixo = `Negros baixo`/pop_negra_baixo,
#     QL_Resto_Pop = `Resto da população`/pop_resto)
#
# tabela_QL_classe_sintese <- tabela_QL_classe |>
#   summarise(
#     mean_QL_Brancos_Alto = mean(QL_Brancos_Alto),
#     mean_QL_Brancos_Intermediario = mean(QL_Brancos_Intermediario),
#     mean_QL_Brancos_Baixo = mean(QL_Brancos_Baixo),
#     mean_QL_Negros_Alto = mean(QL_Negros_Alto),
#     mean_QL_Negros_Intermediario = mean(QL_Negros_Intermediario),
#     mean_QL_Negros_Baixo = mean(QL_Negros_Baixo),
#     mean_QL_Resto_Pop = mean(QL_Resto_Pop),
#     desv_pad_QL_Brancos_Alto = sd(QL_Brancos_Alto),
#     desv_pad_QL_Brancos_Intermediario = sd(QL_Brancos_Intermediario),
#     desv_pad_QL_Brancos_Baixo = sd(QL_Brancos_Baixo),
#     desv_pad_QL_Negros_Alto = sd(QL_Negros_Alto),
#     desv_pad_QL_Negros_Intermediario = sd(QL_Negros_Intermediario),
#     desv_pad_QL_Negros_Baixo = sd(QL_Negros_Baixo),
#     desv_pad_QL_Resto_Pop = sd(QL_Resto_Pop)
#   )
