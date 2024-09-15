options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx, patchwork, openxlsx)
source("./R/X_funcao_indices.R") # importando funcoes para usar funcao de fazer tabelas

## Importacao dos dados

anos <- c(2000,2010)
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(i in 1: length(anos)){
  ano = anos[i]

  for(k in 1: length(RMs)){
  RM = RMs[k]

  # Importacao dos dados
  load(file.path("./output/SM",paste0("resultados_QL_indice_",ano,".RData")))
  load(file.path("./output/SM",paste0("resultados_D_indice_",ano,".RData")))

  # Criacao de uma lista para cada RM e ano
  index_total <- list(
    D_geral = get(glue::glue("resultados_D_index_{ano}"))[[k]][[1]],
    D_classes = get(glue::glue("resultados_D_index_{ano}"))[[k]][[2]],
    QL_geral = get(glue::glue("resultados_QL_index_{ano}"))[[k]][[3]],
    QL_classe = get(glue::glue("resultados_QL_index_{ano}"))[[k]][[4]],
    QL_classe_ap = get(glue::glue("resultados_QL_index_{ano}"))[[k]][[2]]
  )

  # Exportacao
  assign(paste0("indices_",ano,"_",RM),index_total)

  # Proximo loop
  rm(index_total)
  }
}

rm(resultados_QL_index_2000,resultados_QL_index_2010, resultados_D_index_2010, resultados_D_index_2000)

# Gerando informações para as tabelas -------------------------------------

for(i in seq_along(anos)){
  ano = anos[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]

    # D - geral

    D_resultado_geral <- get(glue::glue("indices_{ano}_{RM}"))$D_geral

    # D - por classe
    D_resultado_classes <- func_fazer_tabela(
      get(glue::glue("indices_{ano}_{RM}"))$D_classes |>
        bind_rows(
          get(glue::glue("indices_{ano}_{RM}"))$D_classes |>
            dplyr::select(grupo, D, cor_classe1 = cor_classe2, cor_classe2 = cor_classe1)
        ) |>
        distinct() |>
        mutate(
          cor_classe1 = factor(
            cor_classe1,
            levels = c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM",
                       "Negros 3SMmais","Negros 1a3SM","Negros meioa1SM","Negros atemeioSM", "Resto da população"),
            labels = c("Brancos - 3SMmais","Brancos - 1a3SM","Brancos - meioa1SM","Brancos - ate meioSM",
                       "Negros - 3SMmais","Negros - 1a3SM","Negros - meioa1SM","Negros - ate meioSM", "Resto da população")
          ),
          cor_classe2 = factor(
            cor_classe2,
            levels = c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM",
                       "Negros 3SMmais","Negros 1a3SM","Negros meioa1SM","Negros atemeioSM", "Resto da população"),
            labels = c("Brancos - 3SMmais","Brancos - 1a3SM","Brancos - meioa1SM","Brancos - ate meioSM",
                       "Negros - 3SMmais","Negros - 1a3SM","Negros - meioa1SM","Negros - ate meioSM", "Resto da população")
          )
        )
    )

    # QL - geral

    QL_resultado_geral <- get(glue::glue("indices_{ano}_{RM}"))$QL_geral |>
      pivot_longer(
        mean_QL_branca:desv_pad_QL_negra,
        names_to = "tipo",
        values_to = "valor"
      ) |>
      mutate(
        medida = c(rep(c("media"),2),rep(c("desv_pad"),2)),
        raca_cor = rep(c("Branca","Negra"),2)
      ) |>
      pivot_wider(
        names_from = medida,
        values_from = valor
      ) |>
      dplyr::select(-tipo) |>
      mutate(across(-raca_cor, ~ replace_na(.x,0))) |>
      summarise(
        Média = sum(media),
        `Desvio Padrão` = sum(desv_pad),
        .by = raca_cor
      )

    # QL - por classe

    QL_resultado_classe_sintese <- get(glue::glue("indices_{ano}_{RM}"))$QL_classe |>
      pivot_longer(
        mean_QL_Brancos_3SMmais:desv_pad_QL_Negros_meioSM,
        names_to = "tipo",
        values_to = "valor"
      ) |>
      mutate(
        medida = c(rep(c("media"),8),rep(c("desv_pad"),8)),
        raca_cor = rep(c("Brancos 3 SM ou mais","Brancos 1a3 SM","Brancos meio a 1 SM","Brancos ate 1 SM",
                         "Negros 3 SM ou mais","Negros 1a3 SM","Negros meio a 1 SM","Negros ate 1 SM"),2)
      ) |>
      pivot_wider(
        names_from = medida,
        values_from = valor
      ) |>
      dplyr::select(-tipo) |>
      mutate(across(-raca_cor, ~ replace_na(.x,0))) |>
      summarise(
        Média = sum(media),
        `Desvio Padrão` = sum(desv_pad),
        .by = raca_cor
      )

    df <- get(glue::glue("indices_{ano}_{RM}"))$QL_classe_ap |> dplyr::select(starts_with("QL_"))

    names(df) <- substring(names(df), 4)

    QL_resultado_classe_correlacao <- cor(df, method = "pearson")

    output <- list(
      D_resultado_geral = D_resultado_geral,
      D_resultado_classes = D_resultado_classes,
      QL_resultado_geral = QL_resultado_geral,
      QL_resultado_classe_sintese = QL_resultado_classe_sintese,
      QL_resultado_classe_correlacao = QL_resultado_classe_correlacao
    )

    assign(paste0("output_",ano,"_",RM),output)

  }
  print(paste("Finalizamos o ano: ",ano,"..."))
}

# Exportando resultados

wb <- openxlsx::loadWorkbook('./output/SM/tabelas/Tabela - indices de segregação por RM.xlsx')

for(i in seq_along(anos)){
  ano = anos[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]
    sheet_number = 1+k
    if (ano == 2000){
      ## D - Geral
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$D_resultado_geral[[1]],
        xy = c(3,6)
      )
      ## D - Classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$D_resultado_classes[3:10,3:10],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,8)
      )
      ## QL - Summaries - geral
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_geral[,2:3],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,31)
      )
      ## QL - Summaries - por classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_classe_sintese[,2:3],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,34)
      )
      ## QL - correlacoes - por classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_classe_correlacao,
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(7,32)
      )
    } else{
      ## D - Geral
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$D_resultado_geral[[1]],
        xy = c(3,17)
      )
      ## D - Classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$D_resultado_classes[3:10,3:10],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,19)
      )
      ## QL - Summaries - geral
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_geral[,2:3],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,44)
      )
      ## QL - Summaries - por classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_classe_sintese[,2:3],
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(3,47)
      )
      ## QL - correlacoes - por classe
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_number,
        x = get(glue::glue("output_{ano}_{RM}"))$QL_resultado_classe_correlacao,
        colNames = FALSE,
        rowNames = FALSE,
        xy = c(7,45)
      )
    }
  }
}

# exportar tabela
openxlsx::saveWorkbook(
  wb,
  paste0('./output/SM/tabelas/','[Ultima atualizacao em ',today(),'] Tabela - indices de segregação por RM.xlsx'),
  overwrite = TRUE
)
