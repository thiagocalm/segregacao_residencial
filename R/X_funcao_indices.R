
# Indice de dissimilaridade -----------------------------------------------

func_calcula_dissimilaridade <-
  function(
    data,
    var_estrato = "estratos_sociais_egp",
    cor_raca = "cor_raca",
    area_ponderacao = "area_ponderacao",
    id_pessoa = "id_pes",
    idade = "idade",
    PO = "PO",
    peso = "peso",
    EGP = TRUE,
    por_classe = TRUE){

    # Preparo da base de dados
    # criar base de dados a ser utilizada e diferencial para EGP ou classificacao por estrato de renda

    vars <- c(var_estrato, cor_raca, area_ponderacao, id_pessoa, peso, idade, PO)

    if(EGP == TRUE){
      df <- data |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        filter(idade >= 10 & PO == 1 & !is.na(var_estrato)) |>
        mutate(classe_raca = case_when(
          var_estrato == 0 & cor_raca == 1 ~ "Brancos baixo",
          var_estrato == 1 & cor_raca == 1 ~ "Brancos intermediário",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos alto",
          var_estrato == 0 & cor_raca == 2 ~ "Negros baixo",
          var_estrato == 1 & cor_raca == 2 ~ "Negros intermediário",
          var_estrato == 2 & cor_raca == 2 ~ "Negros alto",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }else{
      df <- data |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        filter(idade >= 10) |>
        mutate(classe_raca = case_when(
          var_estrato == 0 & cor_raca == 1 ~ "Brancos baixo",
          var_estrato == 1 & cor_raca == 1 ~ "Brancos intermediário",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos alto",
          var_estrato == 0 & cor_raca == 2 ~ "Negros baixo",
          var_estrato == 1 & cor_raca == 2 ~ "Negros intermediário",
          var_estrato == 2 & cor_raca == 2 ~ "Negros alto",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pessoa, weights = peso)
    }

    print("Etapa de criação de variável finalizada...")

    # cria tabela de base para calculo
    tabela <- df |>
      summarise(cor_raca = 0,
                area_ponderacao = 0,
                n = survey_total(na.rm = T)) |>
      bind_rows(
        df |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca) |>
          summarise(
            area_ponderacao = 0,
            n = survey_total(na.rm = T))
      ) |>
      bind_rows(
        df |>
          group_by(area_ponderacao) |>
          summarise(
            cor_raca = 0,
            n = survey_total(na.rm = T))
      ) |>
      bind_rows(
        df |>
          filter(cor_raca %in% c(1,2)) |>
          group_by(cor_raca,area_ponderacao) |>
          summarise(
            n = survey_total(na.rm = T))
      )
    if(por_classe == TRUE){
      tabela_por_classe <- df |>
        group_by(classe_raca) |>
        summarise(area_ponderacao = 0,
                  n = survey_total(na.rm = T)) |>
        bind_rows(
          df |>
            group_by(classe_raca, area_ponderacao) |>
            summarise(
              n = survey_total(na.rm = T))
        )
    }

    print("Etapa de construição das tabelas de base finalizada...")

    # calcula o indice
    output_geral <- tabela |>
      select(-n_se) |>
      mutate(pop_branca = n[cor_raca == 1 & area_ponderacao == 0],
             pop_negra = n[cor_raca == 2 & area_ponderacao == 0]) |>
      filter(cor_raca != 0 & area_ponderacao != 0) |>
      pivot_wider(names_from = cor_raca, values_from = n) |>
      mutate(ratio_branca = `1`/pop_branca,
             ratio_negra = `2`/pop_negra,
             ratio_dif = abs(ratio_branca - ratio_negra)) |>
      mutate(across(starts_with("ratio_"), ~ replace_na(.x, 0))) |>
      summarise(D = sum(ratio_dif)*.5)

    output_classe <- tabela_por_classe |>
      select(-n_se) |>
      mutate(pop_branca_baixo = n[classe_raca == "Brancos baixo" & area_ponderacao == 0],
             pop_branca_intermediario = n[classe_raca == "Brancos intermediário" & area_ponderacao == 0],
             pop_branca_alto = n[classe_raca == "Brancos alto" & area_ponderacao == 0],
             pop_negra_baixo = n[classe_raca == "Negros baixo" & area_ponderacao == 0],
             pop_negra_intermediario = n[classe_raca == "Negros intermediário" & area_ponderacao == 0],
             pop_negra_alto = n[classe_raca == "Negros alto" & area_ponderacao == 0],
             pop_resto = n[classe_raca == "Resto da população" & area_ponderacao == 0]) |>
      pivot_wider(names_from = classe_raca, values_from = n) |>
      filter(area_ponderacao != 0) |>
      mutate(
        # Razoes de cada grupo
        ratio_Brancos_Alto = `Brancos alto`/pop_branca_alto,
        ratio_Brancos_Intermediario = `Brancos intermediário`/pop_branca_intermediario,
        ratio_Brancos_Baixo = `Brancos baixo`/pop_branca_baixo,
        ratio_Negros_Alto = `Negros alto`/pop_negra_alto,
        ratio_Negros_Intermediario = `Negros intermediário`/pop_negra_intermediario,
        ratio_Negros_Baixo = `Negros baixo`/pop_negra_baixo,
        ratio_Resto_Pop = `Resto da população`/pop_resto,
        ## Ratio diff

        # Brancos
        dif_branco_baixo_branco_alto = abs(ratio_Brancos_Baixo - ratio_Brancos_Alto),
        dif_branco_intermediario_branco_alto = abs(ratio_Brancos_Intermediario - ratio_Brancos_Alto),
        dif_negro_baixo_branco_alto = abs(ratio_Negros_Baixo - ratio_Brancos_Alto),
        dif_negro_intermediario_branco_alto = abs(ratio_Negros_Intermediario - ratio_Brancos_Alto),
        dif_negro_alto_branco_alto = abs(ratio_Negros_Alto - ratio_Brancos_Alto),
        dif_resto_branco_alto = abs(ratio_Resto_Pop - ratio_Brancos_Alto),

        dif_branco_intermediario_branco_baixo = abs(ratio_Brancos_Intermediario - ratio_Brancos_Baixo),
        dif_negro_baixo_branco_baixo = abs(ratio_Negros_Baixo - ratio_Brancos_Baixo),
        dif_negro_intermediario_branco_baixo = abs(ratio_Negros_Intermediario - ratio_Brancos_Baixo),
        dif_negro_alto_branco_baixo = abs(ratio_Negros_Alto - ratio_Brancos_Baixo),
        dif_resto_branco_baixo = abs(ratio_Resto_Pop - ratio_Brancos_Baixo),

        dif_negro_baixo_branco_intermediario = abs(ratio_Negros_Baixo - ratio_Brancos_Intermediario),
        dif_negro_intermediario_branco_intermediario = abs(ratio_Negros_Intermediario - ratio_Brancos_Intermediario),
        dif_negro_alto_branco_intermediario = abs(ratio_Negros_Alto - ratio_Brancos_Intermediario),
        dif_resto_branco_intermediario = abs(ratio_Resto_Pop - ratio_Brancos_Intermediario),

        # Negros
        dif_negro_intermediario_negro_baixo = abs(ratio_Negros_Intermediario - ratio_Negros_Baixo),
        dif_negro_alto_negro_baixo = abs(ratio_Negros_Alto - ratio_Negros_Baixo),
        dif_resto_negro_baixo = abs(ratio_Resto_Pop - ratio_Negros_Baixo),
        dif_negro_alto_negro_intermediario = abs(ratio_Negros_Alto - ratio_Negros_Intermediario),
        dif_resto_negro_intermediario = abs(ratio_Resto_Pop - ratio_Negros_Intermediario),
        dif_resto_negro_alto = abs(ratio_Resto_Pop - ratio_Brancos_Intermediario)
      ) |>
      mutate(across(starts_with("dif_"), ~ replace_na(.x, 0))) |>
      summarise(
        D_negro_alto_branco_alto = sum(dif_negro_alto_branco_alto)*.5,
        D_negro_alto_branco_intermediario = sum(dif_negro_alto_branco_intermediario)*.5,
        D_negro_alto_branco_baixo = sum(dif_negro_alto_branco_baixo)*.5,
        D_negro_intermediario_branco_alto = sum(dif_negro_intermediario_branco_alto)*.5,
        D_negro_intermediario_branco_intermediario = sum(dif_negro_intermediario_branco_intermediario)*.5,
        D_negro_intermediario_branco_baixo = sum(dif_negro_intermediario_branco_baixo)*.5,
        D_negro_baixo_branco_alto = sum(dif_negro_baixo_branco_alto)*.5,
        D_negro_baixo_branco_intermediario = sum(dif_negro_baixo_branco_intermediario)*.5,
        D_negro_baixo_branco_baixo = sum(dif_negro_baixo_branco_baixo)*.5,
        D_negro_baixo_negro_alto = sum(dif_negro_alto_negro_baixo)*.5,
        D_negro_baixo_negro_intermediario = sum(dif_negro_intermediario_negro_baixo)*.5,
        D_negro_alto_negro_intermediario = sum(dif_negro_alto_negro_intermediario)*.5,
        D_branco_baixo_branco_intermediario = sum(dif_branco_intermediario_branco_baixo)*.5,
        D_branco_baixo_branco_alto = sum(dif_branco_baixo_branco_alto)*.5,
        D_branco_alto_branco_intermediario = sum(dif_branco_intermediario_branco_alto)*.5,
        D_negro_alto_resto = sum(dif_resto_negro_alto)*.5,
        D_negro_intermediario_resto = sum(dif_resto_negro_intermediario)*.5,
        D_negro_baixo_resto = sum(dif_resto_negro_baixo)*.5,
        D_branco_alto_resto = sum(dif_resto_branco_alto)*.5,
        D_branco_intermediario_resto = sum(dif_resto_branco_intermediario)*.5,
        D_branco_baixo_resto = sum(dif_resto_branco_baixo)*.5
      ) |>
      pivot_longer(D_negro_alto_branco_alto:D_branco_baixo_resto, names_to = "grupo", values_to = "D") |>
      mutate(
        cor_classe1 = c(rep("Negros alto",3),rep("Negros intermediário",3),rep("Negros baixo",5),
                        rep("Negros alto",1),rep("Brancos baixo",2),rep("Brancos alto",1),
                        "Negros alto","Negros intermediário","Negro baixo","Branco alto",
                        "Branco intermediário","Branco baixo"),
        cor_classe2 = c(rep(c("Branco alto","Branco intermediário","Branco baixo"),3), "Negro alto",
                        rep("Negro intermediário",2),"Branco intermediário","Branco alto",
                        "Branco intermediário",rep("Resto da população",6))
      )

    if(por_classe == TRUE){
      output <- list(geral = output_geral,
                     classe = output_classe)

    }else{
      output <- list(geral = output_geral)
    }
    return(output)
  }

func_fazer_tabela <- function(tabela){
  if(is.list(tabela)){
    tabela <- tabela[[classe]]
  }

  tabela_cruzada <- ftable(xtabs(D ~ cor_classe1 + cor_classe2,
                                 tabela),
                           col.vars = c("cor_classe1"),
                           row.vars = c("cor_classe2")) |>
    stats:::format.ftable(quote = FALSE, dec = ",") |>
    trimws() |>
    as.data.frame()

  return(tabela_cruzada)
}


# Indice Quiciente locacional ---------------------------------------------

func_calcula_quociente_locacional <-
  function(
    data,
    var_estrato = "estratos_sociais_egp",
    cor_raca = "cor_raca",
    area_ponderacao = "area_ponderacao",
    idade = "idade",
    PO = "PO",
    id_pessoa = "id_pes",
    peso = "peso",
    EGP = TRUE,
    por_classe = TRUE){

    # Preparo da base de dados
    # criar base de dados a ser utilizada e diferencial para EGP ou classificacao por estrato de renda

    vars <- c(var_estrato, cor_raca, area_ponderacao, id_pessoa, peso, idade, PO)

    if(EGP == TRUE){
      df <- data |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        filter(idade >= 10 & PO == 1 & !is.na(var_estrato)) |>
        mutate(classe_raca = case_when(
          var_estrato == 0 & cor_raca == 1 ~ "Brancos baixo",
          var_estrato == 1 & cor_raca == 1 ~ "Brancos intermediário",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos alto",
          var_estrato == 0 & cor_raca == 2 ~ "Negros baixo",
          var_estrato == 1 & cor_raca == 2 ~ "Negros intermediário",
          var_estrato == 2 & cor_raca == 2 ~ "Negros alto",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }else{
      df <- data |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        filter(idade >= 10) |>
        mutate(classe_raca = case_when(
          var_estrato == 0 & cor_raca == 1 ~ "Brancos baixo",
          var_estrato == 1 & cor_raca == 1 ~ "Brancos intermediário",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos alto",
          var_estrato == 0 & cor_raca == 2 ~ "Negros baixo",
          var_estrato == 1 & cor_raca == 2 ~ "Negros intermediário",
          var_estrato == 2 & cor_raca == 2 ~ "Negros alto",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pessoa, weights = peso)
    }

    print("Etapa de criação de variável finalizada...")

    # cria tabela de base para calculo
    tabela <- df |>
      group_by(cor_raca) |>
      summarise(area_ponderacao = 0,
                prop = survey_mean(na.rm = T)) |>
      bind_rows(
        df |>
          group_by(area_ponderacao, cor_raca) |>
          summarise(prop = survey_mean(na.rm = T))
      ) |>
      select(-prop_se)

    if(por_classe == TRUE){
      tabela_por_classe <- df |>
        select(classe_raca) |>
        group_by(classe_raca) |>
        summarise(area_ponderacao = 0,
                  prop = survey_mean(na.rm = T)) |>
        bind_rows(
          df |>
            select(classe_raca, area_ponderacao) |>
            group_by(area_ponderacao, classe_raca) |>
            summarise(prop = survey_mean(na.rm = T))
        ) |>
        select(-prop_se)
    }

    print("Etapa de construição das tabelas de base finalizada...")

    # calcula o indice
    output_geral <- tabela |>
      mutate(prop_branca = prop[cor_raca == 1 & area_ponderacao == 0],
             prop_negra = prop[cor_raca == 2 & area_ponderacao == 0],
             prop_resto = prop[cor_raca == 0 & area_ponderacao == 0]) |>
      filter(area_ponderacao != 0) |>
      pivot_wider(names_from = cor_raca, values_from = prop) |>
      mutate(across(c(`0`,`1`,`2`), ~ replace_na(.x, 0))) |>
      mutate(QL_branca = `1`/prop_branca,
             QL_negra = `2`/prop_negra,
             QL_resto = `0`/prop_resto)

    output_classe <- tabela_por_classe |>
      mutate(pop_branca_baixo = prop[classe_raca == "Brancos baixo" & area_ponderacao == 0],
             pop_branca_intermediario = prop[classe_raca == "Brancos intermediário" & area_ponderacao == 0],
             pop_branca_alto = prop[classe_raca == "Brancos alto" & area_ponderacao == 0],
             pop_negra_baixo = prop[classe_raca == "Negros baixo" & area_ponderacao == 0],
             pop_negra_intermediario = prop[classe_raca == "Negros intermediário" & area_ponderacao == 0],
             pop_negra_alto = prop[classe_raca == "Negros alto" & area_ponderacao == 0],
             pop_resto = prop[classe_raca == "Resto da população" & area_ponderacao == 0]) |>
      pivot_wider(names_from = classe_raca, values_from = prop) |>
      mutate(across(c(`Resto da população`,`Negros baixo`,`Negros intermediário`,
                      `Negros alto`,`Brancos baixo`,`Brancos intermediário`,
                      `Brancos alto`), ~ replace_na(.x, 0))) |>
      filter(area_ponderacao != 0) |>
      mutate(
        # Razoes de cada grupo
        QL_Brancos_Alto = `Brancos alto`/pop_branca_alto,
        QL_Brancos_Intermediario = `Brancos intermediário`/pop_branca_intermediario,
        QL_Brancos_Baixo = `Brancos baixo`/pop_branca_baixo,
        QL_Negros_Alto = `Negros alto`/pop_negra_alto,
        QL_Negros_Intermediario = `Negros intermediário`/pop_negra_intermediario,
        QL_Negros_Baixo = `Negros baixo`/pop_negra_baixo,
        QL_Resto_Pop = `Resto da população`/pop_resto)

    # Sintese dos resultados
    output_geral_sintese <- output_geral |>
      summarise(
        mean_QL_branca = mean(QL_branca),
        mean_QL_negra = mean(QL_negra),
        mean_QL_resto = mean(QL_resto),
        desv_pad_QL_branca = sd(QL_branca),
        desv_pad_QL_negra = sd(QL_negra),
        desv_pad_QL_resto = sd(QL_resto)
      )

    output_classe_sintese <- output_classe |>
      summarise(
        mean_QL_Brancos_Alto = mean(QL_Brancos_Alto),
        mean_QL_Brancos_Intermediario = mean(QL_Brancos_Intermediario),
        mean_QL_Brancos_Baixo = mean(QL_Brancos_Baixo),
        mean_QL_Negros_Alto = mean(QL_Negros_Alto),
        mean_QL_Negros_Intermediario = mean(QL_Negros_Intermediario),
        mean_QL_Negros_Baixo = mean(QL_Negros_Baixo),
        mean_QL_Resto_Pop = mean(QL_Resto_Pop),
        desv_pad_QL_Brancos_Alto = sd(QL_Brancos_Alto),
        desv_pad_QL_Brancos_Intermediario = sd(QL_Brancos_Intermediario),
        desv_pad_QL_Brancos_Baixo = sd(QL_Brancos_Baixo),
        desv_pad_QL_Negros_Alto = sd(QL_Negros_Alto),
        desv_pad_QL_Negros_Intermediario = sd(QL_Negros_Intermediario),
        desv_pad_QL_Negros_Baixo = sd(QL_Negros_Baixo),
        desv_pad_QL_Resto_Pop = sd(QL_Resto_Pop)
      )

    if(por_classe == TRUE){
      output <- list(geral = output_geral,
                     classe = output_classe,
                     geral_sintese = output_geral_sintese,
                     classe_sintese = output_classe_sintese)

    }else{
      output <- list(geral = output_geral,
                     geral_sintese = output_geral_sintese)
    }
    return(output)
  }

