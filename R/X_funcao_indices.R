
# Indice de dissimilaridade -----------------------------------------------

func_calcula_dissimilaridade <-
  function(
    data,
    var_estrato,
    cor_raca = "cor_raca",
    raca_d = FALSE,
    area_ponderacao = "area_ponderacao",
    id_pessoa = "id_pes",
    idade = "idade",
    PO = "PO",
    peso = "peso",
    tipo_variavel, # pode receber "SM" para Salarios Minimos; "EGP" para classes EGP ou outros...
    por_classe = TRUE){

    # Preparo da base de dados
    # criar base de dados a ser utilizada e diferencial para EGP ou classificacao por estrato de renda

    vars <- c(var_estrato, cor_raca, area_ponderacao, id_pessoa, peso, idade, PO)

    if(tipo_variavel == "EGP"){
      df <- data |>
        filter(situacao_dom == 1) |>
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
    }
    if(tipo_variavel == "SM"){
      df <- data |>
        filter(situacao_dom == 1) |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        mutate(classe_raca = case_when(
          var_estrato == 1 & cor_raca == 1 ~ "Brancos ate meio SM",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos meio a 1SM",
          var_estrato == 3 & cor_raca == 1 ~ "Brancos 1 a 3SM",
          var_estrato == 4 & cor_raca == 1 ~ "Brancos 3SM mais",
          var_estrato == 1 & cor_raca == 2 ~ "Negros ate meio SM",
          var_estrato == 2 & cor_raca == 2 ~ "Negros meio a 1SM",
          var_estrato == 3 & cor_raca == 2 ~ "Negros 1 a 3SM",
          var_estrato == 4 & cor_raca == 2 ~ "Negros 3SM mais",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }
    if(tipo_variavel == "SM_NE"){
      df <- data |>
        filter(situacao_dom == 1) |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        mutate(classe_raca = case_when(
          # Brancos
          var_estrato == 1 & cor_raca_d == 1 ~ "Brancos - [0 a 0,25 SM)",
          var_estrato == 2 & cor_raca_d == 1 ~ "Brancos - [0,25 a 0,5 SM)",
          var_estrato == 3 & cor_raca_d == 1 ~ "Brancos - [0,5 a 0,75 SM)",
          var_estrato == 4 & cor_raca_d == 1 ~ "Brancos - [0,75 a 1,25 SM)",
          var_estrato == 5 & cor_raca_d == 1 ~ "Brancos - [1,25 a 4 SM)",
          var_estrato == 6 & cor_raca_d == 1 ~ "Brancos - [4+ SM)",
          # Pretos
          var_estrato == 1 & cor_raca_d == 2 ~ "Pretos - [0 a 0,25 SM)",
          var_estrato == 2 & cor_raca_d == 2 ~ "Pretos - [0,25 a 0,5 SM)",
          var_estrato == 3 & cor_raca_d == 2 ~ "Pretos - [0,5 a 0,75 SM)",
          var_estrato == 4 & cor_raca_d == 2 ~ "Pretos - [0,75 a 1,25 SM)",
          var_estrato == 5 & cor_raca_d == 2 ~ "Pretos - [1,25 a 4 SM)",
          var_estrato == 6 & cor_raca_d == 2 ~ "Pretos - [4+ SM)",
          # Pardos
          var_estrato == 1 & cor_raca_d == 4 ~ "Pardos - [0 a 0,25 SM)",
          var_estrato == 2 & cor_raca_d == 4 ~ "Pardos - [0,25 a 0,5 SM)",
          var_estrato == 3 & cor_raca_d == 4 ~ "Pardos - [0,5 a 0,75 SM)",
          var_estrato == 4 & cor_raca_d == 4 ~ "Pardos - [0,75 a 1,25 SM)",
          var_estrato == 5 & cor_raca_d == 4 ~ "Pardos - [1,25 a 4 SM)",
          var_estrato == 6 & cor_raca_d == 4 ~ "Pardos - [4+ SM)"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }
    if(tipo_variavel == "SM_SUL"){
      df <- data |>
        filter(situacao_dom == 1) |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        mutate(classe_raca = case_when(
          # Brancos
          var_estrato == 1 & cor_raca_d == 1 ~ "Brancos - [0 a 0,5 SM)",
          var_estrato == 2 & cor_raca_d == 1 ~ "Brancos - [0,5 a 1,25 SM)",
          var_estrato == 3 & cor_raca_d == 1 ~ "Brancos - [1,25 SM a 2 SM)",
          var_estrato == 4 & cor_raca_d == 1 ~ "Brancos - [2 SM a 4 SM)",
          var_estrato == 5 & cor_raca_d == 1 ~ "Brancos - [4+ SM)",
          # Pretos
          var_estrato == 1 & cor_raca_d == 2 ~ "Pretos - [0 a 0,5 SM)",
          var_estrato == 2 & cor_raca_d == 2 ~ "Pretos - [0,5 a 1,25 SM)",
          var_estrato == 3 & cor_raca_d == 2 ~ "Pretos - [1,25 SM a 2 SM)",
          var_estrato == 4 & cor_raca_d == 2 ~ "Pretos - [2 SM a 4 SM)",
          var_estrato == 5 & cor_raca_d == 2 ~ "Pretos - [4+ SM)",
          # Pardos
          var_estrato == 1 & cor_raca_d == 4 ~ "Pardos - [0 a 0,5 SM)",
          var_estrato == 2 & cor_raca_d == 4 ~ "Pardos - [0,5 a 1,25 SM)",
          var_estrato == 3 & cor_raca_d == 4 ~ "Pardos - [1,25 SM a 2 SM)",
          var_estrato == 4 & cor_raca_d == 4 ~ "Pardos - [2 SM a 4 SM)",
          var_estrato == 5 & cor_raca_d == 4 ~ "Pardos - [4+ SM)"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }else{
      df <- data |>
        filter(situacao_dom == 1) |>
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
    if(raca_d == TRUE){
      tabela <- df |>
        summarise(cor_raca_d = 0,
                  area_ponderacao = 0,
                  n = survey_total(na.rm = T)) |>
        bind_rows(
          df |>
            filter(cor_raca_d %in% c(1,2,4)) |>
            group_by(cor_raca_d) |>
            summarise(
              area_ponderacao = 0,
              n = survey_total(na.rm = T))
        ) |>
        bind_rows(
          df |>
            group_by(area_ponderacao) |>
            summarise(
              cor_raca_d = 0,
              n = survey_total(na.rm = T))
        ) |>
        bind_rows(
          df |>
            filter(cor_raca_d %in% c(1,2,4)) |>
            group_by(cor_raca_d,area_ponderacao) |>
            summarise(
              n = survey_total(na.rm = T))
        )
    } else{
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
    }
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
        ) |>
        filter(!is.na(classe_raca))
    }

    print("Etapa de construição das tabelas de base finalizada...")

    # calcula o indice
    if(raca_d == TRUE){
      output_geral <- tabela |>
        select(-n_se) |>
        mutate(pop_branca = n[cor_raca_d == 1 & area_ponderacao == 0],
               pop_preta = n[cor_raca_d == 2 & area_ponderacao == 0],
               pop_parda = n[cor_raca_d == 4 & area_ponderacao == 0]) |>
        filter(cor_raca != 0 & area_ponderacao != 0) |>
        pivot_wider(names_from = cor_raca_d, values_from = n) |>
        mutate(ratio_branca = `1`/pop_branca,
               ratio_preta = `2`/pop_preta,
               ratio_parda = `4`/pop_parda,
               ratio_dif_branca_preta = abs(ratio_branca - ratio_preta),
               ratio_dif_branca_parda = abs(ratio_branca - ratio_parda),
               ratio_dif_preta_parda = abs(ratio_preta - ratio_parda),
               ) |>
        mutate(across(starts_with("ratio_"), ~ replace_na(.x, 0))) |>
        summarise(
          D_branca_preta = sum(ratio_dif_branca_preta)*.5,
          D_branca_parda = sum(ratio_dif_branca_parda)*.5,
          D_preta_parda = sum(ratio_dif_preta_parda)*.5
        )
    } else{
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
    }

    if(tipo_variavel == "SM"){
      output_classe <- tabela_por_classe |>
        select(-n_se) |>
        mutate(pop_branca_meioSM = n[classe_raca == "Brancos ate meio SM" & area_ponderacao == 0],
               pop_branca_meioa1SM = n[classe_raca == "Brancos meio a 1SM" & area_ponderacao == 0],
               pop_branca_1a3SM = n[classe_raca == "Brancos 1 a 3SM" & area_ponderacao == 0],
               pop_branca_3SMmais = n[classe_raca == "Brancos 3SM mais" & area_ponderacao == 0],

               pop_negra_meioSM = n[classe_raca == "Negros ate meio SM" & area_ponderacao == 0],
               pop_negra_meioa1SM = n[classe_raca == "Negros meio a 1SM" & area_ponderacao == 0],
               pop_negra_1a3SM = n[classe_raca == "Negros 1 a 3SM" & area_ponderacao == 0],
               pop_negra_3SMmais = n[classe_raca == "Negros 3SM mais" & area_ponderacao == 0],

               pop_resto = n[classe_raca == "Resto da população" & area_ponderacao == 0]) |>
        pivot_wider(names_from = classe_raca, values_from = n) |>
        filter(area_ponderacao != 0) |>
        mutate(
          # Razoes de cada grupo
          ratio_Brancos_meioSM = `Brancos ate meio SM`/pop_branca_meioSM,
          ratio_Brancos_meioa1SM = `Brancos meio a 1SM`/pop_branca_meioa1SM,
          ratio_Brancos_1a3SM = `Brancos 1 a 3SM`/pop_branca_1a3SM,
          ratio_Brancos_3SMmais = `Brancos 3SM mais`/pop_branca_3SMmais,

          ratio_Negros_meioSM = `Negros ate meio SM`/pop_negra_meioSM,
          ratio_Negros_meioa1SM = `Negros meio a 1SM`/pop_negra_meioa1SM,
          ratio_Negros_1a3SM = `Negros 1 a 3SM`/pop_negra_1a3SM,
          ratio_Negros_3SMmais = `Negros 3SM mais`/pop_negra_3SMmais,

          ratio_Resto_Pop = `Resto da população`/pop_resto,

          ## Ratio diff

          # Brancos 3SMmais
          dif_branco_atemeioSM_branco_3SMmais = abs(ratio_Brancos_meioSM - ratio_Brancos_3SMmais),
          dif_branco_meioa1SM_branco_3SMmais = abs(ratio_Brancos_meioa1SM - ratio_Brancos_3SMmais),
          dif_branco_1a3SM_branco_3SMmais = abs(ratio_Brancos_1a3SM - ratio_Brancos_3SMmais),

          dif_negro_atemeioSM_branco_3SMmais = abs(ratio_Negros_meioSM - ratio_Brancos_3SMmais),
          dif_negro_meioa1SM_branco_3SMmais = abs(ratio_Negros_meioa1SM - ratio_Brancos_3SMmais),
          dif_negro_1a3SM_branco_3SMmais = abs(ratio_Negros_1a3SM - ratio_Brancos_3SMmais),
          dif_negro_3SMmais_branco_3SMmais = abs(ratio_Negros_3SMmais - ratio_Brancos_3SMmais),

          dif_resto_branco_3SMmais = abs(ratio_Resto_Pop - ratio_Brancos_3SMmais),

          # Brancos 1a3SM
          dif_branco_atemeioSM_branco_1a3SM = abs(ratio_Brancos_meioSM - ratio_Brancos_1a3SM),
          dif_branco_meioa1SM_branco_1a3SM = abs(ratio_Brancos_meioa1SM - ratio_Brancos_1a3SM),

          dif_negro_atemeioSM_branco_1a3SM = abs(ratio_Negros_meioSM - ratio_Brancos_1a3SM),
          dif_negro_meioa1SM_branco_1a3SM = abs(ratio_Negros_meioa1SM - ratio_Brancos_1a3SM),
          dif_negro_1a3SM_branco_1a3SM = abs(ratio_Negros_1a3SM - ratio_Brancos_1a3SM),
          dif_negro_3SMmais_branco_1a3SM = abs(ratio_Negros_3SMmais - ratio_Brancos_1a3SM),

          dif_resto_branco_1a3SM = abs(ratio_Resto_Pop - ratio_Brancos_1a3SM),

          # Brancos meioa1SM
          dif_branco_atemeioSM_branco_meioa1SM = abs(ratio_Brancos_meioSM - ratio_Brancos_meioa1SM),

          dif_negro_atemeioSM_branco_meioa1SM = abs(ratio_Negros_meioSM - ratio_Brancos_meioa1SM),
          dif_negro_meioa1SM_branco_meioa1SM = abs(ratio_Negros_meioa1SM - ratio_Brancos_meioa1SM),
          dif_negro_1a3SM_branco_meioa1SM = abs(ratio_Negros_1a3SM - ratio_Brancos_meioa1SM),
          dif_negro_3SMmais_branco_meioa1SM = abs(ratio_Negros_3SMmais - ratio_Brancos_meioa1SM),

          dif_resto_branco_meioa1SM = abs(ratio_Resto_Pop - ratio_Brancos_meioa1SM),

          # Brancos atemeioSM

          dif_negro_atemeioSM_branco_atemeioSM = abs(ratio_Negros_meioSM - ratio_Brancos_meioSM),
          dif_negro_meioa1SM_branco_atemeioSM = abs(ratio_Negros_meioa1SM - ratio_Brancos_meioSM),
          dif_negro_1a3SM_branco_atemeioSM = abs(ratio_Negros_1a3SM - ratio_Brancos_meioSM),
          dif_negro_3SMmais_branco_atemeioSM = abs(ratio_Negros_3SMmais - ratio_Brancos_meioSM),

          dif_resto_branco_atemeioSM = abs(ratio_Resto_Pop - ratio_Brancos_meioSM),

          # Negros 3SMmais

          dif_negro_atemeioSM_negro_3SMmais = abs(ratio_Negros_meioSM - ratio_Negros_3SMmais),
          dif_negro_meioa1SM_negro_3SMmais = abs(ratio_Negros_meioa1SM - ratio_Negros_3SMmais),
          dif_negro_1a3SM_negro_3SMmais = abs(ratio_Negros_1a3SM - ratio_Negros_3SMmais),

          dif_resto_negro_3SMmais = abs(ratio_Resto_Pop - ratio_Negros_3SMmais),

          # Negros 1a3SM

          dif_negro_atemeioSM_negro_1a3SM = abs(ratio_Negros_meioSM - ratio_Negros_1a3SM),
          dif_negro_meioa1SM_negro_1a3SM = abs(ratio_Negros_meioa1SM - ratio_Negros_1a3SM),

          dif_resto_negro_1a3SM = abs(ratio_Resto_Pop - ratio_Negros_1a3SM),

          # Negros meioa1SM

          dif_negro_atemeioSM_negro_meioa1SM = abs(ratio_Negros_meioSM - ratio_Negros_meioa1SM),

          dif_resto_negro_meioa1SM = abs(ratio_Resto_Pop - ratio_Negros_meioa1SM),

          # Negros atemeioSM

          dif_resto_negro_atemeioSM = abs(ratio_Resto_Pop - ratio_Negros_meioSM)

        ) |>
        mutate(across(starts_with("dif_"), ~ replace_na(.x, 0))) |>
        summarise(
          # negros 3SMmais
          D_negro_3SMmais_branco_3SMmais = sum(dif_negro_3SMmais_branco_3SMmais)*.5,
          D_negro_3SMmais_branco_1a3SM = sum(dif_negro_3SMmais_branco_1a3SM)*.5,
          D_negro_3SMmais_branco_meioa1SM = sum(dif_negro_3SMmais_branco_meioa1SM)*.5,
          D_negro_3SMmais_branco_atemeioSM = sum(dif_negro_3SMmais_branco_atemeioSM)*.5,

          D_negro_3SMmais_negro_1a3SM = sum(dif_negro_1a3SM_negro_3SMmais)*.5,
          D_negro_3SMmais_negro_meioa1SM = sum(dif_negro_meioa1SM_negro_3SMmais)*.5,
          D_negro_3SMmais_negro_atemeioSM = sum(dif_negro_atemeioSM_negro_3SMmais)*.5,

          D_negro_3SMmais_resto = sum(dif_resto_negro_3SMmais)*.5,

          # Negros 1a3SM
          D_negro_1a3SM_branco_3SMmais = sum(dif_negro_1a3SM_branco_3SMmais)*.5,
          D_negro_1a3SM_branco_1a3SM = sum(dif_negro_1a3SM_branco_1a3SM)*.5,
          D_negro_1a3SM_branco_meioa1SM = sum(dif_negro_1a3SM_branco_meioa1SM)*.5,
          D_negro_1a3SM_branco_atemeioSM = sum(dif_negro_1a3SM_branco_atemeioSM)*.5,

          D_negro_1a3SM_negro_meioa1SM = sum(dif_negro_meioa1SM_negro_1a3SM)*.5,
          D_negro_1a3SM_negro_atemeioSM = sum(dif_negro_atemeioSM_negro_1a3SM)*.5,

          D_negro_1a3SM_resto = sum(dif_resto_negro_1a3SM)*.5,

          # Negros meio a 1 SM
          D_negro_meioa1SM_branco_3SMmais = sum(dif_branco_meioa1SM_branco_3SMmais)*.5,
          D_negro_meioa1SM_branco_1a3SM = sum(dif_negro_meioa1SM_branco_1a3SM)*.5,
          D_negro_meioa1SM_branco_meioa1SM = sum(dif_negro_meioa1SM_branco_meioa1SM)*.5,
          D_negro_meioa1SM_branco_atemeioSM = sum(dif_negro_meioa1SM_branco_atemeioSM)*.5,

          D_negro_meioa1SM_negro_atemeioSM = sum(dif_negro_atemeioSM_negro_meioa1SM)*.5,

          D_negro_meioa1SM_resto = sum(dif_resto_negro_meioa1SM)*.5,

          # Negros ate meio SM
          D_negro_atemeioSM_branco_3SMmais = sum(dif_negro_atemeioSM_branco_3SMmais)*.5,
          D_negro_atemeioSM_branco_1a3SM = sum(dif_negro_atemeioSM_branco_1a3SM)*.5,
          D_negro_atemeioSM_branco_meioa1SM = sum(dif_negro_atemeioSM_branco_meioa1SM)*.5,
          D_negro_atemeioSM_branco_atemeioSM = sum(dif_negro_atemeioSM_branco_atemeioSM)*.5,

          D_negro_atemeioSM_resto = sum(dif_resto_negro_atemeioSM)*.5,

          # Brancos 3SMmais
          D_branco_3SMmais_branco_1a3SM = sum(dif_branco_1a3SM_branco_3SMmais)*.5,
          D_branco_3SMmais_branco_meioa1SM = sum(dif_branco_meioa1SM_branco_3SMmais)*.5,
          D_branco_3SMmais_branco_atemeioSM = sum(dif_branco_atemeioSM_branco_3SMmais)*.5,

          D_branco_3SMmais_resto = sum(dif_resto_branco_3SMmais)*.5,

          # Brancos 1a3 SM
          D_branco_1a3SM_branco_meioa1SM = sum(dif_branco_meioa1SM_branco_1a3SM)*.5,
          D_branco_1a3SM_branco_atemeioSM = sum(dif_branco_atemeioSM_branco_1a3SM)*.5,

          D_branco_1a3SM_resto = sum(dif_resto_branco_1a3SM)*.5,

          # Brancos meio a 1 SM
          D_branco_meioa1SM_branco_atemeioSM = sum(dif_branco_atemeioSM_branco_meioa1SM)*.5,

          D_branco_meioa1SM_resto = sum(dif_resto_branco_meioa1SM)*.5,

          # Brancos ate meio SM

          D_branco_atemeioSM_resto = sum(dif_resto_branco_atemeioSM)*.5

        ) |>
        pivot_longer(D_negro_3SMmais_branco_3SMmais:D_branco_atemeioSM_resto, names_to = "grupo", values_to = "D") |>
        mutate(
          cor_classe1 = c(rep("Negros 3SMmais",8),rep("Negros 1a3SM",7),rep("Negros meioa1SM",6),
                          rep("Negros atemeioSM",5),rep("Brancos 3SMmais",4),rep("Brancos 1a3SM",3),
                          rep("Brancos meioa1SM",2),rep("Brancos atemeioSM",1)),
          cor_classe2 = c(c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM"),
                          c("Negros 1a3SM","Negros meioa1SM","Negros atemeioSM", "Resto da população"),
                          c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM"),
                          c("Negros meioa1SM","Negros atemeioSM", "Resto da população"),
                          c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM"),
                          c("Negros atemeioSM", "Resto da população"),
                          c("Brancos 3SMmais","Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM"),
                          c("Resto da população"),
                          c("Brancos 1a3SM","Brancos meioa1SM","Brancos atemeioSM"),
                          c("Resto da população"),c("Brancos meioa1SM","Brancos atemeioSM"),
                          c("Resto da população"),c("Brancos atemeioSM"),
                          c("Resto da população"),c("Resto da população"))
        )
    }
    if(tipo_variavel == "SM_NE"){
      output_classe <- tabela_por_classe |>
        select(-n_se) |>
        mutate(
          # brancos
          pop_branca_0a025 = n[classe_raca == "Brancos - [0 a 0,25 SM)" & area_ponderacao == 0],
          pop_branca_025a050 = n[classe_raca == "Brancos - [0,25 a 0,5 SM)" & area_ponderacao == 0],
          pop_branca_050a075 = n[classe_raca == "Brancos - [0,5 a 0,75 SM)" & area_ponderacao == 0],
          pop_branca_075a125 = n[classe_raca == "Brancos - [0,75 a 1,25 SM)" & area_ponderacao == 0],
          pop_branca_125a4 = n[classe_raca == "Brancos - [1,25 a 4 SM)" & area_ponderacao == 0],
          pop_branca_4mais = n[classe_raca == "Brancos - [4+ SM)" & area_ponderacao == 0],

          # pretos
          pop_preta_0a025 = n[classe_raca == "Pretos - [0 a 0,25 SM)" & area_ponderacao == 0],
          pop_preta_025a050 = n[classe_raca == "Pretos - [0,25 a 0,5 SM)" & area_ponderacao == 0],
          pop_preta_050a075 = n[classe_raca == "Pretos - [0,5 a 0,75 SM)" & area_ponderacao == 0],
          pop_preta_075a125 = n[classe_raca == "Pretos - [0,75 a 1,25 SM)" & area_ponderacao == 0],
          pop_preta_125a4 = n[classe_raca == "Pretos - [1,25 a 4 SM)" & area_ponderacao == 0],
          pop_preta_4mais = n[classe_raca == "Pretos - [4+ SM)" & area_ponderacao == 0],

          # pardos
          pop_parda_0a025 = n[classe_raca == "Pardos - [0 a 0,25 SM)" & area_ponderacao == 0],
          pop_parda_025a050 = n[classe_raca == "Pardos - [0,25 a 0,5 SM)" & area_ponderacao == 0],
          pop_parda_050a075 = n[classe_raca == "Pardos - [0,5 a 0,75 SM)" & area_ponderacao == 0],
          pop_parda_075a125 = n[classe_raca == "Pardos - [0,75 a 1,25 SM)" & area_ponderacao == 0],
          pop_parda_125a4 = n[classe_raca == "Pardos - [1,25 a 4 SM)" & area_ponderacao == 0],
          pop_parda_4mais = n[classe_raca == "Pardos - [4+ SM)" & area_ponderacao == 0],
        ) |>
        pivot_wider(names_from = classe_raca, values_from = n) |>
        filter(area_ponderacao != 0) |>
        mutate(
          ### Razoes de cada grupo

          # brancos
          ratio_branca_0a025 = `Brancos - [0 a 0,25 SM)`/pop_branca_0a025,
          ratio_branca_025a050 = `Brancos - [0,25 a 0,5 SM)`/pop_branca_025a050,
          ratio_branca_050a075 = `Brancos - [0,5 a 0,75 SM)`/pop_branca_050a075,
          ratio_branca_075a125 = `Brancos - [0,75 a 1,25 SM)`/pop_branca_075a125,
          ratio_branca_125a4 = `Brancos - [1,25 a 4 SM)`/pop_branca_125a4,
          ratio_branca_4mais = `Brancos - [4+ SM)`/pop_branca_4mais,

          # pretos
          ratio_preta_0a025 = `Pretos - [0 a 0,25 SM)`/pop_preta_0a025,
          ratio_preta_025a050 = `Pretos - [0,25 a 0,5 SM)`/pop_preta_025a050,
          ratio_preta_050a075 = `Pretos - [0,5 a 0,75 SM)`/pop_preta_050a075,
          ratio_preta_075a125 = `Pretos - [0,75 a 1,25 SM)`/pop_preta_075a125,
          ratio_preta_125a4 = `Pretos - [1,25 a 4 SM)`/pop_preta_125a4,
          ratio_preta_4mais = `Pretos - [4+ SM)`/pop_preta_4mais,

          # pardos
          ratio_parda_0a025 = `Pardos - [0 a 0,25 SM)`/pop_parda_0a025,
          ratio_parda_025a050 = `Pardos - [0,25 a 0,5 SM)`/pop_parda_025a050,
          ratio_parda_050a075 = `Pardos - [0,5 a 0,75 SM)`/pop_parda_050a075,
          ratio_parda_075a125 = `Pardos - [0,75 a 1,25 SM)`/pop_parda_075a125,
          ratio_parda_125a4 = `Pardos - [1,25 a 4 SM)`/pop_parda_125a4,
          ratio_parda_4mais = `Pardos - [4+ SM)`/pop_parda_4mais,



          #
          ### Diferenca das razoes
          #

          ## Brancos - 0a025
          # x brancos
          dif_branco_0a025_branco_025a050 = abs(ratio_branca_0a025 - ratio_branca_025a050),
          dif_branco_0a025_branco_050a075 = abs(ratio_branca_0a025 - ratio_branca_050a075),
          dif_branco_0a025_branco_075a125 = abs(ratio_branca_0a025 - ratio_branca_075a125),
          dif_branco_0a025_branco_125a4 = abs(ratio_branca_0a025 - ratio_branca_125a4),
          dif_branco_0a025_branco_4mais = abs(ratio_branca_0a025 - ratio_branca_4mais),
          # x pretos
          dif_branco_0a025_preto_0a025 = abs(ratio_branca_0a025 - ratio_preta_0a025),
          dif_branco_0a025_preto_025a050 = abs(ratio_branca_0a025 - ratio_preta_025a050),
          dif_branco_0a025_preto_050a075 = abs(ratio_branca_0a025 - ratio_preta_050a075),
          dif_branco_0a025_preto_075a125 = abs(ratio_branca_0a025 - ratio_preta_075a125),
          dif_branco_0a025_preto_125a4 = abs(ratio_branca_0a025 - ratio_preta_125a4),
          dif_branco_0a025_preto_4mais = abs(ratio_branca_0a025 - ratio_preta_4mais),
          # x pardos
          dif_branco_0a025_pardo_0a025 = abs(ratio_branca_0a025 - ratio_parda_0a025),
          dif_branco_0a025_pardo_025a050 = abs(ratio_branca_0a025 - ratio_parda_025a050),
          dif_branco_0a025_pardo_050a075 = abs(ratio_branca_0a025 - ratio_parda_050a075),
          dif_branco_0a025_pardo_075a125 = abs(ratio_branca_0a025 - ratio_parda_075a125),
          dif_branco_0a025_pardo_125a4 = abs(ratio_branca_0a025 - ratio_parda_125a4),
          dif_branco_0a025_pardo_4mais = abs(ratio_branca_0a025 - ratio_parda_4mais),

          ## Brancos - 025a050
          # x brancos
          dif_branco_025a050_branco_050a075 = abs(ratio_branca_025a050 - ratio_branca_050a075),
          dif_branco_025a050_branco_075a125 = abs(ratio_branca_025a050 - ratio_branca_075a125),
          dif_branco_025a050_branco_125a4 = abs(ratio_branca_025a050 - ratio_branca_125a4),
          dif_branco_025a050_branco_4mais = abs(ratio_branca_025a050 - ratio_branca_4mais),
          # x pretos
          dif_branco_025a050_preto_0a025 = abs(ratio_branca_025a050 - ratio_preta_0a025),
          dif_branco_025a050_preto_025a050 = abs(ratio_branca_025a050 - ratio_preta_025a050),
          dif_branco_025a050_preto_050a075 = abs(ratio_branca_025a050 - ratio_preta_050a075),
          dif_branco_025a050_preto_075a125 = abs(ratio_branca_025a050 - ratio_preta_075a125),
          dif_branco_025a050_preto_125a4 = abs(ratio_branca_025a050 - ratio_preta_125a4),
          dif_branco_025a050_preto_4mais = abs(ratio_branca_025a050 - ratio_preta_4mais),
          # x pardos
          dif_branco_025a050_pardo_0a025 = abs(ratio_branca_025a050 - ratio_parda_0a025),
          dif_branco_025a050_pardo_025a050 = abs(ratio_branca_025a050 - ratio_parda_025a050),
          dif_branco_025a050_pardo_050a075 = abs(ratio_branca_025a050 - ratio_parda_050a075),
          dif_branco_025a050_pardo_075a125 = abs(ratio_branca_025a050 - ratio_parda_075a125),
          dif_branco_025a050_pardo_125a4 = abs(ratio_branca_025a050 - ratio_parda_125a4),
          dif_branco_025a050_pardo_4mais = abs(ratio_branca_025a050 - ratio_parda_4mais),

          ## Brancos - 050a075
          # x brancos
          dif_branco_050a075_branco_075a125 = abs(ratio_branca_050a075 - ratio_branca_075a125),
          dif_branco_050a075_branco_125a4 = abs(ratio_branca_050a075 - ratio_branca_125a4),
          dif_branco_050a075_branco_4mais = abs(ratio_branca_050a075 - ratio_branca_4mais),
          # x pretos
          dif_branco_050a075_preto_0a025 = abs(ratio_branca_050a075 - ratio_preta_0a025),
          dif_branco_050a075_preto_025a050 = abs(ratio_branca_050a075 - ratio_preta_025a050),
          dif_branco_050a075_preto_050a075 = abs(ratio_branca_050a075 - ratio_preta_050a075),
          dif_branco_050a075_preto_075a125 = abs(ratio_branca_050a075 - ratio_preta_075a125),
          dif_branco_050a075_preto_125a4 = abs(ratio_branca_050a075 - ratio_preta_125a4),
          dif_branco_050a075_preto_4mais = abs(ratio_branca_050a075 - ratio_preta_4mais),
          # x pardos
          dif_branco_050a075_pardo_0a025 = abs(ratio_branca_050a075 - ratio_parda_0a025),
          dif_branco_050a075_pardo_025a050 = abs(ratio_branca_050a075 - ratio_parda_025a050),
          dif_branco_050a075_pardo_050a075 = abs(ratio_branca_050a075 - ratio_parda_050a075),
          dif_branco_050a075_pardo_075a125 = abs(ratio_branca_050a075 - ratio_parda_075a125),
          dif_branco_050a075_pardo_125a4 = abs(ratio_branca_050a075 - ratio_parda_125a4),
          dif_branco_050a075_pardo_4mais = abs(ratio_branca_050a075 - ratio_parda_4mais),

          ## Brancos - 075a125
          # x brancos
          dif_branco_075a125_branco_125a4 = abs(ratio_branca_075a125 - ratio_branca_125a4),
          dif_branco_075a125_branco_4mais = abs(ratio_branca_075a125 - ratio_branca_4mais),
          # x pretos
          dif_branco_075a125_preto_0a025 = abs(ratio_branca_075a125 - ratio_preta_0a025),
          dif_branco_075a125_preto_025a050 = abs(ratio_branca_075a125 - ratio_preta_025a050),
          dif_branco_075a125_preto_050a075 = abs(ratio_branca_075a125 - ratio_preta_050a075),
          dif_branco_075a125_preto_075a125 = abs(ratio_branca_075a125 - ratio_preta_075a125),
          dif_branco_075a125_preto_125a4 = abs(ratio_branca_075a125 - ratio_preta_125a4),
          dif_branco_075a125_preto_4mais = abs(ratio_branca_075a125 - ratio_preta_4mais),
          # x pardos
          dif_branco_075a125_pardo_0a025 = abs(ratio_branca_075a125 - ratio_parda_0a025),
          dif_branco_075a125_pardo_025a050 = abs(ratio_branca_075a125 - ratio_parda_025a050),
          dif_branco_075a125_pardo_050a075 = abs(ratio_branca_075a125 - ratio_parda_050a075),
          dif_branco_075a125_pardo_075a125 = abs(ratio_branca_075a125 - ratio_parda_075a125),
          dif_branco_075a125_pardo_125a4 = abs(ratio_branca_075a125 - ratio_parda_125a4),
          dif_branco_075a125_pardo_4mais = abs(ratio_branca_075a125 - ratio_parda_4mais),

          ## Brancos - 125a4
          # x brancos
          dif_branco_125a4_branco_4mais = abs(ratio_branca_125a4 - ratio_branca_4mais),
          # x pretos
          dif_branco_125a4_preto_0a025 = abs(ratio_branca_125a4 - ratio_preta_0a025),
          dif_branco_125a4_preto_025a050 = abs(ratio_branca_125a4 - ratio_preta_025a050),
          dif_branco_125a4_preto_050a075 = abs(ratio_branca_125a4 - ratio_preta_050a075),
          dif_branco_125a4_preto_075a125 = abs(ratio_branca_125a4 - ratio_preta_075a125),
          dif_branco_125a4_preto_125a4 = abs(ratio_branca_125a4 - ratio_preta_125a4),
          dif_branco_125a4_preto_4mais = abs(ratio_branca_125a4 - ratio_preta_4mais),
          # x pardos
          dif_branco_125a4_pardo_0a025 = abs(ratio_branca_125a4 - ratio_parda_0a025),
          dif_branco_125a4_pardo_025a050 = abs(ratio_branca_125a4 - ratio_parda_025a050),
          dif_branco_125a4_pardo_050a075 = abs(ratio_branca_125a4 - ratio_parda_050a075),
          dif_branco_125a4_pardo_075a125 = abs(ratio_branca_125a4 - ratio_parda_075a125),
          dif_branco_125a4_pardo_125a4 = abs(ratio_branca_125a4 - ratio_parda_125a4),
          dif_branco_125a4_pardo_4mais = abs(ratio_branca_125a4 - ratio_parda_4mais),

          ## Brancos - 4mais
          # x pretos
          dif_branco_4mais_preto_0a025 = abs(ratio_branca_4mais - ratio_preta_0a025),
          dif_branco_4mais_preto_025a050 = abs(ratio_branca_4mais - ratio_preta_025a050),
          dif_branco_4mais_preto_050a075 = abs(ratio_branca_4mais - ratio_preta_050a075),
          dif_branco_4mais_preto_075a125 = abs(ratio_branca_4mais - ratio_preta_075a125),
          dif_branco_4mais_preto_125a4 = abs(ratio_branca_4mais - ratio_preta_125a4),
          dif_branco_4mais_preto_4mais = abs(ratio_branca_4mais - ratio_preta_4mais),
          # x pardos
          dif_branco_4mais_pardo_0a025 = abs(ratio_branca_4mais - ratio_parda_0a025),
          dif_branco_4mais_pardo_025a050 = abs(ratio_branca_4mais - ratio_parda_025a050),
          dif_branco_4mais_pardo_050a075 = abs(ratio_branca_4mais - ratio_parda_050a075),
          dif_branco_4mais_pardo_075a125 = abs(ratio_branca_4mais - ratio_parda_075a125),
          dif_branco_4mais_pardo_125a4 = abs(ratio_branca_4mais - ratio_parda_125a4),
          dif_branco_4mais_pardo_4mais = abs(ratio_branca_4mais - ratio_parda_4mais),

          ## Pretos - 0a25
          # x pretos
          dif_preto_0a025_preto_025a050 = abs(ratio_preta_0a025 - ratio_preta_025a050),
          dif_preto_0a025_preto_050a075 = abs(ratio_preta_0a025 - ratio_preta_050a075),
          dif_preto_0a025_preto_075a125 = abs(ratio_preta_0a025 - ratio_preta_075a125),
          dif_preto_0a025_preto_125a4 = abs(ratio_preta_0a025 - ratio_preta_125a4),
          dif_preto_0a025_preto_4mais = abs(ratio_preta_0a025 - ratio_preta_4mais),
          # x pardos
          dif_preto_0a025_pardo_0a025 = abs(ratio_preta_0a025 - ratio_parda_0a025),
          dif_preto_0a025_pardo_025a050 = abs(ratio_preta_0a025 - ratio_parda_025a050),
          dif_preto_0a025_pardo_050a075 = abs(ratio_preta_0a025 - ratio_parda_050a075),
          dif_preto_0a025_pardo_075a125 = abs(ratio_preta_0a025 - ratio_parda_075a125),
          dif_preto_0a025_pardo_125a4 = abs(ratio_preta_0a025 - ratio_parda_125a4),
          dif_preto_0a025_pardo_4mais = abs(ratio_preta_0a025 - ratio_parda_4mais),

          ## Pretos - 025a050
          # x pretos
          dif_preto_025a050_preto_050a075 = abs(ratio_preta_025a050 - ratio_preta_050a075),
          dif_preto_025a050_preto_075a125 = abs(ratio_preta_025a050 - ratio_preta_075a125),
          dif_preto_025a050_preto_125a4 = abs(ratio_preta_025a050 - ratio_preta_125a4),
          dif_preto_025a050_preto_4mais = abs(ratio_preta_025a050 - ratio_preta_4mais),
          # x pardos
          dif_preto_025a050_pardo_0a025 = abs(ratio_preta_025a050 - ratio_parda_0a025),
          dif_preto_025a050_pardo_025a050 = abs(ratio_preta_025a050 - ratio_parda_025a050),
          dif_preto_025a050_pardo_050a075 = abs(ratio_preta_025a050 - ratio_parda_050a075),
          dif_preto_025a050_pardo_075a125 = abs(ratio_preta_025a050 - ratio_parda_075a125),
          dif_preto_025a050_pardo_125a4 = abs(ratio_preta_025a050 - ratio_parda_125a4),
          dif_preto_025a050_pardo_4mais = abs(ratio_preta_025a050 - ratio_parda_4mais),

          ## Pretos - 050a075
          # x pretos
          dif_preto_050a075_preto_075a125 = abs(ratio_preta_050a075 - ratio_preta_075a125),
          dif_preto_050a075_preto_125a4 = abs(ratio_preta_025a050 - ratio_preta_125a4),
          dif_preto_050a075_preto_4mais = abs(ratio_preta_025a050 - ratio_preta_4mais),
          # x pardos
          dif_preto_050a075_pardo_0a025 = abs(ratio_preta_025a050 - ratio_parda_0a025),
          dif_preto_050a075_pardo_025a050 = abs(ratio_preta_025a050 - ratio_parda_025a050),
          dif_preto_050a075_pardo_050a075 = abs(ratio_preta_025a050 - ratio_parda_050a075),
          dif_preto_050a075_pardo_075a125 = abs(ratio_preta_025a050 - ratio_parda_075a125),
          dif_preto_050a075_pardo_125a4 = abs(ratio_preta_025a050 - ratio_parda_125a4),
          dif_preto_050a075_pardo_4mais = abs(ratio_preta_025a050 - ratio_parda_4mais),

          ## Pretos - 075a125
          # x pretos
          dif_preto_075a125_preto_125a4 = abs(ratio_preta_075a125 - ratio_preta_125a4),
          dif_preto_075a125_preto_4mais = abs(ratio_preta_075a125 - ratio_preta_4mais),
          # x pardos
          dif_preto_075a125_pardo_0a025 = abs(ratio_preta_075a125 - ratio_parda_0a025),
          dif_preto_075a125_pardo_025a050 = abs(ratio_preta_075a125 - ratio_parda_025a050),
          dif_preto_075a125_pardo_050a075 = abs(ratio_preta_075a125 - ratio_parda_050a075),
          dif_preto_075a125_pardo_075a125 = abs(ratio_preta_075a125 - ratio_parda_075a125),
          dif_preto_075a125_pardo_125a4 = abs(ratio_preta_075a125 - ratio_parda_125a4),
          dif_preto_075a125_pardo_4mais = abs(ratio_preta_075a125 - ratio_parda_4mais),

          ## Pretos - 125a4
          # x pretos
          dif_preto_125a4_preto_4mais = abs(ratio_preta_125a4 - ratio_preta_4mais),
          # x pardos
          dif_preto_125a4_pardo_0a025 = abs(ratio_preta_125a4 - ratio_parda_0a025),
          dif_preto_125a4_pardo_025a050 = abs(ratio_preta_125a4 - ratio_parda_025a050),
          dif_preto_125a4_pardo_050a075 = abs(ratio_preta_125a4 - ratio_parda_050a075),
          dif_preto_125a4_pardo_075a125 = abs(ratio_preta_125a4 - ratio_parda_075a125),
          dif_preto_125a4_pardo_125a4 = abs(ratio_preta_125a4 - ratio_parda_125a4),
          dif_preto_125a4_pardo_4mais = abs(ratio_preta_125a4 - ratio_parda_4mais),

          ## Pretos - 4mais
          # x pardos
          dif_preto_4mais_pardo_0a025 = abs(ratio_preta_4mais - ratio_parda_0a025),
          dif_preto_4mais_pardo_025a050 = abs(ratio_preta_4mais - ratio_parda_025a050),
          dif_preto_4mais_pardo_050a075 = abs(ratio_preta_4mais - ratio_parda_050a075),
          dif_preto_4mais_pardo_075a125 = abs(ratio_preta_4mais - ratio_parda_075a125),
          dif_preto_4mais_pardo_125a4 = abs(ratio_preta_4mais - ratio_parda_125a4),
          dif_preto_4mais_pardo_4mais = abs(ratio_preta_4mais - ratio_parda_4mais),

          ## Pardos - 0a025
          # x pardos
          dif_pardo_0a025_pardo_025a050 = abs(ratio_parda_0a025 - ratio_parda_025a050),
          dif_pardo_0a025_pardo_050a075 = abs(ratio_parda_0a025 - ratio_parda_050a075),
          dif_pardo_0a025_pardo_075a125 = abs(ratio_parda_0a025 - ratio_parda_075a125),
          dif_pardo_0a025_pardo_125a4 = abs(ratio_parda_0a025 - ratio_parda_125a4),
          dif_pardo_0a025_pardo_4mais = abs(ratio_parda_0a025 - ratio_parda_4mais),

          ## Pardos - 025a050
          # x pardos
          dif_pardo_025a050_pardo_050a075 = abs(ratio_parda_025a050 - ratio_parda_050a075),
          dif_pardo_025a050_pardo_075a125 = abs(ratio_parda_025a050 - ratio_parda_075a125),
          dif_pardo_025a050_pardo_125a4 = abs(ratio_parda_025a050 - ratio_parda_125a4),
          dif_pardo_025a050_pardo_4mais = abs(ratio_parda_025a050 - ratio_parda_4mais),

          ## Pardos - 050a075
          # x pardos
          dif_pardo_050a075_pardo_075a125 = abs(ratio_parda_050a075 - ratio_parda_075a125),
          dif_pardo_050a075_pardo_125a4 = abs(ratio_parda_050a075 - ratio_parda_125a4),
          dif_pardo_050a075_pardo_4mais = abs(ratio_parda_050a075 - ratio_parda_4mais),

          ## Pardos - 075a125
          # x pardos
          dif_pardo_075a125_pardo_125a4 = abs(ratio_parda_075a125 - ratio_parda_125a4),
          dif_pardo_075a125_pardo_4mais = abs(ratio_parda_075a125 - ratio_parda_4mais),

          ## Pardos - 125a4
          # x pardos
          dif_pardo_125a4_pardo_4mais = abs(ratio_parda_125a4 - ratio_parda_4mais),

        ) |>
        mutate(across(starts_with("dif_"), ~ replace_na(.x, 0))) |>
        summarise(

          #
          ### Divisao das razoes por 2
          #

          ## Brancos - 0a025
          # x brancos
          D_branco_0a025_branco_025a050 = sum(dif_branco_0a025_branco_025a050) * 0.5,
          D_branco_0a025_branco_050a075 = sum(dif_branco_0a025_branco_050a075) * 0.5,
          D_branco_0a025_branco_075a125 = sum(dif_branco_0a025_branco_075a125) * 0.5,
          D_branco_0a025_branco_125a4 = sum(dif_branco_0a025_branco_125a4) * 0.5,
          D_branco_0a025_branco_4mais = sum(dif_branco_0a025_branco_4mais) * 0.5,
          # x pretos
          D_branco_0a025_preto_0a025 = sum(dif_branco_0a025_preto_0a025) * 0.5,
          D_branco_0a025_preto_025a050 = sum(dif_branco_0a025_preto_025a050) * 0.5,
          D_branco_0a025_preto_050a075 = sum(dif_branco_0a025_preto_050a075) * 0.5,
          D_branco_0a025_preto_075a125 = sum(dif_branco_0a025_preto_075a125) * 0.5,
          D_branco_0a025_preto_125a4 = sum(dif_branco_0a025_preto_125a4) * 0.5,
          D_branco_0a025_preto_4mais = sum(dif_branco_0a025_preto_4mais) * 0.5,
          # x pardos
          D_branco_0a025_pardo_0a025 = sum(dif_branco_0a025_pardo_0a025) * 0.5,
          D_branco_0a025_pardo_025a050 = sum(dif_branco_0a025_pardo_025a050) * 0.5,
          D_branco_0a025_pardo_050a075 = sum(dif_branco_0a025_pardo_050a075) * 0.5,
          D_branco_0a025_pardo_075a125 = sum(dif_branco_0a025_pardo_075a125) * 0.5,
          D_branco_0a025_pardo_125a4 = sum(dif_branco_0a025_pardo_125a4) * 0.5,
          D_branco_0a025_pardo_4mais = sum(dif_branco_0a025_pardo_4mais) * 0.5,

          ## Brancos - 025a050
          # x brancos
          D_branco_025a050_branco_050a075 = sum(dif_branco_025a050_branco_050a075) * 0.5,
          D_branco_025a050_branco_075a125 = sum(dif_branco_025a050_branco_075a125) * 0.5,
          D_branco_025a050_branco_125a4 = sum(dif_branco_025a050_branco_125a4) * 0.5,
          D_branco_025a050_branco_4mais = sum(dif_branco_025a050_branco_4mais) * 0.5,
          # x pretos
          D_branco_025a050_preto_0a025 = sum(dif_branco_025a050_preto_0a025) * 0.5,
          D_branco_025a050_preto_025a050 = sum(dif_branco_025a050_preto_025a050) * 0.5,
          D_branco_025a050_preto_050a075 = sum(dif_branco_025a050_preto_050a075) * 0.5,
          D_branco_025a050_preto_075a125 = sum(dif_branco_025a050_preto_075a125) * 0.5,
          D_branco_025a050_preto_125a4 = sum(dif_branco_025a050_preto_125a4) * 0.5,
          D_branco_025a050_preto_4mais = sum(dif_branco_025a050_preto_4mais) * 0.5,
          # x pardos
          D_branco_025a050_pardo_0a025 = sum(dif_branco_025a050_pardo_0a025) * 0.5,
          D_branco_025a050_pardo_025a050 = sum(dif_branco_025a050_pardo_025a050) * 0.5,
          D_branco_025a050_pardo_050a075 = sum(dif_branco_025a050_pardo_050a075) * 0.5,
          D_branco_025a050_pardo_075a125 = sum(dif_branco_025a050_pardo_075a125) * 0.5,
          D_branco_025a050_pardo_125a4 = sum(dif_branco_025a050_pardo_125a4) * 0.5,
          D_branco_025a050_pardo_4mais = sum(dif_branco_025a050_pardo_4mais) * 0.5,

          ## Brancos - 050a075
          # x brancos
          D_branco_050a075_branco_075a125 = sum(dif_branco_050a075_branco_075a125) * 0.5,
          D_branco_050a075_branco_125a4 = sum(dif_branco_050a075_branco_125a4) * 0.5,
          D_branco_050a075_branco_4mais = sum(dif_branco_050a075_branco_4mais) * 0.5,
          # x pretos
          D_branco_050a075_preto_0a025 = sum(dif_branco_050a075_preto_0a025) * 0.5,
          D_branco_050a075_preto_025a050 = sum(dif_branco_050a075_preto_025a050) * 0.5,
          D_branco_050a075_preto_050a075 = sum(dif_branco_050a075_preto_050a075) * 0.5,
          D_branco_050a075_preto_075a125 = sum(dif_branco_050a075_preto_075a125) * 0.5,
          D_branco_050a075_preto_125a4 = sum(dif_branco_050a075_preto_125a4) * 0.5,
          D_branco_050a075_preto_4mais = sum(dif_branco_050a075_preto_4mais) * 0.5,
          # x pardos
          D_branco_050a075_pardo_0a025 = sum(dif_branco_050a075_pardo_0a025) * 0.5,
          D_branco_050a075_pardo_025a050 = sum(dif_branco_050a075_pardo_025a050) * 0.5,
          D_branco_050a075_pardo_050a075 = sum(dif_branco_050a075_pardo_050a075) * 0.5,
          D_branco_050a075_pardo_075a125 = sum(dif_branco_050a075_pardo_075a125) * 0.5,
          D_branco_050a075_pardo_125a4 = sum(dif_branco_050a075_pardo_125a4) * 0.5,
          D_branco_050a075_pardo_4mais = sum(dif_branco_050a075_pardo_4mais) * 0.5,

          ## Brancos - 075a125
          # x brancos
          D_branco_075a125_branco_125a4 = sum(dif_branco_075a125_branco_125a4) * 0.5,
          D_branco_075a125_branco_4mais = sum(dif_branco_075a125_branco_4mais) * 0.5,
          # x pretos
          D_branco_075a125_preto_0a025 = sum(dif_branco_075a125_preto_0a025) * 0.5,
          D_branco_075a125_preto_025a050 = sum(dif_branco_075a125_preto_025a050) * 0.5,
          D_branco_075a125_preto_050a075 = sum(dif_branco_075a125_preto_050a075) * 0.5,
          D_branco_075a125_preto_075a125 = sum(dif_branco_075a125_preto_075a125) * 0.5,
          D_branco_075a125_preto_125a4 = sum(dif_branco_075a125_preto_125a4) * 0.5,
          D_branco_075a125_preto_4mais = sum(dif_branco_075a125_preto_4mais) * 0.5,
          # x pardos
          D_branco_075a125_pardo_0a025 = sum(dif_branco_075a125_pardo_0a025) * 0.5,
          D_branco_075a125_pardo_025a050 = sum(dif_branco_075a125_pardo_025a050) * 0.5,
          D_branco_075a125_pardo_050a075 = sum(dif_branco_075a125_pardo_050a075) * 0.5,
          D_branco_075a125_pardo_075a125 = sum(dif_branco_075a125_pardo_075a125) * 0.5,
          D_branco_075a125_pardo_125a4 = sum(dif_branco_075a125_pardo_125a4) * 0.5,
          D_branco_075a125_pardo_4mais = sum(dif_branco_075a125_pardo_4mais) * 0.5,

          ## Brancos - 125a4
          # x brancos
          D_branco_125a4_branco_4mais = sum(dif_branco_125a4_branco_4mais) * 0.5,
          # x pretos
          D_branco_125a4_preto_0a025 = sum(dif_branco_125a4_preto_0a025) * 0.5,
          D_branco_125a4_preto_025a050 = sum(dif_branco_125a4_preto_025a050) * 0.5,
          D_branco_125a4_preto_050a075 = sum(dif_branco_125a4_preto_050a075) * 0.5,
          D_branco_125a4_preto_075a125 = sum(dif_branco_125a4_preto_075a125) * 0.5,
          D_branco_125a4_preto_125a4 = sum(dif_branco_125a4_preto_125a4) * 0.5,
          D_branco_125a4_preto_4mais = sum(dif_branco_125a4_preto_4mais) * 0.5,
          # x pardos
          D_branco_125a4_pardo_0a025 = sum(dif_branco_125a4_pardo_0a025) * 0.5,
          D_branco_125a4_pardo_025a050 = sum(dif_branco_125a4_pardo_025a050) * 0.5,
          D_branco_125a4_pardo_050a075 = sum(dif_branco_125a4_pardo_050a075) * 0.5,
          D_branco_125a4_pardo_075a125 = sum(dif_branco_125a4_pardo_075a125) * 0.5,
          D_branco_125a4_pardo_125a4 = sum(dif_branco_125a4_pardo_125a4) * 0.5,
          D_branco_125a4_pardo_4mais = sum(dif_branco_125a4_pardo_4mais) * 0.5,

          ## Brancos - 4mais
          # x pretos
          D_branco_4mais_preto_0a025 = sum(dif_branco_4mais_preto_0a025) * 0.5,
          D_branco_4mais_preto_025a050 = sum(dif_branco_4mais_preto_025a050) * 0.5,
          D_branco_4mais_preto_050a075 = sum(dif_branco_4mais_preto_050a075) * 0.5,
          D_branco_4mais_preto_075a125 = sum(dif_branco_4mais_preto_075a125) * 0.5,
          D_branco_4mais_preto_125a4 = sum(dif_branco_4mais_preto_125a4) * 0.5,
          D_branco_4mais_preto_4mais = sum(dif_branco_4mais_preto_4mais) * 0.5,
          # x pardos
          D_branco_4mais_pardo_0a025 = sum(dif_branco_4mais_pardo_0a025) * 0.5,
          D_branco_4mais_pardo_025a050 = sum(dif_branco_4mais_pardo_025a050) * 0.5,
          D_branco_4mais_pardo_050a075 = sum(dif_branco_4mais_pardo_050a075) * 0.5,
          D_branco_4mais_pardo_075a125 = sum(dif_branco_4mais_pardo_075a125) * 0.5,
          D_branco_4mais_pardo_125a4 = sum(dif_branco_4mais_pardo_125a4) * 0.5,
          D_branco_4mais_pardo_4mais = sum(dif_branco_4mais_pardo_4mais) * 0.5,

          ## Pretos - 0a25
          # x pretos
          D_preto_0a025_preto_025a050 = sum(dif_preto_0a025_preto_025a050) * 0.5,
          D_preto_0a025_preto_050a075 = sum(dif_preto_0a025_preto_050a075) * 0.5,
          D_preto_0a025_preto_075a125 = sum(dif_preto_0a025_preto_075a125) * 0.5,
          D_preto_0a025_preto_125a4 = sum(dif_preto_0a025_preto_125a4) * 0.5,
          D_preto_0a025_preto_4mais = sum(dif_preto_0a025_preto_4mais) * 0.5,
          # x pardos
          D_preto_0a025_pardo_0a025 = sum(dif_preto_0a025_pardo_0a025) * 0.5,
          D_preto_0a025_pardo_025a050 = sum(dif_preto_0a025_pardo_025a050) * 0.5,
          D_preto_0a025_pardo_050a075 = sum(dif_preto_0a025_pardo_050a075) * 0.5,
          D_preto_0a025_pardo_075a125 = sum(dif_preto_0a025_pardo_075a125) * 0.5,
          D_preto_0a025_pardo_125a4 = sum(dif_preto_0a025_pardo_125a4) * 0.5,
          D_preto_0a025_pardo_4mais = sum(dif_preto_0a025_pardo_4mais) * 0.5,

          ## Pretos - 025a050
          # x pretos
          D_preto_025a050_preto_050a075 = sum(dif_preto_025a050_preto_050a075) * 0.5,
          D_preto_025a050_preto_075a125 = sum(dif_preto_025a050_preto_075a125) * 0.5,
          D_preto_025a050_preto_125a4 = sum(dif_preto_025a050_preto_125a4) * 0.5,
          D_preto_025a050_preto_4mais = sum(dif_preto_025a050_preto_4mais) * 0.5,
          # x pardos
          D_preto_025a050_pardo_0a025 = sum(dif_preto_025a050_pardo_0a025) * 0.5,
          D_preto_025a050_pardo_025a050 = sum(dif_preto_025a050_pardo_025a050) * 0.5,
          D_preto_025a050_pardo_050a075 = sum(dif_preto_025a050_pardo_050a075) * 0.5,
          D_preto_025a050_pardo_075a125 = sum(dif_preto_025a050_pardo_075a125) * 0.5,
          D_preto_025a050_pardo_125a4 = sum(dif_preto_025a050_pardo_125a4) * 0.5,
          D_preto_025a050_pardo_4mais = sum(dif_preto_025a050_pardo_4mais) * 0.5,

          ## Pretos - 050a075
          # x pretos
          D_preto_050a075_preto_075a125 = sum(dif_preto_050a075_preto_075a125) * 0.5,
          D_preto_050a075_preto_125a4 = sum(dif_preto_050a075_preto_125a4) * 0.5,
          D_preto_050a075_preto_4mais = sum(dif_preto_050a075_preto_4mais) * 0.5,
          # x pardos
          D_preto_050a075_pardo_0a025 = sum(dif_preto_050a075_pardo_0a025) * 0.5,
          D_preto_050a075_pardo_025a050 = sum(dif_preto_050a075_pardo_025a050) * 0.5,
          D_preto_050a075_pardo_050a075 = sum(dif_preto_050a075_pardo_050a075) * 0.5,
          D_preto_050a075_pardo_075a125 = sum(dif_preto_050a075_pardo_075a125) * 0.5,
          D_preto_050a075_pardo_125a4 = sum(dif_preto_050a075_pardo_125a4) * 0.5,
          D_preto_050a075_pardo_4mais = sum(dif_preto_050a075_pardo_4mais) * 0.5,

          ## Pretos - 075a125
          # x pretos
          D_preto_075a125_preto_125a4 = sum(dif_preto_075a125_preto_125a4) * 0.5,
          D_preto_075a125_preto_4mais = sum(dif_preto_075a125_preto_4mais) * 0.5,
          # x pardos
          D_preto_075a125_pardo_0a025 = sum(dif_preto_075a125_pardo_0a025) * 0.5,
          D_preto_075a125_pardo_025a050 = sum(dif_preto_075a125_pardo_025a050) * 0.5,
          D_preto_075a125_pardo_050a075 = sum(dif_preto_075a125_pardo_050a075) * 0.5,
          D_preto_075a125_pardo_075a125 = sum(dif_preto_075a125_pardo_075a125) * 0.5,
          D_preto_075a125_pardo_125a4 = sum(dif_preto_075a125_pardo_125a4) * 0.5,
          D_preto_075a125_pardo_4mais = sum(dif_preto_075a125_pardo_4mais) * 0.5,

          ## Pretos - 125a4
          # x pretos
          D_preto_125a4_preto_4mais = sum(dif_preto_125a4_preto_4mais) * 0.5,
          # x pardos
          D_preto_125a4_pardo_0a025 = sum(dif_preto_125a4_pardo_0a025) * 0.5,
          D_preto_125a4_pardo_025a050 = sum(dif_preto_125a4_pardo_025a050) * 0.5,
          D_preto_125a4_pardo_050a075 = sum(dif_preto_125a4_pardo_050a075) * 0.5,
          D_preto_125a4_pardo_075a125 = sum(dif_preto_125a4_pardo_075a125) * 0.5,
          D_preto_125a4_pardo_125a4 = sum(dif_preto_125a4_pardo_125a4) * 0.5,
          D_preto_125a4_pardo_4mais = sum(dif_preto_125a4_pardo_4mais) * 0.5,

          ## Pretos - 4mais
          # x pardos
          D_preto_4mais_pardo_0a025 = sum(dif_preto_4mais_pardo_0a025) * 0.5,
          D_preto_4mais_pardo_025a050 = sum(dif_preto_4mais_pardo_025a050) * 0.5,
          D_preto_4mais_pardo_050a075 = sum(dif_preto_4mais_pardo_050a075) * 0.5,
          D_preto_4mais_pardo_075a125 = sum(dif_preto_4mais_pardo_075a125) * 0.5,
          D_preto_4mais_pardo_125a4 = sum(dif_preto_4mais_pardo_125a4) * 0.5,
          D_preto_4mais_pardo_4mais = sum(dif_preto_4mais_pardo_4mais) * 0.5,

          ## Pardos - 0a025
          # x pardos
          D_pardo_0a025_pardo_025a050 = sum(dif_pardo_0a025_pardo_025a050) * 0.5,
          D_pardo_0a025_pardo_050a075 = sum(dif_pardo_0a025_pardo_050a075) * 0.5,
          D_pardo_0a025_pardo_075a125 = sum(dif_pardo_0a025_pardo_075a125) * 0.5,
          D_pardo_0a025_pardo_125a4 = sum(dif_pardo_0a025_pardo_125a4) * 0.5,
          D_pardo_0a025_pardo_4mais = sum(dif_pardo_0a025_pardo_4mais) * 0.5,

          ## Pardos - 025a050
          # x pardos
          D_pardo_025a050_pardo_050a075 = sum(dif_pardo_025a050_pardo_050a075) * 0.5,
          D_pardo_025a050_pardo_075a125 = sum(dif_pardo_025a050_pardo_075a125) * 0.5,
          D_pardo_025a050_pardo_125a4 = sum(dif_pardo_025a050_pardo_125a4) * 0.5,
          D_pardo_025a050_pardo_4mais = sum(dif_pardo_025a050_pardo_4mais) * 0.5,

          ## Pardos - 050a075
          # x pardos
          D_pardo_050a075_pardo_075a125 = sum(dif_pardo_050a075_pardo_075a125) * 0.5,
          D_pardo_050a075_pardo_125a4 = sum(dif_pardo_050a075_pardo_125a4) * 0.5,
          D_pardo_050a075_pardo_4mais = sum(dif_pardo_050a075_pardo_4mais) * 0.5,

          ## Pardos - 075a125
          # x pardos
          D_pardo_075a125_pardo_125a4 = sum(dif_pardo_075a125_pardo_125a4) * 0.5,
          D_pardo_075a125_pardo_4mais = sum(dif_pardo_075a125_pardo_4mais) * 0.5,

          ## Pardos - 125a4
          # x pardos
          D_pardo_125a4_pardo_4mais = sum(dif_pardo_125a4_pardo_4mais) * 0.5,

        ) |>
        pivot_longer(D_branco_0a025_branco_025a050:D_pardo_125a4_pardo_4mais, names_to = "grupo", values_to = "D") |>
        mutate(
          cor_classe1 = c(
            rep("Brancos - [0 a 0,25 SM)",17),rep("Brancos - [0,25 a 0,5 SM)",16),rep("Brancos - [0,5 a 0,75 SM",15),
            rep("Brancos - [0,75 a 1,25 SM)",14),rep("Brancos - [1,25 a 4 SM)",13),rep("Brancos - [4+ SM)",12),
            rep("Pretos - [0 a 0,25 SM)",11),rep("Pretos - [0,25 a 0,5 SM)",10),rep("Pretos - [0,5 a 0,75 SM",9),
            rep("Pretos - [0,75 a 1,25 SM)",8),rep("Pretos - [1,25 a 4 SM)",7),rep("Pretos - [4+ SM)",6),
            rep("Pardos - [0 a 0,25 SM)",5),rep("Pardos - [0,25 a 0,5 SM)",4),rep("Pardos - [0,5 a 0,75 SM",3),
            rep("Pardos - [0,75 a 1,25 SM)",2),rep("Pardos - [1,25 a 4 SM)",1)
          ),
          cor_classe2 = c(
            c("Brancos - [0,25 a 0,5 SM)","Brancos - [0,5 a 0,75 SM)","Brancos - [0,75 a 1,25 SM)",
              "Brancos - [1,25 a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [0,5 a 0,75 SM)","Brancos - [0,75 a 1,25 SM)",
              "Brancos - [1,25 a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [0,75 a 1,25 SM)","Brancos - [1,25 a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [1,25 a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0 a 0,25 SM)","Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0,25 a 0,5 SM)","Pretos - [0,5 a 0,75 SM)",
              "Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0,5 a 0,75 SM)","Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0,75 a 1,25 SM)","Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [1,25 a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0 a 0,25 SM)","Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0,25 a 0,5 SM)","Pardos - [0,5 a 0,75 SM)",
              "Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0,5 a 0,75 SM)","Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0,75 a 1,25 SM)","Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [1,25 a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [4+ SM)")
          )
        )
    }
    if(tipo_variavel == "SM_SUL"){
      output_classe <- tabela_por_classe |>
        select(-n_se) |>
        mutate(
          # brancos
          pop_branca_0a050 = n[classe_raca == "Brancos - [0 a 0,5 SM)" & area_ponderacao == 0],
          pop_branca_050a125 = n[classe_raca == "Brancos - [0,5 a 1,25 SM)" & area_ponderacao == 0],
          pop_branca_125a2 = n[classe_raca == "Brancos - [1,25 SM a 2 SM)" & area_ponderacao == 0],
          pop_branca_2a4 = n[classe_raca == "Brancos - [2 SM a 4 SM)" & area_ponderacao == 0],
          pop_branca_4mais = n[classe_raca == "Brancos - [4+ SM)" & area_ponderacao == 0],

          # pretos
          pop_preta_0a050 = n[classe_raca == "Pretos - [0 a 0,5 SM)" & area_ponderacao == 0],
          pop_preta_050a125 = n[classe_raca == "Pretos - [0,5 a 1,25 SM)" & area_ponderacao == 0],
          pop_preta_125a2 = n[classe_raca == "Pretos - [1,25 SM a 2 SM)" & area_ponderacao == 0],
          pop_preta_2a4 = n[classe_raca == "Pretos - [2 SM a 4 SM)" & area_ponderacao == 0],
          pop_preta_4mais = n[classe_raca == "Pretos - [4+ SM)" & area_ponderacao == 0],

          # pardos
          pop_parda_0a050 = n[classe_raca == "Pardos - [0 a 0,5 SM)" & area_ponderacao == 0],
          pop_parda_050a125 = n[classe_raca == "Pardos - [0,5 a 1,25 SM)" & area_ponderacao == 0],
          pop_parda_125a2 = n[classe_raca == "Pardos - [1,25 SM a 2 SM)" & area_ponderacao == 0],
          pop_parda_2a4 = n[classe_raca == "Pardos - [2 SM a 4 SM)" & area_ponderacao == 0],
          pop_parda_4mais = n[classe_raca == "Pardos - [4+ SM)" & area_ponderacao == 0],
        ) |>
        pivot_wider(names_from = classe_raca, values_from = n) |>
        filter(area_ponderacao != 0) |>
        mutate(
          ### Razoes de cada grupo

          # brancos
          ratio_branca_0a050 = `Brancos - [0 a 0,5 SM)`/pop_branca_0a050,
          ratio_branca_050a125 = `Brancos - [0,5 a 1,25 SM)`/pop_branca_050a125,
          ratio_branca_125a2 = `Brancos - [1,25 SM a 2 SM)`/pop_branca_125a2,
          ratio_branca_2a4 = `Brancos - [2 SM a 4 SM)`/pop_branca_2a4,
          ratio_branca_4mais = `Brancos - [4+ SM)`/pop_branca_4mais,

          # pretos
          ratio_preta_0a050 = `Pretos - [0 a 0,5 SM)`/pop_preta_0a050,
          ratio_preta_050a125 = `Pretos - [0,5 a 1,25 SM)`/pop_preta_050a125,
          ratio_preta_125a2 = `Pretos - [1,25 SM a 2 SM)`/pop_preta_125a2,
          ratio_preta_2a4 = `Pretos - [2 SM a 4 SM)`/pop_preta_2a4,
          ratio_preta_4mais = `Pretos - [4+ SM)`/pop_preta_4mais,

          # pardos
          ratio_parda_0a050 = `Pardos - [0 a 0,5 SM)`/pop_parda_0a050,
          ratio_parda_050a125 = `Pardos - [0,5 a 1,25 SM)`/pop_parda_050a125,
          ratio_parda_125a2 = `Pardos - [1,25 SM a 2 SM)`/pop_parda_125a2,
          ratio_parda_2a4 = `Pardos - [2 SM a 4 SM)`/pop_parda_2a4,
          ratio_parda_4mais = `Pardos - [4+ SM)`/pop_parda_4mais,



          #
          ### Diferenca das razoes
          #

          ## Brancos - 0a050
          # x brancos
          dif_branco_0a050_branco_050a125 = abs(ratio_branca_0a050 - ratio_branca_050a125),
          dif_branco_0a050_branco_125a2 = abs(ratio_branca_0a050 - ratio_branca_125a2),
          dif_branco_0a050_branco_2a4 = abs(ratio_branca_0a050 - ratio_branca_2a4),
          dif_branco_0a050_branco_4mais = abs(ratio_branca_0a050 - ratio_branca_4mais),
          # x pretos
          dif_branco_0a050_preto_0a050 = abs(ratio_branca_0a050 - ratio_preta_0a050),
          dif_branco_0a050_preto_050a125 = abs(ratio_branca_0a050 - ratio_preta_050a125),
          dif_branco_0a050_preto_125a2 = abs(ratio_branca_0a050 - ratio_preta_125a2),
          dif_branco_0a050_preto_2a4 = abs(ratio_branca_0a050 - ratio_preta_2a4),
          dif_branco_0a050_preto_4mais = abs(ratio_branca_0a050 - ratio_preta_4mais),
          # x pardos
          dif_branco_0a050_pardo_0a050 = abs(ratio_branca_0a050 - ratio_parda_0a050),
          dif_branco_0a050_pardo_050a125 = abs(ratio_branca_0a050 - ratio_parda_050a125),
          dif_branco_0a050_pardo_125a2 = abs(ratio_branca_0a050 - ratio_parda_125a2),
          dif_branco_0a050_pardo_2a4 = abs(ratio_branca_0a050 - ratio_parda_2a4),
          dif_branco_0a050_pardo_4mais = abs(ratio_branca_0a050 - ratio_parda_4mais),

          ## Brancos - 050a125
          # x brancos
          dif_branco_050a125_branco_125a2 = abs(ratio_branca_050a125 - ratio_branca_125a2),
          dif_branco_050a125_branco_2a4 = abs(ratio_branca_050a125 - ratio_branca_2a4),
          dif_branco_050a125_branco_4mais = abs(ratio_branca_050a125 - ratio_branca_4mais),
          # x pretos
          dif_branco_050a125_preto_0a050 = abs(ratio_branca_050a125 - ratio_preta_0a050),
          dif_branco_050a125_preto_050a125 = abs(ratio_branca_050a125 - ratio_preta_050a125),
          dif_branco_050a125_preto_125a2 = abs(ratio_branca_050a125 - ratio_preta_125a2),
          dif_branco_050a125_preto_2a4 = abs(ratio_branca_050a125 - ratio_preta_2a4),
          dif_branco_050a125_preto_4mais = abs(ratio_branca_050a125 - ratio_preta_4mais),
          # x pardos
          dif_branco_050a125_pardo_0a050 = abs(ratio_branca_050a125 - ratio_parda_0a050),
          dif_branco_050a125_pardo_050a125 = abs(ratio_branca_050a125 - ratio_parda_050a125),
          dif_branco_050a125_pardo_125a2 = abs(ratio_branca_050a125 - ratio_parda_125a2),
          dif_branco_050a125_pardo_2a4 = abs(ratio_branca_050a125 - ratio_parda_2a4),
          dif_branco_050a125_pardo_4mais = abs(ratio_branca_050a125 - ratio_parda_4mais),

          ## Brancos - 125a2
          # x brancos
          dif_branco_125a2_branco_2a4 = abs(ratio_branca_125a2 - ratio_branca_2a4),
          dif_branco_125a2_branco_4mais = abs(ratio_branca_125a2 - ratio_branca_4mais),
          # x pretos
          dif_branco_125a2_preto_0a050 = abs(ratio_branca_125a2 - ratio_preta_0a050),
          dif_branco_125a2_preto_050a125 = abs(ratio_branca_125a2 - ratio_preta_050a125),
          dif_branco_125a2_preto_125a2 = abs(ratio_branca_125a2 - ratio_preta_125a2),
          dif_branco_125a2_preto_2a4 = abs(ratio_branca_125a2 - ratio_preta_2a4),
          dif_branco_125a2_preto_4mais = abs(ratio_branca_125a2 - ratio_preta_4mais),
          # x pardos
          dif_branco_125a2_pardo_0a050 = abs(ratio_branca_125a2 - ratio_parda_0a050),
          dif_branco_125a2_pardo_050a125 = abs(ratio_branca_125a2 - ratio_parda_050a125),
          dif_branco_125a2_pardo_125a2 = abs(ratio_branca_125a2 - ratio_parda_125a2),
          dif_branco_125a2_pardo_2a4 = abs(ratio_branca_125a2 - ratio_parda_2a4),
          dif_branco_125a2_pardo_4mais = abs(ratio_branca_125a2 - ratio_parda_4mais),

          ## Brancos - 2a4
          # x brancos
          dif_branco_2a4_branco_4mais = abs(ratio_branca_2a4 - ratio_branca_4mais),
          # x pretos
          dif_branco_2a4_preto_0a050 = abs(ratio_branca_2a4 - ratio_preta_0a050),
          dif_branco_2a4_preto_050a125 = abs(ratio_branca_2a4 - ratio_preta_050a125),
          dif_branco_2a4_preto_125a2 = abs(ratio_branca_2a4 - ratio_preta_125a2),
          dif_branco_2a4_preto_2a4 = abs(ratio_branca_2a4 - ratio_preta_2a4),
          dif_branco_2a4_preto_4mais = abs(ratio_branca_2a4 - ratio_preta_4mais),
          # x pardos
          dif_branco_2a4_pardo_0a050 = abs(ratio_branca_2a4 - ratio_parda_0a050),
          dif_branco_2a4_pardo_050a125 = abs(ratio_branca_2a4 - ratio_parda_050a125),
          dif_branco_2a4_pardo_125a2 = abs(ratio_branca_2a4 - ratio_parda_125a2),
          dif_branco_2a4_pardo_2a4 = abs(ratio_branca_2a4 - ratio_parda_2a4),
          dif_branco_2a4_pardo_4mais = abs(ratio_branca_2a4 - ratio_parda_4mais),

          ## Brancos - 4mais
          # x pretos
          dif_branco_4mais_preto_0a050 = abs(ratio_branca_4mais - ratio_preta_0a050),
          dif_branco_4mais_preto_050a125 = abs(ratio_branca_4mais - ratio_preta_050a125),
          dif_branco_4mais_preto_125a2 = abs(ratio_branca_4mais - ratio_preta_125a2),
          dif_branco_4mais_preto_2a4 = abs(ratio_branca_4mais - ratio_preta_2a4),
          dif_branco_4mais_preto_4mais = abs(ratio_branca_4mais - ratio_preta_4mais),
          # x pardos
          dif_branco_4mais_pardo_0a050 = abs(ratio_branca_4mais - ratio_parda_0a050),
          dif_branco_4mais_pardo_050a125 = abs(ratio_branca_4mais - ratio_parda_050a125),
          dif_branco_4mais_pardo_125a2 = abs(ratio_branca_4mais - ratio_parda_125a2),
          dif_branco_4mais_pardo_2a4 = abs(ratio_branca_4mais - ratio_parda_2a4),
          dif_branco_4mais_pardo_4mais = abs(ratio_branca_4mais - ratio_parda_4mais),

          ## Pretos - 0a25
          # x pretos
          dif_preto_0a050_preto_050a125 = abs(ratio_preta_0a050 - ratio_preta_050a125),
          dif_preto_0a050_preto_125a2 = abs(ratio_preta_0a050 - ratio_preta_125a2),
          dif_preto_0a050_preto_2a4 = abs(ratio_preta_0a050 - ratio_preta_2a4),
          dif_preto_0a050_preto_4mais = abs(ratio_preta_0a050 - ratio_preta_4mais),
          # x pardos
          dif_preto_0a050_pardo_0a050 = abs(ratio_preta_0a050 - ratio_parda_0a050),
          dif_preto_0a050_pardo_050a125 = abs(ratio_preta_0a050 - ratio_parda_050a125),
          dif_preto_0a050_pardo_125a2 = abs(ratio_preta_0a050 - ratio_parda_125a2),
          dif_preto_0a050_pardo_2a4 = abs(ratio_preta_0a050 - ratio_parda_2a4),
          dif_preto_0a050_pardo_4mais = abs(ratio_preta_0a050 - ratio_parda_4mais),

          ## Pretos - 050a125
          # x pretos
          dif_preto_050a125_preto_125a2 = abs(ratio_preta_050a125 - ratio_preta_125a2),
          dif_preto_050a125_preto_2a4 = abs(ratio_preta_050a125 - ratio_preta_2a4),
          dif_preto_050a125_preto_4mais = abs(ratio_preta_050a125 - ratio_preta_4mais),
          # x pardos
          dif_preto_050a125_pardo_0a050 = abs(ratio_preta_050a125 - ratio_parda_0a050),
          dif_preto_050a125_pardo_050a125 = abs(ratio_preta_050a125 - ratio_parda_050a125),
          dif_preto_050a125_pardo_125a2 = abs(ratio_preta_050a125 - ratio_parda_125a2),
          dif_preto_050a125_pardo_2a4 = abs(ratio_preta_050a125 - ratio_parda_2a4),
          dif_preto_050a125_pardo_4mais = abs(ratio_preta_050a125 - ratio_parda_4mais),

          ## Pretos - 125a2
          # x pretos
          dif_preto_125a2_preto_2a4 = abs(ratio_preta_050a125 - ratio_preta_2a4),
          dif_preto_125a2_preto_4mais = abs(ratio_preta_050a125 - ratio_preta_4mais),
          # x pardos
          dif_preto_125a2_pardo_0a050 = abs(ratio_preta_050a125 - ratio_parda_0a050),
          dif_preto_125a2_pardo_050a125 = abs(ratio_preta_050a125 - ratio_parda_050a125),
          dif_preto_125a2_pardo_125a2 = abs(ratio_preta_050a125 - ratio_parda_125a2),
          dif_preto_125a2_pardo_2a4 = abs(ratio_preta_050a125 - ratio_parda_2a4),
          dif_preto_125a2_pardo_4mais = abs(ratio_preta_050a125 - ratio_parda_4mais),

          ## Pretos - 2a4
          # x pretos
          dif_preto_2a4_preto_4mais = abs(ratio_preta_2a4 - ratio_preta_4mais),
          # x pardos
          dif_preto_2a4_pardo_0a050 = abs(ratio_preta_2a4 - ratio_parda_0a050),
          dif_preto_2a4_pardo_050a125 = abs(ratio_preta_2a4 - ratio_parda_050a125),
          dif_preto_2a4_pardo_125a2 = abs(ratio_preta_2a4 - ratio_parda_125a2),
          dif_preto_2a4_pardo_2a4 = abs(ratio_preta_2a4 - ratio_parda_2a4),
          dif_preto_2a4_pardo_4mais = abs(ratio_preta_2a4 - ratio_parda_4mais),

          ## Pretos - 4mais
          # x pardos
          dif_preto_4mais_pardo_0a050 = abs(ratio_preta_4mais - ratio_parda_0a050),
          dif_preto_4mais_pardo_050a125 = abs(ratio_preta_4mais - ratio_parda_050a125),
          dif_preto_4mais_pardo_125a2 = abs(ratio_preta_4mais - ratio_parda_125a2),
          dif_preto_4mais_pardo_2a4 = abs(ratio_preta_4mais - ratio_parda_2a4),
          dif_preto_4mais_pardo_4mais = abs(ratio_preta_4mais - ratio_parda_4mais),

          ## Pardos - 0a050
          # x pardos
          dif_pardo_0a050_pardo_050a125 = abs(ratio_parda_0a050 - ratio_parda_050a125),
          dif_pardo_0a050_pardo_125a2 = abs(ratio_parda_0a050 - ratio_parda_125a2),
          dif_pardo_0a050_pardo_2a4 = abs(ratio_parda_0a050 - ratio_parda_2a4),
          dif_pardo_0a050_pardo_4mais = abs(ratio_parda_0a050 - ratio_parda_4mais),

          ## Pardos - 050a125
          # x pardos
          dif_pardo_050a125_pardo_125a2 = abs(ratio_parda_050a125 - ratio_parda_125a2),
          dif_pardo_050a125_pardo_2a4 = abs(ratio_parda_050a125 - ratio_parda_2a4),
          dif_pardo_050a125_pardo_4mais = abs(ratio_parda_050a125 - ratio_parda_4mais),

          ## Pardos - 125a2
          # x pardos
          dif_pardo_125a2_pardo_2a4 = abs(ratio_parda_125a2 - ratio_parda_2a4),
          dif_pardo_125a2_pardo_4mais = abs(ratio_parda_125a2 - ratio_parda_4mais),

          ## Pardos - 2a4
          # x pardos
          dif_pardo_2a4_pardo_4mais = abs(ratio_parda_2a4 - ratio_parda_4mais),

        ) |>
        mutate(across(starts_with("dif_"), ~ replace_na(.x, 0))) |>
        summarise(

          #
          ### Divisao das razoes por 2
          #

          ## Brancos - 0a050
          # x brancos
          D_branco_0a050_branco_050a125 = sum(dif_branco_0a050_branco_050a125) * 0.5,
          D_branco_0a050_branco_125a2 = sum(dif_branco_0a050_branco_125a2) * 0.5,
          D_branco_0a050_branco_2a4 = sum(dif_branco_0a050_branco_2a4) * 0.5,
          D_branco_0a050_branco_4mais = sum(dif_branco_0a050_branco_4mais) * 0.5,
          # x pretos
          D_branco_0a050_preto_0a050 = sum(dif_branco_0a050_preto_0a050) * 0.5,
          D_branco_0a050_preto_050a125 = sum(dif_branco_0a050_preto_050a125) * 0.5,
          D_branco_0a050_preto_125a2 = sum(dif_branco_0a050_preto_125a2) * 0.5,
          D_branco_0a050_preto_2a4 = sum(dif_branco_0a050_preto_2a4) * 0.5,
          D_branco_0a050_preto_4mais = sum(dif_branco_0a050_preto_4mais) * 0.5,
          # x pardos
          D_branco_0a050_pardo_0a050 = sum(dif_branco_0a050_pardo_0a050) * 0.5,
          D_branco_0a050_pardo_050a125 = sum(dif_branco_0a050_pardo_050a125) * 0.5,
          D_branco_0a050_pardo_125a2 = sum(dif_branco_0a050_pardo_125a2) * 0.5,
          D_branco_0a050_pardo_2a4 = sum(dif_branco_0a050_pardo_2a4) * 0.5,
          D_branco_0a050_pardo_4mais = sum(dif_branco_0a050_pardo_4mais) * 0.5,

          ## Brancos - 050a125
          # x brancos
          D_branco_050a125_branco_125a2 = sum(dif_branco_050a125_branco_125a2) * 0.5,
          D_branco_050a125_branco_2a4 = sum(dif_branco_050a125_branco_2a4) * 0.5,
          D_branco_050a125_branco_4mais = sum(dif_branco_050a125_branco_4mais) * 0.5,
          # x pretos
          D_branco_050a125_preto_0a050 = sum(dif_branco_050a125_preto_0a050) * 0.5,
          D_branco_050a125_preto_050a125 = sum(dif_branco_050a125_preto_050a125) * 0.5,
          D_branco_050a125_preto_125a2 = sum(dif_branco_050a125_preto_125a2) * 0.5,
          D_branco_050a125_preto_2a4 = sum(dif_branco_050a125_preto_2a4) * 0.5,
          D_branco_050a125_preto_4mais = sum(dif_branco_050a125_preto_4mais) * 0.5,
          # x pardos
          D_branco_050a125_pardo_0a050 = sum(dif_branco_050a125_pardo_0a050) * 0.5,
          D_branco_050a125_pardo_050a125 = sum(dif_branco_050a125_pardo_050a125) * 0.5,
          D_branco_050a125_pardo_125a2 = sum(dif_branco_050a125_pardo_125a2) * 0.5,
          D_branco_050a125_pardo_2a4 = sum(dif_branco_050a125_pardo_2a4) * 0.5,
          D_branco_050a125_pardo_4mais = sum(dif_branco_050a125_pardo_4mais) * 0.5,

          ## Brancos - 125a2
          # x brancos
          D_branco_125a2_branco_2a4 = sum(dif_branco_125a2_branco_2a4) * 0.5,
          D_branco_125a2_branco_4mais = sum(dif_branco_125a2_branco_4mais) * 0.5,
          # x pretos
          D_branco_125a2_preto_0a050 = sum(dif_branco_125a2_preto_0a050) * 0.5,
          D_branco_125a2_preto_050a125 = sum(dif_branco_125a2_preto_050a125) * 0.5,
          D_branco_125a2_preto_125a2 = sum(dif_branco_125a2_preto_125a2) * 0.5,
          D_branco_125a2_preto_2a4 = sum(dif_branco_125a2_preto_2a4) * 0.5,
          D_branco_125a2_preto_4mais = sum(dif_branco_125a2_preto_4mais) * 0.5,
          # x pardos
          D_branco_125a2_pardo_0a050 = sum(dif_branco_125a2_pardo_0a050) * 0.5,
          D_branco_125a2_pardo_050a125 = sum(dif_branco_125a2_pardo_050a125) * 0.5,
          D_branco_125a2_pardo_125a2 = sum(dif_branco_125a2_pardo_125a2) * 0.5,
          D_branco_125a2_pardo_2a4 = sum(dif_branco_125a2_pardo_2a4) * 0.5,
          D_branco_125a2_pardo_4mais = sum(dif_branco_125a2_pardo_4mais) * 0.5,

          ## Brancos - 2a4
          # x brancos
          D_branco_2a4_branco_4mais = sum(dif_branco_2a4_branco_4mais) * 0.5,
          # x pretos
          D_branco_2a4_preto_0a050 = sum(dif_branco_2a4_preto_0a050) * 0.5,
          D_branco_2a4_preto_050a125 = sum(dif_branco_2a4_preto_050a125) * 0.5,
          D_branco_2a4_preto_125a2 = sum(dif_branco_2a4_preto_125a2) * 0.5,
          D_branco_2a4_preto_2a4 = sum(dif_branco_2a4_preto_2a4) * 0.5,
          D_branco_2a4_preto_4mais = sum(dif_branco_2a4_preto_4mais) * 0.5,
          # x pardos
          D_branco_2a4_pardo_0a050 = sum(dif_branco_2a4_pardo_0a050) * 0.5,
          D_branco_2a4_pardo_050a125 = sum(dif_branco_2a4_pardo_050a125) * 0.5,
          D_branco_2a4_pardo_125a2 = sum(dif_branco_2a4_pardo_125a2) * 0.5,
          D_branco_2a4_pardo_2a4 = sum(dif_branco_2a4_pardo_2a4) * 0.5,
          D_branco_2a4_pardo_4mais = sum(dif_branco_2a4_pardo_4mais) * 0.5,

          ## Brancos - 4mais
          # x pretos
          D_branco_4mais_preto_0a050 = sum(dif_branco_4mais_preto_0a050) * 0.5,
          D_branco_4mais_preto_050a125 = sum(dif_branco_4mais_preto_050a125) * 0.5,
          D_branco_4mais_preto_125a2 = sum(dif_branco_4mais_preto_125a2) * 0.5,
          D_branco_4mais_preto_2a4 = sum(dif_branco_4mais_preto_2a4) * 0.5,
          D_branco_4mais_preto_4mais = sum(dif_branco_4mais_preto_4mais) * 0.5,
          # x pardos
          D_branco_4mais_pardo_0a050 = sum(dif_branco_4mais_pardo_0a050) * 0.5,
          D_branco_4mais_pardo_050a125 = sum(dif_branco_4mais_pardo_050a125) * 0.5,
          D_branco_4mais_pardo_125a2 = sum(dif_branco_4mais_pardo_125a2) * 0.5,
          D_branco_4mais_pardo_2a4 = sum(dif_branco_4mais_pardo_2a4) * 0.5,
          D_branco_4mais_pardo_4mais = sum(dif_branco_4mais_pardo_4mais) * 0.5,

          ## Pretos - 0a25
          # x pretos
          D_preto_0a050_preto_050a125 = sum(dif_preto_0a050_preto_050a125) * 0.5,
          D_preto_0a050_preto_125a2 = sum(dif_preto_0a050_preto_125a2) * 0.5,
          D_preto_0a050_preto_2a4 = sum(dif_preto_0a050_preto_2a4) * 0.5,
          D_preto_0a050_preto_4mais = sum(dif_preto_0a050_preto_4mais) * 0.5,
          # x pardos
          D_preto_0a050_pardo_0a050 = sum(dif_preto_0a050_pardo_0a050) * 0.5,
          D_preto_0a050_pardo_050a125 = sum(dif_preto_0a050_pardo_050a125) * 0.5,
          D_preto_0a050_pardo_125a2 = sum(dif_preto_0a050_pardo_125a2) * 0.5,
          D_preto_0a050_pardo_2a4 = sum(dif_preto_0a050_pardo_2a4) * 0.5,
          D_preto_0a050_pardo_4mais = sum(dif_preto_0a050_pardo_4mais) * 0.5,

          ## Pretos - 050a125
          # x pretos
          D_preto_050a125_preto_125a2 = sum(dif_preto_050a125_preto_125a2) * 0.5,
          D_preto_050a125_preto_2a4 = sum(dif_preto_050a125_preto_2a4) * 0.5,
          D_preto_050a125_preto_4mais = sum(dif_preto_050a125_preto_4mais) * 0.5,
          # x pardos
          D_preto_050a125_pardo_0a050 = sum(dif_preto_050a125_pardo_0a050) * 0.5,
          D_preto_050a125_pardo_050a125 = sum(dif_preto_050a125_pardo_050a125) * 0.5,
          D_preto_050a125_pardo_125a2 = sum(dif_preto_050a125_pardo_125a2) * 0.5,
          D_preto_050a125_pardo_2a4 = sum(dif_preto_050a125_pardo_2a4) * 0.5,
          D_preto_050a125_pardo_4mais = sum(dif_preto_050a125_pardo_4mais) * 0.5,

          ## Pretos - 125a2
          # x pretos
          D_preto_125a2_preto_2a4 = sum(dif_preto_125a2_preto_2a4) * 0.5,
          D_preto_125a2_preto_4mais = sum(dif_preto_125a2_preto_4mais) * 0.5,
          # x pardos
          D_preto_125a2_pardo_0a050 = sum(dif_preto_125a2_pardo_0a050) * 0.5,
          D_preto_125a2_pardo_050a125 = sum(dif_preto_125a2_pardo_050a125) * 0.5,
          D_preto_125a2_pardo_125a2 = sum(dif_preto_125a2_pardo_125a2) * 0.5,
          D_preto_125a2_pardo_2a4 = sum(dif_preto_125a2_pardo_2a4) * 0.5,
          D_preto_125a2_pardo_4mais = sum(dif_preto_125a2_pardo_4mais) * 0.5,

          ## Pretos - 2a4
          # x pretos
          D_preto_2a4_preto_4mais = sum(dif_preto_2a4_preto_4mais) * 0.5,
          # x pardos
          D_preto_2a4_pardo_0a050 = sum(dif_preto_2a4_pardo_0a050) * 0.5,
          D_preto_2a4_pardo_050a125 = sum(dif_preto_2a4_pardo_050a125) * 0.5,
          D_preto_2a4_pardo_125a2 = sum(dif_preto_2a4_pardo_125a2) * 0.5,
          D_preto_2a4_pardo_2a4 = sum(dif_preto_2a4_pardo_2a4) * 0.5,
          D_preto_2a4_pardo_4mais = sum(dif_preto_2a4_pardo_4mais) * 0.5,

          ## Pretos - 4mais
          # x pardos
          D_preto_4mais_pardo_0a050 = sum(dif_preto_4mais_pardo_0a050) * 0.5,
          D_preto_4mais_pardo_050a125 = sum(dif_preto_4mais_pardo_050a125) * 0.5,
          D_preto_4mais_pardo_125a2 = sum(dif_preto_4mais_pardo_125a2) * 0.5,
          D_preto_4mais_pardo_2a4 = sum(dif_preto_4mais_pardo_2a4) * 0.5,
          D_preto_4mais_pardo_4mais = sum(dif_preto_4mais_pardo_4mais) * 0.5,

          ## Pardos - 0a050
          # x pardos
          D_pardo_0a050_pardo_050a125 = sum(dif_pardo_0a050_pardo_050a125) * 0.5,
          D_pardo_0a050_pardo_125a2 = sum(dif_pardo_0a050_pardo_125a2) * 0.5,
          D_pardo_0a050_pardo_2a4 = sum(dif_pardo_0a050_pardo_2a4) * 0.5,
          D_pardo_0a050_pardo_4mais = sum(dif_pardo_0a050_pardo_4mais) * 0.5,

          ## Pardos - 050a125
          # x pardos
          D_pardo_050a125_pardo_125a2 = sum(dif_pardo_050a125_pardo_125a2) * 0.5,
          D_pardo_050a125_pardo_2a4 = sum(dif_pardo_050a125_pardo_2a4) * 0.5,
          D_pardo_050a125_pardo_4mais = sum(dif_pardo_050a125_pardo_4mais) * 0.5,

          ## Pardos - 125a2
          # x pardos
          D_pardo_125a2_pardo_2a4 = sum(dif_pardo_125a2_pardo_2a4) * 0.5,
          D_pardo_125a2_pardo_4mais = sum(dif_pardo_125a2_pardo_4mais) * 0.5,

          ## Pardos - 2a4
          # x pardos
          D_pardo_2a4_pardo_4mais = sum(dif_pardo_2a4_pardo_4mais) * 0.5,

        ) |>
        pivot_longer(D_branco_0a050_branco_050a125:D_pardo_2a4_pardo_4mais, names_to = "grupo", values_to = "D") |>
        mutate(
          cor_classe1 = c(
            rep("Brancos - [0 a 0,5 SM)",14),rep("Brancos - [0,5 a 1,25 SM)",13),rep("Brancos - [0,5 a 0,75 SM",12),
            rep("Brancos - [2 SM a 4 SM)",11),rep("Brancos - [4+ SM)",10),
            rep("Pretos - [0 a 0,5 SM)",9),rep("Pretos - [0,5 a 1,25 SM)",8),rep("Pretos - [0,5 a 0,75 SM",7),
            rep("Pretos - [2 SM a 4 SM)",6),rep("Pretos - [4+ SM)",5),
            rep("Pardos - [0 a 0,5 SM)",4),rep("Pardos - [0,5 a 1,25 SM)",3),rep("Pardos - [0,5 a 0,75 SM",2),
            rep("Pardos - [2 SM a 4 SM)",1)
          ),
          cor_classe2 = c(
            c("Brancos - [0,5 a 1,25 SM)","Brancos - [1,25 SM a 2 SM)",
              "Brancos - [2 SM a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,5 SM)","Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [1,25 SM a 2 SM)",
              "Brancos - [2 SM a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,5 SM)","Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [2 SM a 4 SM)","Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,5 SM)","Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Brancos - [4+ SM)"),
            c("Pretos - [0 a 0,5 SM)","Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0 a 0,5 SM)","Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [0,5 a 1,25 SM)","Pretos - [1,25 SM a 2 SM)",
              "Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [1,25 SM a 2 SM)","Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [2 SM a 4 SM)","Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pretos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0 a 0,5 SM)","Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [0,5 a 1,25 SM)","Pardos - [1,25 SM a 2 SM)",
              "Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [1,25 SM a 2 SM)","Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [2 SM a 4 SM)","Pardos - [4+ SM)"),
            c("Pardos - [4+ SM)")
          )
        )
    }else{
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
          cor_classe1 = c(rep("Negro alto",3),rep("Negro intermediário",3),rep("Negro baixo",5),
                          rep("Negro alto",1),rep("Branco baixo",2),rep("Branco alto",1),
                          "Negro alto","Negro intermediário","Negro baixo","Branco alto",
                          "Branco intermediário","Branco baixo"),
          cor_classe2 = c(rep(c("Branco alto","Branco intermediário","Branco baixo"),3), "Negro alto",
                          rep("Negro intermediário",2),"Branco intermediário","Branco alto",
                          "Branco intermediário",rep("Resto da população",6))
        )
    }




    if(por_classe == TRUE){
      output <- list(geral = output_geral,
                     classe = output_classe)

    }else{
      output <- list(geral = output_geral)
    }
    return(output)
  }

func_fazer_tabela <- function(tabela){
  # OBS.: essa parte foi comentada por nao ter necessidade a principio
  # if(is.list(tabela)){
  #   stop("Use a data.table/tibble object in 'tabela' argument!")
  # }

    tabela_cruzada <- ftable(xtabs(D ~ cor_classe1 + cor_classe2,
                                   tabela),
                             col.vars = c("cor_classe1"),
                             row.vars = c("cor_classe2")) |>
      stats:::format.ftable(quote = FALSE, dec = ",",digits = 4) |>
      trimws() |>
      as.data.frame()


  return(tabela_cruzada)
}


# Indice Quiciente locacional ---------------------------------------------

func_calcula_quociente_locacional <-
  function(
    data,
    var_estrato,
    cor_raca = "cor_raca",
    area_ponderacao = "area_ponderacao",
    idade = "idade",
    PO = "PO",
    id_pessoa = "id_pes",
    peso = "peso",
    tipo_variavel, # pode receber "SM" para Salarios Minimos; "EGP" para classes EGP ou outros...
    por_classe = TRUE){

    # Preparo da base de dados
    # criar base de dados a ser utilizada e diferencial para EGP, renda em SM ou classificacao por estrato de renda

    vars <- c(var_estrato, cor_raca, area_ponderacao, id_pessoa, peso, idade, PO)

    if(tipo_variavel == "EGP"){
      df <- data |>
        filter(situacao_dom == 1) |>
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
    }
    if(tipo_variavel == "SM"){
      df <- data |>
        filter(situacao_dom == 1) |>
        select(all_of(vars)) |>
        select(var_estrato = all_of(var_estrato), everything()) |>
        mutate(classe_raca = case_when(
          var_estrato == 1 & cor_raca == 1 ~ "Brancos ate meio SM",
          var_estrato == 2 & cor_raca == 1 ~ "Brancos meio a 1SM",
          var_estrato == 3 & cor_raca == 1 ~ "Brancos 1 a 3SM",
          var_estrato == 4 & cor_raca == 1 ~ "Brancos 3SM mais",
          var_estrato == 1 & cor_raca == 2 ~ "Negros ate meio SM",
          var_estrato == 2 & cor_raca == 2 ~ "Negros meio a 1SM",
          var_estrato == 3 & cor_raca == 2 ~ "Negros 1 a 3SM",
          var_estrato == 4 & cor_raca == 2 ~ "Negros 3SM mais",
          TRUE ~ "Resto da população"
        )) |>
        as_survey_design(ids = id_pes, weights = peso)
    }else{
      df <- data |>
        filter(situacao_dom == 1) |>
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
      ungroup() |>
      select(area_ponderacao, cor_raca, everything()) |>
      bind_rows(
        df |>
          group_by(area_ponderacao, cor_raca) |>
          summarise(prop = survey_mean(na.rm = T)) |>
          ungroup() |>
          select(area_ponderacao, cor_raca, everything())
      ) |>
      select(-prop_se)

    if(por_classe == TRUE){
      tabela_por_classe <- df |>
        select(classe_raca) |>
        group_by(classe_raca) |>
        summarise(area_ponderacao = 0,
                  prop = survey_mean(na.rm = T)) |>
        ungroup() |>
        select(area_ponderacao, classe_raca, everything()) |>
        bind_rows(
          df |>
            select(area_ponderacao, classe_raca) |>
            group_by(area_ponderacao, classe_raca) |>
            summarise(prop = survey_mean(na.rm = T)) |>
            ungroup() |>
            select(area_ponderacao, classe_raca, everything())
        ) |>
        select(-prop_se)
    }

    print("Etapa de construição das tabelas de base finalizada...")

    # calcula o indice
    output_geral <- tabela |>
      mutate(prop_branca = prop[cor_raca == 1 & area_ponderacao == 0],
             prop_negra = prop[cor_raca == 2 & area_ponderacao == 0]) |>
      filter(area_ponderacao != 0) |>
      pivot_wider(names_from = cor_raca, values_from = prop) |>
      mutate(across(c(`1`,`2`), ~ replace_na(.x, 0))) |>
      mutate(QL_branca = `1`/prop_branca,
             QL_negra = `2`/prop_negra)

    if(tipo_variavel == "SM"){
      output_classe <- tabela_por_classe |>
        mutate(
          # Prop pop branca
          prop_branca_meioSM = prop[classe_raca == "Brancos ate meio SM" & area_ponderacao == 0],
          prop_branca_meioa1SM = prop[classe_raca == "Brancos meio a 1SM" & area_ponderacao == 0],
          prop_branca_1a3SM = prop[classe_raca == "Brancos 1 a 3SM" & area_ponderacao == 0],
          prop_branca_3SMmais = prop[classe_raca == "Brancos 3SM mais" & area_ponderacao == 0],
          # Prop pop negra
          prop_negra_meioSM = prop[classe_raca == "Negros ate meio SM" & area_ponderacao == 0],
          prop_negra_meioa1SM = prop[classe_raca == "Negros meio a 1SM" & area_ponderacao == 0],
          prop_negra_1a3SM = prop[classe_raca == "Negros 1 a 3SM" & area_ponderacao == 0],
          prop_negra_3SMmais = prop[classe_raca == "Negros 3SM mais" & area_ponderacao == 0],
        ) |>
        pivot_wider(names_from = classe_raca, values_from = prop) |>
        mutate(
          across(c(`Brancos ate meio SM`,`Brancos meio a 1SM`,`Brancos 1 a 3SM`,
                   `Brancos 3SM mais`,`Negros ate meio SM`,`Negros meio a 1SM`,
                   `Negros 1 a 3SM`, `Negros 3SM mais`), ~ replace_na(.x, 0))
        ) |>
        filter(area_ponderacao != 0) |>
        mutate(
          # Razoes de cada grupo

          QL_Brancos_3SMmais = `Brancos 3SM mais`/prop_branca_3SMmais,
          QL_Brancos_1a3SM = `Brancos 1 a 3SM`/prop_branca_1a3SM,
          QL_Brancos_meioa1SM = `Brancos meio a 1SM`/prop_branca_meioa1SM,
          QL_Brancos_meioSM = `Brancos ate meio SM`/prop_branca_meioSM,

          QL_Negros_3SMmais = `Negros 3SM mais`/prop_negra_3SMmais,
          QL_Negros_1a3SM = `Negros 1 a 3SM`/prop_negra_1a3SM,
          QL_Negros_meioa1SM = `Negros meio a 1SM`/prop_negra_meioa1SM,
          QL_Negros_meioSM = `Negros ate meio SM`/prop_negra_meioSM
        )
    }else{
      output_classe <- tabela_por_classe |>
        mutate(prop_branca_baixo = prop[classe_raca == "Brancos baixo" & area_ponderacao == 0],
               prop_branca_intermediario = prop[classe_raca == "Brancos intermediário" & area_ponderacao == 0],
               prop_branca_alto = prop[classe_raca == "Brancos alto" & area_ponderacao == 0],
               prop_negra_baixo = prop[classe_raca == "Negros baixo" & area_ponderacao == 0],
               prop_negra_intermediario = prop[classe_raca == "Negros intermediário" & area_ponderacao == 0],
               prop_negra_alto = prop[classe_raca == "Negros alto" & area_ponderacao == 0]) |>
        pivot_wider(names_from = classe_raca, values_from = prop) |>
        mutate(across(c(`Negros baixo`,`Negros intermediário`,
                        `Negros alto`,`Brancos baixo`,`Brancos intermediário`,
                        `Brancos alto`), ~ replace_na(.x, 0))) |>
        filter(area_ponderacao != 0) |>
        mutate(
          # Razoes de cada grupo
          QL_Brancos_Alto = `Brancos alto`/prop_branca_alto,
          QL_Brancos_Intermediario = `Brancos intermediário`/prop_branca_intermediario,
          QL_Brancos_Baixo = `Brancos baixo`/prop_branca_baixo,
          QL_Negros_Alto = `Negros alto`/prop_negra_alto,
          QL_Negros_Intermediario = `Negros intermediário`/prop_negra_intermediario,
          QL_Negros_Baixo = `Negros baixo`/prop_negra_baixo)
    }

    # Sintese dos resultados
    output_geral_sintese <- output_geral |>
      summarise(
        mean_QL_branca = mean(QL_branca),
        mean_QL_negra = mean(QL_negra),
        desv_pad_QL_branca = sd(QL_branca),
        desv_pad_QL_negra = sd(QL_negra)
      )

    if(por_classe == TRUE){
      if(tipo_variavel == "SM"){
        output_classe_sintese <- output_classe |>
          summarise(
            mean_QL_Brancos_3SMmais = mean(QL_Brancos_3SMmais),
            mean_QL_Brancos_1a3SM = mean(QL_Brancos_1a3SM),
            mean_QL_Brancos_meioa1SM = mean(QL_Brancos_meioa1SM),
            mean_QL_Brancos_meioSM = mean(QL_Brancos_meioSM),

            mean_QL_Negros_3SMmais = mean(QL_Negros_3SMmais),
            mean_QL_Negros_1a3SM = mean(QL_Negros_1a3SM),
            mean_QL_Negros_meioa1SM = mean(QL_Negros_meioa1SM),
            mean_QL_Negros_meioSM = mean(QL_Negros_meioSM),

            desv_pad_QL_Brancos_3SMmais = sd(QL_Brancos_3SMmais),
            desv_pad_QL_Brancos_1a3SM = sd(QL_Brancos_1a3SM),
            desv_pad_QL_Brancos_meioa1SM = sd(QL_Brancos_meioa1SM),
            desv_pad_QL_Brancos_meioSM = sd(QL_Brancos_meioSM),

            desv_pad_QL_Negros_3SMmais = sd(QL_Negros_3SMmais),
            desv_pad_QL_Negros_1a3SM = sd(QL_Negros_1a3SM),
            desv_pad_QL_Negros_meioa1SM = sd(QL_Negros_meioa1SM),
            desv_pad_QL_Negros_meioSM = sd(QL_Negros_meioSM)
          )
      }else{
        output_classe_sintese <- output_classe |>
          summarise(
            mean_QL_Brancos_Alto = mean(QL_Brancos_Alto),
            mean_QL_Brancos_Intermediario = mean(QL_Brancos_Intermediario),
            mean_QL_Brancos_Baixo = mean(QL_Brancos_Baixo),
            mean_QL_Negros_Alto = mean(QL_Negros_Alto),
            mean_QL_Negros_Intermediario = mean(QL_Negros_Intermediario),
            mean_QL_Negros_Baixo = mean(QL_Negros_Baixo),
            desv_pad_QL_Brancos_Alto = sd(QL_Brancos_Alto),
            desv_pad_QL_Brancos_Intermediario = sd(QL_Brancos_Intermediario),
            desv_pad_QL_Brancos_Baixo = sd(QL_Brancos_Baixo),
            desv_pad_QL_Negros_Alto = sd(QL_Negros_Alto),
            desv_pad_QL_Negros_Intermediario = sd(QL_Negros_Intermediario),
            desv_pad_QL_Negros_Baixo = sd(QL_Negros_Baixo)
          )
      }
    }

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

