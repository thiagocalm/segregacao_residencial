
# funcao lisa ------------------------------------------------------------------

func_lisa_classes <- function(data, data_w = queen_w, social_class = classe){
  if(classe == "SM"){

    # Branco - meioSM

    lisa <- local_moran(data_w, data["QL_Brancos_meioSM"])

    data <- data |>
      mutate(
        LISA_BmeioSM_value = lisa_values(gda_lisa = lisa),
        LISA_BmeioSM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_BmeioSM_centralizado = case_when(
          LISA_BmeioSM_pvalue >  0.1 ~ 0,
          (LISA_BmeioSM_value - mean(LISA_BmeioSM_value)) > 0 & (QL_Brancos_meioSM - mean(QL_Brancos_meioSM)) > 0 ~ 4,
          (LISA_BmeioSM_value - mean(LISA_BmeioSM_value)) > 0 & (QL_Brancos_meioSM - mean(QL_Brancos_meioSM)) < 0 ~ 1,
          (LISA_BmeioSM_value - mean(LISA_BmeioSM_value)) < 0 & (QL_Brancos_meioSM - mean(QL_Brancos_meioSM)) > 0 ~ 3,
          (LISA_BmeioSM_value - mean(LISA_BmeioSM_value)) < 0 & (QL_Brancos_meioSM - mean(QL_Brancos_meioSM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_BmeioSM_map = factor(
          LISA_BmeioSM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Branco - meioa1SM

    lisa <- local_moran(data_w, data["QL_Brancos_meioa1SM"])

    data <- data |>
      mutate(
        LISA_Bmeioa1SM_value = lisa_values(gda_lisa = lisa),
        LISA_Bmeioa1SM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_Bmeioa1SM_centralizado = case_when(
          LISA_Bmeioa1SM_pvalue >  0.1 ~ 0,
          (LISA_Bmeioa1SM_value - mean(LISA_Bmeioa1SM_value)) > 0 & (QL_Brancos_meioa1SM - mean(QL_Brancos_meioa1SM)) > 0 ~ 4,
          (LISA_Bmeioa1SM_value - mean(LISA_Bmeioa1SM_value)) > 0 & (QL_Brancos_meioa1SM - mean(QL_Brancos_meioa1SM)) < 0 ~ 1,
          (LISA_Bmeioa1SM_value - mean(LISA_Bmeioa1SM_value)) < 0 & (QL_Brancos_meioa1SM - mean(QL_Brancos_meioa1SM)) > 0 ~ 3,
          (LISA_Bmeioa1SM_value - mean(LISA_Bmeioa1SM_value)) < 0 & (QL_Brancos_meioa1SM - mean(QL_Brancos_meioa1SM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_Bmeioa1SM_map = factor(
          LISA_BmeioSM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Branco - 1a3SM

    lisa <- local_moran(data_w, data["QL_Brancos_1a3SM"])

    data <- data |>
      mutate(
        LISA_B1a3SM_value = lisa_values(gda_lisa = lisa),
        LISA_B1a3SM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_B1a3SM_centralizado = case_when(
          LISA_B1a3SM_pvalue >  0.1 ~ 0,
          (LISA_B1a3SM_value - mean(LISA_B1a3SM_value)) > 0 & (QL_Brancos_1a3SM - mean(QL_Brancos_1a3SM)) > 0 ~ 4,
          (LISA_B1a3SM_value - mean(LISA_B1a3SM_value)) > 0 & (QL_Brancos_1a3SM - mean(QL_Brancos_1a3SM)) < 0 ~ 1,
          (LISA_B1a3SM_value - mean(LISA_B1a3SM_value)) < 0 & (QL_Brancos_1a3SM - mean(QL_Brancos_1a3SM)) > 0 ~ 3,
          (LISA_B1a3SM_value - mean(LISA_B1a3SM_value)) < 0 & (QL_Brancos_1a3SM - mean(QL_Brancos_1a3SM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_B1a3SM_map = factor(
          LISA_B1a3SM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Branco - 3SMmais

    lisa <- local_moran(data_w, data["QL_Brancos_3SMmais"])

    data <- data |>
      mutate(
        LISA_B3SMmais_value = lisa_values(gda_lisa = lisa),
        LISA_B3SMmais_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_B3SMmais_centralizado = case_when(
          LISA_B3SMmais_pvalue >  0.1 ~ 0,
          (LISA_B3SMmais_value - mean(LISA_B3SMmais_value)) > 0 & (QL_Brancos_3SMmais - mean(QL_Brancos_3SMmais)) > 0 ~ 4,
          (LISA_B3SMmais_value - mean(LISA_B3SMmais_value)) > 0 & (QL_Brancos_3SMmais - mean(QL_Brancos_3SMmais)) < 0 ~ 1,
          (LISA_B3SMmais_value - mean(LISA_B3SMmais_value)) < 0 & (QL_Brancos_3SMmais - mean(QL_Brancos_3SMmais)) > 0 ~ 3,
          (LISA_B3SMmais_value - mean(LISA_B3SMmais_value)) < 0 & (QL_Brancos_3SMmais - mean(QL_Brancos_3SMmais)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_B3SMmais_map = factor(
          LISA_B3SMmais_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - meioSM

    lisa <- local_moran(data_w, data["QL_Negros_meioSM"])

    data <- data |>
      mutate(
        LISA_NmeioSM_value = lisa_values(gda_lisa = lisa),
        LISA_NmeioSM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_NmeioSM_centralizado = case_when(
          LISA_NmeioSM_pvalue >  0.1 ~ 0,
          (LISA_NmeioSM_value - mean(LISA_NmeioSM_value)) > 0 & (QL_Negros_meioSM - mean(QL_Negros_meioSM)) > 0 ~ 4,
          (LISA_NmeioSM_value - mean(LISA_NmeioSM_value)) > 0 & (QL_Negros_meioSM - mean(QL_Negros_meioSM)) < 0 ~ 1,
          (LISA_NmeioSM_value - mean(LISA_NmeioSM_value)) < 0 & (QL_Negros_meioSM - mean(QL_Negros_meioSM)) > 0 ~ 3,
          (LISA_NmeioSM_value - mean(LISA_NmeioSM_value)) < 0 & (QL_Negros_meioSM - mean(QL_Negros_meioSM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_NmeioSM_map = factor(
          LISA_NmeioSM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - meioa1SM

    lisa <- local_moran(data_w, data["QL_Negros_meioa1SM"])

    data <- data |>
      mutate(
        LISA_Nmeioa1SM_value = lisa_values(gda_lisa = lisa),
        LISA_Nmeioa1SM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_Nmeioa1SM_centralizado = case_when(
          LISA_Nmeioa1SM_pvalue >  0.1 ~ 0,
          (LISA_Nmeioa1SM_value - mean(LISA_Nmeioa1SM_value)) > 0 & (QL_Negros_meioa1SM - mean(QL_Negros_meioa1SM)) > 0 ~ 4,
          (LISA_Nmeioa1SM_value - mean(LISA_Nmeioa1SM_value)) > 0 & (QL_Negros_meioa1SM - mean(QL_Negros_meioa1SM)) < 0 ~ 1,
          (LISA_Nmeioa1SM_value - mean(LISA_Nmeioa1SM_value)) < 0 & (QL_Negros_meioa1SM - mean(QL_Negros_meioa1SM)) > 0 ~ 3,
          (LISA_Nmeioa1SM_value - mean(LISA_Nmeioa1SM_value)) < 0 & (QL_Negros_meioa1SM - mean(QL_Negros_meioa1SM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_Nmeioa1SM_map = factor(
          LISA_NmeioSM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - 1a3SM

    lisa <- local_moran(data_w, data["QL_Negros_1a3SM"])

    data <- data |>
      mutate(
        LISA_N1a3SM_value = lisa_values(gda_lisa = lisa),
        LISA_N1a3SM_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_N1a3SM_centralizado = case_when(
          LISA_N1a3SM_pvalue >  0.1 ~ 0,
          (LISA_N1a3SM_value - mean(LISA_N1a3SM_value)) > 0 & (QL_Negros_1a3SM - mean(QL_Negros_1a3SM)) > 0 ~ 4,
          (LISA_N1a3SM_value - mean(LISA_N1a3SM_value)) > 0 & (QL_Negros_1a3SM - mean(QL_Negros_1a3SM)) < 0 ~ 1,
          (LISA_N1a3SM_value - mean(LISA_N1a3SM_value)) < 0 & (QL_Negros_1a3SM - mean(QL_Negros_1a3SM)) > 0 ~ 3,
          (LISA_N1a3SM_value - mean(LISA_N1a3SM_value)) < 0 & (QL_Negros_1a3SM - mean(QL_Negros_1a3SM)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_N1a3SM_map = factor(
          LISA_N1a3SM_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - 3SMmais

    lisa <- local_moran(data_w, data["QL_Negros_3SMmais"])

    data <- data |>
      mutate(
        LISA_N3SMmais_value = lisa_values(gda_lisa = lisa),
        LISA_N3SMmais_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_N3SMmais_centralizado = case_when(
          LISA_N3SMmais_pvalue >  0.1 ~ 0,
          (LISA_N3SMmais_value - mean(LISA_N3SMmais_value)) > 0 & (QL_Negros_3SMmais - mean(QL_Negros_3SMmais)) > 0 ~ 4,
          (LISA_N3SMmais_value - mean(LISA_N3SMmais_value)) > 0 & (QL_Negros_3SMmais - mean(QL_Negros_3SMmais)) < 0 ~ 1,
          (LISA_N3SMmais_value - mean(LISA_N3SMmais_value)) < 0 & (QL_Negros_3SMmais - mean(QL_Negros_3SMmais)) > 0 ~ 3,
          (LISA_N3SMmais_value - mean(LISA_N3SMmais_value)) < 0 & (QL_Negros_3SMmais - mean(QL_Negros_3SMmais)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_N3SMmais_map = factor(
          LISA_N3SMmais_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )
  }
  if(classe == "EGP"){
    # Branco - Baixo

    lisa <- local_moran(data_w, data["QL_Brancos_Baixo"])

    data <- data |>
      mutate(
        LISA_BB_value = lisa_values(gda_lisa = lisa),
        LISA_BB_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_BB_centralizado = case_when(
          LISA_BB_pvalue >  0.1 ~ 0,
          (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 4,
          (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 1,
          (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 3,
          (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_BB_map = factor(
          LISA_BB_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Branco - Intermediario

    lisa <- local_moran(data_w, data["QL_Brancos_Intermediario"])

    data <- data |>
      mutate(
        LISA_BI_value = lisa_values(gda_lisa = lisa),
        LISA_BI_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_BI_centralizado = case_when(
          LISA_BI_pvalue >  0.1  ~ 0,
          (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 4,
          (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 1,
          (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 3,
          (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_BI_map = factor(
          LISA_BI_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Branco - Superior

    lisa <- local_moran(data, data["QL_Brancos_Alto"])

    data <- data |>
      mutate(
        LISA_BS_value = lisa_values(gda_lisa = lisa),
        LISA_BS_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_BS_centralizado = case_when(
          LISA_BS_pvalue >  0.1  ~ 0,
          (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 4,
          (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 1,
          (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 3,
          (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_BS_map = factor(
          LISA_BS_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - Baixo

    lisa <- local_moran(data_w, data["QL_Negros_Baixo"])

    data <- data |>
      mutate(
        LISA_NB_value = lisa_values(gda_lisa = lisa),
        LISA_NB_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_NB_centralizado = case_when(
          LISA_NB_pvalue >  0.1  ~ 0,
          (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 4,
          (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 1,
          (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 3,
          (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_NB_map = factor(
          LISA_NB_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negro - Intermediario

    lisa <- local_moran(data_w, data["QL_Negros_Intermediario"])

    data <- data |>
      mutate(
        LISA_NI_value = lisa_values(gda_lisa = lisa),
        LISA_NI_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_NI_centralizado = case_when(
          LISA_NI_pvalue >  0.1  ~ 0,
          (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 4,
          (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 1,
          (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 3,
          (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_NI_map = factor(
          LISA_NI_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )

    # Negros - Superior

    lisa <- local_moran(data_w, data["QL_Negros_Alto"])

    data <- data |>
      mutate(
        LISA_NS_value = lisa_values(gda_lisa = lisa),
        LISA_NS_pvalue = lisa_pvalues(gda_lisa = lisa),
        LISA_NS_centralizado = case_when(
          LISA_NS_pvalue >  0.1  ~ 0,
          (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 4,
          (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 1,
          (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 3,
          (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 2,
          TRUE ~ NA_real_
        ),
        LISA_NS_map = factor(
          LISA_NS_centralizado,
          levels = c(2,1,3,4,0),
          labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
        )
      )
  }
  return(data)
}


# funcao moran ------------------------------------------------------------

func_moran_classes <- function(data, w = "queen", social_classes = classe){

  # Configurando para ajuste de poligonos sem vizinhanca

  # set.ZeroPolicyOption(TRUE)
  get.ZeroPolicyOption()

  data <- data |>
    filter(!st_is_empty(data)) # exclusao de valores ausentes

  # Gerando matriz para moran

  if(w == "queen"){
    queen_w <- nb2listw(poly2nb(data), style = "W", zero.policy = TRUE)
  }

  # gerando base com moran

  if(social_classes == "EGP"){
    moran_test <- tibble(
      Brancos_Baixo_value = moran.test(data$QL_Brancos_Baixo, queen_w)[[3]][[1]],
      Brancos_Baixo_pvalue = round(moran.test(data$QL_Brancos_Baixo, queen_w)[[2]],4),
      Brancos_Intermediario_value = moran.test(data$QL_Brancos_Intermediario, queen_w)[[3]][[1]],
      Brancos_Intermediario_pvalue = round(moran.test(data$QL_Brancos_Intermediario, queen_w)[[2]],4),
      Brancos_Superior_value = moran.test(data$QL_Brancos_Alto, queen_w)[[3]][[1]],
      Brancos_Superior_pvalue = round(moran.test(data$QL_Brancos_Alto, queen_w)[[2]],4),
      Negros_Baixo_value = moran.test(data$QL_Negros_Baixo, queen_w)[[3]][[1]],
      Negros_Baixo_pvalue = round(moran.test(data$QL_Negros_Baixo, queen_w)[[2]],4),
      Negros_Intermediario_value = moran.test(data$QL_Negros_Intermediario, queen_w)[[3]][[1]],
      Negros_Intermediario_pvalue = round(moran.test(data$QL_Negros_Intermediario, queen_w)[[2]],4),
      Negros_Superior_value = moran.test(data$QL_Negros_Alto, queen_w)[[3]][[1]],
      Negros_Superior_pvalue = round(moran.test(data$QL_Negros_Alto, queen_w)[[2]],4)
    )
  }
  if(social_classes == "SM"){
    moran_test <- tibble(
      Brancos_meioSM_value = moran.test(data$QL_Brancos_meioSM, queen_w)[[3]][[1]],
      Brancos_meioSM_pvalue = round(moran.test(data$QL_Brancos_meioSM, queen_w)[[2]],4),
      Brancos_meioa1SM_value = moran.test(data$QL_Brancos_meioa1SM, queen_w)[[3]][[1]],
      Brancos_meioa1SM_pvalue = round(moran.test(data$QL_Brancos_meioa1SM, queen_w)[[2]],4),
      Brancos_1a3SM_value = moran.test(data$QL_Brancos_1a3SM, queen_w)[[3]][[1]],
      Brancos_1a3SM_pvalue = round(moran.test(data$QL_Brancos_1a3SM, queen_w)[[2]],4),
      Brancos_3SMmais_value = moran.test(data$QL_Brancos_3SMmais, queen_w)[[3]][[1]],
      Brancos_3SMmais_pvalue = round(moran.test(data$QL_Brancos_3SMmais, queen_w)[[2]],4),
      Negros_meioSM_value = moran.test(data$QL_Negros_meioSM, queen_w)[[3]][[1]],
      Negros_meioSM_pvalue = round(moran.test(data$QL_Negros_meioSM, queen_w)[[2]],4),
      Negros_meioa1SM_value = moran.test(data$QL_Negros_meioa1SM, queen_w)[[3]][[1]],
      Negros_meioa1SM_pvalue = round(moran.test(data$QL_Negros_meioa1SM, queen_w)[[2]],4),
      Negros_1a3SM_value = moran.test(data$QL_Negros_1a3SM, queen_w)[[3]][[1]],
      Negros_1a3SM_pvalue = round(moran.test(data$QL_Negros_1a3SM, queen_w)[[2]],4),
      Negros_3SMmais_value = moran.test(data$QL_Negros_3SMmais, queen_w)[[3]][[1]],
      Negros_3SMmais_pvalue = round(moran.test(data$QL_Negros_3SMmais, queen_w)[[2]],4)
    )
  }

  # manipulacao da tabela

  moran_test <- moran_test |>
    pivot_longer(
      everything(),
      values_to = "valor",
      names_to = "Raça-classe"
    ) |>
    mutate(
      `Raça-classe` = str_remove(`Raça-classe`, "_value"),
      `Raça-classe` = str_remove(`Raça-classe`, "_pvalue")
    ) |>
    mutate(
      tipo = rep(c("Moran","P_value"),8)
    ) |>
    pivot_wider(
      names_from = tipo,
      values_from = valor
    )

  # exportacao da funcao

  return(moran_test)

}
