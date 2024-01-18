options(scipen = 999)
sf::sf_use_s2(FALSE) # desativando opcao de trabalhar com geometrias esfericas para os shps
rm(list = ls())

# Referencias -------------------------------------------------------------

# https://rpubs.com/quarcs-lab/spatial-autocorrelation
# https://github.com/GeoDaCenter/rgeoda
# https://rpubs.com/amrofi/Regional_Economics_Spatial
# https://portaldemapas.ibge.gov.br/portal.php#mapa201586
# https://pypi.org/project/geobr/0.1.4/
# https://www.usp.br/nereus/wp-content/uploads/Aula_4_AEDE.pdf
# https://pt.slideshare.net/vitor_vasconcelos/anlise-de-dependncia-espacial-em-r
# https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html
# https://rpubs.com/FelipeSantos/LISA_tmap_geoda
# https://geodacenter.github.io/workbook/6a_local_auto/lab6a.html

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx, geobr, sf, rgeoda, patchwork, spdep)

## Importacao dos dados

anos <- c(2000,2010)
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")
k = 2

for(i in 1: length(anos)){
  ano = anos[i]

  # for(k in 1: length(RMs)){
    RM = RMs[k]

    # Importacao dos dados
    load(file.path("./output",paste0("resultados_QL_indice_",ano,".RData")))

    # exportacao
    if(ano == 2000){
      assign(paste0("QL_",ano,"_",RM),resultados_QL_index_2000[[k]][[2]])
    }else{
      assign(paste0("QL_",ano,"_",RM),resultados_QL_index_2010[[k]][[2]])
    }

    # Proximo loop
  # }
}

rm(resultados_QL_index_2000,resultados_QL_index_2010)

# Tratamento dos dados espaciais - 2000 --------------------------------------
# Seguiremos o método sugerido na Issue #152 pelo Pedro e Rafael do pacote geobr!
# Consiste em criarmos a AP de 2000 via setores censitarios e relacao de setores e APs
# disponível nos arquivos auxiliares dos microdados do IBGE (Arquivos Auxiliares/Composicao das Areas de Ponderacao.txt.)

# importacao dos arquivos auxiliares

sc_to_ap <- read_delim(
  file = file.path("input","2000","2000_Composicao das Areas de Ponderacao.txt"),
  delim = " ",
  col_names = c("setor_censitario","area_ponderacao"),
  skip = 2,
  locale = locale(encoding = "latin1")
)

# importacao do arquivo com setores censitarios

sc_shp <- geobr::read_census_tract(code_tract = "all", year = 2000) |>
  mutate(code_tract = as.numeric(code_tract),
         code_muni = as.numeric(code_muni))

# juncao dos dados de ap na base de shp para sc

ap_2000 <- sc_to_ap |>
  left_join(
    sc_shp,
    by = c("setor_censitario" = "code_tract"),
    keep = FALSE
  ) |>
  filter(!is.na(code_muni)) |>
  distinct()

ap_2000 <- ap_2000 |>
  as.data.frame() |>
  st_as_sf() |>
  group_by(area_ponderacao) |>
  summarise()

ap_2000 |>
  ggplot() +
  geom_sf(fill = "green")

# Juncao dos dados espaciais ao QL

ap_RMCampinas_2000 <- QL_2000_RMCampinas |>
  left_join(
    ap_2000,
    by = c("area_ponderacao"),
    keep = FALSE
  ) |>
  distinct() |>
  mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
  st_as_sf()

rm(ap_2000, sc_shp, sc_to_ap)

rm_shp_2000 <- geobr::read_metro_area(year = 2001)

# Tratamento dos dados espaciais - 2010 --------------------------------------

rm_shp_2010 <- geobr::read_metro_area(year = 2010)

ap_2010 <- geobr::read_weighting_area() |>
  mutate(code_weighting = as.numeric(code_weighting),
         code_muni = as.numeric(code_muni))

ap_2010 |> ggplot() + geom_sf(fill = "grey")

ap_RMCampinas_2010 <- QL_2010_RMCampinas |>
  left_join(
    ap_2010,
    by = c("area_ponderacao" = "code_weighting"),
    keep = FALSE
  ) |>
  distinct() |>
  mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
  st_as_sf()

# ap_RMCampinas_2010 <- read_sf(dsn = "./input/2010", layer = "RMCAM2010 urbano pop area de ponderação")
#
# ap_RMCampinas_2010 <- QL_2010_RMCampinas |>
#   left_join(
#     ap_RMCampinas_2010 |> mutate(aps = as.numeric(aps)),
#     by = c("area_ponderacao" = "aps"),
#     keep = FALSE
#   ) |>
#   distinct() |>
#   select(-starts_with(c("pop","QL","Br","Ne","Re"))) |>
#   mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |> View()
#   st_as_sf()

rm(ap_2010)

# visualizacao

(rm_shp_2000 |>
    filter(code_metro == 22) |>
    ggplot() +
    geom_sf(fill = "transparent") +
    geom_sf(data = ap_RMCampinas_2000, fill = "#2b8cbe") +
    theme_minimal() +
    labs(title = "2000")
) +
  (rm_shp_2010 |>
     filter(name_metro == "RM Campinas") |>
     ggplot() +
     geom_sf(fill = "transparent") +
     geom_sf(data = ap_RMCampinas_2010, fill = "#2ca25f") +
     theme_minimal() +
     labs(title = "2010")
  )

# LISA - 2000 --------------------------------------------------------------

# Selecao de APs sem geometria vazia

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |> filter(!st_is_empty(geom))

# criacao de peso com base no método queen (mais permissivo)

queen_w <- queen_weights(ap_RMCampinas_2000)

# Branco - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Baixo"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_BB_value = lisa_values(gda_lisa = lisa),
    LISA_BB_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BB_map = case_when(
      LISA_BB_pvalue >  0.05 ~ 0,
      (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 4,
      (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 2,
      (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 3,
      (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 1
    ),
    LISA_BB_map = factor(
      LISA_BB_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Branco - Intermediario

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Intermediario"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_BI_value = lisa_values(gda_lisa = lisa),
    LISA_BI_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BI_map = case_when(
      LISA_BI_pvalue >  0.05 ~ 0,
      (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 4,
      (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 2,
      (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 3,
      (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 1
    ),
    LISA_BI_map = factor(
      LISA_BI_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Branco - Superior

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Alto"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_BS_value = lisa_values(gda_lisa = lisa),
    LISA_BS_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BS_map = case_when(
      LISA_BS_pvalue >  0.05 ~ 0,
      (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 4,
      (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 2,
      (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 3,
      (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 1
    ),
    LISA_BS_map = factor(
      LISA_BS_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negro - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Baixo"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_NB_value = lisa_values(gda_lisa = lisa),
    LISA_NB_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NB_map = case_when(
      LISA_NB_pvalue >  0.05 ~ 0,
      (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 4,
      (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 2,
      (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 3,
      (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 1
    ),
    LISA_NB_map = factor(
      LISA_BB_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negro - Intermediario

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Intermediario"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_NI_value = lisa_values(gda_lisa = lisa),
    LISA_NI_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NI_map = case_when(
      LISA_NI_pvalue >  0.05 ~ 0,
      (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 4,
      (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 2,
      (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 3,
      (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 1
    ),
    LISA_NI_map = factor(
      LISA_NI_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negros - Superior

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Alto"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  mutate(
    LISA_NS_value = lisa_values(gda_lisa = lisa),
    LISA_NS_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NS_map = case_when(
      LISA_NS_pvalue >  0.05 ~ 0,
      (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 4,
      (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 2,
      (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 3,
      (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 1
    ),
    LISA_NS_map = factor(
      LISA_NS_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# LISA - 2010 --------------------------------------------------------------

# Selecao de APs sem geometria vazia

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |> filter(!st_is_empty(geom))

# criacao de peso com base no método queen (mais permissivo)

queen_w <- queen_weights(ap_RMCampinas_2010)

# Branco - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2010["QL_Brancos_Baixo"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_BB_value = lisa_values(gda_lisa = lisa),
    LISA_BB_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BB_map = case_when(
      LISA_BB_pvalue >  0.05 ~ 0,
      (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 4,
      (LISA_BB_value - mean(LISA_BB_value)) > 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 2,
      (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) > 0 ~ 3,
      (LISA_BB_value - mean(LISA_BB_value)) < 0 & (QL_Brancos_Baixo - mean(QL_Brancos_Baixo)) < 0 ~ 1
    ),
    LISA_BB_map = factor(
      LISA_BB_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Branco - Intermediario

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Intermediario"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_BI_value = lisa_values(gda_lisa = lisa),
    LISA_BI_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BI_map = case_when(
      LISA_BI_pvalue >  0.05 ~ 0,
      (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 4,
      (LISA_BI_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 2,
      (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) > 0 ~ 3,
      (LISA_BI_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Intermediario - mean(QL_Brancos_Intermediario)) < 0 ~ 1
    ),
    LISA_BI_map = factor(
      LISA_BI_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Branco - Superior

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Alto"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_BS_value = lisa_values(gda_lisa = lisa),
    LISA_BS_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BS_map = case_when(
      LISA_BS_pvalue >  0.05 ~ 0,
      (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 4,
      (LISA_BS_value - mean(LISA_BI_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 2,
      (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 3,
      (LISA_BS_value - mean(LISA_BI_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 1
    ),
    LISA_BS_map = factor(
      LISA_BS_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negro - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Baixo"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_NB_value = lisa_values(gda_lisa = lisa),
    LISA_NB_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NB_map = case_when(
      LISA_NB_pvalue >  0.05 ~ 0,
      (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 4,
      (LISA_NB_value - mean(LISA_NB_value)) > 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 2,
      (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) > 0 ~ 3,
      (LISA_NB_value - mean(LISA_NB_value)) < 0 & (QL_Negros_Baixo - mean(QL_Negros_Baixo)) < 0 ~ 1
    ),
    LISA_NB_map = factor(
      LISA_BB_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negro - Intermediario

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Intermediario"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_NI_value = lisa_values(gda_lisa = lisa),
    LISA_NI_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NI_map = case_when(
      LISA_NI_pvalue >  0.05 ~ 0,
      (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 4,
      (LISA_NI_value - mean(LISA_NI_value)) > 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 2,
      (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) > 0 ~ 3,
      (LISA_NI_value - mean(LISA_NI_value)) < 0 & (QL_Negros_Intermediario - mean(QL_Negros_Intermediario)) < 0 ~ 1
    ),
    LISA_NI_map = factor(
      LISA_NI_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )

# Negros - Superior

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Alto"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_NS_value = lisa_values(gda_lisa = lisa),
    LISA_NS_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_NS_map = case_when(
      LISA_NS_pvalue >  0.05 ~ 0,
      (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 4,
      (LISA_NS_value - mean(LISA_NS_value)) > 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 2,
      (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) > 0 ~ 3,
      (LISA_NS_value - mean(LISA_NS_value)) < 0 & (QL_Negros_Alto - mean(QL_Negros_Alto)) < 0 ~ 1
    ),
    LISA_NS_map = factor(
      LISA_NS_map,
      levels = c(0,1,2,3,4),
      labels = c("Não significativo (95%)","Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto")
    )
  )


# Mapas - 2000 ------------------------------------------------------------

# Mapas - 2010 ------------------------------------------------------------
