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
pacman::p_load(tidyverse, srvyr, readr, xlsx, geobr, sf, rgeoda, patchwork, ggspatial, spdep)

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
      QL_index_total <- resultados_QL_index_2000[[k]][[1]] |>
        select(area_ponderacao, `1`,`2`) |>
        rename(
          prop_branca = `1`,
          prop_negra = `2`,
        ) |>
        left_join(
          resultados_QL_index_2000[[k]][[2]],
          by = c("area_ponderacao"),
          keep = FALSE
        )

      assign(paste0("QL_",ano,"_",RM),QL_index_total)
    }else{
      QL_index_total <- resultados_QL_index_2010[[k]][[1]] |>
        select(area_ponderacao, `1`,`2`) |>
        rename(
          prop_branca = `1`,
          prop_negra = `2`,
        ) |>
        left_join(
          resultados_QL_index_2010[[k]][[2]],
          by = c("area_ponderacao"),
          keep = FALSE
        )

      assign(paste0("QL_",ano,"_",RM),QL_index_total)
    }

    # Proximo loop
  # }
}

rm(resultados_QL_index_2000,resultados_QL_index_2010, QL_index_total)

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

sc_shp <- geobr::read_census_tract(code_tract = "all", year = 2010) |>
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

# Juncao dos dados espaciais ao QL - Aqui começamos a falar da RM Campinas somente!

ap_RMCampinas_2000 <- QL_2000_RMCampinas |>
  left_join(
    ap_2000,
    by = c("area_ponderacao"),
    keep = FALSE
  ) |>
  distinct() |>
  mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
  st_as_sf()

# Recaptura de poligonos vazios

poligonos_vazios <- ap_RMCampinas_2000 |>
  select(area_ponderacao, code_munic, geom) |>
  filter(st_is_empty(geom))

rm(ap_2000, sc_shp, sc_to_ap, poligonos_vazios)

rm_shp_2000 <- geobr::read_metro_area(year = 2001) |>
  filter(code_metro == "022")

# Tratamento dos dados espaciais - 2010 --------------------------------------

rm_shp_2010 <- geobr::read_metro_area(year = 2010) |>
  filter(name_metro == "RM Campinas")

# importacao dos arquivos auxiliares

sc_to_ap <- read_delim(
  file = file.path("input","2010","2010_Composicao das Areas de Ponderacao.txt"),
  delim = "\t",
  col_names = c("area_ponderacao","setor_censitario"),
  skip = 2,
  locale = locale(encoding = "UTF-8")
)

# importacao do arquivo com setores censitarios

sc_shp <- geobr::read_census_tract(code_tract = "all", year = 2010) |>
  mutate(code_tract = as.numeric(code_tract),
         code_muni = as.numeric(code_muni))

# juncao dos dados de ap na base de shp para sc

ap_2010 <- sc_to_ap |>
  left_join(
    sc_shp,
    by = c("setor_censitario" = "code_tract"),
    keep = FALSE
  ) |>
  filter(!is.na(code_muni)) |>
  distinct()

ap_2010 <- ap_2010 |>
  as.data.frame() |>
  st_as_sf() |>
  group_by(area_ponderacao) |>
  summarise()


# ap_2010 |> ggplot() + geom_sf(fill = "grey")

ap_RMCampinas_2010 <- QL_2010_RMCampinas |>
  left_join(
    ap_2010,
    by = c("area_ponderacao"),
    keep = FALSE
  ) |>
  distinct() |>
  mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
  st_as_sf()

# Recaptura de poligonos vazios

poligonos_vazios <- ap_RMCampinas_2010 |>
  select(area_ponderacao, code_munic, geom) |>
  filter(st_is_empty(geom))

rm(ap_2010)


# Visualizacao de ambas os anos conjuntamente -----------------------------

(rm_shp_2000 |>
    ggplot() +
    geom_sf(data = ap_RMCampinas_2000, fill = "#2b8cbe") +
    geom_sf(fill = "transparent", colour = "black", size = .7) +
    theme_minimal() +
    labs(title = "2000") +
    theme(
      plot.title = element_text(size = 12, hjust = .5, vjust = .5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
      panel.background = element_blank()) +
    annotation_scale(
      location = "bl",
      pad_x = unit(0.0, "in"),
      width_hint = 0.5
    ) +
    annotation_north_arrow(
      location = "bl", which_north = "true",
      pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
      style = north_arrow_fancy_orienteering
    )
) +
  (rm_shp_2010 |>
     ggplot() +
     geom_sf(data = ap_RMCampinas_2010, fill = "#2ca25f") +
     geom_sf(fill = "transparent", colour = "black", size = .7) +
     theme_minimal() +
     labs(title = "2010") +
     theme(
       plot.title = element_text(size = 12, hjust = .5, vjust = .5),
       axis.text = element_blank(),
       axis.ticks = element_blank(),
       panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
       panel.background = element_blank()) +
     annotation_scale(
       location = "bl",
       pad_x = unit(0.0, "in"),
       width_hint = 0.5
     ) +
     annotation_north_arrow(
       location = "bl", which_north = "true",
       pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
       style = north_arrow_fancy_orienteering
     )
  )

ggsave(
  filename = "comparativo das APs nas RMs em 2000 e 2010.jpeg",
  device = "jpeg",
  path = file.path("output","mapas"),
  width = 13,
  height = 13,
  units = "in"
)


# Visualizacao dos dados da RM com nomes dos muncis -----------------------

rm_shp_2010 |>
  ggplot() +
  geom_sf(data = ap_RMCampinas_2010, fill = "#f7f7f7", color = "#d9d9d9") +
  geom_sf(fill = "transparent", colour = "black", size = .9) +
  geom_sf_text(
    aes(label = name_muni),
    size = 3,
    color = "black",
    fontface = "bold",
    check_overlap = TRUE,
    fun.geometry = sf::st_centroid
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = .5, vjust = .5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#ffffff",linewidth = .01),
    panel.background = element_blank()) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.5
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "RM de Campinas e seus municipios.jpeg",
  device = "jpeg",
  path = file.path("output","mapas"),
  width = 13,
  height = 13,
  units = "in"
)

# LISA - 2000 --------------------------------------------------------------

# criacao de peso com base no método queen (mais permissivo)

queen_w <- queen_weights(ap_RMCampinas_2000, include_lower_order = TRUE)

# Branco - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Baixo"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Intermediario"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Alto"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Baixo"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Intermediario"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Alto"])

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
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

# LISA - 2010 --------------------------------------------------------------

# criacao de peso com base no método queen (mais permissivo)

queen_w <- queen_weights(ap_RMCampinas_2010, include_lower_order = TRUE)

# Branco - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2010["QL_Brancos_Baixo"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_BB_value = lisa_values(gda_lisa = lisa),
    LISA_BB_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BB_centralizado = case_when(
      LISA_BB_pvalue >  0.1  ~ 0,
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Intermediario"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Brancos_Alto"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  mutate(
    LISA_BS_value = lisa_values(gda_lisa = lisa),
    LISA_BS_pvalue = lisa_pvalues(gda_lisa = lisa),
    LISA_BS_centralizado = case_when(
      LISA_BS_pvalue >  0.1  ~ 0,
      (LISA_BS_value - mean(LISA_BS_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 4,
      (LISA_BS_value - mean(LISA_BS_value)) > 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 1,
      (LISA_BS_value - mean(LISA_BS_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) > 0 ~ 3,
      (LISA_BS_value - mean(LISA_BS_value)) < 0 & (QL_Brancos_Alto - mean(QL_Brancos_Alto)) < 0 ~ 2,
      TRUE ~ NA_real_
    ),
    LISA_BS_map = factor(
      LISA_BS_centralizado,
      levels = c(2,1,3,4,0),
      labels = c("Baixo-Baixo","Alto-Baixo","Baixo-Alto","Alto-Alto","Não significativo")
    )
  )

# Negro - Baixo

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Baixo"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Intermediario"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
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

lisa <- local_moran(queen_w, ap_RMCampinas_2000["QL_Negros_Alto"])

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
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

# Mapas ------------------------------------------------------------

anos <- c(2000,2010)
relacoes <- c("BB","BI","BS","NB","NI","NS")
relacoes_extensa <- c("Branco-Baixo","Branco-Intermediário","Branco-Superior",
                     "Negro-Baixo","Negro-Intermediário","Negro-Superior")

for(i in seq_along(anos)){
  ano = anos[i]
  for(j in seq_along(relacoes)){
    relacao = relacoes[j]
    relacao_extensa = relacoes_extensa[j]

    # Mapa
    get(glue::glue("ap_RMCampinas_{ano}")) |>
      select(LISA_var = glue::glue("LISA_{relacao}_map")) |>
      filter(!is.na(LISA_var)) |>
      ggplot() +
      geom_sf(aes(fill = LISA_var),
              lwd = 0) +
      geom_sf(data = get(glue::glue("rm_shp_{ano}")),
              fill = "transparent",
              colour = "black", size = 0.5) +
      scale_fill_manual(
        values = c(
          "Baixo-Baixo" = "#045a8d", "Alto-Baixo" = "#a6bddb","Baixo-Alto" = "#fc9272",
          "Alto-Alto" = "#a50f15","Não significativo" =  "#f0f0f0"
        )) +
      guides(fill = guide_legend(title = glue::glue("LISA Map RM Campinas: {relacao_extensa}"))) +
      labs(
        caption = glue::glue("Fonte: IBGE, Censo Demográfico {ano}")
      ) +
      # tira sistema cartesiano
      theme(
        plot.caption = element_text(size = 8),
        legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
        legend.text = element_text(size = 8, hjust = 0, vjust = .9),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "#ffffff",linewidth = .01),
        panel.background = element_blank()) +
      annotation_scale(
        location = "bl",
        pad_x = unit(0.0, "in"),
        width_hint = 0.5
      ) +
      annotation_north_arrow(
        location = "bl", which_north = "true",
        pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
        style = north_arrow_fancy_orienteering
      )
    # Exportacao
    ggsave(
      filename = glue::glue("{ano}_LISA_{relacao}.jpeg"),
      device = "jpeg",
      path = file.path("output","mapas"),
      width = 13,
      height = 13,
      units = "in"
    )
    print(paste0("Finalizamos a categoria ",relacao,"..."))
  }
  print(paste0("Finalizamos o ano de ",ano,"..."))
}

# Indice de Moran ---------------------------------------------------------

# Nomes das variaveis
oldnames = colnames(ap_RMCampinas_2000 |> select(starts_with(c("QL_Br","QL_Ne"))))[1:6] |>
  as_tibble() |>
  mutate(
    value = str_remove(value, "QL_"),
    value = paste0(value,"_value")
  ) |> pull()

newnames = colnames(
  ap_RMCampinas_2000 |>
  select(starts_with(c("QL_Br","QL_Ne")))
) |>
  as_tibble() |>
  filter(value != "geom") |>
  mutate(
    value = str_remove(value, "QL_"),
    value = str_replace(value, "_"," ")
  ) |> pull()

# Configurando para ajuste de poligonos sem vizinhanca

set.ZeroPolicyOption(TRUE)
get.ZeroPolicyOption()

# Gerando matriz para moran

queen_w_2000 <- nb2listw(poly2nb(ap_RMCampinas_2000), style = "W", zero.policy = TRUE)
queen_w_2010 <- nb2listw(poly2nb(ap_RMCampinas_2010), style = "W", zero.policy = TRUE)

# gerando base com moran

moran_test <- tibble(
  ano = c(2000,2010),
  Brancos_Baixo_value = c(
    moran.test(ap_RMCampinas_2000$QL_Brancos_Baixo, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Brancos_Baixo, queen_w_2010)[[3]][[1]]
  ),
  Brancos_Baixo_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Brancos_Baixo, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Brancos_Baixo, queen_w_2010)[[2]],4)
  ),
  Brancos_Intermediario_value = c(
    moran.test(ap_RMCampinas_2000$QL_Brancos_Intermediario, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Brancos_Intermediario, queen_w_2010)[[3]][[1]]
  ),
  Brancos_Intermediario_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Brancos_Intermediario, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Brancos_Intermediario, queen_w_2010)[[2]],4)
  ),
  Brancos_Superior_value = c(
    moran.test(ap_RMCampinas_2000$QL_Brancos_Alto, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Brancos_Alto, queen_w_2010)[[3]][[1]]
  ),
  Brancos_Superior_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Brancos_Alto, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Brancos_Alto, queen_w_2010)[[2]],4)
  ),
  Negros_Baixo_value = c(
    moran.test(ap_RMCampinas_2000$QL_Negros_Baixo, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Negros_Baixo, queen_w_2010)[[3]][[1]]
  ),
  Negros_Baixo_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Negros_Baixo, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Negros_Baixo, queen_w_2010)[[2]],4)
  ),
  Negros_Intermediario_value = c(
    moran.test(ap_RMCampinas_2000$QL_Negros_Intermediario, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Negros_Intermediario, queen_w_2010)[[3]][[1]]
  ),
  Negros_Intermediario_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Negros_Intermediario, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Negros_Intermediario, queen_w_2010)[[2]],4)
  ),
  Negros_Superior_value = c(
    moran.test(ap_RMCampinas_2000$QL_Negros_Alto, queen_w_2000)[[3]][[1]],
    moran.test(ap_RMCampinas_2010$QL_Negros_Alto, queen_w_2010)[[3]][[1]]
  ),
  Negros_Superior_pvalue = c(
    round(moran.test(ap_RMCampinas_2000$QL_Negros_Alto, queen_w_2000)[[2]],4),
    round(moran.test(ap_RMCampinas_2010$QL_Negros_Alto, queen_w_2010)[[2]],4)
  )
) |>
  select(-ends_with("_pvalue"))

# manipulacao da tabela

oldnames = colnames(moran_test)

newnames = colnames(moran_test) |>
  as_tibble() |>
  mutate(
    value = str_remove(value, "_value")
  ) |> pull()

moran_test <- moran_test |>
  rename_at(vars(oldnames), ~ newnames) |>
  pivot_longer(
    Brancos_Baixo:Negros_Superior,
    values_to = "Moran",
    names_to = "Raça-classe"
  ) |>
  pivot_wider(
    names_from = "ano",
    values_from = "Moran"
  )

moran_test
clipr::write_last_clip()

# Mapas da proporção de negros e brancos por ano --------------------------

# Importacao dos dados do censo para criar variaveis

anos <- c(2000,2010)

RMs <- c("RMCampinas")

for(i in 1: length(anos)){
  ano = anos[i]
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

ap_RMCampinas_2000 <- ap_RMCampinas_2000 |>
  left_join(
    censo_2000_RMCampinas |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp)) |>
      select(area_ponderacao, cor_raca, peso) |>
      summarise(
        n = sum(peso),
        .by = c(cor_raca, area_ponderacao)
      ) |>
      filter(cor_raca != 0) |>
      group_by(cor_raca) |>
      mutate(
        prop_ap = round((n/sum(n))*100,2)
      ) |>
      ungroup() |>
      mutate(cor_raca = factor(cor_raca, levels = c(1,2), labels = c("Branco","Negro"))) |>
      select(-n) |>
      pivot_wider(
        names_from = cor_raca,
        values_from = prop_ap
      ),
    by = c("area_ponderacao"),
    keep = FALSE
  )

ap_RMCampinas_2010 <- ap_RMCampinas_2010 |>
  left_join(
    censo_2010_RMCampinas |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp)) |>
      select(area_ponderacao, cor_raca, peso) |>
      summarise(
        n = sum(peso),
        .by = c(cor_raca, area_ponderacao)
      ) |>
      filter(cor_raca != 0) |>
      group_by(cor_raca) |>
      mutate(
        prop_ap = round((n/sum(n))*100,2)
      ) |>
      ungroup() |>
      mutate(cor_raca = factor(cor_raca, levels = c(1,2), labels = c("Branco","Negro"))) |>
      select(-n) |>
      pivot_wider(
        names_from = cor_raca,
        values_from = prop_ap
      ),
    by = c("area_ponderacao"),
    keep = FALSE
  )


# Mapa 1 - Proporcao de brancos e negros por ap

ap_RMCampinas_2000 |>
  select(area_ponderacao, prop_branca, prop_negra, geom) |>
  pivot_longer(
    prop_branca:prop_negra,
    names_to = "cor_raca",
    values_to = "prop"
  ) |>
  mutate(ano = 2000) |>
  bind_rows(
    ap_RMCampinas_2010 |>
      select(area_ponderacao, prop_branca, prop_negra, geom) |>
      pivot_longer(
        prop_branca:prop_negra,
        names_to = "cor_raca",
        values_to = "prop"
      ) |>
      mutate(ano = 2010)
  ) |>
  mutate(
    cor_raca = str_remove(cor_raca,"prop_"),
    ano_fct = as.factor(ano),
    prop = round((prop*100),2),
    cor_raca = case_when(cor_raca == "negra" ~ "Negro", TRUE ~ "Branco")
  ) |>
  ggplot() +
  geom_sf(
    aes(fill = prop),
    lwd = 0
  ) +
  geom_sf(
    data = rm_shp_2010,
    fill = "transparent",
    colour = "black",
    size = .7
  ) +
  lemon::facet_rep_grid(cor_raca ~ ano_fct) +
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = guide_colorbar(title = "Parcela (%) da população ocupada acima de 10 anos\nde cada AP por cor ou raça")) +
  labs(
    caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = 1),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    # axis.title = element_text(size = 8, face = "bold", hjust = .5, vjust = .5),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#ffffff",linewidth = .01),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, hjust = .5, vjust = .5)
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.3
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "proporcao de cor ou raca das APs nas RMs em 2000 e 2010.jpeg",
  device = "jpeg",
  path = file.path("output","mapas"),
  width = 13,
  height = 13,
  units = "in"
)

# Mapa 2 - Distribuicao relativa de brancos e negros na RM

ap_RMCampinas_2000 |>
  select(area_ponderacao, Branco, Negro, geom) |>
  pivot_longer(
    Branco:Negro,
    names_to = "cor_raca",
    values_to = "prop"
  ) |>
  mutate(ano = 2000) |>
  bind_rows(
    ap_RMCampinas_2010 |>
      select(area_ponderacao, Branco, Negro, geom) |>
      pivot_longer(
        Branco:Negro,
        names_to = "cor_raca",
        values_to = "prop"
      ) |>
      mutate(ano = 2010)
  ) |>
  mutate(
    cor_raca = as.factor(cor_raca),
    ano_fct = as.factor(ano)
  ) |>
  ggplot() +
  geom_sf(
    aes(fill = prop),
    lwd = 0
  ) +
  geom_sf(
    data = rm_shp_2010,
    fill = "transparent",
    colour = "black",
    size = .7
  ) +
  lemon::facet_rep_grid(cor_raca ~ ano_fct) +
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = guide_colorbar(title = "Distribuição relativa (%) da população ocupada acima de 10 anos\nde cada cor ou raça por AP")) +
  labs(
    caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = 1),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    # axis.title = element_text(size = 8, face = "bold", hjust = .5, vjust = .5),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#ffffff",linewidth = .01),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, hjust = .5, vjust = .5)
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.3
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "distribuicao relativa da pop das APs por cor ou raca.jpeg",
  device = "jpeg",
  path = file.path("output","mapas"),
  width = 13,
  height = 13,
  units = "in"
)
