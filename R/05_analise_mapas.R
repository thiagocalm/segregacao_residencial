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
pacman::p_load(tidyverse, srvyr, readr, xlsx, geobr, sf, rgeoda, patchwork, ggspatial, spdep, jpeg, magick)
source("./R/X_funcao_metodos_espacial.R") # importando funcoes para usar funcao de fazer tabelas

# Definicao de parametros --------------------------------

# Definicao de qual classe usar
classe <- "SM"

# Criacao de arquivo de referencia das RMs

rm_codes = tibble(
  name = c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
           "RMRJ","RMSalvador","RMSP"),
  codes = c("RM Belo Horizonte","RM Campinas","RM Curitiba","RM Fortaleza","RM Porto Alegre","RM Recife",
            "RM Rio de Janeiro","RM Salvador","RM São Paulo")
)

# Parametros de ano e RM
anos <- c(2000,2010)
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")


# Importacao dos dados ----------------------------------------------------

for(i in 1: length(anos)){
  ano = anos[i]

  for(k in 1: length(RMs)){
    RM = RMs[k]

    # Importacao dos dados
    load(file.path("./output",classe,paste0("resultados_QL_indice_",ano,".RData")))

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
  }
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

# Juncao dos dados espaciais ao QL

RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(k in 1: length(RMs)){

  RM = RMs[k]

  ap_RM_2000 <- get(glue::glue("QL_2000_{RM}")) |>
    left_join(
      ap_2000,
      by = c("area_ponderacao"),
      keep = FALSE
    ) |>
    distinct() |>
    mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
    st_as_sf()

  # Recaptura de poligonos vazios

  poligonos_vazios <- ap_RM_2000 |>
    select(area_ponderacao, code_munic, geom) |>
    filter(st_is_empty(geom))

  # retorno do contingente de poligonos vazios

  print(paste0("poligonos vazios para a ", RM," foi de ", dim(poligonos_vazios)[[1]]))

  assign(paste0("ap_",RM,"_",2000),ap_RM_2000)
}

rm(ap_2000, ap_RM_2000, sc_shp, sc_to_ap, poligonos_vazios)

# Tratamento dos dados espaciais - 2010 --------------------------------------

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

# Juntando os dados de QL com os dados espaciais

RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(k in 1: length(RMs)){

  RM = RMs[k]

  ap_RM_2010 <- get(glue::glue("QL_2010_{RM}")) |>
    left_join(
      ap_2010,
      by = c("area_ponderacao"),
      keep = FALSE
    ) |>
    distinct() |>
    mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
    st_as_sf()

  # Recaptura de poligonos vazios

  poligonos_vazios <- ap_RM_2010 |>
    select(area_ponderacao, code_munic, geom) |>
    filter(st_is_empty(geom))

  # Recaptura de poligonos vazios

  poligonos_vazios <- ap_RM_2010 |>
    select(area_ponderacao, code_munic, geom) |>
    filter(st_is_empty(geom))

  # retorno do contingente de poligonos vazios

  print(paste0("poligonos vazios para a ", RM," foi de ", dim(poligonos_vazios)[[1]]))

  assign(paste0("ap_",RM,"_",2010),ap_RM_2010)
}

rm(ap_2010, ap_RM_2010, sc_shp, sc_to_ap, poligonos_vazios)

# Visualizacao de ambas os anos conjuntamente -----------------------------

# Importacao de SHP das RMs
shp_2000_rms <- geobr::read_metro_area(year = 2001)
shp_2010_rms <- geobr::read_metro_area(year = 2010)

## Construcao dos mapas

for(k in seq_along(RMs)){
  # RM em analise
  RM <- RMs[k]

  # Selecao de dados
  rm_code <- rm_codes |> filter(name %in% RM) |> pluck(2)
  shp_2000_rm <- shp_2000_rms |> filter(name_metro %in% rm_code)
  shp_2010_rm <- shp_2010_rms |> filter(name_metro %in% rm_code)

  # Mapa
  fig <- (shp_2000_rm |>
            ggplot() +
            geom_sf(data = get(glue::glue("ap_{RM}_2000")), fill = "#2b8cbe") +
            geom_sf(fill = "transparent", colour = "black", size = .7) +
            theme_minimal() +
            labs(title = paste0(RM, " - 2010")) +
            theme(
              plot.title = element_text(face = "bold",size = 12, hjust = .5, vjust = .5),
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
              location = "tl", which_north = "true",
              pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
              style = north_arrow_fancy_orienteering
            )
  ) +
    (shp_2010_rm |>
       ggplot() +
       geom_sf(data = get(glue::glue("ap_{RM}_2010")), fill = "#2ca25f") +
       geom_sf(fill = "transparent", colour = "black", size = .7) +
       theme_minimal() +
       labs(title = paste0(RM, " - 2010")) +
       theme(
         plot.title = element_text(face = "bold",size = 12, hjust = .5, vjust = .5),
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
         location = "tl", which_north = "true",
         pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
         style = north_arrow_fancy_orienteering
       )
    )

  # Salvando imagem
  ggsave(
    plot = fig,
    filename = paste0(RM," - comparativo das APs nas RMs em 2000 e 2010.jpeg"),
    device = "jpeg",
    path = file.path("output",classe,"mapas"),
    width = 13,
    height = 13,
    units = "in"
  )

  # Proximo loop
  print(paste0("Finalizamos mapa para : ", RM,"..."))
  rm(fig,shp_2000_rm,shp_2010_rm,rm_code)

}

# Visualizacao dos dados da RM com nomes dos muncis -----------------------

for(k in 1: length(RMs)){

  # RM em analise
  RM <- RMs[k]

  # Selecao de dados
  rm_code <- rm_codes |> filter(name %in% RM) |> pluck(2)
  shp_2000_rm <- shp_2000_rms |> filter(name_metro %in% rm_code)
  shp_2010_rm <- shp_2010_rms |> filter(name_metro %in% rm_code)

  ap_RM <- get(glue::glue("ap_{RM}_2010"))

  fig <- (
    shp_2000_rm |>
      ggplot() +
      geom_sf(data = get(glue::glue("ap_{RM}_2000")), fill = "#f7f7f7", color = "#d9d9d9") +
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
      labs(title = paste0(RM, " - 2000")) +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
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
        location = "tl", which_north = "true",
        pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
        style = north_arrow_fancy_orienteering
      )
  ) + (
    shp_2010_rm |>
      ggplot() +
      geom_sf(data = get(glue::glue("ap_{RM}_2010")), fill = "#f7f7f7", color = "#d9d9d9") +
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
      labs(title = paste0(RM, " - 2010")) +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
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
        location = "tl", which_north = "true",
        pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
        style = north_arrow_fancy_orienteering
      )
  )

  ggsave(
    filename = glue::glue("{RM} - Relacao de municipios.jpeg"),
    device = "jpeg",
    path = file.path("output",classe,"mapas"),
    width = 13,
    height = 13,
    units = "in"
  )

  # Proximo loop
  print(paste0("Finalizamos mapa para : ", RM,"..."))
  rm(fig,shp_2000_rm,shp_2010_rm,rm_code)
}

# Mapas da proporção de negros e brancos por ano --------------------------

# Importacao dos dados do censo para criar variaveis

for(k in 1: length(RMs)){

  # Definindo parametros
  RM = RMs[k]
  # Selecao de dados
  rm_code <- rm_codes |> filter(name %in% RM) |> pluck(2)
  shp_2000_rm <- shp_2000_rms |> filter(name_metro %in% rm_code)
  shp_2010_rm <- shp_2010_rms |> filter(name_metro %in% rm_code)

  ## Importacao dos dados

  # 2000
  load(file.path("./dados",paste0("censo_tratado_2000_",RM,".RData")))

  # Ajuste dos dados na base do censo
  if(classe == "EGP"){
    censo <- censo |>
      mutate(
        estratos_sociais_egp = case_when(
          EGP11 %in% c(5, 1) ~ 2,
          EGP11 %in% c(2, 8, 3) ~ 1,
          EGP11 %in% c(4, 9, 10) ~ 0)
      ) |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp))
  } else{
    censo <- censo |>
      filter(!is.na(estrato_renda_sm))
  }

  # juncao de dados e calculo
  ap_graf_2000 <- get(glue::glue("ap_{RM}_2000")) |>
    left_join(
      censo |>
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

  rm(censo)

  # 2010
  load(file.path("./dados",paste0("censo_tratado_2010_",RM,".RData")))

  # Ajuste dos dados na base do censo
  if(classe == "EGP"){
    censo <- censo |>
      mutate(
        estratos_sociais_egp = case_when(
          EGP11 %in% c(5, 1) ~ 2,
          EGP11 %in% c(2, 8, 3) ~ 1,
          EGP11 %in% c(4, 9, 10) ~ 0)
      ) |>
      filter(idade >= 10 & PO == 1 & !is.na(estratos_sociais_egp))
  } else{
    censo <- censo |>
      filter(!is.na(estrato_renda_sm))
  }

  # juncao dos dados
  ap_graf_2010 <- get(glue::glue("ap_{RM}_2010")) |>
    left_join(
      censo |>
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

  rm(censo)

  # Mapa 1 - Proporcao de brancos e negros por ap

  fig2 <- ap_graf_2000 |>
    select(area_ponderacao, prop_branca, prop_negra, geom) |>
    pivot_longer(
      prop_branca:prop_negra,
      names_to = "cor_raca",
      values_to = "prop"
    ) |>
    mutate(ano = 2000) |>
    bind_rows(
      ap_graf_2010 |>
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
      color = "transparent",
      lwd = 0
    ) +
    geom_sf(
      data = shp_2010_rm,
      fill = "transparent",
      colour = "black",
      size = .7
    ) +
    lemon::facet_rep_grid(cor_raca ~ ano_fct) +
    scale_fill_distiller(palette = "Spectral")

  if(classe == "EGP"){
    fig2 <- fig2 +
      guides(fill = guide_colorbar(title = "Parcela (%) da população ocupada acima de 10 anos\nde cada AP por cor ou raça")) +
      labs(
        caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
      )
  } else{
    fig2 <- fig2 +
      guides(fill = guide_colorbar(title = "Parcela (%) da população urbana de cada AP\npor cor ou raça")) +
      labs(
        caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
      )
  }

  fig2 <- fig2 +
    # tira sistema cartesiano
    theme(
      plot.caption = element_blank(),
      legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = 1),
      legend.text = element_text(size = 8, hjust = 0, vjust = .5),
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_text(size = 12),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "#ffffff",linewidth = .01),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12, hjust = .5, vjust = .5)
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
    filename = paste0(RM, " - proporcao de cor ou raca das APs nas RMs em 2000 e 2010.jpeg"),
    device = "jpeg",
    path = file.path("output",classe, "mapas"),
    width = 13,
    height = 13,
    units = "in"
  )

  # Mapa 2 - Distribuicao relativa de brancos e negros na RM

  fig2 <- ap_graf_2000 |>
    select(area_ponderacao, Branco, Negro, geom) |>
    pivot_longer(
      Branco:Negro,
      names_to = "cor_raca",
      values_to = "prop"
    ) |>
    mutate(ano = 2000) |>
    bind_rows(
      ap_graf_2010 |>
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
      color = "transparent",
      lwd = 0
    ) +
    geom_sf(
      data = shp_2010_rm,
      fill = "transparent",
      colour = "black",
      size = .7
    ) +
    lemon::facet_rep_grid(cor_raca ~ ano_fct) +
    scale_fill_distiller(palette = "Spectral")

  if(classe == "EGP"){
    fig2 <- fig2 +
      guides(fill = guide_colorbar(title = "Distribuição relativa (%) da população ocupada acima de 10 anos\nde cada cor ou raça por AP")) +
      labs(
        caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
      )
  } else{
    fig2 <- fig2 +
      guides(fill = guide_colorbar(title = "Distribuição relativa (%) da população urbana de cada\ncor ou raça por AP")) +
      labs(
        caption = "Fonte: IBGE, Censo Demográfico, 2000 e 2010."
      )
  }

  fig2 <- fig2 +
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
    filename = paste0(RM, " - distribuicao relativa da pop das APs por cor ou raca.jpeg"),
    device = "jpeg",
    path = file.path("output",classe, "mapas"),
    width = 13,
    height = 13,
    units = "in"
  )

  # exportacao

  # Proximo loop
  print(paste0("Finalizamos mapas para RM: ",RM,"!!!"))
  rm(ap_graf_2000,ap_graf_2010,fig2)
  invisible(gc())
}

# LISA - 2000 e 2010 ---------------------------------------------------------

for(i in seq_along(anos)){
  ano = anos[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]
    # definindo base
    df <- get(glue::glue("ap_{RM}_{ano}"))

    # Aplicacao de Lisa
    df <- func_lisa_classes(data = df)

    # retorno
    assign(paste0("ap_",RM,"_",ano),df)

    # proximo loop
    rm(df)
    print(paste0("Finalizamos criacao LISA para: ", RM," em ", ano,"..."))
  }
}

# Mapas ------------------------------------------------------------

if(classe == "EGP"){
  relacoes <- c("BB","BI","BS","NB","NI","NS")
  relacoes_extensa <- c(
    "Branco-Baixo","Branco-Intermediário","Branco-Superior",
    "Negro-Baixo","Negro-Intermediário","Negro-Superior"
  )
} else{
  relacoes <- c("BmeioSM","Bmeioa1SM","B1a3SM","B3SMmais","NmeioSM","Nmeioa1SM","N1a3SM","N3SMmais")
  relacoes_extensa <- c(
    "Branco - Até 1/2 SM","Branco - 1/2 a 1 SM", "Branco - 1 a 3 SM","Branco - 3 SM ou mais",
    "Negro - Até 1/2 SM","Negro - 1/2 a 1 SM", "Negro - 1 a 3 SM","Negro - 3 SM ou mais"
  )
}

for(i in seq_along(anos)){
  ano = anos[i]
  for(j in seq_along(relacoes)){
    relacao = relacoes[j]
    relacao_extensa = relacoes_extensa[j]
    for(k in seq_along(RMs)){
      # selecionando paramentros em relacao a RM
      RM = RMs[k]
      rm_code <- rm_codes |> filter(name %in% RM) |> pluck(2)
      if(ano == 2000){
        shp_2000_rm <- shp_2000_rms |> filter(name_metro %in% rm_code)
      } else{
        shp_2010_rm <- shp_2010_rms |> filter(name_metro %in% rm_code)
      }

      # Mapa
      get(glue::glue("ap_{RM}_{ano}")) |>
        select(LISA_var = glue::glue("LISA_{relacao}_map")) |>
        filter(!is.na(LISA_var)) |>
        ggplot() +
        geom_sf(aes(fill = LISA_var),
                lwd = 0) +
        geom_sf(data = get(glue::glue("shp_{ano}_rm")),
                fill = "transparent",
                colour = "black", size = 0.5) +
        scale_fill_manual(
          values = c(
            "Baixo-Baixo" = "#045a8d", "Alto-Baixo" = "#a6bddb","Baixo-Alto" = "#fc9272",
            "Alto-Alto" = "#a50f15","Não significativo" =  "#f0f0f0"
          )) +
        guides(fill = guide_legend(title = glue::glue("LISA Map {RM}: {relacao_extensa}"))) +
        labs(
          title = glue::glue("{ano}"),
          caption = glue::glue("Fonte: IBGE, Censo Demográfico {ano}.")
        ) +
        # tira sistema cartesiano
        theme(
          plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
          plot.caption = element_blank(),
          legend.title = element_text(face = "bold", size = 12, hjust = 0, vjust = .5),
          legend.text = element_text(size = 12, hjust = 0, vjust = .9),
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
          location = "tl", which_north = "true",
          pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"),
          style = north_arrow_fancy_orienteering
        )
      # Exportacao
      ggsave(
        filename = glue::glue("{RM} - LISA_{ano}_{relacao}.jpeg"),
        device = "jpeg",
        path = file.path("output",classe,"mapas"),
        width = 13,
        height = 13,
        units = "in"
      )
    }
    print(paste0("Finalizamos a categoria ",relacao,"..."))
  }
  print(paste0("Finalizamos o ano de ",ano,"..."))
}

# Indice de Moran ---------------------------------------------------------

for(i in seq_along(anos)){
  ano = anos[i]
  for(k in seq_along(RMs)){
    RM = RMs[k]
    # definindo base
    df <- get(glue::glue("ap_{RM}_{ano}"))

    # Aplicacao de Lisa
    df <- func_moran_classes(data = df)

    # exportacao
    if(i == 1 & k == 1){
      df_export <- df |>
        mutate(
          ano = ano,
          RM = RM
        )
    } else{
      df_export <- df_export |>
        bind_rows(
          df |>
            mutate(
              ano = ano,
              RM = RM
            )
        )
    }
    if(i == length(anos) & k == length(RMs)){
      assign(paste0("moran_index"),df_export)
    }


    # proximo loop
    rm(df, queen_w)
    print(paste0("Finalizamos criacao Moran para: ", RM," em ", ano,"..."))
  }
}

# Exportando materiais ----------------------------------------------------

wb <- openxlsx::loadWorkbook(paste0('./output/',classe,'/tabelas/Tabela - Autocorrelacao espacial por RM.xlsx'))

for(k in seq_along(RMs)){
  RM = RMs[k]
  rm = RMs[k]
  sheet_number = 1+k
  # importacao de imagens
  fig1 <- file.path("output",classe,"mapas",glue::glue("{RM} - comparativo das APs nas RMs em 2000 e 2010.jpeg"))
  fig2 <- file.path("output",classe,"mapas",glue::glue("{RM} - distribuicao relativa da pop das APs por cor ou raca.jpeg"))
  fig3 <- file.path("output",classe,"mapas",glue::glue("{RM} - proporcao de cor ou raca das APs nas RMs em 2000 e 2010.jpeg"))
  if(classe == "EGP"){
    fig4 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_BB.jpeg"))
    fig5 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_BI.jpeg"))
    fig6 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_BS.jpeg"))
    fig7 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_NB.jpeg"))
    fig8 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_NI.jpeg"))
    fig9 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_NS.jpeg"))
    fig10 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_BB.jpeg"))
    fig11 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_BI.jpeg"))
    fig12 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_BS.jpeg"))
    fig13 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_NB.jpeg"))
    fig14 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_NI.jpeg"))
    fig15 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_NS.jpeg"))
  }else{
    fig4 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_BmeioSM.jpeg"))
    fig5 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_Bmeioa1SM.jpeg"))
    fig6 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_B1a3SM.jpeg"))
    fig7 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_B3SMmais.jpeg"))
    fig8 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_NmeioSM.jpeg"))
    fig9 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_Nmeioa1SM.jpeg"))
    fig10 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_N1a3SM.jpeg"))
    fig11 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2000_N3SMmais.jpeg"))
    fig12 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_BmeioSM.jpeg"))
    fig13 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_Bmeioa1SM.jpeg"))
    fig14 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_B1a3SM.jpeg"))
    fig15 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_B3SMmais.jpeg"))
    fig16 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_NmeioSM.jpeg"))
    fig17 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_Nmeioa1SM.jpeg"))
    fig18 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_N1a3SM.jpeg"))
    fig19 <- file.path("output",classe,"mapas",glue::glue("{RM} - LISA_2010_N3SMmais.jpeg"))
  }

  # Substituicao de arquivos no excel
  if(classe == "SM"){
    # Moran - 2000
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_number,
      x = moran_index |> filter(ano == 2000 & RM == rm) |> select(2:3),
      xy = c(4,5),
      colNames = FALSE,
      rowNames = FALSE,
      keepNA = FALSE
    )
    # Moran - 2010
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_number,
      x = moran_index |> filter(ano == 2010 & RM == rm) |> select(2:3),
      xy = c(6,5),
      colNames = FALSE,
      rowNames = FALSE,
      keepNA = FALSE
    )
  }else{
    # Moran - 2000
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_number,
      x = moran_index |> filter(ano == 2000 & RM == rm) |> select(2:3),
      xy = c(3,5),
      colNames = FALSE,
      rowNames = FALSE,
      keepNA = FALSE
    )
    # Moran - 2010
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_number,
      x = moran_index |> filter(ano == 2010 & RM == rm) |> select(2:3),
      xy = c(5,5),
      colNames = FALSE,
      rowNames = FALSE,
      keepNA = FALSE
    )
  }
  # Colocando imagens
  openxlsx::insertImage(
    wb,
    sheet_number,
    fig1,
    startRow = 15,
    startCol = 2,
    width = 6,
    height = 6
  )
  openxlsx::insertImage(
    wb,
    sheet_number,
    fig2,
    startRow = 2,
    startCol = 9,
    width = 6,
    height = 6
  )
  openxlsx::insertImage(
    wb,
    sheet_number,
    fig3,
    startRow = 32,
    startCol = 9,
    width = 6,
    height = 6
  )
  if(classe == "SM"){
    openxlsx::insertImage(wb, sheet_number,fig4, startRow = 2, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig5,startRow = 32, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig6, startRow = 62, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig7, startRow = 92, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig8, startRow = 2, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig9, startRow = 32, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig10, startRow = 62, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig11, startRow = 92, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig12, startRow = 2, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig13,startRow = 32, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig14, startRow = 62, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig15, startRow = 92, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig16, startRow = 2, startCol = 33, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig17, startRow = 32, startCol = 33, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig18, startRow = 62, startCol = 33, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig19, startRow = 92, startCol = 33, width = 6, height = 6)
  }else{
    openxlsx::insertImage(wb, sheet_number,fig4, startRow = 2, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig5,startRow = 32, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig6, startRow = 62, startCol = 15, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig7, startRow = 2, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig8, startRow = 32, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig9, startRow = 62, startCol = 21, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig10, startRow = 2, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig11,startRow = 32, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig12, startRow = 62, startCol = 27, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig13, startRow = 2, startCol = 33, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig14, startRow = 32, startCol = 33, width = 6, height = 6)
    openxlsx::insertImage(wb, sheet_number,fig15, startRow = 62, startCol = 33, width = 6, height = 6)
  }
}

openxlsx::saveWorkbook(
  wb,
  paste0('./output/',classe,'/tabelas/','[Ultima atualizacao em ',today(),'] Tabela - Autocorrelacao espacial por RM.xlsx'),
  overwrite = TRUE
)
