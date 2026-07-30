#'-----------------------------------------
#'CALCULA INDICES DE DISSIMILARIDADE LOCAL E GLOBAL ESPACIALIZADOS
#'Ref: Barros e Feitosa (2024), Feitosa et al. (2007)
#'-----------------------------------------

options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, sf, geobr, spdep, FNN, Matrix,
               terra, tmap, scales, patchwork)


# 1. Importacao de areas de ponderacao ------------------------------------

# importa rms
rms <- geobr::read_metro_area(year = 2010)

# importa areas de ponderacao
ap <- geobr::read_weighting_area(year = 2010)

# seleciona APs dentro das RMs
ap_sf <- ap |>
  st_filter(rms, .predicate = st_intersects)

rm(rms, ap)

ap_sf |>
  ggplot() +
  geom_sf()

# 2. prepara dados por grupos para cada RM --------------------------------

anos <- 2010
RMs <- c("RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife")

for(i in 1: length(anos)){
  ANO = anos[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]
    # Importacao dos dados
    load(file.path("./dados",paste0("censo_tratado_",ANO,"_",RM,".RData")))
    # criando dataset
    censo <- censo |>
      mutate(rm = RM, ano = ANO) |>
      select(ano, rm, everything())
    # atribuindo a objeto agregando censos
    if(ANO == 2000){
      if(k == 1){
        censo_2000_RMs <- censo
      } else{
        censo_2000_RMs <- censo_2000_RMs |>
          bind_rows(censo)
      }
    } else{
      if(k == 1){
        censo_2010_RMs <- censo
      } else{
        censo_2010_RMs <- censo_2010_RMs |>
          bind_rows(censo)
      }
    }
    # Proximo loop
    print(paste0("Finalizamos a RM: ",RM," para o ano ",ANO,"!!!"))
    rm(censo)
    gc()
  }
}

# Variaveis derivadas: faixas de SM

censo_ne <- censo_2010_RMs |>
  mutate(regiao = str_sub(area_ponderacao,1,1)) |>
  filter(regiao == 2) |>
  mutate(
    SM = case_when(
      renda_pc < 510 * 0.25 ~ 1,
      renda_pc >= 510 * 0.25 & renda_pc < 510 * 0.5 ~ 2,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 0.75 ~ 3,
      renda_pc >= 510 * 0.75 & renda_pc < 510 * 1.25 ~ 4,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 4 ~ 5,
      TRUE ~ 6
    )
  )

censo_sul <- censo_2010_RMs |>
  mutate(regiao = str_sub(area_ponderacao,1,1)) |>
  filter(regiao == 4) |>
  mutate(
    SM = case_when(
      renda_pc < 510 * 0.5 ~ 1,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 1.25 ~ 2,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 2 ~ 3,
      renda_pc >= 510 * 2 & renda_pc < 510 * 4 ~ 4,
      TRUE ~ 5
    )
  )

# combinacao de faixa de renda e raca

censo_ne <- censo_ne |>
  mutate(
    SM = factor(SM,
                levels = seq(1L,6L),
                labels = c("[0 a 0,25 SM)", "[0,25 a 0,5 SM)", "[0,5 a 0,75 SM)",
                           "[0,75 a 1,25 SM)", "[1,25 a 4 SM)","[4 SM+")),
    cor_raca_d = factor(cor_raca_d,
                        levels = c(1,2,4,0),
                        labels = c("Branco","Preto","Pardo","Outros"))
  )

censo_sul <- censo_sul |>
  mutate(
    SM = factor(SM,
                levels = seq(1L,5L),
                labels = c("[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 2 SM)",
                           "[2 a 4 SM)", "[4 SM+")),
    cor_raca_d = factor(cor_raca_d,
                        levels = c(1,2,4,0),
                        labels = c("Branco","Preto","Pardo","Outros"))
  )


# calcula populacao absoluta, cria categorias combinadas e pivot_wider dados

pop_grupos_ne <- censo_ne |>
  summarise(
    pop = sum(peso),
    .by = c(rm, cor_raca_d, SM, area_ponderacao)
  ) |>
  filter(cor_raca_d != "Outros") |>
  mutate(
    grupo = paste0(cor_raca_d, " - ", SM)
  ) |>
  select(-cor_raca_d, -SM) |>
  arrange(rm, grupo) |>
  pivot_wider(
    names_from = grupo,
    values_from = pop,
    values_fill = 0
  )

pop_grupos_sul <- censo_sul |>
  summarise(
    pop = sum(peso),
    .by = c(rm, cor_raca_d, SM, area_ponderacao)
  ) |>
  filter(cor_raca_d != "Outros") |>
  mutate(
    grupo = paste0(cor_raca_d, " - ", SM)
  ) |>
  select(-cor_raca_d, -SM) |>
  arrange(rm, grupo) |>
  pivot_wider(
    names_from = grupo,
    values_from = pop,
    values_fill = 0
  )

rm(censo_2010_RMs, censo_ne, censo_sul)


# 3. Combina dados espaciais e estatisticos -------------------------------

ap_sf_sul <- ap_sf |>
  inner_join(
    pop_grupos_sul,
    by = c("code_weighting" = "area_ponderacao")
  )

ap_sf_ne <- ap_sf |>
  inner_join(
    pop_grupos_ne,
    by = c("code_weighting" = "area_ponderacao")
  )

rm(ap_sf, pop_grupos_ne, pop_grupos_sul)


# 4. parametros --------------------------------------------

# bandwidths

bandwidths <- c(
  1,
  700,
  1000,
  2000,
  3000,
  4000,
  5000,
  6000,
  7000
)

# nomes das colunas dos grupos

grupos_ne <- ap_sf_ne |>
  st_drop_geometry() |>
  select(-1:-10) |>
  colnames()

grupos_sul <- ap_sf_sul |>
  st_drop_geometry() |>
  select(-1:-10) |>
  colnames()

# 5. Calcula indice -------------------------------------------------------

calc_dissimilarity <- function(data, grupo, bw){

  ###
  # calcula matriz de distancia
  ###

  Dmat <- data |>
    st_distance() |>
    units::drop_units() |>
    as.matrix()

  ###
  # matriz de populacao
  ###

  X <- data |>
    st_drop_geometry() |>
    select(all_of(grupo)) |>
    as.matrix()

  ## número de áreas

  J <- nrow(X)

  ## número de grupos

  M <- ncol(X)

  ## população de cada área

  Nj <- rowSums(X)

  ## população total

  N <- sum(Nj)

  ## proporção metropolitana

  tau_global <- colSums(X) / N

  ## índice de informação

  I <- sum( tau_global * (1 - tau_global) )

  ###
  # kernel gaussiano
  ###

  # calcula matriz de pesos
  W <- exp(-(Dmat^2)/(2*bw^2))

  # Incluindo auto-interacao
  diag(W) <- 1

  ## padronizacao

  rs <- rowSums(W)
  rs[rs==0] <- 1
  W <- W / rs

  W <- Matrix(W, sparse = TRUE)

  ###
  # população suavizada
  ###

  L <- W %*% X

  # população total suavizada

  Lj <- rowSums(L)

  ###
  # proporções locais
  ###

  tau_local <- sweep(L, 1, Lj, "/")

  ###
  # diferença absoluta
  ###

  dif <- abs( sweep(tau_local,2,tau_global,"-") )

  ###
  # índice local
  ###

  d_local <- (Nj/(N*I)) * ( rowSums(dif)/2 )

  ###
  # índice global
  ###

  D_global <- sum(d_local)

  output <- data |>
    mutate(
      bandwidth = bw,
      D_global = D_global,
      D_local = d_local
    )

  ###
  # return
  ###

  return(output)
}

###
# calcular para todas as escalas
###

# resultado <- purrr::map_dfr(
#   bandwidths,
#   \(bw)
#   calc_dissimilarity(
#     data  = ap_sf_ne,
#     grupo = grupos_ne,
#     bw    = bw,
#     Dmat  = dist_ne
#   )
# )

resultado_ne <- split(ap_sf_ne, ap_sf_ne$rm) |>
  purrr::imap_dfr(\(dados_rm, rm){
    purrr::map_dfr(bandwidths,
                   \(bw)
                   calc_dissimilarity(
                     data = dados_rm,
                     grupo = grupos_ne,
                     bw = bw
                    )
                   )
    }
  )

resultado_sul <- split(ap_sf_sul, ap_sf_sul$rm) |>
  purrr::imap_dfr(\(dados_rm, rm){
    purrr::map_dfr(bandwidths,
                   \(bw)
                   calc_dissimilarity(
                     data = dados_rm,
                     grupo = grupos_sul,
                     bw = bw
                   )
    )
   }
  )

# combinando ambos

spatial_d <- resultado_ne |>
  mutate(regiao = "Nordeste") |>
  select(rm, regiao, bandwidth, D_global, D_local, geometry,
         "area_ponderacao" = code_weighting, "municipio" = name_muni) |>
  distinct() |>
  bind_rows(
    resultado_sul |>
      mutate(regiao = "Sul") |>
      select(rm, regiao, bandwidth, D_global, D_local, geometry,
             "area_ponderacao" = code_weighting, "municipio" = name_muni) |>
      distinct()
  ) |>
  mutate(rm = str_sub(rm, 3))


###
# índice global
###

spatial_d |>
  st_drop_geometry() |>
  select(rm, bandwidth, D_global) |>
  distinct()

###
# perfil multiescalar
###

spatial_d |>
  st_drop_geometry() |>
  distinct() |>
  ggplot() +
  aes(bandwidth , D_global, color = rm) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  coord_cartesian(ylim = c(0,round(max(spatial_d$D_global) * 1.2,2))) +
  scale_x_continuous(breaks = bandwidths) +
  scale_y_continuous() +
  facet_wrap(. ~ regiao) +
  labs(
    x = "Bandwidth (m)",
    y = "Índice Global de Dissimilaridade"
  )+
  theme_bw()

###
# mapas
###

# tema

theme_maps <- function(){
  theme(
    plot.title = element_text(lineheight=1, size=30, face="bold",hjust = 0.5),
    plot.subtitle = element_text(lineheight=1, size=25, hjust = 0),
    plot.caption = element_text(lineheight=1, size=15, hjust=0.5),
    legend.title = element_blank (),
    legend.text = element_text(colour="black", size = 13),
    legend.position="bottom",
    legend.background = element_rect(fill=NA, colour = NA),
    legend.key.size = unit(1.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.title.x = element_blank (),
    axis.text.x  = element_text(colour = "grey80"),
    axis.title.y =  element_blank (),
    axis.text.y  = element_text(colour = "grey80"),
    axis.ticks= element_blank (),
    strip.text = element_text(size=15, face="bold",margin = margin(.1,0,.1,0, "cm")),
    plot.background =  element_rect(fill = "white"),
    panel.grid.major=element_line(colour="grey95",linewidth=.5),
    panel.grid.minor=element_line(colour="grey95",linewidth=.15),
    panel.border = element_rect(colour = "grey95", fill=NA, linewidth=.75),
    panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF")
  )
}

# ajustes na base

spatial_d <- spatial_d |>
  mutate(
    D_quartil = cut(
      D_local,
      breaks = quantile(D_local,
                        probs = seq(0,1,0.25),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1","Q2","Q3","Q4")
    ),
    .by = rm
  ) |>
  mutate(
    D_quartil = factor(D_quartil,
                       levels = c("Q1","Q2","Q3","Q4"))
  )

## Curitiba

df_spatial <- spatial_d |>
  filter(rm == "Curitiba")

map_1 <- df_spatial |>
  filter(bandwidth %in% c(700,7000)) |>
  mutate(bandwidth = factor(bandwidth,
                            levels = c(700,7000),
                            labels = c("Bandwidth = 700m","Bandwidth = 7000m"))) |>
  ggplot() +
  geom_sf(
    aes(fill = D_quartil),
    color = NA
  ) +
  scale_fill_manual(
    name = "D (local)",
    values = c("Q1" = "#FEE5D9","Q2" = "#fdbb84","Q3" = "#e34a33","Q4" = "#7f0000"),
    # Legend on the bottom part of the graph
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  coord_sf() +
  labs(title = paste0(df_spatial$rm |> unique())) +
  facet_wrap(
    . ~ bandwidth,
    ncol = 4
  ) +
  theme_maps()

ggsave(file.path("output","ConfigLocal","Mapa_DLocal_Curitiba.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 15)

## Porto Alegre

df_spatial <- spatial_d |>
  filter(rm == "PortoAlegre")

map_2 <- df_spatial |>
  filter(bandwidth %in% c(700,7000)) |>
  mutate(bandwidth = factor(bandwidth,
                            levels = c(700,7000),
                            labels = c("Bandwidth = 700m","Bandwidth = 7000m")),
         rm = case_when(rm == "PortoAlegre" ~ "Porto Alegre")) |>
  ggplot() +
  geom_sf(
    aes(fill = D_quartil),
    color = NA
  ) +
  scale_fill_manual(
    name = "D (local)",
    values = c("Q1" = "#FEE5D9","Q2" = "#fdbb84","Q3" = "#e34a33","Q4" = "#7f0000"),
    # Legend on the bottom part of the graph
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  coord_sf() +
  labs(title = paste0(df_spatial$rm |> unique())) +
  facet_wrap(
    . ~ bandwidth,
    ncol = 4
  ) +
  theme_maps()

ggsave(file.path("output","ConfigLocal","Mapa_DLocal_PortoAlegre.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 15)

## Fortaleza

df_spatial <- spatial_d |>
  filter(rm == "Fortaleza")

map_3 <- df_spatial |>
  filter(bandwidth %in% c(700,7000)) |>
  mutate(bandwidth = factor(bandwidth,
                            levels = c(700,7000),
                            labels = c("Bandwidth = 700m","Bandwidth = 7000m"))) |>
  ggplot() +
  geom_sf(
    aes(fill = D_quartil),
    color = NA
  ) +
  scale_fill_manual(
    name = "D (local)",
    values = c("Q1" = "#FEE5D9","Q2" = "#fdbb84","Q3" = "#e34a33","Q4" = "#7f0000"),
    # Legend on the bottom part of the graph
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  coord_sf() +
  labs(title = paste0(df_spatial$rm |> unique())) +
  facet_wrap(
    . ~ bandwidth,
    ncol = 4
  ) +
  theme_maps()

ggsave(file.path("output","ConfigLocal","Mapa_DLocal_Fortaleza.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 15)

## Recife

df_spatial <- spatial_d |>
  filter(rm == "Recife")

map_4 <- df_spatial |>
  filter(bandwidth %in% c(700,7000)) |>
  mutate(bandwidth = factor(bandwidth,
                            levels = c(700,7000),
                            labels = c("Bandwidth = 700m","Bandwidth = 7000m"))) |>
  ggplot() +
  geom_sf(
    aes(fill = D_quartil),
    color = NA
  ) +
  scale_fill_manual(
    name = "D (local)",
    values = c("Q1" = "#FEE5D9","Q2" = "#fdbb84","Q3" = "#e34a33","Q4" = "#7f0000"),
    # Legend on the bottom part of the graph
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  coord_sf() +
  labs(title = paste0(df_spatial$rm |> unique())) +
  facet_wrap(
    . ~ bandwidth,
    ncol = 4
  ) +
  theme_maps()

ggsave(file.path("output","ConfigLocal","Mapa_DLocal_Recife.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 15)


# Exportacao --------------------------------------------------------------

# exportar tabela com a base de dados

df <- spatial_d |>
  st_drop_geometry() |>
  select(regiao, rm, municipio, area_ponderacao, everything())

clipr::write_clip(df)

# dissimilaridade global

spatial_d |>
  st_drop_geometry() |>
  select(rm, bandwidth, D_global) |>
  distinct() |>
  pivot_wider(
    names_from = rm,
    values_from = D_global
  )

clipr::write_last_clip()
