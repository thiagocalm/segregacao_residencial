#'-----------------------------------------
#'CALCULA INDICES DE DISSIMILARIDADE LOCAL E GLOBAL ESPACIALIZADOS
#'Ref: Barros e Feitosa (2024), Feitosa et al. (2007)
#'-----------------------------------------

options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, sf, geobr, spdep, FNN, Matrix,
               terra, tmap, scales)


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

