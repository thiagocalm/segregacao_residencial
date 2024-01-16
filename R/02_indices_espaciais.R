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

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx, geobr, sf, spdep)

## Importacao dos dados

anos <- c(2000,2010)
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

for(i in 1: length(anos)){
  ano = anos[i]
  for(k in 1: length(RMs)){
    RM = RMs[k]
    # Importacao dos dados
    load(file.path("./output",paste0("resultados_QL_indice_",ano,".RData")))

    # exportacao
    assign(paste0("QL_",ano,"_",RM),resultados_QL_index_2000[[k]][[2]])

    # Proximo loop
  }
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

ap_2000_sp <- ap_2000_sp |>
  as.data.frame() |>
  st_as_sf() |>
  group_by(area_ponderacao) |>
  summarise()

ap_2000_sp |>
  ggplot() +
  geom_sf(fill = "green")

ap_RMCampinas <- QL_2000_RMCampinas |>
  left_join(
    ap_2000_sp,
    by = c("area_ponderacao"),
    keep = FALSE
  ) |>
  distinct() |>
  select(-starts_with(c("pop","QL","Br","Ne","Re"))) |>
  mutate(code_munic = as.numeric(str_sub(area_ponderacao, 1, 7))) |>
  st_as_sf()

# Tratamento dos dados espaciais - 2010 --------------------------------------

rm_shp <- geobr::read_metro_area(year = 2010)

ap_2010 <- geobr::read_weighting_area() |>
  mutate(code_weighting = as.numeric(code_weighting),
         code_muni = as.numeric(code_muni))

ap_2010 |> ggplot() + geom_sf(fill = "grey")

# manual method
# ap_rmbh <- read_sf(dsn = "./input/2010", layer = "RMBH2010 urbano pop area de ponderação") |>
  # mutate(aps = as.numeric(aps))

# Tratamento dos dados  ------------------------------------------

QL_2010_RMBH <- QL_2010_RMBH |>
  select(area_ponderacao, starts_with("QL_")) |>
  mutate(munic = as.numeric(substr(area_ponderacao,1,7)))

QL_2010_RMBH_shp <- QL_2010_RMBH |>
  left_join(areas_ponderacao, by = c("munic" = "code_muni", "area_ponderacao" = "aps"),
            keep = TRUE) |>
  select(munic, code_muni, area_ponderacao, code_weighting, starts_with("QL_")) |> View()

areas_ponderacao <- st_as_sf(areas_ponderacao) # transformando em arquivo de classe sf

