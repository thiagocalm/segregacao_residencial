options(scipen = 99999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(SAScii,tidyverse, srvyr, readr)

# Teste para uma UF - Ceara -----------------------------------------------

UF <- "CE"
DIR <- file.path("./dados/bruto/2000")
DIR_dom <- file.path("./dados/bruto/2000", UF, list.files(file.path(DIR, UF))[1])
DIR_pes <- file.path("./dados/bruto/2000", UF, list.files(file.path(DIR, UF))[3])
DIR_fam <- file.path("./dados/bruto/2000", UF, list.files(file.path(DIR, UF))[2])

# importacao de arquivo para imput (sas)
load(file.path(DIR, "sas_imput.RData"))

# importacao do .txt

censo_dom <- readr::read_fwf(
  file = DIR_dom,
  col_positions = readr::fwf_widths(
    widths = abs(sas_imput$domicilio$width),
    col_names = sas_imput$domicilio$varname
  )
) |>
  filter(v1006 == 1 & !is.na(v1004)) |>
  mutate(id_dom = v0300) |>
  select(-c(v0102, v1002, v1003, v0104, v0105, v0300, v0400, v1001, v1004, areap,
            v1005, v1006, v1007,p001, starts_with("m")))

censo_pes <- readr::read_fwf(
  file = DIR_pes,
  col_positions = readr::fwf_widths(
    widths = abs(sas_imput$pessoa$width),
    col_names = sas_imput$pessoa$varname
  )
) |>
  filter(v1006 == 1 & !is.na(v1004)) |>
  mutate(id_pes = as.numeric(paste0(v0300, v0400)),
         id_dom = v0300) |>
  select(-c(v0300, starts_with("m")))

censo <- censo_pes |>
  left_join(censo_dom, by = c("id_dom"), keep = FALSE)

### proximo passo é selecionar as variaveis necessarias para filtrar e ja termos uma base selecionada
# posso deletar as variaveis de v0102 a v1007 da base de domicilios, porque elas se repetem na de pessoas
