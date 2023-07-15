options(scipen = 99999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(c(SAScii,tidyverse, srvyr, readr, lodown, fs))

# Download dos dados usando "lodown" -----------------------------------------------

DIR <- file.path("./dados/bruto/2000")
ano <- 2000

catalog <- lodown::get_catalog(data_name = "censo", output_dir = "./dados/bruto") |>
  dplyr::filter(year == ano) |>
  lodown::lodown(data_name = "censo")

## Conversao de arquivo de instrucao de importacao de SAS para R
# como mais de uma UF foi baixada, mas somente um arquivo de input é necessario
# (dado que todos sao iguais), foi selecionado o primeiro

sas_imput_dom <- SAScii::parse.SAScii(catalog$dom_sas[1]) |>
  dplyr::mutate(varname = stringr::str_to_lower(varname))

sas_imput_pes <- SAScii::parse.SAScii(catalog$pes_sas[1]) |>
  dplyr::mutate(varname = stringr::str_to_lower(varname))

sas_imput_fam <- SAScii::parse.SAScii(catalog$fam_sas[1]) |>
  dplyr::mutate(varname = stringr::str_to_lower(varname))

# criacao de um arquivo que contem todos os .sas

sas_imput <- list(domicilio = sas_imput_dom,
                  pessoa = sas_imput_pes,
                  familia = sas_imput_fam)

# salvando arquivos que contem todos os .sas
save(sas_imput, file = file.path(DIR, "sas_imput.RData"))

