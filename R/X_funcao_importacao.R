# Função de importacao e manipulacao dos dados ----------------------------

func_import_handl_data <- function(UF, ano) {
  # import packages

  pacman::p_load(SAScii,tidyverse, srvyr, readr)

  # Definindo diretorios de importacao

  DIR <- file.path("./dados/bruto",ano)

  if(ano == 2010){
    # unziping arquivos

    # unzip(zipfile = file.path(DIR, paste0(UF,".zip")),
    #       files = unzip(zipfile = file.path(DIR, paste0(UF,".zip")), list = TRUE)[c(2,5),1],
    #       exdir = DIR)

    # definicao do diretorio de importacao dos dados - 2010
    DIR_dom <- file.path(DIR, UF, list.files(file.path(DIR, UF))[1])
    DIR_pes <- file.path(DIR, UF, list.files(file.path(DIR, UF))[2])
  } else{
    # definicao do diretorio de importacao dos dados - 2000
    DIR_dom <- file.path(DIR, UF, list.files(file.path(DIR, UF))[1])
    DIR_pes <- file.path(DIR, UF, list.files(file.path(DIR, UF))[3])

  }

  # importacao de arquivo para imput (sas)
  load(file.path(DIR, "sas_imput.RData"))

  # importacao do .txt

  if(ano == 2000){
    censo_dom <- readr::read_fwf(
      file = DIR_dom,
      col_positions = readr::fwf_widths(
        widths = abs(sas_imput$domicilio$width),
        col_names = sas_imput$domicilio$varname
      )
    ) |>
      filter(v1006 == 1 &
               v1004 %in% c("03","05","07","08","13","14","16","17","26")) |>
      mutate(id_dom = v0300) |>
      select(-c(v0102, v1002, v1003, v0104,v0105, v0300, v0400, v1001, v1004,
                areap, v1005, v1006, v1007,p001, starts_with("m")))

    print(paste0("Domicilios para ",UF, " e ano ",ano, " importados!"))

    censo_pes <- readr::read_fwf(
      file = DIR_pes,
      col_positions = readr::fwf_widths(
        widths = abs(sas_imput$pessoa$width),
        col_names = sas_imput$pessoa$varname
      )
    ) |>
      filter(v1006 == 1 &
               v1004 %in% c("03","05","07","08","13","14","16","17","26")) |>
      mutate(id_pes = as.numeric(paste0(v0300, v0400)),
             id_dom = v0300) |>
      select(-c(v0300, starts_with("m")))

    print(paste0("Pessoas para ",UF, " e ano ",ano, " importados!"))

    censo <- censo_pes |>
      left_join(censo_dom, by = c("id_dom"), keep = FALSE)

    # selecionando variaveis a serem utilizadas

    vars <- c("id_dom", "id_pes", "v1004", "v0103","v1007", "areap","p001", "v0408",
              "v4572","v0401","v4300","v0439","v0440","v0441","v0442","v0443","v0444",
              "v0447","v0448","v0449","v0450","v0455","v0456","v4452","v4462",
              "v4511","v4512","v4513","v4521","v4522","v4523","v4525","v4614",
              "v0201","v7100","v0402")

    censo <- censo |>
      select(all_of(vars)) |>
      mutate_all(as.numeric)

    print(paste0("Iniciando manipulação dos dados para ",UF, " e ano ",ano, "!"))

    # manipulacao das variaveis

    censo <- censo |>
      # lidando com as relacoes intradomiciliares que nao serao consideradas
      mutate(relacao_delet = case_when(v0402 == 10 | v0402 == 11 ~ 1, TRUE ~ 0)) |>
      mutate(total_relacao_delet = sum(relacao_delet), .by = id_dom) |>
      mutate(n_pes_dom = v7100 - total_relacao_delet) |>
      # cor ou raca
      # filter(v0408 %in% c(1, 2, 4)) |>
      mutate(cor_raca = case_when(v0408 == 1 ~ 1,v0408 %in% c(2, 4) ~ 2, TRUE ~ 0)) |>
      # deflacionamento do rendimento total
      mutate(v4614_defl = deflateBR::deflate(v4614, as.Date("2000-08-01"), "08/2022","inpc")) |>
      select(- c(relacao_delet, total_relacao_delet, v7100, v0408))
  } else{

    censo_dom <- readr::read_fwf(
      file = DIR_dom,
      col_positions = readr::fwf_widths(
        widths = abs(sas_imput$domicilio$width),
        col_names = sas_imput$domicilio$varname
      )
    ) |>
      filter(v1006 == 1 &
               v1004 %in% c("07","11","15","16","19","20","22","23","34")) |>
      mutate(id_dom = v0300) |>
      select(-c(v0001, v0002, v0011, v0300, v0010, v1001, v1002, v1003, v1004,
                v1006, starts_with("m")))

    print(paste0("Domicilios para ",UF, " e ano ",ano, " importados!"))

    censo_pes <- readr::read_fwf(
      file = DIR_pes,
      col_positions = readr::fwf_widths(
        widths = abs(sas_imput$pessoa$width),
        col_names = sas_imput$pessoa$varname
      )
    ) |>
      filter(v1006 == 1 &
               v1004 %in% c("07","11","15","16","19","20","22","23","34")) |>
      mutate(id_pes = as.numeric(paste0(v0300, v0504)),
             id_dom = v0300) |>
      select(-c(v0300, starts_with("m")))

    print(paste0("Pessoas para ",UF, " e ano ",ano, " importados!"))

    censo <- censo_pes |>
      left_join(censo_dom, by = c("id_dom"), keep = FALSE)

    # selecionando variaveis a serem utilizadas

    vars <- c("id_dom", "id_pes", "v1004", "v0002", "v0011","v0010", "v0606",
              "v6036","v0601","v6400","v6910","v6900","v0652","v0643","v0644","v0645",
              "v6461","v6471","v0648","v0649","v0650","v0654","v0656","v0651",
              "v6511","v6513","v0652","v6521","v6525","v6527","v4001","v0401",
              "v0502")

    censo <- censo |>
      select(all_of(vars)) |>
      mutate_all(as.numeric)

    print(paste0("Iniciando manipulação dos dados para ",UF, " e ano ",ano, "!"))

    # manipulacao das variaveis

    censo <- censo |>
      # lidando com as relacoes intradomiciliares que nao serao consideradas
      mutate(relacao_delet = case_when(v0502 == 18 | v0502 == 19 ~ 1, TRUE ~ 0)) |>
      mutate(total_relacao_delet = sum(relacao_delet), .by = id_dom) |>
      mutate(n_pes_dom = v0401 - total_relacao_delet) |>
      # cor ou raca
      # filter(v0606 %in% c(1, 2, 4)) |>
      mutate(cor_raca = case_when(v0606 == 1 ~ 1,v0606 %in% c(2, 4) ~ 2, TRUE ~ 0)) |>
      # deflacionamento do rendimento total
      mutate(v6527_defl = deflateBR::deflate(v6527, as.Date("2010-08-01"), "08/2022","inpc")) |>
      select(- c(relacao_delet, total_relacao_delet, v0401, v0606))
  }

  # salvando em novo arquivo

  return(censo)
}

# func_import_handl_data_2010 <- function(UF, ano) {
#   # import packages
#
#   pacman::p_load(SAScii,tidyverse, srvyr, readr)
#
#   # unziping arquivos
#   DIR <- file.path("./dados/bruto",ano)
#
#   unzip(zipfile = file.path(DIR, paste0(UF,".zip")),
#         files = unzip(zipfile = file.path(DIR, paste0(UF,".zip")), list = TRUE)[c(2,5),1],
#         exdir = DIR)
#
#   # Definindo diretorios de importacao
#
#   DIR_dom <- file.path(DIR, UF, list.files(file.path(DIR, UF))[1])
#   DIR_pes <- file.path(DIR, UF, list.files(file.path(DIR, UF))[2])
#
#   # importacao de arquivo para imput (sas)
#   load(file.path(DIR, "sas_imput.RData"))
#
#   # importacao do .txt
#
#   censo_dom <- readr::read_fwf(
#     file = DIR_dom,
#     col_positions = readr::fwf_widths(
#       widths = abs(sas_imput$domicilio$width),
#       col_names = sas_imput$domicilio$varname
#     )
#   ) |>
#     filter(v1006 == 1 &
#              v1004 %in% c("03","05","07","08","13","14","16","17","26")) |>
#     mutate(id_dom = v0300) |>
#     select(-c(v0102, v1002, v1003, v0104,v0105, v0300, v0400, v1001, v1004,
#               areap, v1005, v1006, v1007,p001, starts_with("m")))
#
#   print(paste0("Domicilios para ",UF, " e ano ",ano, " importados!"))
#
#   censo_pes <- readr::read_fwf(
#     file = DIR_pes,
#     col_positions = readr::fwf_widths(
#       widths = abs(sas_imput$pessoa$width),
#       col_names = sas_imput$pessoa$varname
#     )
#   ) |>
#     filter(v1006 == 1 &
#              v1004 %in% c("03","05","07","08","13","14","16","17","26")) |>
#     mutate(id_pes = as.numeric(paste0(v0300, v0400)),
#            id_dom = v0300) |>
#     select(-c(v0300, starts_with("m")))
#
#   print(paste0("Pessoas para ",UF, " e ano ",ano, " importados!"))
#
#   censo <- censo_pes |>
#     left_join(censo_dom, by = c("id_dom"), keep = FALSE)
#
#   # selecionando variaveis a serem utilizadas
#
#   vars <- c("id_dom", "id_pes", "v1004", "v0103","v1007", "areap","p001", "v0408",
#             "v4572","v0401","v4300","v0439","v0440","v0441","v0442","v0443","v0444",
#             "v0447","v0448","v0449","v0450","v0455","v0456","v4452","v4462",
#             "v4511","v4512","v4513","v4521","v4522","v4523","v4525","v4614",
#             "v0201","v7100","v0402")
#
#   censo <- censo |>
#     select(all_of(vars)) |>
#     mutate_all(as.numeric)
#
#   print(paste0("Iniciando manipulação dos dados para ",UF, " e ano ",ano, "!"))
#
#   # manipulacao das variaveis
#
#   censo <- censo |>
#     # lidando com as relacoes intradomiciliares que nao serao consideradas
#     mutate(relacao_delet = case_when(v0402 == 10 | v0402 == 10 ~ 1, TRUE ~ 0)) |>
#     mutate(total_relacao_delet = sum(relacao_delet), .by = id_dom) |>
#     mutate(n_pes_dom = v7100 - total_relacao_delet) |>
#     # cor ou raca
#     filter(v0408 %in% c(1, 2, 4)) |>
#     mutate(cor_raca = case_when(v0408 == 1 ~ 1, TRUE ~ 2)) |>
#     # deflacionamento do rendimento total
#     mutate(v4614_defl = deflateBR::deflate(v4614, as.Date("2000-08-01"), "08/2022","inpc")) |>
#     select(- c(relacao_delet, total_relacao_delet, v7100, v0408))
#
#   # salvando em novo arquivo
#
#   return(censo)
# }
