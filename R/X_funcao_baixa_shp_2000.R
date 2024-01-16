
# Funcao para baixar shp de SC 2000 ---------------------------------------

# Os SC do pacote {geobr} estao com algum problema de consistencia interna
# Dado que nao encontramos outro pacote ou arquivo que construa uma base unica
# de shp para todas as UFs, vamos construir a nossa, do zero.

func_download_shp_sc <- function(ano, UF = "all") {
  # import packages

  pacman::p_load(downloader,tidyverse, rvest, httr, xml2, sf)

  # error messages

  if(ano != 2000){
    stop("This function have been implemented only for 2000 yet...")
  }

  # diretorios

  temp_dir <- tempdir()

  if(UF == "all"){
    ufs <- c("ro","ac","am","rr","pa","ap","to","ma","pi","ce","rn","pb","pe","al",
             "se","ba","mg","es","rj","sp","pr","sc","rs","ms","mt","go","df")
  }else{
    ufs <- UF
  }

  for(k in seq_along(ufs)){
    # criando objeto para a uf da vez
    uf <- ufs[k]

    url_root <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/"
    url_ano <- paste0("censo_",ano)
    url_uf <- paste0("/setor_urbano/",uf,"/")
    url <- paste0(url_root, url_ano, url_uf)

    sc_por_uf <- httr::GET(
      url
    ) |>
      xml2::read_html() |>
      rvest::html_table()

    sc_por_uf <- sc_por_uf[[1]] |>
      select(Name) |>
      filter(!Name %in% c("Parent Directory","")) |>
      mutate(Name = str_remove(Name, "/")) |> pull()

    for(i in seq_along(sc_por_uf)){
      # definindo diretorios de loop
      sc <- sc_por_uf[i]
      url_download <- paste0(url,sc,"/",sc,".zip")

      # download file
      # download.file(url_download, temp_file)
      download(url_download, dest=paste0(temp_dir,"/",sc,".zip"), mode="wb")

      unzip (paste0(temp_dir,"/",sc,".zip"), exdir = paste0(temp_dir,"/",sc))

      # importacao do arquivo

      if(i == 1){
        sc_shp_uf <- read_sf(
          dsn = file.path(temp_dir,sc),
          layer = sc
        ) |>
          st_transform(crs = 4674) |>
          rename(
            setor_censitario = ID_,
            geom = geometry
          ) |>
          select(setor_censitario, geom) |>
          mutate(
            state_code = as.numeric(str_sub(setor_censitario,1,2)),
            setor_censitario = as.numeric(setor_censitario)
          ) |>
          select(setor_censitario, state_code, geom)
      } else{
        sc_shp_uf <- sc_shp_uf |>
          bind_rows(
            read_sf(
              dsn = file.path(temp_dir,sc),
              layer = sc
            ) |>
              st_transform(crs = 4674) |>
              rename(
                setor_censitario = ID_,
                geom = geometry
              ) |>
              select(setor_censitario, geom) |>
              mutate(
                state_code = as.numeric(str_sub(setor_censitario,1,2)),
                setor_censitario = as.numeric(setor_censitario)
              ) |>
              select(setor_censitario, state_code, geom)
          )
      }
    }

    # juncao com os dados de outras ufs
    if(k == 1){
      sc_shp <- sc_shp_uf
    } else{
      sc_shp <- sc_shp |>
        bind_rows(sc_shp_uf)
    }

    # proximo loop
    rm(sc_shp_uf,sc_por_uf)
    print(paste0("Finalizamos a uf ", uf," do ano ", ano,"!!!"))
  }
  return(sc_shp)
}
