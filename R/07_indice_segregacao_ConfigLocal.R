options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)

# Importacao de dados -----------------------------------------------------

## Definicao de parametros de importacao

# anos <- c(2000,2010)
anos <- 2010
RMs <- c("RMBH","RMCampinas","RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife",
         "RMRJ","RMSalvador","RMSP")

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

invisible(gc())

# Variaveis derivadas -----------------------------------------------------

###
# Faixas de salario minimo
###

censo_2010_RMs <- censo_2010_RMs |>
  mutate(
    SM_desagregado = case_when(
      renda_pc < 510 * 0.25 ~ 1,
      renda_pc >= 510* 0.25 & renda_pc < 510 * 0.5 ~ 2,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 0.75 ~ 3,
      renda_pc >= 510 * 0.75 & renda_pc < 510 * 1 ~ 4,
      renda_pc >= 510 & renda_pc < 510 * 1.25 ~ 5,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 1.5 ~ 6,
      renda_pc >= 510 * 1.5 & renda_pc < 510 * 1.75 ~ 7,
      renda_pc >= 510 * 1.75 & renda_pc < 510 * 2 ~ 8,
      renda_pc >= 510 * 2 & renda_pc < 510 * 2.25 ~ 9,
      renda_pc >= 510 * 2.25 & renda_pc < 510 * 2.5 ~ 10,
      renda_pc >= 510 * 2.5 & renda_pc < 510 * 2.75 ~ 11,
      renda_pc >= 510 * 2.75 & renda_pc < 510 * 3 ~ 12,
      renda_pc >= 510 * 3 & renda_pc < 510 * 3.25 ~ 13,
      renda_pc >= 510 * 3.25 & renda_pc < 510 * 3.5 ~ 14,
      renda_pc >= 510 * 3.5 & renda_pc < 510 * 3.75 ~ 15,
      renda_pc >= 510 * 3.75 & renda_pc < 510 * 4 ~ 16,
      TRUE ~ 17
    ),
    SM_local = case_when(
      renda_pc < 510 * 0.5 ~ 1,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 1.25 ~ 2,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 4 ~ 3,
      renda_pc >= 510 * 4  ~ 4
    ),
    SM_NE = case_when(
      renda_pc < 510 * 0.25 ~ 1,
      renda_pc >= 510 * 0.25 & renda_pc < 510 * 0.5 ~ 2,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 0.75 ~ 3,
      renda_pc >= 510 * 0.75 & renda_pc < 510 * 1.25 ~ 4,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 4 ~ 5,
      TRUE ~ 6
    ),
    SM_SUL = case_when(
      renda_pc < 510 * 0.5 ~ 1,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 1.25 ~ 2,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 2 ~ 3,
      renda_pc >= 510 * 2 & renda_pc < 510 * 4 ~ 4,
      TRUE ~ 5
    )
  )


# Selecao de RMs ----------------------------------------------------------

censo_2010_RMs <- censo_2010_RMs |>
  filter(
    rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")
  )


# Table 1 - distribuicao da categoria criada por par de rms ---------------

t1 <- censo_2010_RMs |>
  filter(
    rm %in% c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre")
  ) |>
  summarise(
    raca = 0,
    n = sum(peso),
    .by = c(ano, rm, SM_local)
  ) |>
  mutate(
    perc = round(n / sum(n) * 100, 2),
    .by = c(ano, rm, raca)
  ) |>
  select(ano, rm, raca, everything()) |>
  bind_rows(
    censo_2010_RMs |>
      filter(
        rm %in% c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre")
      ) |>
      filter(cor_raca_d != 0) |>
      summarise(
        n = sum(peso),
        .by = c(ano, rm, cor_raca_d, SM_local)
      ) |>
      mutate(
        perc = round(n / sum(n) * 100, 2),
        .by = c(ano, rm, cor_raca_d)
      ) |>
      select(ano, rm, "raca" = cor_raca_d, everything())
  ) |>
  arrange(ano, rm, raca, SM_local)

# ajustes de tabela

t1 <- t1 |>
  mutate(
    raca = factor(
      raca,
      levels = c(0,1,2,4),
      labels = c("Total","Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(1L,4L),
      labels = c("[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    rm = factor(rm,
                levels = c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre"),
                ordered = TRUE)) |>
  select(-n, -ano) |>
  pivot_wider(
    names_from = raca,
    values_from = perc
  ) |>
  arrange(rm,SM_local)

clipr::write_clip(t1)

# Calculo do D ------------------------------------------------------------

# funcao
source("./R/X_funcao_indices.R")

RMs <- c("RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife")

for(k in 1: length(RMs)){
  RM = RMs[k]

  print(paste0("Começando a RM: ",RM,"!!!"))

  # Importacao dos dados
  censo <- censo_2010_RMs |> filter(rm == RM)

  # definindo variavel renda e tipo de variavel
  var_renda <- "SM_local"
  tipo <- "SM_local"


  # Calcula D
  D <- func_calcula_dissimilaridade(
    data = censo,
    var_estrato = var_renda,
    tipo_variavel = tipo,
    cor_raca = "cor_raca_d",
    raca_d = TRUE
  )
  assign(paste0("D_2010_",RM),D)

  print(paste0("Finalizamos o cáculo do D para RM: ",RM,"!!!"))

  # Proximo loop
  print(paste0("Finalizamos a RM: ",RM,"!!!"))
  rm(censo, D)
  invisible(gc())
}

###
# Criando tabelas para os resultados
###

RMs <- c("RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife")

for(k in 1: length(RMs)){
  RM = RMs[k]

  print(paste0("Começando a RM: ",RM,"!!!"))

  # D - geral

  D_resultado_geral <- get(glue::glue("D_2010_{RM}"))$geral

  # D - por classe
  D_resultado_classes <- func_fazer_tabela(
    get(glue::glue("D_2010_{RM}"))$classe |>
      bind_rows(
        get(glue::glue("D_2010_{RM}"))$classe |>
          dplyr::select(grupo, D, cor_classe1 = cor_classe2, cor_classe2 = cor_classe1)
      ) |>
      distinct()
  )
  output <- list(
    D_resultado_geral = D_resultado_geral,
    D_resultado_classes = D_resultado_classes
  )

  assign(paste0("output_",RM),output)

  # Proximo loop
  print(paste0("Finalizamos a RM: ",RM,"!!!"))
  invisible(gc())
}

###
# Resultados
###

# Fortaleza
output_RMFortaleza$D_resultado_geral
output_RMFortaleza$D_resultado_classes

clipr::write_last_clip()

# Recife
output_RMRecife$D_resultado_geral
output_RMRecife$D_resultado_classes

clipr::write_last_clip()

# Curitiba
output_RMCuritiba$D_resultado_geral
output_RMCuritiba$D_resultado_classes

clipr::write_last_clip()

# PortoAlegre
output_RMPortoAlegre$D_resultado_geral
output_RMPortoAlegre$D_resultado_classes

clipr::write_last_clip()

# Calculo do QL ------------------------------------------------------------

# funcao
source("./R/X_funcao_indices.R")

RMs <- c("RMCuritiba","RMFortaleza","RMPortoAlegre","RMRecife")

for(k in 1: length(RMs)){
  RM = RMs[k]

  print(paste0("Começando a RM: ",RM,"!!!"))

  # Importacao dos dados
  censo <- censo_2010_RMs |> filter(rm == RM)

  # definindo variavel renda e tipo de variavel
  var_renda <- "SM_local"
  tipo <- "SM_local"

  # Calcula QL
  QL <- func_calcula_quociente_locacional(
    data = censo,
    var_estrato = var_renda,
    tipo_variavel = tipo,
    cor_raca = "cor_raca_d",
    raca_d = TRUE
  )
  assign(paste0("QL_2010_",RM),QL)

  print(paste0("Finalizamos o cáculo do QL para RM: ",RM,"!!!"))

  # Proximo loop
  print(paste0("Finalizamos a RM: ",RM,"!!!"))
  rm(censo, QL)
  invisible(gc())
}

###
# Resultados
###

#'-----
#'Tabelao por raca e classe
#'-----

### Curitiba

cor <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Porto Alegre

cor <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Fortaleza

cor <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Recife

cor <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)


#'-----
#'Tabela por classe
#'-----

### Curitiba
# Brancos
cor <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pretos
cor <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pardos
cor <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMCuritiba$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Porto Alegre
# Brancos
cor <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pretos
cor <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pardos
cor <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMPortoAlegre$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Fortaleza
# Brancos
cor <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pretos
cor <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pardos
cor <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMFortaleza$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

### Recife
# Brancos
cor <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_branco")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pretos
cor <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_preto")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)

# Pardos
cor <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(1) |>
  round(2)

clipr::write_clip(cor)

pvalue <- QL_2010_RMRecife$classe |>
  select(starts_with("QL_pardo")) |>
  as.matrix() |>
  Hmisc::rcorr(type="pearson") |>
  pluck(3) |>
  round(2)

clipr::write_clip(pvalue)


# QL - visualizacao -------------------------------------------------------

# tabela

tabela_ql <- QL_2010_RMFortaleza$classe_sintese |>
  mutate(rm = "Fortaleza") |>
  bind_rows(
    QL_2010_RMRecife$classe_sintese |>
      mutate(rm = "Recife")
  ) |>
  bind_rows(
    QL_2010_RMCuritiba$classe_sintese |>
      mutate(rm = "Curitiba")
  ) |>
  bind_rows(
    QL_2010_RMPortoAlegre$classe_sintese |>
      mutate(rm = "Porto Alegre")
  ) |>
  select(rm, everything()) |>
  pivot_longer(1:length(QL_2010_RMPortoAlegre$classe_sintese)+1,
               names_to = "group",
               values_to = "value") |>
  mutate(raca = c(rep(c(rep("Branco",8),rep("Preto",8),rep("Pardo",8)),4)),
         classe = c(rep(c("[4SM+","[1,25-4SM)","[0,5-1,25SM)","[0-0,5SM)"),24)),
         medida = c(rep(c(rep(c(rep("media",4),rep("sd",4)),3)),4))
  ) |>
  select(rm, raca, classe, medida, value)

tabela_ql |>
  pivot_wider(names_from = c(rm, medida), values_from = value)

clipr::write_last_clip()

# Visualizacao

tabela_plot <- tabela_ql |>
  mutate(classe1 = case_when(classe == "[4SM+" ~ "Classe Alta",
                             classe == "[0-0,5SM)" ~ "Classe Baixa",
                             TRUE ~ NA_character_),
         regiao = case_when(rm %in% c("Fortaleza","Recife") ~ "Nordeste",
                            TRUE ~ "Sul"),
         medida = factor(medida,
                         levels = c("media","sd"),
                         labels = c("Média QL", "Desv. Padrão QL"))) |>
  select(-classe) |>
  drop_na() |>
  pivot_wider(names_from = classe1, values_from = value)

data_long <- tabela_plot |>
  pivot_longer(
    cols = c(`Classe Alta`, `Classe Baixa`),
    names_to = "classe",
    values_to = "valor"
  ) |>
  mutate(
    classe = factor(
      classe,
      levels = c("Classe Baixa", "Classe Alta")
    ),
    raca = factor(
      raca,
      levels = c("Branco", "Preto", "Pardo")
    ),
    rm_raca = paste(rm, raca, sep = " — ")
  )

# plot

ggplot(data_long,
       aes(x = valor, y = rm_raca, color = classe)) +
  # linha ligando Classe Baixa e Classe Alta
  geom_segment(data = tabela_plot |> mutate(rm_raca = paste(rm, raca, sep = " — ")),
               aes(x = `Classe Baixa`,xend = `Classe Alta`,y = rm_raca,yend = rm_raca),
               inherit.aes = FALSE,
               linewidth = 1.1,
               color = "grey65"
               ) +
  # pontos
  geom_point(size = 3.5) +
  # uma coluna para cada região e uma linha para cada medida
  facet_grid(regiao ~ medida,
             scales = "free_y",
             space = "free_y") +
  scale_color_manual(values = c("Classe Baixa" = "#377EB8","Classe Alta"  = "#E41A1C")) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Quociente Locacional entre classes de renda e raça",
       subtitle = "Classe Baixa: '[0 - 0,5 SM)'; Classe Alta: '[4 SM +'."
       ) +
  theme_minimal(base_size = 13) +
  theme(
    # remove grades desnecessárias
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    # linha vertical de referência
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.4
    ),
    # espaço entre regiões
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(0.8, "lines"),
    # cabeçalho dos facets
    strip.background = element_rect(
      fill = "grey95",
      color = NA
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    strip.text.y = element_blank(),
    # nomes RM + raça
    axis.text.y = element_text(
      size = 10
    ),
    # legenda no topo
    legend.position = "top",
    legend.text = element_text(size = 11),
    # título
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      color = "grey35",
      size = 11
    )
  )
