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
# Estrutura de rendimentos em decis para Brasil e RMs
###

# censo_2000_RMs <- censo_2000_RMs |>
#   # brasil
#   mutate(
#     decimos_renda_br = ntile(renda_pc_def, 10)
#   ) |>
#   # rm
#   mutate(
#     decimos_renda_rm = ntile(renda_pc_def, 10),
#     .by = rm
#   )

###
# Estrutura de rendimentos em decis para Brasil e RMs
###

censo_2010_RMs <- censo_2010_RMs |>
  # brasil
  mutate(
    decimos_renda_br = ntile(renda_pc, 10)
  ) |>
  # rm
  mutate(
    decimos_renda_rm = ntile(renda_pc, 10),
    .by = rm
  )

###
# Salarios minimos
###

# censo_2000_RMs <- censo_2000_RMs |>
#   mutate(
#     SM_desagregado = case_when(
#       renda_pc_def < 1212 * 0.25 ~ 1,
#       renda_pc_def >= 1212* 0.25 & renda_pc_def < 1212 * 0.5 ~ 2,
#       renda_pc_def >= 1212 * 0.5 & renda_pc_def < 1212 * 0.75 ~ 3,
#       renda_pc_def >= 1212 * 0.75 & renda_pc_def < 1212 * 1 ~ 4,
#       renda_pc_def >= 1212 & renda_pc_def < 1212 * 1.25 ~ 5,
#       renda_pc_def >= 1212 * 1.25 & renda_pc_def < 1212 * 1.5 ~ 6,
#       renda_pc_def >= 1212 * 1.5 & renda_pc_def < 1212 * 1.75 ~ 7,
#       renda_pc_def >= 1212 * 1.75 & renda_pc_def < 1212 * 2 ~ 8,
#       renda_pc_def >= 1212 * 2 & renda_pc_def < 1212 * 2.25 ~ 9,
#       renda_pc_def >= 1212 * 2.25 & renda_pc_def < 1212 * 2.5 ~ 10,
#       renda_pc_def >= 1212 * 2.5 & renda_pc_def < 1212 * 2.75 ~ 11,
#       renda_pc_def >= 1212 * 2.75 & renda_pc_def < 1212 * 3 ~ 12,
#       renda_pc_def >= 1212 * 3 & renda_pc_def < 1212 * 3.25 ~ 13,
#       renda_pc_def >= 1212 * 3.25 & renda_pc_def < 1212 * 3.5 ~ 14,
#       renda_pc_def >= 1212 * 3.5 & renda_pc_def < 1212 * 3.75 ~ 15,
#       renda_pc_def >= 1212 * 3.75 & renda_pc_def < 1212 * 4 ~ 16,
#       TRUE ~ 17
#     )
#   )

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
    )
  )

# Tabela 1 ----------------------------------------------------------------
#' Renda mínima de cada percentil
#' Desagregacao: RMs e Brasil, Brancos, Pretos, Pardos

# t1_2000 <- censo_2000_RMs |>
#   summarise(
#     min = min(renda_pc_def),
#     .by = c(ano, rm, decimos_renda_rm)
#   ) |>
#   arrange(ano, rm, decimos_renda_rm) |>
#   filter(
#     decimos_renda_rm %in% c(1,5,10)
#   ) |>
#   mutate(
#     raca = 0
#   ) |>
#   select(ano,rm, raca,decimos_renda_rm,min) |>
#   bind_rows(
#     censo_2000_RMs |>
#       summarise(
#         min = min(renda_pc_def),
#         .by = c(ano, rm, cor_raca_d, decimos_renda_rm)
#       ) |>
#       arrange(ano, rm, cor_raca_d, decimos_renda_rm) |>
#       filter(
#         decimos_renda_rm %in% c(1,5,10),
#         cor_raca_d != 0
#       ) |>
#       select(ano, rm, "raca" = cor_raca_d, decimos_renda_rm, min)
#   )

t1_2010 <- censo_2010_RMs |>
  summarise(
    min = min(renda_pc),
    .by = c(ano, rm, decimos_renda_rm)
  ) |>
  arrange(ano, rm, decimos_renda_rm) |>
  filter(
    decimos_renda_rm %in% c(2,5,10)
  ) |>
  mutate(
    raca = 0
  ) |>
  select(ano,rm, raca,decimos_renda_rm,min) |>
  bind_rows(
    censo_2010_RMs |>
      summarise(
        min = min(renda_pc),
        .by = c(ano, rm, cor_raca_d, decimos_renda_rm)
      ) |>
      arrange(ano, rm, cor_raca_d, decimos_renda_rm) |>
      filter(
        decimos_renda_rm %in% c(2,5,10),
        cor_raca_d != 0
      ) |>
      select(ano, rm, "raca" = cor_raca_d, decimos_renda_rm, min)
  )

# juntando dados para os anos

t1 <-
  # t1_2000 |>
  bind_rows(t1_2010) |>
  mutate(
    raca = factor(
      raca,
      levels = c(0,1,2,4),
      labels = c("Total","Branco","Preto","Pardo")
    ),
    decimos_renda_rm = factor(
      decimos_renda_rm,
      levels = c(2,5,10),
      labels = c("P10","P50","P90")
    )
  ) |>
  pivot_wider(
    names_from = raca,
    values_from = min
  )

#'------------------------------ Usando dados de decil de renda para Brasil
#
# # t1_2000 <- censo_2000_RMs |>
# #   summarise(
# #     min = min(renda_pc_def),
# #     .by = c(ano, rm, decimos_renda_br)
# #   ) |>
# #   arrange(ano, rm, decimos_renda_br) |>
# #   filter(
# #     decimos_renda_br %in% c(1,5,10)
# #   ) |>
# #   mutate(
# #     raca = 0
# #   ) |>
# #   select(ano,rm, raca,decimos_renda_br,min) |>
# #   bind_rows(
# #     censo_2000_RMs |>
# #       summarise(
# #         min = min(renda_pc_def),
# #         .by = c(ano, rm, cor_raca_d, decimos_renda_br)
# #       ) |>
# #       arrange(ano, rm, cor_raca_d, decimos_renda_br) |>
# #       filter(
# #         decimos_renda_br %in% c(1,5,10),
# #         cor_raca_d != 0
# #       ) |>
# #       select(ano, rm, "raca" = cor_raca_d, decimos_renda_br, min)
# #   )
#
# t1_2010 <- censo_2010_RMs |>
#   summarise(
#     min = min(renda_pc),
#     .by = c(ano, rm, decimos_renda_br)
#   ) |>
#   arrange(ano, rm, decimos_renda_br) |>
#   filter(
#     decimos_renda_br %in% c(2,5,10)
#   ) |>
#   mutate(
#     raca = 0
#   ) |>
#   select(ano,rm, raca,decimos_renda_br,min) |>
#   bind_rows(
#     censo_2010_RMs |>
#       summarise(
#         min = min(renda_pc),
#         .by = c(ano, rm, cor_raca_d, decimos_renda_br)
#       ) |>
#       arrange(ano, rm, cor_raca_d, decimos_renda_br) |>
#       filter(
#         decimos_renda_br %in% c(2,5,10),
#         cor_raca_d != 0
#       ) |>
#       select(ano, rm, "raca" = cor_raca_d, decimos_renda_br, min)
#   )
#
# # juntando dados para os anos
#
# t1 <-
#   # t1_2000 |>
#   bind_rows(t1_2010) |>
#   mutate(
#     raca = factor(
#       raca,
#       levels = c(0,1,2,4),
#       labels = c("Brasil","Branco","Preto","Pardo")
#     ),
#     decimos_renda_br = factor(
#       decimos_renda_br,
#       levels = c(2,5,10),
#       labels = c("P10","P50","P90")
#     )
#   ) |>
#   pivot_wider(
#     names_from = raca,
#     values_from = min
#   )

# Tabela 2 ----------------------------------------------------------------
#' Rendimento em SMs desagregados
#' Desagregacao: RMs e Brasil, Brancos, Pretos, Pardos

# t2_2000 <- censo_2000_RMs |>
#   summarise(
#     raca = 0,
#     n = sum(peso),
#     .by = c(ano, rm, SM_desagregado)
#   ) |>
#   mutate(
#     perc = round(n / sum(n) * 100, 2),
#     .by = c(ano, rm, raca)
#   ) |>
#   select(ano, rm, raca, everything()) |>
#   bind_rows(
#     censo_2000_RMs |>
#       filter(cor_raca_d != 0) |>
#       summarise(
#         n = sum(peso),
#         .by = c(ano, rm, cor_raca_d, SM_desagregado)
#       ) |>
#       mutate(
#         perc = round(n / sum(n) * 100, 2),
#         .by = c(ano, rm, cor_raca_d)
#       ) |>
#       select(ano, rm, "raca" = cor_raca_d, everything())
#   ) |>
#   arrange(ano, rm, raca, SM_desagregado)

t2_2010 <- censo_2010_RMs |>
  summarise(
    raca = 0,
    n = sum(peso),
    .by = c(ano, rm, SM_desagregado)
  ) |>
  mutate(
    perc = round(n / sum(n) * 100, 2),
    .by = c(ano, rm, raca)
  ) |>
  select(ano, rm, raca, everything()) |>
  bind_rows(
    censo_2010_RMs |>
      filter(cor_raca_d != 0) |>
      summarise(
        n = sum(peso),
        .by = c(ano, rm, cor_raca_d, SM_desagregado)
      ) |>
      mutate(
        perc = round(n / sum(n) * 100, 2),
        .by = c(ano, rm, cor_raca_d)
      ) |>
      select(ano, rm, "raca" = cor_raca_d, everything())
  ) |>
  arrange(ano, rm, raca, SM_desagregado)

# juntando anos

t2 <-
  # t2_2000 |>
  bind_rows(t2_2010) |>
  mutate(
    raca = factor(
      raca,
      levels = c(0,1,2,4),
      labels = c("Total","Branco","Preto","Pardo")
    ),
    SM_desagregado = factor(
      SM_desagregado,
      levels = seq(1L,17L),
      labels = c(
        "[0 a 0.25 SM)", "[0.25 a 0.5 SM)",  "[0.5 a 0.75 SM)",
        "[0.75 a 1 SM)", "[1 a 1.25 SM)", "[1.25 a 1.5 SM)",
        "[1.5 a 1.75 SM)", "[1.75 a 2 SM)", "[2 a 2.25 SM)",
        "[2.25 a 2.5 SM)", "[2.5 a 2.75 SM)", "[2.75 a 3 SM)",
        "[3 a 3.25 SM)", "[3.25 a 3.5 SM)", "[3.5 a 3.75 SM)",
        "[3.75 a 4 SM)", "[4 SM+"
      )
    )
  ) |>
  select(-n) |>
  pivot_wider(
    names_from = raca,
    values_from = perc
  )

# grafico

t2_br <- t2 |>
  select(rm, SM_desagregado, "perc" = Total) |>
  mutate(raca_cor = "Total") |>
  filter(rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba"))

t2 |>
  pivot_longer(
    Branco:Pardo,
    names_to = "raca_cor",
    values_to = "perc"
  ) |>
  filter(rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")) |>
  ggplot() +
  geom_col(aes(x = SM_desagregado, y = perc, fill = raca_cor, , group = raca_cor), position = position_dodge2()) +
  geom_line(
    data = t2_br,
    aes(x = SM_desagregado, y = perc, group = raca_cor, color = raca_cor),
    color = "black",
    linewidth = 1.1,
    alpha = 0.6
  ) +
  geom_point(
    data = t2_br,
    aes(x = SM_desagregado, y = perc, group = raca_cor, color = raca_cor),
    color = "black",
    size = 4,
    alpha = 0.4
  ) +
  facet_wrap(. ~ rm) +
  labs(
    x = "Faixas de Salário Mínimo em 2010 (SM = R$ 510,00)",
    y = "Distribuição relativa (%)",
    fill = "Cor ou raça",
    caption = "Fonte: IBGE, Censo Demográfico 2010, microdados.\nNotas:\nLinha contínua no gráfico: Distribuição para o agregado de grupos raciais em cada RM.\n'[': Contido no intervalo.\n ')': Não contido no intervalo."
  ) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  coord_flip() +
  # theme_grey(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold", hjust = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(.94,.94),
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  )
