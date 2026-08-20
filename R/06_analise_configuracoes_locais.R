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

###
# Estrutura de rendimentos em decis para Brasil, RMs e Raca
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
  ) |>
  # rm e raca
  mutate(
    decimos_renda_rm_raca = ntile(renda_pc, 10),
    .by = c(rm, cor_raca_d)
  ) |>
  # centesimos de renda rm e raca
  mutate(
    centesimos_renda_rm_raca = ntile(renda_pc, 100),
    .by = c(rm, cor_raca_d)
  )

###
# Salarios minimos
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
    )
  )

# Tabela 1 ----------------------------------------------------------------
#' Renda mínima de cada percentil calculado dentro de cada RM e grupo racial
#' Desagregacao: RMs e Brasil, Brancos, Pretos, Pardos

t1_2010 <- censo_2010_RMs |>
  summarise(
    min = min(renda_pc),
    .by = c(ano, rm, decimos_renda_rm_raca)
  ) |>
  arrange(ano, rm, decimos_renda_rm_raca) |>
  filter(
    decimos_renda_rm_raca %in% c(2,5,10)
  ) |>
  mutate(
    raca = 0
  ) |>
  select(ano,rm, raca,decimos_renda_rm_raca,min) |>
  bind_rows(
    censo_2010_RMs |>
      summarise(
        min = min(renda_pc),
        .by = c(ano, rm, cor_raca_d, decimos_renda_rm_raca)
      ) |>
      arrange(ano, rm, cor_raca_d, decimos_renda_rm_raca) |>
      filter(
        decimos_renda_rm_raca %in% c(2,5,10),
        cor_raca_d != 0
      ) |>
      select(ano, rm, "raca" = cor_raca_d, decimos_renda_rm_raca, min)
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
    decimos_renda_rm_raca = factor(
      decimos_renda_rm_raca,
      levels = c(2,5,10),
      labels = c("P10","P50","P90")
    )
  ) |>
  pivot_wider(
    names_from = raca,
    values_from = min
  )

# Tabela 2 ----------------------------------------------------------------
#' Rendimento em SMs desagregados
#' Desagregacao: RMs e Brasil, Brancos, Pretos, Pardos

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

# Tabela 3 - Desigualdade de renda, Ratio 90/10 ----------------------------------

t3_2010 <- censo_2010_RMs |>
  summarise(
    renda = weighted.mean(renda_pc,peso),
    .by = c(rm, cor_raca_d, decimos_renda_rm_raca)
  ) |>
  filter(decimos_renda_rm_raca %in% c(1,10),
         cor_raca_d != 0) |>
  pivot_wider(names_from = decimos_renda_rm_raca,
              values_from = renda,
              names_prefix = "decil_") |>
  mutate(ratio = decil_10 / decil_1) |>
  # agregando resultados para populacao total
  bind_rows(
    censo_2010_RMs |>
      summarise(
        cor_raca_d = 0,
        renda = weighted.mean(renda_pc,peso),
        .by = c(rm, decimos_renda_rm_raca)
      ) |>
      filter(decimos_renda_rm_raca %in% c(1,10)) |>
      pivot_wider(names_from = decimos_renda_rm_raca,
                  values_from = renda,
                  names_prefix = "decil_") |>
      mutate(ratio = decil_10 / decil_1)
  ) |>
  arrange(rm, cor_raca_d) |>
  select(-starts_with("decil_"))

t3 <- t3_2010 |>
  # bind_rows(t3_2000) |>
  filter(rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")) |>
  mutate(raca = factor(cor_raca_d,
                       levels = c(0,1,2,4),
                       labels = c("Total","Branco","Preto","Pardo")),
         rm = factor(rm,
                     levels = c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre"),
                     ordered = TRUE)) |>
  select(-cor_raca_d) |>
  arrange(rm) |>
  pivot_wider(names_from = raca,values_from = ratio)

# Tabela 4 - Desigualdade de renda, Gini ----------------------------------

t4_2010 <- censo_2010_RMs |>
  arrange(rm, cor_raca_d, renda_pc) |>
  summarise(
    pop = sum(peso),
    renda = weighted.mean(renda_pc,peso),
    .by = c(rm, cor_raca_d, centesimos_renda_rm_raca)
  ) |>
  mutate(
    pop_share = pop / sum(pop),
    renda_share = renda / sum(renda),
    pop_cum = cumsum(pop_share),
    renda_cum = cumsum(renda_share),
    .by = c(rm, cor_raca_d)
  ) |>
  select(rm, cor_raca_d, centesimos_renda_rm_raca, pop_cum, renda_cum) |>
  filter(cor_raca_d != 0) |>
  # agregando resultados para populacao total
  bind_rows(
    censo_2010_RMs |>
      arrange(rm, renda_pc) |>
      summarise(
        cor_raca_d = 0,
        pop = sum(peso),
        renda = weighted.mean(renda_pc,peso),
        .by = c(rm, centesimos_renda_rm_raca)
      ) |>
      mutate(
        pop_share = pop / sum(pop),
        renda_share = renda / sum(renda),
        pop_cum = cumsum(pop_share),
        renda_cum = cumsum(renda_share),
        .by = c(rm, cor_raca_d)
      ) |>
      select(rm, cor_raca_d, centesimos_renda_rm_raca, pop_cum, renda_cum)
  ) |>
  arrange(rm, cor_raca_d)

t4_gini <- t4_2010 |>
  # bind_rows(t3_2000) |>
  filter(rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")) |>
  mutate(raca = factor(cor_raca_d,
                       levels = c(0,1,2,4),
                       labels = c("Total","Branco","Preto","Pardo")),
         rm = factor(rm,
                     levels = c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre"),
                     ordered = TRUE)) |>
  select(-cor_raca_d) |>
  arrange(rm) |>
  summarise(
    gini = mean(2 * renda_cum) * 100,
    .by = c(rm, raca)
  )

t4_curve <- t4_2010 |>
  # bind_rows(t3_2000) |>
  filter(rm %in% c("RMFortaleza","RMPortoAlegre","RMRecife","RMCuritiba")) |>
  mutate(raca = factor(cor_raca_d,
                       levels = c(0,1,2,4),
                       labels = c("Total","Branco","Preto","Pardo")),
         rm = factor(rm,
                     levels = c("RMFortaleza","RMRecife","RMCuritiba","RMPortoAlegre"),
                     ordered = TRUE)) |>
  select(-cor_raca_d) |>
  arrange(rm)

t4_curve |>
  ggplot() +
  aes(x = pop_cum, y = renda_cum, color = raca, group = interaction(raca,raca)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  lemon::facet_rep_wrap(. ~ rm,repeat.tick.labels = TRUE) +
  labs(
    title = "Curva de Lorenz para distribuição de renda domicilar total per capita por RMs e raça.",
    x = "Distribuição da populalação acumulada",
    y = "Distribuição da renda acumulada",
    color = "Cor ou raça",
    caption = "Fonte: IBGE, Censo Demográfico 2010, microdados."
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", hjust = .5),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    # legend.position.inside = c(.94,.94),
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  )
