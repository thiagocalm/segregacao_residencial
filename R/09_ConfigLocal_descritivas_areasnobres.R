options(scipen = 999)
rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, srvyr, readr, xlsx)

# Importacao de dados -----------------------------------------------------

## Definicao de parametros de importacao

anos <- 2010
RMs <- c("RMFortaleza","RMRecife")

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


# Criacao de grupos de areas nobres ---------------------------------------

AN_Recife <- c(2611606005032,
               2611606005009,
               2611606005010,
               2611606005033,
               2611606005018,
               2609600005010,
               2611606005034,
               2607901005013,
               2611606005012,
               2607901005003)

AN_Fortaleza <- c(2304400005024,
                  2304400005002,
                  2304400005022,
                  2304400005027,
                  2304400005058,
                  2304400005011,
                  2304400005016)

censo_2010_RMs <- censo_2010_RMs |>
  mutate(
    area_nobre = case_when(rm == "RMFortaleza" & area_ponderacao %in% AN_Fortaleza ~ 1,
                           rm == "RMRecife" & area_ponderacao %in% AN_Recife ~ 1,
                           TRUE ~ 0),
    SM_local = case_when(
      renda_pc < 510 * 0.5 ~ 1,
      renda_pc >= 510 * 0.5 & renda_pc < 510 * 1.25 ~ 2,
      renda_pc >= 510 * 1.25 & renda_pc < 510 * 4 ~ 3,
      renda_pc >= 510 * 4  ~ 4
    ))


# Tabelas descritivas -----------------------------------------------------

# Por SM

t1 <- censo_2010_RMs |>
  summarise(
    n = sum(peso),
    .by = c(rm, cor_raca_d, area_nobre, SM_local)
  ) |>
  mutate(perc = n / sum(n) * 100,
         .by = c(rm, cor_raca_d, area_nobre)) |>
  filter(cor_raca_d != 0) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(1L,4L),
      labels = c("[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    area_nobre = factor(
      area_nobre,
      levels = c(1,0),
      labels = c("AP - Area nobre","AP - Area não-nobre")
    )) |>
  arrange(rm, cor_raca_d, area_nobre, SM_local) |>
  select(-n) |>
  pivot_wider(names_from = c(rm, area_nobre),
              values_from = perc)

# por renda

t2 <- censo_2010_RMs |>
  summarise(
    SM_local = 0,
    media = weighted.mean(renda_pc, w = peso),
    .by = c(rm, cor_raca_d, area_nobre)
  ) |>
  bind_rows(
    censo_2010_RMs |>
      summarise(
        media = weighted.mean(renda_pc, w = peso),
        .by = c(rm, cor_raca_d, area_nobre, SM_local)
      )
  ) |>
  filter(cor_raca_d != 0) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(0L,4L),
      labels = c("Total","[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    area_nobre = factor(
      area_nobre,
      levels = c(1,0),
      labels = c("AP - Area nobre","AP - Area não-nobre")
    )) |>
  arrange(rm, cor_raca_d, area_nobre, SM_local) |>
  pivot_wider(names_from = c(rm, area_nobre),
              values_from = media) |>
  mutate(RMFortaleza_razao = `RMFortaleza_AP - Area nobre` / `RMFortaleza_AP - Area não-nobre`,
         RMRecife_razao = `RMRecife_AP - Area nobre` / `RMRecife_AP - Area não-nobre`) |>
  select(cor_raca_d, SM_local, starts_with("RMFortaleza"),everything())

# por escolaridade - pop 25 anos ou mais

t3 <- censo_2010_RMs |>
  filter(idade >= 25) |>
  summarise(
    SM_local = 0,
    n = sum(peso),
    .by = c(rm, cor_raca_d, area_nobre,anos_estudo)
  ) |>
  bind_rows(
    censo_2010_RMs |>
      filter(idade >= 25) |>
      summarise(
        n = sum(peso),
        .by = c(rm, cor_raca_d, area_nobre, SM_local,anos_estudo)
      )
  ) |>
  filter(cor_raca_d != 0, anos_estudo != 5) |>
  mutate(perc = n / sum(n) * 100,
         .by = c(rm, cor_raca_d, area_nobre, SM_local)) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(0L,4L),
      labels = c("Total","[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    area_nobre = factor(
      area_nobre,
      levels = c(1,0),
      labels = c("AP - Area nobre","AP - Area não-nobre")
    ),
    anos_estudo = factor(
      anos_estudo,
      levels = seq(1L,4L),
      labels = c("Sem instrução ou fund. incompleto","Fund. completo ou médio incompleto",
                 "Médio completo ou superior incompleto","Superior completo")
    )) |>
  arrange(rm, cor_raca_d, area_nobre, SM_local,anos_estudo) |>
  select(-n) |>
  pivot_wider(names_from = c(rm, area_nobre),
              values_from = perc) |>
  select(cor_raca_d, SM_local, everything())

# por concentracao em areas nobres

t4 <- censo_2010_RMs |>
  summarise(
    n = sum(peso),
    .by = c(rm, cor_raca_d, area_nobre, SM_local)
  ) |>
  mutate(perc = n / sum(n) * 100,
         .by = c(rm, cor_raca_d, SM_local)) |>
  filter(cor_raca_d != 0, area_nobre == 1) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(1L,4L),
      labels = c("[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    )) |>
  arrange(rm, cor_raca_d, SM_local) |>
  select(-n, -area_nobre) |>
  pivot_wider(names_from = c(rm),
              values_from = perc)

# Analises por casais -----------------------------------------------------

df_casais <- censo_2010_RMs |>
  mutate(tem_conjuge = if_else(relacao_dom == 2,1 , 0)) |>
  # numero de pessoas por domicilio
  mutate(val = 1,
         num_pessoas = sum(val),
         tem_conjuge = max(tem_conjuge),
         .by = id_dom) |>
  filter(num_pessoas >= 2,
         tem_conjuge == 1) |>
  arrange(id_dom, relacao_dom) |>
  mutate(raca_conjuge = lead(cor_raca_d),
         .by = id_dom) |>
  filter(relacao_dom == 1) |>
  mutate(raca_relacao = case_when(cor_raca_d == 1 & raca_conjuge == 1 ~ "Homogamia",
                                  cor_raca_d == 1 & raca_conjuge == 2 ~ "Heterogamia - Preto(a)",
                                  cor_raca_d == 1 & raca_conjuge == 4 ~ "Heterogamia - Pardo(a)",
                                  cor_raca_d == 2 & raca_conjuge == 1 ~ "Heterogamia - Branco(a)",
                                  cor_raca_d == 2 & raca_conjuge == 2 ~ "Homogamia",
                                  cor_raca_d == 2 & raca_conjuge == 4 ~ "Heterogamia - Pardo(a)",
                                  cor_raca_d == 4 & raca_conjuge == 1 ~ "Heterogamia - Branco(a)",
                                  cor_raca_d == 4 & raca_conjuge == 2 ~ "Heterogamia - Preto(a)",
                                  cor_raca_d == 4 & raca_conjuge == 4 ~ "Homogamia"))

# tabela - percentual

t5 <- df_casais |>
  summarise(n = sum(peso),
            .by = c(rm, cor_raca_d, area_nobre, SM_local, raca_relacao)) |>
  bind_rows(
    df_casais |>
      summarise(SM_local = 0,
                n = sum(peso),
                .by = c(rm, cor_raca_d, area_nobre, raca_relacao))
  ) |>
  mutate(perc = round(n / sum(n) * 100,2),
         .by = c(rm, cor_raca_d, SM_local, area_nobre)) |>
  filter(cor_raca_d != 0, !is.na(raca_relacao)) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(0L,4L),
      labels = c("Total","[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    raca_relacao = factor(
      raca_relacao,
      levels = c("Homogamia", "Heterogamia - Preto(a)", "Heterogamia - Pardo(a)", "Heterogamia - Branco(a)")
    ),
    area_nobre = factor(
      area_nobre,
      levels = c(1,0),
      labels = c("Area nobre","Area não-nobre"))) |>
  arrange(rm, cor_raca_d, SM_local, area_nobre, raca_relacao) |>
  select(-n) |>
  pivot_wider(names_from = c(rm, SM_local,area_nobre),
              values_from = perc)

# tabela - proporção

t6 <- df_casais |>
  summarise(n = sum(peso),
            .by = c(rm, cor_raca_d, area_nobre, SM_local, raca_relacao)) |>
  bind_rows(
    df_casais |>
      summarise(SM_local = 0,
                n = sum(peso),
                .by = c(rm, cor_raca_d, area_nobre, raca_relacao))
  ) |>
  mutate(prop = round(n / sum(n) * 100,2),
         .by = c(rm, cor_raca_d, SM_local, raca_relacao)) |>
  filter(cor_raca_d != 0, area_nobre == 1, !is.na(raca_relacao)) |>
  mutate(
    cor_raca_d = factor(
      cor_raca_d,
      levels = c(1,2,4),
      labels = c("Branco","Preto","Pardo")
    ),
    SM_local = factor(
      SM_local,
      levels = seq(0L,4L),
      labels = c("Total","[0 a 0,5 SM)", "[0,5 a 1,25 SM)",  "[1,25 a 4 SM)","[4 SM+")
    ),
    raca_relacao = factor(
      raca_relacao,
      levels = c("Homogamia", "Heterogamia - Preto(a)", "Heterogamia - Pardo(a)", "Heterogamia - Branco(a)")
    )) |>
  arrange(rm, cor_raca_d, SM_local, raca_relacao) |>
  select(-n, -area_nobre) |>
  pivot_wider(names_from = c(rm,SM_local),
              values_from = prop)


# exportacao --------------------------------------------------------------

clipr::write_clip(t1)
clipr::write_clip(t2)
clipr::write_clip(t3)
clipr::write_clip(t4)
clipr::write_clip(t5)
clipr::write_clip(t6)
