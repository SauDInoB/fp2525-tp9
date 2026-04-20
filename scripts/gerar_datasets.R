library(tidyverse)
library(here)

set.seed(20260421)

n <- 30
n_grupo <- 15

clamp_round <- function(x, lo = -Inf, hi = Inf, digits = 1) {
  x |>
    pmax(lo) |>
    pmin(hi) |>
    round(digits)
}

sample_by_group <- function(grupo, valores = 1:5, probs_intervencao, probs_controlo) {
  out <- integer(length(grupo))
  out[grupo == "intervencao"] <- sample(
    valores, sum(grupo == "intervencao"), replace = TRUE, prob = probs_intervencao
  )
  out[grupo == "controlo"] <- sample(
    valores, sum(grupo == "controlo"), replace = TRUE, prob = probs_controlo
  )
  out
}

# ---- htncare: ensaio fictício sobre controlo da hipertensão ----

grupo_htn <- rep(c("intervencao", "controlo"), each = n_grupo)

idade_htn <- rnorm(n, mean = 58, sd = 9) |> clamp_round(40, 80, 1)

sexo_htn <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45))

imc_htn <- rnorm(n, mean = 28, sd = 4) |> clamp_round(18, 40, 1)

diabetes_htn <- rbinom(n, 1, 0.25)

sbp_baseline_htn <- rnorm(n, mean = 152, sd = 12) |> clamp_round(141, 180, 1)

sbp_delta <- ifelse(grupo_htn == "intervencao", -12, -4)
sbp_semana12_raw <- sbp_baseline_htn + sbp_delta + rnorm(n, 0, 8)
missing_idx_sbp <- sample(n, size = round(n * 0.10))
sbp_semana12_raw[missing_idx_sbp] <- NA
sbp_semana12_htn <- clamp_round(sbp_semana12_raw, 90, 200, 1)

dbp_baseline_htn <- rnorm(n, mean = 92, sd = 8) |> clamp_round(60, 110, 1)

dbp_delta <- ifelse(grupo_htn == "intervencao", -7, -2.5)
dbp_semana12_htn <- (dbp_baseline_htn + dbp_delta + rnorm(n, 0, 5)) |>
  clamp_round(50, 120, 1)
dbp_semana12_htn[missing_idx_sbp] <- NA

adesao_htn <- ifelse(
  grupo_htn == "intervencao",
  rbeta(n, shape1 = 20, shape2 = 4.4),
  rbeta(n, shape1 = 15, shape2 = 5.0)
) |> round(2)

htncare <- tibble(
  patient_id    = sprintf("HTN%03d", seq_len(n)),
  grupo         = grupo_htn,
  idade         = idade_htn,
  sexo          = sexo_htn,
  imc           = imc_htn,
  diabetes      = diabetes_htn,
  sbp_baseline  = sbp_baseline_htn,
  sbp_semana12  = sbp_semana12_htn,
  dbp_baseline  = dbp_baseline_htn,
  dbp_semana12  = dbp_semana12_htn,
  adesao        = adesao_htn
)

write_csv(htncare, here("ficheiros", "htncare.csv"))

# ---- respira: ensaio sobre autogestão da asma ----

grupo_rsp <- rep(c("intervencao", "controlo"), each = n_grupo)

idade_rsp <- runif(n, min = 18, max = 65) |> round(1)

sexo_rsp <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.60, 0.40))

act_baseline_rsp <- rnorm(n, mean = 16, sd = 3) |> clamp_round(5, 25, 1)

act_delta_rsp <- ifelse(grupo_rsp == "intervencao", 4, 1)
act_semana12_raw <- act_baseline_rsp + act_delta_rsp + rnorm(n, 0, 3)
missing_idx_rsp <- sample(n, size = round(n * 0.10))
act_semana12_raw[missing_idx_rsp] <- NA
act_semana12_rsp <- clamp_round(act_semana12_raw, 5, 25, 1)

peak_flow_baseline_rsp <- rnorm(n, mean = 320, sd = 60) |> clamp_round(180, 600, 0)

peak_flow_delta_rsp <- ifelse(grupo_rsp == "intervencao", 40, 8)
peak_flow_semana12_rsp <- (peak_flow_baseline_rsp + peak_flow_delta_rsp + rnorm(n, 0, 25)) |>
  clamp_round(150, 700, 0)
peak_flow_semana12_rsp[missing_idx_rsp] <- NA

exacerbacoes_12sem_rsp <- rpois(
  n,
  lambda = ifelse(grupo_rsp == "intervencao", 0.6, 1.5)
)

aqlq_semana12_rsp <- rnorm(
  n,
  mean = ifelse(grupo_rsp == "intervencao", 5.2, 4.4),
  sd = 0.8
) |> clamp_round(1, 7, 1)
aqlq_semana12_rsp[missing_idx_rsp] <- NA

adesao_rsp <- ifelse(
  grupo_rsp == "intervencao",
  rbeta(n, shape1 = 22, shape2 = 4.8),
  rbeta(n, shape1 = 15, shape2 = 5.0)
) |> round(2)

respira <- tibble(
  patient_id              = sprintf("RSP%03d", seq_len(n)),
  grupo                   = grupo_rsp,
  idade                   = idade_rsp,
  sexo                    = sexo_rsp,
  act_baseline            = act_baseline_rsp,
  act_semana12            = act_semana12_rsp,
  peak_flow_baseline      = peak_flow_baseline_rsp,
  peak_flow_semana12      = peak_flow_semana12_rsp,
  exacerbacoes_12sem      = exacerbacoes_12sem_rsp,
  aqlq_semana12           = aqlq_semana12_rsp,
  adesao                  = adesao_rsp
)

write_csv(respira, here("ficheiros", "respira.csv"))

# ---- mindmove: ensaio sobre actividade física e depressão em jovens adultos ----

grupo_mmv <- rep(c("intervencao", "controlo"), each = n_grupo)

idade_mmv <- runif(n, min = 18, max = 30) |> round(1)

sexo_mmv <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.65, 0.35))

phq9_baseline_mmv <- rnorm(n, mean = 10, sd = 2) |> clamp_round(5, 19, 0)

phq9_delta_mmv <- ifelse(grupo_mmv == "intervencao", -4, -1)
phq9_semana8_raw <- phq9_baseline_mmv + phq9_delta_mmv + rnorm(n, 0, 2.5)
missing_idx_mmv <- sample(n, size = round(n * 0.10))
phq9_semana8_raw[missing_idx_mmv] <- NA
phq9_semana8_mmv <- clamp_round(phq9_semana8_raw, 0, 27, 0)

gad7_baseline_mmv <- rnorm(n, mean = 8, sd = 2) |> clamp_round(3, 18, 0)

gad7_delta_mmv <- ifelse(grupo_mmv == "intervencao", -3, -0.5)
gad7_semana8_mmv <- (gad7_baseline_mmv + gad7_delta_mmv + rnorm(n, 0, 2)) |>
  clamp_round(0, 21, 0)
gad7_semana8_mmv[missing_idx_mmv] <- NA

atividade_fisica_min_semana_mmv <- rnorm(
  n,
  mean = ifelse(grupo_mmv == "intervencao", 180, 90),
  sd = 45
) |> clamp_round(0, 600, 0)

psqi_semana8_mmv <- rnorm(
  n,
  mean = ifelse(grupo_mmv == "intervencao", 6.5, 8.2),
  sd = 1.8
) |> clamp_round(0, 21, 1)
psqi_semana8_mmv[missing_idx_mmv] <- NA

satisfacao_mmv <- sample_by_group(
  grupo_mmv,
  probs_intervencao = c(0.05, 0.10, 0.20, 0.40, 0.25),
  probs_controlo    = c(0.10, 0.20, 0.35, 0.25, 0.10)
)

mindmove <- tibble(
  patient_id                  = sprintf("MMV%03d", seq_len(n)),
  grupo                       = grupo_mmv,
  idade                       = idade_mmv,
  sexo                        = sexo_mmv,
  phq9_baseline               = phq9_baseline_mmv,
  phq9_semana8                = phq9_semana8_mmv,
  gad7_baseline               = gad7_baseline_mmv,
  gad7_semana8                = gad7_semana8_mmv,
  atividade_fisica_min_semana = atividade_fisica_min_semana_mmv,
  psqi_semana8                = psqi_semana8_mmv,
  satisfacao                  = satisfacao_mmv
)

write_csv(mindmove, here("ficheiros", "mindmove.csv"))

# ---- glucocheck: ensaio sobre automonitorização da glicemia na diabetes tipo 2 ----

grupo_glc <- rep(c("intervencao", "controlo"), each = n_grupo)

idade_glc <- runif(n, min = 40, max = 75) |> round(1)

sexo_glc <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.50, 0.50))

hba1c_baseline_glc <- rnorm(n, mean = 8.2, sd = 0.6) |> clamp_round(7.0, 10.5, 1)

hba1c_delta_glc <- ifelse(grupo_glc == "intervencao", -0.7, -0.2)
hba1c_6meses_raw <- hba1c_baseline_glc + hba1c_delta_glc + rnorm(n, 0, 0.4)
missing_idx_glc <- sample(n, size = round(n * 0.10))
hba1c_6meses_raw[missing_idx_glc] <- NA
hba1c_6meses_glc <- clamp_round(hba1c_6meses_raw, 5.5, 12.0, 1)

glicemia_jejum_baseline_glc <- rnorm(n, mean = 155, sd = 25) |> clamp_round(110, 250, 1)

glicemia_delta_glc <- ifelse(grupo_glc == "intervencao", -20, -5)
glicemia_jejum_6meses_glc <- (glicemia_jejum_baseline_glc + glicemia_delta_glc + rnorm(n, 0, 12)) |>
  clamp_round(80, 250, 1)
glicemia_jejum_6meses_glc[missing_idx_glc] <- NA

peso_baseline_glc <- rnorm(n, mean = 88, sd = 14) |> clamp_round(55, 130, 1)

peso_delta_glc <- ifelse(grupo_glc == "intervencao", -2.5, -0.5)
peso_6meses_glc <- (peso_baseline_glc + peso_delta_glc + rnorm(n, 0, 2)) |>
  clamp_round(50, 130, 1)
peso_6meses_glc[missing_idx_glc] <- NA

des_sf_glc <- rnorm(
  n,
  mean = ifelse(grupo_glc == "intervencao", 7.8, 6.5),
  sd = 1.2
) |> clamp_round(1, 10, 1)

satisfacao_glc <- sample_by_group(
  grupo_glc,
  probs_intervencao = c(0.05, 0.10, 0.20, 0.38, 0.27),
  probs_controlo    = c(0.10, 0.22, 0.35, 0.23, 0.10)
)

glucocheck <- tibble(
  patient_id                = sprintf("GLC%03d", seq_len(n)),
  grupo                     = grupo_glc,
  idade                     = idade_glc,
  sexo                      = sexo_glc,
  hba1c_baseline            = hba1c_baseline_glc,
  hba1c_6meses              = hba1c_6meses_glc,
  glicemia_jejum_baseline   = glicemia_jejum_baseline_glc,
  glicemia_jejum_6meses     = glicemia_jejum_6meses_glc,
  peso_baseline             = peso_baseline_glc,
  peso_6meses               = peso_6meses_glc,
  des_sf                    = des_sf_glc,
  satisfacao                = satisfacao_glc
)

write_csv(glucocheck, here("ficheiros", "glucocheck.csv"))

message("Datasets gerados com sucesso em ficheiros/")
message("  htncare.csv    : ", nrow(htncare), " observacoes, ", ncol(htncare), " variaveis")
message("  respira.csv    : ", nrow(respira), " observacoes, ", ncol(respira), " variaveis")
message("  mindmove.csv   : ", nrow(mindmove), " observacoes, ", ncol(mindmove), " variaveis")
message("  glucocheck.csv : ", nrow(glucocheck), " observacoes, ", ncol(glucocheck), " variaveis")
