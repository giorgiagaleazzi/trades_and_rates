# ============================================================
# 02_estimation.R
#
#  Galeazzi (2026) — "Why Do Exchange Rate Movements Often
#  Fail to Generate Trade Adjustment?"
#
#  Eq. 5.2 (baseline, H1):
#    X_ij,t = exp( b1 q^P_ij,t + b2 q^T_ij,t + mu_ij + lambda_t ) * eps_ij,t
#    estimated by PPML (Poisson pseudo-maximum-likelihood) in
#    LEVELS (exports), with pair and year fixed effects.
#
#    H1: trade responds to persistent (b1 != 0) but not
#        transitory (b2 ~ 0) exchange rate movements.
#
#  Eq. 5.3 (GPR interaction, H2):
#    adds b2' * (q^P_ij,t x ln GPR_t). The ln_gpr main effect is
#    absorbed by year FE; only the interaction is identified.
#
#    H2: a negative b2' indicates elevated geopolitical risk
#        weakens the trade response to persistent movements.
#
#  All specs estimated via fixest::feglm(family = "poisson")
#  per cfg$estimation$estimator = "ppml", with pair + year FE
#  and clustering at the pair level (cfg$estimation$cluster_level).
#
#  Outputs:
#    - TABLES_DIR/baseline_gpr_results.rds  (list of fixest models)
#    - LATEX_DIR/table_baseline_gpr.tex     (etable LaTeX output)
# ============================================================

# ============================================================
# SECTION 0 — SETUP
# ============================================================

stopifnot(exists("panel_clean"))

PPML_FAMILY <- if (identical(ESTIMATOR, "ppml")) "poisson" else ESTIMATOR

fe_formula <- if (INCLUDE_PAIR_FE && INCLUDE_TIME_FE) {
  "pair_id + year_id"
} else if (INCLUDE_PAIR_FE) {
  "pair_id"
} else if (INCLUDE_TIME_FE) {
  "year_id"
} else {
  NULL
}

cluster_formula <- as.formula(paste0("~", CLUSTER_LEVEL, "_id"))
# fall back if cluster_level == "pair" but column is pair_id (no suffix issue)
if (!CLUSTER_LEVEL %in% c("pair", "year")) {
  stop("Unsupported cluster_level: ", CLUSTER_LEVEL)
}
cluster_formula <- as.formula(sprintf("~%s_id", CLUSTER_LEVEL))


# ============================================================
# SECTION 1 — BASELINE GRAVITY MODEL (Eq. 5.2, H1)
# ============================================================

message("\n-- Eq. 5.2: Baseline gravity (PPML, pair + year FE) --")

f_baseline <- as.formula(paste(
  "exports ~ q_persistent + q_transitory |", fe_formula
))

m_baseline <- feglm(
  f_baseline,
  data    = panel_clean,
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_baseline))

panel_clean <- panel_clean |>
  mutate(ln_gpr_c = ln_gpr - mean(ln_gpr, na.rm = TRUE))

# ============================================================
# SECTION 2 — GPR INTERACTION MODEL (Eq. 5.3, H2)
# ============================================================

message("\n-- Eq. 5.3: GPR interaction (PPML, pair + year FE) --")

f_gpr <- as.formula(paste(
  "exports ~ q_persistent + q_persistent:ln_gpr_c + q_transitory |", fe_formula
))

m_gpr <- feglm(
  f_gpr,
  data    = panel_clean,
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_gpr))


# ============================================================
# SECTION 3 — GPR SUBSAMPLE SPLITS (H2, robustness)
# ============================================================
#
#  Re-estimate the baseline (Eq. 5.2) separately for high- and
#  low-GPR years (gpr_hi = top quartile, from panel_final).
#  H2 predicts a smaller/attenuated b1 in the high-GPR subsample.

message("\n-- Eq. 5.2 on high-GPR subsample --")

m_baseline_gpr_hi <- feglm(
  f_baseline,
  data    = panel_clean |> filter(gpr_hi == 1),
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_baseline_gpr_hi))

message("\n-- Eq. 5.2 on low-GPR subsample --")

m_baseline_gpr_lo <- feglm(
  f_baseline,
  data    = panel_clean |> filter(gpr_hi == 0),
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_baseline_gpr_lo))


# ============================================================
# SECTION 4 — ALTERNATIVE GPR THRESHOLD (robustness)
# ============================================================
#
#  Re-derive a median-split GPR dummy (in addition to the
#  top-quartile gpr_hi already in panel_final) and re-estimate
#  the interaction model.

panel_clean <- panel_clean |>
  mutate(gpr_hi_median = as.integer(gpr > median(gpr, na.rm = TRUE)))

message("\n-- Eq. 5.3 with median-split GPR interaction --")

f_gpr_median <- as.formula(paste(
  "exports ~ q_persistent + q_persistent:gpr_hi_median + q_transitory |",
  fe_formula
))

m_gpr_median <- feglm(
  f_gpr_median,
  data    = panel_clean,
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_gpr_median))


# ============================================================
# SECTION 5 — SAVE RESULTS
# ============================================================

results <- list(
  baseline          = m_baseline,
  gpr_interaction   = m_gpr,
  baseline_gpr_hi   = m_baseline_gpr_hi,
  baseline_gpr_lo   = m_baseline_gpr_lo,
  gpr_median        = m_gpr_median
)

out_rds <- file.path(TABLES_DIR, "baseline_gpr_results.rds")
saveRDS(results, out_rds)
message(sprintf("\nSaved estimation results: %s", out_rds))

# LaTeX table: baseline vs GPR interaction (main results)
out_tex <- file.path(LATEX_DIR, "table_baseline_gpr.tex")

etable(
  m_baseline, m_gpr,
  title   = "Exchange Rate Persistence and Bilateral Trade",
  headers = c("Baseline (Eq. 5.2)", "GPR Interaction (Eq. 5.3)"),
  digits  = 3,
  notes   = "ln\\_gpr in column (2) is mean-centred at its sample mean (4.58). The coefficient on q\\_persistent therefore represents the effect at average GPR, not at GPR = 0.",
  tex     = TRUE,
  file    = out_tex,
  replace = TRUE
)
message(sprintf("Saved LaTeX table: %s", out_tex))

# LaTeX table: GPR subsample splits + median threshold (appendix)
out_tex_sub <- file.path(LATEX_DIR, "table_gpr_subsamples.tex")

etable(
  m_baseline_gpr_hi, m_baseline_gpr_lo, m_gpr_median,
  title = "GPR Subsample Splits and Alternative Threshold",
  headers = c("High GPR", "Low GPR", "Median-split interaction"),
  digits = 3,
  tex = TRUE,
  file = out_tex_sub,
  replace = TRUE
)
message(sprintf("Saved LaTeX table: %s", out_tex_sub))

