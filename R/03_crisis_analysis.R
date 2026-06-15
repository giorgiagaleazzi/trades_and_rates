# ============================================================
# 03_crisis_analysis.R
#
#  Galeazzi (2026) — "Why Do Exchange Rate Movements Often
#  Fail to Generate Trade Adjustment?"
#
#  Robustness of the baseline gravity model (Eq. 5.2, H1) across
#  crisis periods, per cfg$crisis_periods:
#    - GFC      : GFC_START - GFC_END
#    - COVID    : COVID_START - COVID_END
#    - Ukraine  : UKR_START - UKR_END (ongoing; no "post" split)
#
#  Two complementary checks:
#   (A) Pre/post subsample splits for GFC and COVID — does b1
#       (response to q_persistent) differ before vs. after?
#   (B) Crisis-window dummy interactions — does b1 change DURING
#       each crisis window relative to the rest of the sample?
#
#  CF decomposition only (q_persistent, q_transitory). HP/Hamilton
#  comparisons are handled separately in the appendix/robustness
#  script.
#
#  Outputs:
#    - TABLES_DIR/crisis_results.rds
#    - LATEX_DIR/table_crisis_prepost.tex
#    - LATEX_DIR/table_crisis_interaction.tex
# ============================================================

# ============================================================
# SECTION 0 — SETUP
# ============================================================

stopifnot(exists("panel_clean"))
stopifnot(exists("f_baseline"), exists("cluster_formula"), exists("PPML_FAMILY"))

# panel_clean$year is integer (calendar year); crisis dates are
# full Date objects from config.R. Compare on calendar years.
gfc_years   <- seq(as.integer(format(GFC_START,   "%Y")),
                   as.integer(format(GFC_END,     "%Y")))
covid_years <- seq(as.integer(format(COVID_START, "%Y")),
                   as.integer(format(COVID_END,   "%Y")))
ukr_years   <- seq(as.integer(format(UKR_START,   "%Y")),
                   as.integer(format(UKR_END,     "%Y")))

message(sprintf("GFC years     : %s", paste(range(gfc_years),   collapse = "-")))
message(sprintf("COVID years   : %s", paste(range(covid_years), collapse = "-")))
message(sprintf("Ukraine years : %s", paste(range(ukr_years),   collapse = "-")))


# ============================================================
# SECTION 1 — PRE/POST SUBSAMPLE SPLITS (GFC, COVID)
# ============================================================
#
#  Re-estimate Eq. 5.2 separately for years strictly before and
#  strictly after each crisis window. Observations that fall
#  WITHIN the crisis window are excluded from both subsamples
#  to keep "pre" and "post" cleanly separated.

run_prepost <- function(crisis_years, label) {
  pre  <- panel_clean |> filter(year < min(crisis_years))
  post <- panel_clean |> filter(year > max(crisis_years))

  message(sprintf("\n-- Eq. 5.2: %s, PRE (years < %d, n=%d) --",
                  label, min(crisis_years), nrow(pre)))
  m_pre <- feglm(f_baseline, data = pre, family = PPML_FAMILY,
                 cluster = cluster_formula)
  print(summary(m_pre))

  message(sprintf("\n-- Eq. 5.2: %s, POST (years > %d, n=%d) --",
                  label, max(crisis_years), nrow(post)))
  m_post <- feglm(f_baseline, data = post, family = PPML_FAMILY,
                  cluster = cluster_formula)
  print(summary(m_post))

  list(pre = m_pre, post = m_post)
}

prepost_gfc   <- run_prepost(gfc_years,   "GFC")
prepost_covid <- run_prepost(covid_years, "COVID")

# Ukraine: no post-period yet (UKR_END is in the future / ongoing).
# Report pre-Ukraine only for reference.
message(sprintf("\n-- Eq. 5.2: Ukraine, PRE (years < %d, n=%d) --",
                min(ukr_years),
                nrow(panel_clean |> filter(year < min(ukr_years)))))
m_pre_ukr <- feglm(
  f_baseline,
  data    = panel_clean |> filter(year < min(ukr_years)),
  family  = PPML_FAMILY,
  cluster = cluster_formula
)
print(summary(m_pre_ukr))


# ============================================================
# SECTION 2 — CRISIS-WINDOW DUMMY INTERACTIONS
# ============================================================
#
#  ln Xij,t = b1 q^P + b1' (q^P x crisis) + b2 q^T + b2' (q^T x crisis)
#             + mu_ij + lambda_t + e
#
#  crisis is a dummy = 1 if year falls within the crisis window.
#  A significant b1' indicates the persistent-REER trade response
#  differs during the crisis relative to the rest of the sample.
#  Year FE absorb the crisis dummy's main effect, so only
#  interactions with q_persistent/q_transitory are identified.

panel_clean <- panel_clean |>
  mutate(
    gfc_dummy   = as.integer(year %in% gfc_years),
    covid_dummy = as.integer(year %in% covid_years),
    ukr_dummy   = as.integer(year %in% ukr_years)
  )

run_crisis_interaction <- function(dummy_var, label) {
  f <- as.formula(sprintf(
    "exports ~ q_persistent + q_persistent:%s + q_transitory + q_transitory:%s | %s",
    dummy_var, dummy_var, fe_formula
  ))

  message(sprintf("\n-- Eq. 5.2 + %s interaction --", label))
  m <- feglm(f, data = panel_clean, family = PPML_FAMILY,
             cluster = cluster_formula)
  print(summary(m))
  m
}

m_gfc_int   <- run_crisis_interaction("gfc_dummy",   "GFC")
m_covid_int <- run_crisis_interaction("covid_dummy", "COVID")
m_ukr_int   <- run_crisis_interaction("ukr_dummy",   "Ukraine")


# ============================================================
# SECTION 3 — SAVE RESULTS
# ============================================================

crisis_results <- list(
  prepost_gfc       = prepost_gfc,
  prepost_covid     = prepost_covid,
  pre_ukraine       = m_pre_ukr,
  interaction_gfc   = m_gfc_int,
  interaction_covid = m_covid_int,
  interaction_ukr   = m_ukr_int
)

out_rds <- file.path(TABLES_DIR, "crisis_results.rds")
saveRDS(crisis_results, out_rds)
message(sprintf("\nSaved crisis analysis results: %s", out_rds))

# LaTeX: pre/post splits (GFC, COVID)
out_tex_prepost <- file.path(LATEX_DIR, "table_crisis_prepost.tex")
etable(
  prepost_gfc$pre, prepost_gfc$post,
  prepost_covid$pre, prepost_covid$post,
  title = "Pre/Post Crisis Subsample Estimates (Eq. 5.2)",
  headers = c("Pre-GFC", "Post-GFC", "Pre-COVID", "Post-COVID"),
  digits = 3,
  tex = TRUE,
  file = out_tex_prepost,
  replace = TRUE
)
message(sprintf("Saved LaTeX table: %s", out_tex_prepost))

# LaTeX: crisis-window interaction models
out_tex_interaction <- file.path(LATEX_DIR, "table_crisis_interaction.tex")
etable(
  m_gfc_int, m_covid_int, m_ukr_int,
  title = "Crisis-Window Interaction Estimates (Eq. 5.2 + crisis dummy)",
  headers = c("GFC", "COVID", "Ukraine"),
  digits = 3,
  tex = TRUE,
  file = out_tex_interaction,
  replace = TRUE
)
message(sprintf("Saved LaTeX table: %s", out_tex_interaction))
