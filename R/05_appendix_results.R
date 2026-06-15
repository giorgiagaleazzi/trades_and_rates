# ============================================================
# 05_appendix_results.R
#
#  Galeazzi (2026) — "Why Do Exchange Rate Movements Often
#  Fail to Generate Trade Adjustment?"
#
#  Final robustness checks per the proposal's Table 1:
#    - alternative decomposition methods (HP, Hamilton vs. CF)
#    - alternative GPR measures (country-specific GPR, GPRT)
#    - country-exclusion robustness (drop one G10 economy at a
#      time, re-estimate Eq. 5.2)
#
#  Outputs:
#    - TABLES_DIR/appendix_results.rds
#    - LATEX_DIR/table_decomposition_robustness.tex
#    - LATEX_DIR/table_country_exclusion.tex
# ============================================================

# ============================================================
# SECTION 0 — SETUP
# ============================================================

stopifnot(exists("panel_clean"))
stopifnot(exists("fe_formula"), exists("cluster_formula"), exists("PPML_FAMILY"))


# ============================================================
# SECTION 1 — ALTERNATIVE DECOMPOSITION METHODS (H1 ROBUSTNESS)
# ============================================================
#
#  Re-estimate Eq. 5.2 replacing (q_persistent, q_transitory)
#  with the HP and Hamilton equivalents. H1 predicts a
#  significant, positive coefficient on the persistent component
#  and an insignificant coefficient on the transitory component,
#  regardless of decomposition method.

message("\n-- Eq. 5.2: CF (primary, for reference) --")
m_cf <- feglm(
  as.formula(paste("exports ~ q_persistent + q_transitory |", fe_formula)),
  data = panel_clean, family = PPML_FAMILY, cluster = cluster_formula
)
print(summary(m_cf))

have_hp <- all(c("q_persistent_hp", "q_transitory_hp") %in% names(panel_clean))
have_hm <- all(c("q_persistent_hamilton", "q_transitory_hamilton") %in% names(panel_clean))

m_hp <- NULL
if (have_hp) {
  message("\n-- Eq. 5.2: Hodrick-Prescott decomposition --")
  m_hp <- feglm(
    as.formula(paste("exports ~ q_persistent_hp + q_transitory_hp |", fe_formula)),
    data = panel_clean |> filter(!is.na(q_persistent_hp), !is.na(q_transitory_hp)),
    family = PPML_FAMILY, cluster = cluster_formula
  )
  print(summary(m_hp))
} else {
  message("\nHP decomposition columns not found — skipping.")
}

m_hamilton <- NULL
if (have_hm) {
  message("\n-- Eq. 5.2: Hamilton (2018) decomposition --")
  m_hamilton <- feglm(
    as.formula(paste("exports ~ q_persistent_hamilton + q_transitory_hamilton |", fe_formula)),
    data = panel_clean |> filter(!is.na(q_persistent_hamilton), !is.na(q_transitory_hamilton)),
    family = PPML_FAMILY, cluster = cluster_formula
  )
  print(summary(m_hamilton))
} else {
  message("\nHamilton decomposition columns not found — skipping.")
}


# ============================================================
# SECTION 2 — ALTERNATIVE GPR MEASURES (H2 ROBUSTNESS)
# ============================================================
#
#  The main analysis uses the global GPR index (Caldara-
#  Iacoviello). The same source provides a "threats-only"
#  variant (GPRT) and country-specific GPR series (GPRC_*).
#  If a threats-only annual series is available, re-estimate
#  Eq. 5.3 with ln_gprt in place of ln_gpr.
#
#  This section is best-effort: if the alternative GPR series
#  was not merged into panel_final, it is skipped with a message
#  explaining what would need to be added to 01_build_panel_G10.R.

have_gprt <- "ln_gprt" %in% names(panel_clean)

m_gprt <- NULL
if (have_gprt) {
  message("\n-- Eq. 5.3: GPR-Threats (GPRT) interaction --")
  m_gprt <- feglm(
    as.formula(paste(
      "exports ~ q_persistent + q_persistent:ln_gprt + q_transitory |", fe_formula
    )),
    data = panel_clean, family = PPML_FAMILY, cluster = cluster_formula
  )
  print(summary(m_gprt))
} else {
  message("\nln_gprt not found in panel_final — skipping GPRT robustness.")
  message("  To enable: in 01_build_panel_G10.R Section 3, also read the")
  message("  'GPRT' column from gpr_raw, aggregate to annual ln_gprt,")
  message("  and merge alongside gpr/ln_gpr in Section 6.")
}


# ============================================================
# SECTION 3 — COUNTRY EXCLUSION ROBUSTNESS
# ============================================================
#
#  Re-estimate Eq. 5.2 dropping one G10 economy (as either
#  reporter or partner) at a time. Stability of b1 (q_persistent)
#  across exclusions indicates the baseline result is not driven
#  by any single country.

countries_in_panel <- sort(union(unique(panel_clean$reporter), unique(panel_clean$partner)))

message(sprintf("\n-- Eq. 5.2: country exclusion robustness (%d countries) --",
                length(countries_in_panel)))

m_excl <- map(countries_in_panel, function(cty) {
  d <- panel_clean |> filter(reporter != cty, partner != cty)
  feglm(
    as.formula(paste("exports ~ q_persistent + q_transitory |", fe_formula)),
    data = d, family = PPML_FAMILY, cluster = cluster_formula
  )
})
names(m_excl) <- paste0("excl_", countries_in_panel)

excl_summary <- map_dfr(names(m_excl), function(nm) {
  ct <- broom::tidy(m_excl[[nm]], conf.int = TRUE) |>
    filter(term == "q_persistent")
  tibble::tibble(
    excluded  = sub("^excl_", "", nm),
    estimate  = ct$estimate,
    std.error = ct$std.error,
    conf.low  = ct$conf.low,
    conf.high = ct$conf.high
  )
})

message("\nCountry-exclusion estimates for q_persistent:")
print(excl_summary)


# ============================================================
# SECTION 4 — SAVE RESULTS
# ============================================================

appendix_results <- list(
  decomposition = list(cf = m_cf, hp = m_hp, hamilton = m_hamilton),
  gprt          = m_gprt,
  exclusion     = m_excl,
  exclusion_summary = excl_summary
)

out_rds <- file.path(TABLES_DIR, "appendix_results.rds")
saveRDS(appendix_results, out_rds)
message(sprintf("\nSaved appendix results: %s", out_rds))

# LaTeX: decomposition robustness (CF vs HP vs Hamilton)
decomp_models  <- list(m_cf)
decomp_headers <- c("CF (primary)")
if (!is.null(m_hp))       { decomp_models <- c(decomp_models, list(m_hp));       decomp_headers <- c(decomp_headers, "Hodrick-Prescott") }
if (!is.null(m_hamilton)) { decomp_models <- c(decomp_models, list(m_hamilton)); decomp_headers <- c(decomp_headers, "Hamilton (2018)") }

out_tex <- file.path(LATEX_DIR, "table_decomposition_robustness.tex")
etable(
  decomp_models,
  title = "Robustness to Alternative Exchange Rate Decompositions (Eq. 5.2)",
  headers = decomp_headers,
  digits = 3,
  tex = TRUE,
  file = out_tex,
  replace = TRUE
)
message(sprintf("Saved: %s", out_tex))

# LaTeX: country exclusion summary (as a simple table, since
# etable with 10 models would be unwieldy)
out_tex <- file.path(LATEX_DIR, "table_country_exclusion.tex")
excl_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Country Exclusion Robustness: Coefficient on $q^P$ (Eq. 5.2)}",
  "\\begin{tabular}{lrrrr}",
  "\\hline",
  "Excluded country & Estimate & Std. Error & CI Low & CI High \\\\",
  "\\hline"
)
for (i in seq_len(nrow(excl_summary))) {
  r <- excl_summary[i, ]
  excl_lines <- c(excl_lines, sprintf(
    "%s & %.3f & %.3f & %.3f & %.3f \\\\",
    r$excluded, r$estimate, r$std.error, r$conf.low, r$conf.high
  ))
}
excl_lines <- c(excl_lines, "\\hline", "\\end{tabular}", "\\end{table}")
writeLines(excl_lines, out_tex)
message(sprintf("Saved: %s", out_tex))


# ============================================================
# DONE
# ============================================================

message("\n05_appendix_results.R complete.")
