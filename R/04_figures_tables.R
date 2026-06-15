# ============================================================
# 04_figures_tables.R
#
#  Galeazzi (2026) — "Why Do Exchange Rate Movements Often
#  Fail to Generate Trade Adjustment?"
#
#  Produces all figures and tables from the estimation results
#  in 02_estimation.R and 03_crisis_analysis.R, plus descriptive
#  figures from the underlying REER decomposition and GPR series.
#
#  Figures (saved to PLOTS_DIR):
#    - fig_reer_decomposition.png   REER + CF/HP/Hamilton trend
#                                     for a sample of currencies
#    - fig_gpr_index.png             GPR index over time, with
#                                     shaded crisis periods
#    - fig_coef_comparison.png       q_persistent coefficient
#                                     across specs (baseline,
#                                     GPR-hi/lo, crisis pre/post)
#
#  Tables (saved to LATEX_DIR / TABLES_DIR):
#    - table_main_results.tex        Eq. 5.2 + Eq. 5.3 (consolidated)
#    - table_crisis_summary.tex      Crisis pre/post + interactions
#    - table_descriptives.tex        Summary statistics
# ============================================================

# ============================================================
# SECTION 0 — SETUP
# ============================================================

stopifnot(exists("panel_clean"))

# Load estimation results (if not already in memory from main.R)
if (!exists("results")) {
  results <- readRDS(file.path(TABLES_DIR, "baseline_gpr_results.rds"))
}
if (!exists("crisis_results")) {
  crisis_results <- readRDS(file.path(TABLES_DIR, "crisis_results.rds"))
}

# bis_decomposed / bis_annual from 01_build_panel_G10.R — needed
# for the REER decomposition figure. If not in memory (e.g. main.R
# loaded panel_final directly without re-running 01), load from
# the intermediate cache if available, else skip that figure.
have_bis_decomposed <- exists("bis_decomposed")
if (!have_bis_decomposed) {
  cache_path <- file.path(CLEAN_DIR, "intermediate", "bis_decomposed.rds")
  if (file.exists(cache_path)) {
    bis_decomposed <- readRDS(cache_path)
    have_bis_decomposed <- TRUE
  } else {
    message("bis_decomposed not available — skipping REER decomposition figure.")
  }
}

have_gpr <- exists("gpr") || exists("gpr_annual")
if (!have_gpr) {
  cache_path <- file.path(CLEAN_DIR, "intermediate", "gpr.rds")
  if (file.exists(cache_path)) {
    gpr <- readRDS(cache_path)
    have_gpr <- TRUE
  } else {
    message("gpr not available — will derive GPR series from panel_clean instead.")
  }
}


# ============================================================
# SECTION 1 — FIGURE: REER DECOMPOSITION (CF / HP / Hamilton)
# ============================================================
#
#  For a small sample of currencies, plot the raw monthly REER
#  alongside the persistent (trend) component from each
#  decomposition method.

if (have_bis_decomposed) {

  sample_currencies <- intersect(c("US", "GB", "JP", "XM"), unique(bis_decomposed$iso2))
  if (length(sample_currencies) == 0) {
    sample_currencies <- unique(bis_decomposed$iso2)[1:min(4, n_distinct(bis_decomposed$iso2))]
  }

  plot_df <- bis_decomposed |>
    filter(iso2 %in% sample_currencies) |>
    select(iso2, date, reer,
           any_of(c("reer_persistent_cf", "reer_persistent_hp", "reer_persistent_hamilton"))) |>
    pivot_longer(
      cols = -c(iso2, date),
      names_to = "series",
      values_to = "value"
    ) |>
    mutate(
      series = recode(series,
        reer                     = "REER (raw)",
        reer_persistent_cf       = "Persistent (CF)",
        reer_persistent_hp       = "Persistent (HP)",
        reer_persistent_hamilton = "Persistent (Hamilton)"
      )
    ) |>
    filter(!is.na(value))

  p_reer <- ggplot(plot_df, aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ iso2, scales = "free_y", ncol = 2) +
    labs(
      title = "REER Decomposition: Persistent Components by Method",
      subtitle = "Christiano-Fitzgerald (primary), Hodrick-Prescott, and Hamilton (2018) filters",
      x = NULL, y = "REER (2020 = 100)", color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  out_path <- file.path(PLOTS_DIR, "fig_reer_decomposition.png")
  ggsave(out_path, p_reer, width = 10, height = 7, dpi = 150)
  message(sprintf("Saved: %s", out_path))
}


# ============================================================
# SECTION 2 — FIGURE: GPR INDEX OVER TIME WITH CRISIS PERIODS
# ============================================================

gpr_series <- if (exists("gpr_annual")) {
  gpr_annual |> select(year, gpr)
} else if (have_gpr) {
  gpr |>
    mutate(year = year(date)) |>
    group_by(year) |>
    summarise(gpr = mean(gpr, na.rm = TRUE), .groups = "drop")
} else {
  panel_clean |> distinct(year, gpr)
}

crisis_shading <- tibble::tibble(
  label = c("GFC", "COVID", "Ukraine"),
  xmin  = c(as.integer(format(GFC_START,   "%Y")),
            as.integer(format(COVID_START, "%Y")),
            as.integer(format(UKR_START,   "%Y"))),
  xmax  = c(as.integer(format(GFC_END,   "%Y")),
            as.integer(format(COVID_END, "%Y")),
            as.integer(format(UKR_END,   "%Y")))
) |>
  # clip to the sample range covered by gpr_series
  filter(xmin <= max(gpr_series$year), xmax >= min(gpr_series$year)) |>
  mutate(
    xmin = pmax(xmin, min(gpr_series$year)),
    xmax = pmin(xmax, max(gpr_series$year))
  )

p_gpr <- ggplot(gpr_series, aes(x = year, y = gpr)) +
  { if (nrow(crisis_shading) > 0)
      geom_rect(
        data = crisis_shading,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = label),
        inherit.aes = FALSE, alpha = 0.15
      )
  } +
  geom_line(linewidth = 0.8, color = "black") +
  labs(
    title = "Caldara-Iacoviello Geopolitical Risk Index",
    subtitle = "Annual average, with shaded crisis periods",
    x = NULL, y = "GPR index", fill = "Crisis period"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

out_path <- file.path(PLOTS_DIR, "fig_gpr_index.png")
ggsave(out_path, p_gpr, width = 9, height = 5, dpi = 150)
message(sprintf("Saved: %s", out_path))


# ============================================================
# SECTION 3 — FIGURE: COEFFICIENT COMPARISON ACROSS SPECS
# ============================================================
#
#  Extract the q_persistent coefficient (and CI) from: baseline,
#  GPR-high, GPR-low, pre-GFC, post-GFC, pre-COVID, post-COVID.

extract_coef <- function(model, term = "q_persistent", label) {
  ct <- broom::tidy(model, conf.int = TRUE) |>
    filter(term == !!term)
  if (nrow(ct) == 0) return(NULL)
  tibble::tibble(
    spec     = label,
    estimate = ct$estimate,
    conf.low = ct$conf.low,
    conf.high = ct$conf.high
  )
}

coef_list <- list(
  extract_coef(results$baseline,        label = "Baseline"),
  extract_coef(results$baseline_gpr_hi, label = "High GPR"),
  extract_coef(results$baseline_gpr_lo, label = "Low GPR"),
  extract_coef(crisis_results$prepost_gfc$pre,    label = "Pre-GFC"),
  extract_coef(crisis_results$prepost_gfc$post,   label = "Post-GFC"),
  extract_coef(crisis_results$prepost_covid$pre,  label = "Pre-COVID"),
  extract_coef(crisis_results$prepost_covid$post, label = "Post-COVID")
)

coef_df <- bind_rows(coef_list) |>
  mutate(spec = factor(spec, levels = rev(spec)))

p_coef <- ggplot(coef_df, aes(x = estimate, y = spec)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Trade Response to Persistent Exchange Rate Movements",
    subtitle = expression(paste("Coefficient on ", q^P, " across specifications (95% CI)")),
    x = expression(beta[1]~"(q"^"P"*")"), y = NULL
  ) +
  theme_minimal(base_size = 11)

out_path <- file.path(PLOTS_DIR, "fig_coef_comparison.png")
ggsave(out_path, p_coef, width = 8, height = 5, dpi = 150)
message(sprintf("Saved: %s", out_path))


# ============================================================
# SECTION 4 — TABLE: MAIN RESULTS (Eq. 5.2 + Eq. 5.3, consolidated)
# ============================================================

out_tex <- file.path(LATEX_DIR, "table_main_results.tex")
etable(
  results$baseline, results$gpr_interaction,
  results$baseline_gpr_hi, results$baseline_gpr_lo,
  title = "Main Results: Baseline, GPR Interaction, and GPR Subsamples",
  headers = c("Baseline (5.2)", "GPR Interaction (5.3)", "High GPR", "Low GPR"),
  digits = 3,
  tex = TRUE,
  file = out_tex,
  replace = TRUE
)
message(sprintf("Saved: %s", out_tex))


# ============================================================
# SECTION 5 — TABLE: CRISIS SUMMARY
# ============================================================

out_tex <- file.path(LATEX_DIR, "table_crisis_summary.tex")
etable(
  crisis_results$prepost_gfc$pre,   crisis_results$prepost_gfc$post,
  crisis_results$prepost_covid$pre, crisis_results$prepost_covid$post,
  crisis_results$interaction_gfc,   crisis_results$interaction_covid,
  crisis_results$interaction_ukr,
  title = "Crisis-Period Robustness: Pre/Post Splits and Window Interactions",
  headers = c("Pre-GFC", "Post-GFC", "Pre-COVID", "Post-COVID",
              "GFC int.", "COVID int.", "Ukraine int."),
  digits = 3,
  tex = TRUE,
  file = out_tex,
  replace = TRUE
)
message(sprintf("Saved: %s", out_tex))


# ============================================================
# SECTION 6 — TABLE: DESCRIPTIVE STATISTICS
# ============================================================

desc_vars <- c("exports", "ln_exports", "q_persistent", "q_transitory",
               "q_persistent_hp", "q_transitory_hp",
               "q_persistent_hamilton", "q_transitory_hamilton",
               "gpr", "ln_gpr", "dist", "ln_dist")

desc_table <- panel_clean |>
  select(all_of(intersect(desc_vars, names(panel_clean)))) |>
  summarise(across(everything(), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd   = ~ sd(.x, na.rm = TRUE),
    min  = ~ min(.x, na.rm = TRUE),
    max  = ~ max(.x, na.rm = TRUE),
    n    = ~ sum(!is.na(.x))
  ))) |>
  pivot_longer(everything(), names_to = "stat", values_to = "value") |>
  separate(stat, into = c("variable", "statistic"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = statistic, values_from = value)

out_csv <- file.path(TABLES_DIR, "table_descriptives.csv")
write_csv(desc_table, out_csv)
message(sprintf("Saved: %s", out_csv))

# LaTeX version via a simple kable-style table written manually
# (avoids adding a kableExtra dependency)
out_tex <- file.path(LATEX_DIR, "table_descriptives.tex")
desc_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Descriptive Statistics}",
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Variable & Mean & SD & Min & Max & N \\\\",
  "\\hline"
)
for (i in seq_len(nrow(desc_table))) {
  r <- desc_table[i, ]
  desc_lines <- c(desc_lines, sprintf(
    "%s & %.3f & %.3f & %.3f & %.3f & %d \\\\",
    r$variable, r$mean, r$sd, r$min, r$max, as.integer(r$n)
  ))
}
desc_lines <- c(desc_lines, "\\hline", "\\end{tabular}", "\\end{table}")
writeLines(desc_lines, out_tex)
message(sprintf("Saved: %s", out_tex))






# ============================================================
# SECTION 7 — FIGURE: MARGINAL EFFECT OF q_persistent OVER GPR
# ============================================================
#
#  From the GPR interaction model (Eq. 5.3):
#    dTrade/dq^P = beta1 + beta2 * ln_gpr_c
#
#  where beta1 = coef on q_persistent,
#        beta2 = coef on q_persistent:ln_gpr_c
#
#  We plot the marginal effect and its 95% CI over the observed
#  range of ln_gpr_c, using the delta method for the SE.
#  This is the key figure for the paper: it shows at what level
#  of geopolitical risk persistent exchange rate movements
#  become statistically significant for trade.

m_gpr_int <- results$gpr_interaction

# Extract coefficients and variance-covariance matrix
b      <- coef(m_gpr_int)
V      <- vcov(m_gpr_int)

# Identify relevant coefficient names (fixest may use : or × notation)
coef_names <- names(b)
b1_name <- coef_names[str_detect(coef_names, "^q_persistent$")]
b2_name <- coef_names[str_detect(coef_names, "q_persistent.*ln_gpr_c|ln_gpr_c.*q_persistent")]

if (length(b1_name) == 1 && length(b2_name) == 1) {
  
  b1 <- b[b1_name]
  b2 <- b[b2_name]
  
  # GPR range: observed min to max of ln_gpr_c in panel_clean
  gpr_c_range <- seq(
    min(panel_clean$ln_gpr - mean(panel_clean$ln_gpr, na.rm = TRUE), na.rm = TRUE),
    max(panel_clean$ln_gpr - mean(panel_clean$ln_gpr, na.rm = TRUE), na.rm = TRUE),
    length.out = 200
  )
  
  # Marginal effect and delta-method SE at each GPR value
  me_df <- tibble::tibble(
    ln_gpr_c = gpr_c_range,
    me       = b1 + b2 * gpr_c_range,
    se_me    = sqrt(V[b1_name, b1_name] +
                      gpr_c_range^2 * V[b2_name, b2_name] +
                      2 * gpr_c_range * V[b1_name, b2_name]),
    ci_lo    = me - 1.96 * se_me,
    ci_hi    = me + 1.96 * se_me,
    # back-transform to actual GPR level for x-axis labels
    gpr_level = exp(ln_gpr_c + mean(panel_clean$ln_gpr, na.rm = TRUE))
  )
  
  # Mark GPR threshold where ME becomes significantly positive
  gpr_threshold <- me_df |> filter(ci_lo > 0) |> slice(1)
  
  p_me <- ggplot(me_df, aes(x = gpr_level)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.20, fill = "steelblue") +
    geom_line(aes(y = me), linewidth = 0.9, color = "steelblue4") +
    { if (nrow(gpr_threshold) > 0)
      geom_vline(xintercept = gpr_threshold$gpr_level,
                 linetype = "dotted", color = "firebrick", linewidth = 0.7)
    } +
    # annotate crisis events on x-axis
    annotate("text", x = 176.3, y = max(me_df$ci_hi) * 0.9,
             label = "Ukraine\npeak", size = 3, color = "grey40", hjust = 1) +
    scale_x_continuous(breaks = c(50, 75, 100, 125, 150, 175)) +
    labs(
      title = expression(
        paste("Marginal Effect of Persistent REER on Exports Over the GPR Range")
      ),
      subtitle = expression(
        paste(frac(partialdiff ~ "Trade", partialdiff ~ q^P), " = ",
              beta[1], " + ", beta[2], " \u00D7 ln GPR"[t])
      ),
      x = "GPR index level",
      y = expression(frac(partialdiff ~ "Exports", partialdiff ~ q^P)),
      caption = "Shaded area: 95% confidence interval (delta method). Dotted line: threshold where ME > 0 at 5% level."
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.caption = element_text(size = 9, color = "grey50"))
  
  out_path <- file.path(PLOTS_DIR, "fig_marginal_effect_gpr.png")
  ggsave(out_path, p_me, width = 9, height = 5.5, dpi = 150)
  message(sprintf("Saved: %s", out_path))
  
  if (nrow(gpr_threshold) > 0) {
    message(sprintf(
      "ME of q_persistent becomes significantly positive at GPR = %.1f (%.0f%% of sample above this)",
      gpr_threshold$gpr_level,
      100 * mean(panel_clean$gpr > gpr_threshold$gpr_level, na.rm = TRUE)
    ))
  }
  
} else {
  message("Could not identify q_persistent and ln_gpr_c coefficients — check model object.")
  message("Available coefficients: ", paste(coef_names, collapse = ", "))
}


# ============================================================
# SECTION 8 — ECONOMIC SIGNIFICANCE ANALYSIS
# ============================================================
#
#  Convert model coefficients into interpretable percentage
#  changes in exports for policy-relevant scenarios.
#  PPML coefficients are semi-elasticities; exp(b*dx) - 1
#  gives the proportional change in exports.
#
#  Scenarios:
#   A) 10% persistent depreciation (q^P falls by 0.10)
#   B) 1 SD persistent movement (SD = 0.216 from descriptives)
#   C) Same scenarios at high GPR (90th pct: GPR = 153.8)
#      using the interaction model
#   D) Same scenarios at low GPR (10th pct: GPR = 67.5)

b_base  <- coef(results$baseline)
b1_base <- b_base["q_persistent"]

# Extract GPR interaction coefficients
b_gpr_int <- coef(results$gpr_interaction)
cn        <- names(b_gpr_int)
b1_int    <- b_gpr_int[cn[str_detect(cn, "^q_persistent$")]]
b2_int    <- b_gpr_int[cn[str_detect(cn, "q_persistent.*ln_gpr_c|ln_gpr_c.*q_persistent")]]

# GPR percentiles (from panel_clean)
gpr_p10 <- quantile(panel_clean$gpr, 0.10, na.rm = TRUE)
gpr_p90 <- quantile(panel_clean$gpr, 0.90, na.rm = TRUE)
gpr_mean <- mean(panel_clean$gpr, na.rm = TRUE)
sd_qP   <- sd(panel_clean$q_persistent, na.rm = TRUE)

ln_gpr_c_p10 <- log(gpr_p10) - mean(panel_clean$ln_gpr, na.rm = TRUE)
ln_gpr_c_p90 <- log(gpr_p90) - mean(panel_clean$ln_gpr, na.rm = TRUE)

# Scenario function: % change in exports
pct_change <- function(b1, b2 = 0, delta_q, ln_gpr_c = 0) {
  me <- b1 + b2 * ln_gpr_c
  (exp(me * delta_q) - 1) * 100
}

econ_sig <- tibble::tibble(
  Scenario = c(
    "10% persistent depreciation, baseline",
    "10% persistent depreciation, high GPR (p90)",
    "10% persistent depreciation, low GPR (p10)",
    "1 SD persistent movement, baseline",
    "1 SD persistent movement, high GPR (p90)",
    "1 SD persistent movement, low GPR (p10)"
  ),
  delta_q = c(-0.10, -0.10, -0.10, -sd_qP, -sd_qP, -sd_qP),
  model   = c("Baseline", "GPR int.", "GPR int.", "Baseline", "GPR int.", "GPR int."),
  gpr_ctx = c(
    sprintf("Mean GPR = %.0f", gpr_mean),
    sprintf("High GPR = %.0f (p90)", gpr_p90),
    sprintf("Low GPR = %.0f (p10)",  gpr_p10),
    sprintf("Mean GPR = %.0f", gpr_mean),
    sprintf("High GPR = %.0f (p90)", gpr_p90),
    sprintf("Low GPR = %.0f (p10)",  gpr_p10)
  ),
  pct_change_exports = c(
    pct_change(b1_base,  delta_q = -0.10),
    pct_change(b1_int, b2_int, delta_q = -0.10, ln_gpr_c = ln_gpr_c_p90),
    pct_change(b1_int, b2_int, delta_q = -0.10, ln_gpr_c = ln_gpr_c_p10),
    pct_change(b1_base,  delta_q = -sd_qP),
    pct_change(b1_int, b2_int, delta_q = -sd_qP, ln_gpr_c = ln_gpr_c_p90),
    pct_change(b1_int, b2_int, delta_q = -sd_qP, ln_gpr_c = ln_gpr_c_p10)
  )
) |>
  mutate(pct_change_exports = round(pct_change_exports, 2))

message("\n── Economic Significance ──────────────────────────────")
print(econ_sig |> select(Scenario, gpr_ctx, pct_change_exports))

out_csv <- file.path(TABLES_DIR, "table_economic_significance.csv")
write_csv(econ_sig, out_csv)
message(sprintf("Saved: %s", out_csv))

# LaTeX version
out_tex <- file.path(LATEX_DIR, "table_economic_significance.tex")
econ_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Economic Significance: Predicted Change in Bilateral Exports}",
  "\\label{tab:econ_sig}",
  "\\begin{tabular}{llr}",
  "\\hline",
  "Scenario & GPR context & \\% change in exports \\\\",
  "\\hline"
)
for (i in seq_len(nrow(econ_sig))) {
  r <- econ_sig[i, ]
  scenario_escaped <- gsub("%", "\\\\%", r$Scenario)   # <-- ADD THIS LINE
  econ_lines <- c(econ_lines, sprintf(
    "%s & %s & %.2f\\%% \\\\",
    scenario_escaped, r$gpr_ctx, r$pct_change_exports   # <-- USE scenario_escaped
  ))
  if (i == 3) econ_lines <- c(econ_lines, "\\hline")
}

econ_lines <- c(econ_lines,
                "\\hline",
                "\\multicolumn{3}{p{12cm}}{\\footnotesize Notes: Percentage changes in bilateral",
                "exports computed as $(\\exp(\\hat{\\beta} \\cdot \\Delta q^P) - 1) \\times 100$.",
                "Baseline uses the unconditional coefficient from Eq.~5.2.",
                "GPR interaction scenarios use Eq.~5.3 evaluated at the 10th and 90th percentiles",
                "of the sample GPR distribution (GPR = %.0f and %.0f respectively).}",
                "\\end{tabular}",
                "\\end{table}"
)
econ_lines[length(econ_lines) - 2] <- sprintf(
  "of the sample GPR distribution (GPR = %.0f and %.0f respectively).}",
  gpr_p10, gpr_p90
)
writeLines(econ_lines, out_tex)
message(sprintf("Saved: %s", out_tex))

# ============================================================
# SECTION 9 — FIGURE: SUMMARY OF MAIN FINDINGS
# ============================================================
#
#  Coefficient plot for q_persistent across all specifications,
#  showing that everything points to zero.
#  Draws from: results (02), crisis_results (03),
#  appendix_results (05).

if (!exists("appendix_results")) {
  ap_path <- file.path(TABLES_DIR, "appendix_results.rds")
  if (file.exists(ap_path)) appendix_results <- readRDS(ap_path)
}

# Helper: extract q_persistent coef + 95% CI from a feglm model
extract_qp <- function(model, label, group) {
  if (is.null(model)) return(NULL)
  ct <- tryCatch(
    broom::tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "^q_persistent$")),
    error = function(e) NULL
  )
  if (is.null(ct) || nrow(ct) == 0) return(NULL)
  tibble::tibble(
    label     = label,
    group     = group,
    estimate  = ct$estimate,
    conf.low  = ct$conf.low,
    conf.high = ct$conf.high
  )
}

# ── Benchmark + decomposition ────────────────────────────────
# Note: results$baseline is contemporaneous CF
# For the lagged version re-extract from appendix_results$decomposition$cf
# which was estimated on the lagged panel if 05 used panel_clean with lags.
# If not available separately, use results$baseline as CF reference.

cf_model  <- if (!is.null(appendix_results$decomposition$cf))
  appendix_results$decomposition$cf else results$baseline
hp_model  <- appendix_results$decomposition$hp
hm_model  <- appendix_results$decomposition$hamilton

decomp_rows <- bind_rows(
  extract_qp(cf_model, "CF (benchmark)", "Decomposition"),
  extract_qp(hp_model, "Hodrick-Prescott", "Decomposition"),
  extract_qp(hm_model, "Hamilton (2018)", "Decomposition")
)

# ── Country exclusions ───────────────────────────────────────
excl_rows <- bind_rows(
  map(names(appendix_results$exclusion), function(nm) {
    cty <- sub("^excl_", "", nm)
    extract_qp(appendix_results$exclusion[[nm]],
               paste0("Excl. ", cty), "Country exclusion")
  })
)

# ── Crisis subsamples ────────────────────────────────────────
crisis_rows <- bind_rows(
  extract_qp(crisis_results$prepost_gfc$pre,    "Pre-GFC",    "Crisis period"),
  extract_qp(crisis_results$prepost_gfc$post,   "Post-GFC",   "Crisis period"),
  extract_qp(crisis_results$prepost_covid$pre,  "Pre-COVID",  "Crisis period"),
  extract_qp(crisis_results$prepost_covid$post, "Post-COVID", "Crisis period")
)

summary_df <- bind_rows(decomp_rows, excl_rows, crisis_rows) |>
  mutate(
    label = factor(label, levels = rev(label)),
    group = factor(group, levels = c("Decomposition", "Country exclusion", "Crisis period"))
  )

p_summary <- ggplot(summary_df, aes(x = estimate, y = label, color = group, shape = group)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  scale_color_manual(values = c(
    "Decomposition"    = "steelblue4",
    "Country exclusion" = "darkorange3",
    "Crisis period"    = "darkgreen"
  )) +
  scale_shape_manual(values = c(
    "Decomposition"    = 16,
    "Country exclusion" = 17,
    "Crisis period"    = 15
  )) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "Summary of Main Findings: Persistent REER Effect Across All Specifications",
    subtitle = expression(paste("Coefficient on ", q^P, " (95% CI) \u2014 all estimates consistent with zero")),
    x        = expression(hat(beta)[1] ~ "(persistent REER component)"),
    y        = NULL,
    color    = NULL, shape = NULL,
    caption  = "PPML gravity model with pair and year fixed effects, clustered SEs at pair level."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",   # colour conveyed by facet label
    strip.text       = element_text(face = "bold", size = 10),
    plot.caption     = element_text(size = 8, color = "grey50"),
    panel.spacing    = unit(0.8, "lines")
  )

out_path <- file.path(PLOTS_DIR, "fig_summary_findings.png")
ggsave(out_path, p_summary, width = 9, height = 8, dpi = 150)
message(sprintf("Saved: %s", out_path))



# ============================================================
# DONE
# ============================================================

message("\n04_figures_tables.R complete.")
message("Figures saved to: ", PLOTS_DIR)
message("Tables saved to:  ", TABLES_DIR)

