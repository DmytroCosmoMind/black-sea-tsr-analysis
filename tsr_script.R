suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(mgcv)
  library(car)
})

# Quick start (run on user machine):
# 1) Install required packages once: readxl, dplyr, tidyr, purrr, ggplot2, mgcv, car
# 2) Set cfg$paths$working_dir to the project folder containing the Excel dataset.
# 3) Keep cfg$paths$dataset_file as the dataset filename (or update it if needed).
# 4) Run the script from R or terminal: Rscript Script.R
# Outputs are written to Results/, including figures and diagnostics.


# ----------------------------
# 0) Project configuration
# ----------------------------
cfg <- list(
  paths = list(
    working_dir = ".",
    dataset_file = "Table_S1_Life_history_dataset.xlsx",
    results_dir = "Results",
    figure_dir = file.path("Results", "figures"),
    diagnostics_dir = file.path("Results", "figures", "diagnostics")
  ),
  output = list(
    save_main = TRUE,
    save_diagnostics = TRUE,
    save_supplementary = TRUE,
    dpi = 320
  ),
  thresholds = list(
    standard_temp_width = 1,
    min_relative_effect_pct = 5,
    min_supported_p = 0.1,
    min_supported_dev_expl = 0.1
  )
)

ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

save_csv <- function(x, file, row.names = FALSE, category = "main", ...) {
  allowed <- switch(
    category,
    main = cfg$output$save_main,
    diagnostics = cfg$output$save_diagnostics,
    supplementary = cfg$output$save_supplementary,
    TRUE
  )
  target_file <- if (dirname(file) %in% c(".", "")) file.path(cfg$paths$results_dir, file) else file
  if (isTRUE(allowed)) write.csv(x, target_file, row.names = row.names, ...)
}

save_plot <- function(filename, plot, width, height, dpi = cfg$output$dpi, category = "main", ...) {
  allowed <- switch(
    category,
    main = cfg$output$save_main,
    diagnostics = cfg$output$save_diagnostics,
    supplementary = cfg$output$save_supplementary,
    TRUE
  )
  target_file <- if (dirname(filename) %in% c(".", "")) file.path(cfg$paths$results_dir, filename) else filename
  if (isTRUE(allowed)) ggsave(target_file, plot, width = width, height = height, dpi = dpi, ...)
}

working_dir <- cfg$paths$working_dir
dataset_file <- cfg$paths$dataset_file

if (!dir.exists(working_dir)) {
  stop(
    "Working directory not found: ", working_dir, "\n",
    "Set 'working_dir' to the folder containing the dataset."
  )
}

setwd(working_dir)
input_file <- file.path(getwd(), dataset_file)

if (!file.exists(input_file)) {
  stop(
    "Input Excel file not found: ", input_file, "\n",
    "Set 'working_dir' to the folder containing ", dataset_file, "."
  )
}

data_raw <- read_excel(input_file)

clean_names <- function(x) {
  orig <- x
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  bad <- is.na(x) | x == ""
  x[bad] <- orig[bad]
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  bad2 <- is.na(x) | x == ""
  if (any(bad2)) x[bad2] <- paste0("col_", which(bad2))
  make.unique(x, sep = "_")
}

pick_col <- function(candidates, nms) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0) NA_character_ else hit[1]
}

normalize_species_name <- function(x) {
  known_species <- c(
    "Scorpaena porcus",
    "Mullus barbatus",
    "Trachurus mediterraneus",
    "Engraulis encrasicolus",
    "Sprattus sprattus",
    "Merlangius merlangus",
    "Zostericessor ophiocephalus",
    "Serranus scriba",
    "Atherina boyeri",
    "Atherina hepsetus",
    "Spicara flexuosa",
    "Chelon auratus"
  )
  x0 <- trimws(as.character(x))
  x0 <- iconv(x0, from = "", to = "ASCII//TRANSLIT")
  x0 <- gsub("[*]+", "", x0)
  x0 <- gsub("\\s+", " ", x0)
  x0 <- trimws(x0)

  out <- vapply(x0, function(s) {
    if (is.na(s) || s == "") return(s)
    if (s %in% known_species) return(s)
    parts <- strsplit(s, "\\s+")[[1]]
    if (length(parts) >= 2) {
      last <- parts[length(parts)]
      parts[length(parts)] <- sub("([A-Za-z]+)[a-z]$", "\\1", last)
      candidate <- paste(parts, collapse = " ")
      if (candidate %in% known_species) return(candidate)
    }
    s
  }, character(1))
  out
}

orig_names <- names(data_raw)
names(data_raw) <- clean_names(names(data_raw))
nms <- names(data_raw)

col_species <- pick_col(c("Species"), nms)
col_region  <- pick_col(c("Region"), nms)
col_region_group <- pick_col(c("Region_group", "BasinGroup"), nms)
col_sex     <- pick_col(c("Sex"), nms)
col_linf    <- pick_col(c("L", "L_cm", "Linf", "L_inf", "Linf_cm"), nms)
col_tl1     <- pick_col(c("TL1", "TL1_cm"), nms)
col_lmat    <- pick_col(c("Lmat", "Lmat_cm"), nms)
col_amat    <- pick_col(c("Amat", "Amat_years"), nms)
col_lmax    <- pick_col(c("Lmax", "Lmax_cm"), nms)
col_amax    <- pick_col(c("Amax", "Amax_years"), nms)
col_sst     <- pick_col(c("SST", "ASST", "SST_C"), nms)
col_pp      <- pick_col(c("PP", "PP_gCm_3year_1", "PP_gCm_2year_1"), nms)
col_s       <- pick_col(c("S", "S_"), nms)

required <- c(col_species, col_sst, col_pp, col_s)
if (any(is.na(required))) {
  stop(
    "Required columns not found after harmonization.\n",
    "Original names: ", paste(orig_names, collapse = ", "), "\n",
    "Cleaned names: ", paste(names(data_raw), collapse = ", ")
  )
}

data <- data_raw %>%
  transmute(
    Species = normalize_species_name(.data[[col_species]]),
    Region = if (!is.na(col_region)) as.character(.data[[col_region]]) else NA_character_,
    RegionGroupRaw = if (!is.na(col_region_group)) as.character(.data[[col_region_group]]) else NA_character_,
    Sex = if (!is.na(col_sex)) as.character(.data[[col_sex]]) else NA_character_,
    Linf = if (!is.na(col_linf)) as.numeric(.data[[col_linf]]) else NA_real_,
    TL1 = if (!is.na(col_tl1)) as.numeric(.data[[col_tl1]]) else NA_real_,
    Lmat = if (!is.na(col_lmat)) as.numeric(.data[[col_lmat]]) else NA_real_,
    Amat = if (!is.na(col_amat)) as.numeric(.data[[col_amat]]) else NA_real_,
    Lmax = if (!is.na(col_lmax)) as.numeric(.data[[col_lmax]]) else NA_real_,
    Amax = if (!is.na(col_amax)) as.numeric(.data[[col_amax]]) else NA_real_,
    SST = as.numeric(.data[[col_sst]]),
    PP = as.numeric(.data[[col_pp]]),
    S = as.numeric(.data[[col_s]])
  ) %>%
  mutate(
    logPP = log10(PP + 1),
    BasinGroup = case_when(
      !is.na(RegionGroupRaw) & grepl("Azov-Black", RegionGroupRaw, ignore.case = TRUE) ~ "Azov-Black",
      !is.na(RegionGroupRaw) & grepl("^Other$", RegionGroupRaw, ignore.case = TRUE) ~ "Other",
      grepl("Black Sea", Region, ignore.case = TRUE) ~ "Azov-Black",
      grepl("Azov Sea", Region, ignore.case = TRUE) ~ "Azov-Black",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(Species)) %>%
  mutate(
    Species = factor(Species),
    BasinGroup = factor(BasinGroup, levels = c("Azov-Black", "Other"))
  ) %>%
  select(-RegionGroupRaw)

cat("\nData structure:\n")
str(data)
cat("\nSummary:\n")
print(summary(data))

# ----------------------------
# 1) Data structure + diagnostics
# ----------------------------
ensure_dir(cfg$paths$results_dir)
ensure_dir(cfg$paths$figure_dir)
ensure_dir(cfg$paths$diagnostics_dir)

responses_all <- c("Linf", "TL1", "Lmat", "Lmax", "Amax")
available_responses <- responses_all[sapply(responses_all, function(v) sum(is.finite(data[[v]])) >= 15)]

cat("\nAvailable responses (>=15 finite observations):\n")
print(available_responses)

if (length(available_responses) == 0) {
  stop("No response variable has enough finite observations.")
}

env_vars <- c("SST", "PP", "S")

overall_ranges <- data %>%
  summarise(across(all_of(env_vars),
    list(
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    )
  ))
save_csv(overall_ranges, "Data_environment_summary.csv", row.names = FALSE, category = "main")

species_coverage <- data %>%
  group_by(Species) %>%
  summarise(
    N = n(),
    SST_min = min(SST, na.rm = TRUE),
    SST_max = max(SST, na.rm = TRUE),
    PP_min = min(PP, na.rm = TRUE),
    PP_max = max(PP, na.rm = TRUE),
    S_min = min(S, na.rm = TRUE),
    S_max = max(S, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(N))
save_csv(species_coverage, "Data_species_coverage.csv", row.names = FALSE, category = "main")

missing_tbl <- bind_rows(lapply(c(env_vars, responses_all), function(v) {
  tibble(variable = v, n_missing = sum(!is.finite(data[[v]])), n_total = nrow(data))
})) %>%
  mutate(missing_pct = 100 * n_missing / pmax(1, n_total))
save_csv(missing_tbl, "Data_missingness.csv", row.names = FALSE, category = "diagnostics")

cor_mat <- suppressWarnings(cor(data[, env_vars], method = "spearman", use = "complete.obs"))
save_csv(cor_mat, "Data_env_spearman_correlation.csv", row.names = TRUE, category = "diagnostics")

vif_table <- tryCatch({
  lm_vif <- lm(SST ~ logPP + S, data = data)
  v <- car::vif(lm_vif)
  tibble(term = names(v), VIF = as.numeric(v))
}, error = function(e) {
  tibble(term = c("logPP", "S"), VIF = NA_real_)
})
save_csv(vif_table, "Data_env_vif.csv", row.names = FALSE, category = "diagnostics")

p_cov <- ggplot(data, aes(x = SST, y = PP, color = Species)) +
  geom_point(alpha = 0.75, size = 1.9) +
  theme_bw() +
  labs(title = "Environmental space by species", x = "SST", y = "PP")
save_plot(file.path(cfg$paths$figure_dir, "Env_space_SST_PP_by_species.png"), p_cov, width = 9, height = 6, category = "supplementary")

p_sst <- ggplot(data, aes(x = Species, y = SST, fill = Species)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "SST distribution by species", x = "Species", y = "SST")
save_plot(file.path(cfg$paths$figure_dir, "SST_by_species_boxplot.png"), p_sst, width = 9, height = 6, category = "supplementary")

# Basin context: Azov-Black vs Other regions
basin_coverage <- data %>%
  group_by(BasinGroup) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(Species),
    SST_mean = mean(SST, na.rm = TRUE),
    PP_mean = mean(PP, na.rm = TRUE),
    S_mean = mean(S, na.rm = TRUE),
    .groups = "drop"
  )
save_csv(basin_coverage, "Basin_coverage_summary.csv", row.names = FALSE, category = "supplementary")

p_basin_env <- ggplot(data, aes(SST, PP, color = S, shape = BasinGroup)) +
  geom_point(alpha = 0.8, size = 2.0) +
  scale_color_gradient(low = "#56B1F7", high = "#132B43") +
  theme_bw() +
  labs(title = "Azov-Black basin in overall environmental space", x = "SST", y = "PP", color = "Salinity", shape = "Basin group")
save_plot(file.path(cfg$paths$figure_dir, "Basin_vs_global_env_space.png"), p_basin_env, width = 9.4, height = 6.4, category = "supplementary")

basin_long <- data %>%
  select(BasinGroup, all_of(available_responses)) %>%
  pivot_longer(cols = -BasinGroup, names_to = "response", values_to = "value") %>%
  filter(is.finite(value)) %>%
  group_by(response) %>%
  mutate(value_z = {
    s <- sd(value, na.rm = TRUE)
    m <- mean(value, na.rm = TRUE)
    ifelse(is.finite(s) & s > 0, (value - m) / s, 0)
  }) %>%
  ungroup()

p_basin_resp <- ggplot(basin_long, aes(BasinGroup, value_z, fill = BasinGroup)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75) +
  geom_jitter(width = 0.2, alpha = 0.25, size = 0.8) +
  facet_wrap(~response, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Azov-Black basin vs global background across life-history traits",
    x = "Basin group",
    y = "Standardized trait value (z)"
  )
save_plot(file.path(cfg$paths$figure_dir, "Basin_vs_global_traits_boxplots.png"), p_basin_resp, width = 11.5, height = 7.5, category = "supplementary")

# ----------------------------
# 2) HGAM pooled models + interaction test
# ----------------------------
safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]", "_", x)

standard_temp_width <- cfg$thresholds$standard_temp_width
min_relative_effect_pct <- cfg$thresholds$min_relative_effect_pct
min_supported_p <- cfg$thresholds$min_supported_p
min_supported_dev_expl <- cfg$thresholds$min_supported_dev_expl

get_interaction_p <- function(cmp) {
  out <- NA_real_
  if (is.null(cmp)) return(out)
  cn <- colnames(cmp)
  pcol <- cn[grepl("Pr", cn)]
  if (length(pcol) == 0) return(out)
  if (nrow(cmp) >= 2) {
    out <- suppressWarnings(as.numeric(cmp[[pcol[1]]][2]))
  }
  out
}

build_prediction_row <- function(d) {
  nd <- tibble(
    SST = median(d$SST, na.rm = TRUE),
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE)
  )
  if ("Species" %in% names(d)) nd$Species <- levels(d$Species)[1]
  if ("SpeciesFS" %in% names(d)) nd$SpeciesFS <- levels(d$SpeciesFS)[1]
  nd
}

predict_without_species_terms <- function(model, nd) {
  sm_labels <- vapply(model$smooth, function(s) s$label, character(1))
  ex_terms <- sm_labels[grepl("Species", sm_labels)]
  as.numeric(predict(model, newdata = nd, type = "response", exclude = ex_terms))
}

extract_temp_delta <- function(model, d, width = standard_temp_width) {
  sst_med <- median(d$SST, na.rm = TRUE)
  sst_rng <- range(d$SST, na.rm = TRUE)
  if (!is.finite(sst_med) || !all(is.finite(sst_rng)) || diff(sst_rng) < width) return(NA_real_)

  sst_vals <- c(sst_med - width / 2, sst_med + width / 2)
  if (sst_vals[1] < sst_rng[1] || sst_vals[2] > sst_rng[2]) return(NA_real_)

  nd <- bind_rows(build_prediction_row(d), build_prediction_row(d))
  nd$SST <- sst_vals
  fit <- predict_without_species_terms(model, nd)
  as.numeric(fit[2] - fit[1])
}

extract_temp_percent_delta <- function(model, d, width = standard_temp_width) {
  sst_med <- median(d$SST, na.rm = TRUE)
  sst_rng <- range(d$SST, na.rm = TRUE)
  if (!is.finite(sst_med) || !all(is.finite(sst_rng)) || diff(sst_rng) < width) return(NA_real_)

  base_row <- build_prediction_row(d)
  base_row$SST <- sst_med
  base_fit <- predict_without_species_terms(model, base_row)[1]
  if (!is.finite(base_fit) || abs(base_fit) < 1e-8) return(NA_real_)

  100 * extract_temp_delta(model, d, width = width) / abs(base_fit)
}

extract_temp_range_delta <- function(model, d) {
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = 120)
  nd <- build_prediction_row(d)[rep(1, length(sst_seq)), , drop = FALSE]
  nd$SST <- sst_seq
  fit <- predict_without_species_terms(model, nd)
  as.numeric(tail(fit, 1) - fit[1])
}

extract_env_delta <- function(model, d, var_name) {
  rng <- range(d[[var_name]], na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) <= 0) return(NA_real_)

  nd <- bind_rows(build_prediction_row(d), build_prediction_row(d))
  nd[[var_name]] <- rng
  fit <- predict_without_species_terms(model, nd)
  as.numeric(fit[2] - fit[1])
}

extract_smooth_p <- function(sm_obj, pattern) {
  out <- NA_real_
  st <- sm_obj$s.table
  if (is.null(st)) return(out)
  idx <- grep(pattern, rownames(st))
  if (length(idx) == 0) return(out)
  pcol <- grep("p-value", colnames(st), value = TRUE)
  if (length(pcol) == 0) return(out)
  suppressWarnings(as.numeric(st[idx[1], pcol[1]]))
}

save_model_diagnostics <- function(model, d, response, prefix = NULL) {
  fitted <- as.numeric(predict(model, type = "response"))
  resid <- as.numeric(residuals(model, type = "deviance"))
  df_diag <- tibble(
    fitted = fitted,
    resid = resid,
    observed = d[[response]]
  )

  file_tag <- if (!is.null(prefix) && nzchar(prefix)) paste0(prefix, "_") else ""
  title_tag <- if (!is.null(prefix) && nzchar(prefix)) paste0("[", prefix, "] ") else ""

  p_res <- ggplot(df_diag, aes(fitted, resid)) +
    geom_point(alpha = 0.65, size = 1.6) +
    geom_hline(yintercept = 0, linetype = 2, color = "red") +
    theme_bw() +
    labs(title = paste0(title_tag, response, ": residuals vs fitted"), x = "Fitted", y = "Deviance residuals")
  save_plot(file.path(cfg$paths$diagnostics_dir, paste0("Diag_resid_fitted_", file_tag, safe_name(response), ".png")),
    p_res,
    width = 7.5, height = 5.2, category = "diagnostics"
  )

  p_qq <- ggplot(df_diag, aes(sample = resid)) +
    stat_qq(alpha = 0.55) +
    stat_qq_line(color = "blue") +
    theme_bw() +
    labs(title = paste0(title_tag, response, ": QQ-plot residuals"), x = "Theoretical", y = "Sample")
  save_plot(file.path(cfg$paths$diagnostics_dir, paste0("Diag_qq_", file_tag, safe_name(response), ".png")),
    p_qq,
    width = 7.0, height = 5.2, category = "diagnostics"
  )

  p_obs <- ggplot(df_diag, aes(observed, fitted)) +
    geom_point(alpha = 0.65, size = 1.6) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red") +
    theme_bw() +
    labs(title = paste0(title_tag, response, ": observed vs fitted"), x = "Observed", y = "Fitted")
  save_plot(file.path(cfg$paths$diagnostics_dir, paste0("Diag_obs_fitted_", file_tag, safe_name(response), ".png")),
    p_obs,
    width = 7.5, height = 5.2, category = "diagnostics"
  )
}

fit_hgam <- function(df, response, k_base = 4) {
  d <- df %>%
    select(all_of(c("Species", "SST", "logPP", "S", response))) %>%
    filter(
      !is.na(Species),
      is.finite(.data[[response]]),
      is.finite(SST),
      is.finite(logPP),
      is.finite(S)
    )

  n_obs <- nrow(d)
  n_sp <- n_distinct(d$Species)
  if (n_obs < 45 || n_sp < 4) {
    return(list(
      response = response,
      fit = NULL,
      reduced = NULL,
      compare = NULL,
      concurvity = NULL,
      n = n_obs,
      n_sp = n_sp,
      note = sprintf("Skipped: insufficient data (n=%d, species=%d)", n_obs, n_sp)
    ))
  }

  sp_counts <- table(d$Species)
  rare_levels <- names(sp_counts)[sp_counts < 12]
  d <- d %>%
    mutate(
      SpeciesFS = as.character(Species),
      SpeciesFS = ifelse(SpeciesFS %in% rare_levels, "Rare", SpeciesFS),
      SpeciesFS = factor(SpeciesFS)
    )

  n_fs_levels <- n_distinct(d$SpeciesFS)
  use_fs <- n_fs_levels >= 3 && n_obs >= 90
  # Conservative basis size reduces overfitting and concurvity pressure.
  k_smooth <- max(3, min(k_base, floor(n_obs / 20)))
  k_fs <- 3

  fs_term <- if (use_fs) paste0(" + s(SST, SpeciesFS, bs='fs', m=1, k=", k_fs, ")") else ""

  f_full <- as.formula(
    paste0(
      response, " ~ s(SST, bs='ts', k=", k_smooth, ") + s(logPP, bs='ts', k=", k_smooth, ") + s(S, bs='ts', k=", k_smooth, ") + s(Species, bs='re')", fs_term
    )
  )

  f_red <- as.formula(
    paste0(
      response, " ~ s(logPP, bs='ts', k=", k_smooth, ") + s(S, bs='ts', k=", k_smooth, ") + s(Species, bs='re')"
    )
  )

  m_full <- tryCatch(bam(f_full, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  m_red <- tryCatch(bam(f_red, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  if (is.null(m_full) || is.null(m_red)) {
    return(list(
      response = response,
      fit = NULL,
      reduced = NULL,
      compare = NULL,
      concurvity = NULL,
      n = n_obs,
      n_sp = n_sp,
      data_used = d,
      note = sprintf("HGAM fit failed (n=%d, species=%d)", n_obs, n_sp)
    ))
  }

  cmp <- tryCatch(anova(m_red, m_full, test = "F"), error = function(e) NULL)
  cc <- tryCatch(mgcv::concurvity(m_full, full = TRUE), error = function(e) NULL)

  list(
    response = response,
    fit = m_full,
    reduced = m_red,
    compare = cmp,
    concurvity = cc,
    n = n_obs,
    n_sp = n_sp,
    data_used = d,
    note = sprintf(
      "Fitted additive HGAM (n=%d, species=%d, k=%d, fs=%s, rare=%d)",
      n_obs, n_sp, k_smooth, ifelse(use_fs, "on", "off"), length(rare_levels)
    )
  )
}

pooled_models <- map(available_responses, ~ fit_hgam(data, .x))
names(pooled_models) <- available_responses

cat("\nHGAM fitting status:\n")
for (r in available_responses) cat(r, "->", pooled_models[[r]]$note, "\n")

pooled_summary <- bind_rows(lapply(pooled_models, function(x) {
  if (is.null(x$fit)) {
    return(tibble(
      response = x$response,
      n = x$n,
      species_n = x$n_sp,
      dev_expl = NA_real_,
      r_sq_adj = NA_real_,
      AIC_full = NA_real_,
      AIC_reduced = NA_real_,
      delta_AIC = NA_real_,
      interaction_p = NA_real_,
      p_sst = NA_real_,
      p_logpp = NA_real_,
      p_sal = NA_real_,
      temp_delta = NA_real_,
      temp_delta_pct = NA_real_,
      temp_delta_range = NA_real_,
      pp_delta = NA_real_,
      s_delta = NA_real_,
      concurvity_worst_max = NA_real_,
      status = x$note
    ))
  }

  sm <- summary(x$fit)
  aic_full <- AIC(x$fit)
  aic_red <- AIC(x$reduced)
  p_int <- get_interaction_p(x$compare)
  p_sst <- extract_smooth_p(sm, "^s\\(SST\\)$")
  p_logpp <- extract_smooth_p(sm, "^s\\(logPP\\)$")
  p_sal <- extract_smooth_p(sm, "^s\\(S\\)$")
  temp_delta <- extract_temp_delta(x$fit, x$data_used)
  temp_delta_pct <- extract_temp_percent_delta(x$fit, x$data_used)
  temp_delta_range <- extract_temp_range_delta(x$fit, x$data_used)
  pp_delta <- extract_env_delta(x$fit, x$data_used, "logPP")
  s_delta <- extract_env_delta(x$fit, x$data_used, "S")

  worst_cc <- NA_real_
  if (!is.null(x$concurvity)) {
    rn <- rownames(x$concurvity)
    if (!is.null(rn) && "worst" %in% rn) {
      v <- as.numeric(x$concurvity["worst", ])
      worst_cc <- max(v[is.finite(v)], na.rm = TRUE)
      if (!is.finite(worst_cc)) worst_cc <- NA_real_
    }
  }

  tibble(
    response = x$response,
    n = nobs(x$fit),
    species_n = x$n_sp,
    dev_expl = sm$dev.expl,
    r_sq_adj = sm$r.sq,
    AIC_full = aic_full,
    AIC_reduced = aic_red,
    delta_AIC = aic_full - aic_red,
    interaction_p = p_int,
    p_sst = p_sst,
    p_logpp = p_logpp,
    p_sal = p_sal,
    temp_delta = temp_delta,
    temp_delta_pct = temp_delta_pct,
    temp_delta_range = temp_delta_range,
    pp_delta = pp_delta,
    s_delta = s_delta,
    concurvity_worst_max = worst_cc,
    status = x$note
  )
}))


# Separate SST x PP interaction screening (secondary analysis)
fit_interaction_screen <- function(df, response, k_base = 4) {
  d <- df %>%
    select(all_of(c("Species", "SST", "logPP", "S", response))) %>%
    filter(!is.na(Species), is.finite(.data[[response]]), is.finite(SST), is.finite(logPP), is.finite(S))

  n_obs <- nrow(d)
  n_sp <- n_distinct(d$Species)
  if (n_obs < 120 || n_sp < 6 || n_distinct(d$SST) < 12 || n_distinct(d$logPP) < 12) {
    return(tibble(
      response = response,
      n = n_obs,
      species_n = n_sp,
      interaction_p = NA_real_,
      delta_AIC_interaction = NA_real_,
      status = "Skipped: insufficient support for stable ti(SST,logPP)"
    ))
  }

  k_s <- max(3, min(k_base, floor(n_obs / 20)))
  k_ti <- max(3, min(4, k_s))

  f_add <- as.formula(
    paste0(
      response,
      " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')"
    )
  )
  f_int <- as.formula(
    paste0(
      response,
      " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + ti(SST, logPP, k=c(", k_ti, ",", k_ti, ")) + s(Species, bs='re')"
    )
  )

  m_add <- tryCatch(bam(f_add, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  m_int <- tryCatch(bam(f_int, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  if (is.null(m_add) || is.null(m_int)) {
    return(tibble(
      response = response,
      n = n_obs,
      species_n = n_sp,
      interaction_p = NA_real_,
      delta_AIC_interaction = NA_real_,
      status = "Fit failed"
    ))
  }

  cmp <- tryCatch(anova(m_add, m_int, test = "F"), error = function(e) NULL)
  p_int <- get_interaction_p(cmp)

  tibble(
    response = response,
    n = n_obs,
    species_n = n_sp,
    interaction_p = p_int,
    delta_AIC_interaction = AIC(m_int) - AIC(m_add),
    status = "OK"
  )
}

interaction_screen <- bind_rows(lapply(available_responses, function(r) fit_interaction_screen(data, r)))
save_csv(interaction_screen, "HGAM_interaction_screen.csv", row.names = FALSE, category = "supplementary")

# Unique SST share on top of PP+S (additive HGAM comparison)
fit_sst_share <- function(df, response, k_base = 4) {
  d <- df %>%
    select(all_of(c("Species", "SST", "logPP", "S", response))) %>%
    filter(!is.na(Species), is.finite(.data[[response]]), is.finite(SST), is.finite(logPP), is.finite(S))

  n_obs <- nrow(d)
  n_sp <- n_distinct(d$Species)
  if (n_obs < 45 || n_sp < 4) {
    return(tibble(
      response = response,
      n = n_obs,
      species_n = n_sp,
      dev_expl_full = NA_real_,
      dev_expl_noSST = NA_real_,
      delta_dev_expl_sst = NA_real_,
      delta_dev_expl_sst_pct = NA_real_,
      r_sq_full = NA_real_,
      r_sq_noSST = NA_real_,
      delta_rsq_sst = NA_real_,
      aic_full = NA_real_,
      aic_noSST = NA_real_,
      delta_aic_sst = NA_real_,
      p_sst_drop = NA_real_,
      status = "Skipped: insufficient data"
    ))
  }

  sp_counts <- table(d$Species)
  rare_levels <- names(sp_counts)[sp_counts < 12]
  d <- d %>%
    mutate(
      SpeciesFS = as.character(Species),
      SpeciesFS = ifelse(SpeciesFS %in% rare_levels, "Rare", SpeciesFS),
      SpeciesFS = factor(SpeciesFS)
    )

  n_fs_levels <- n_distinct(d$SpeciesFS)
  use_fs <- n_fs_levels >= 3 && n_obs >= 90
  k_s <- max(3, min(k_base, floor(n_obs / 20)))
  k_fs <- 3
  fs_term <- if (use_fs) paste0(" + s(SST, SpeciesFS, bs='fs', m=1, k=", k_fs, ")") else ""

  f_full <- as.formula(
    paste0(
      response,
      " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')",
      fs_term
    )
  )
  f_no_sst <- as.formula(
    paste0(
      response,
      " ~ s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')"
    )
  )

  m_full <- tryCatch(bam(f_full, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  m_no <- tryCatch(bam(f_no_sst, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  if (is.null(m_full) || is.null(m_no)) {
    return(tibble(
      response = response,
      n = n_obs,
      species_n = n_sp,
      dev_expl_full = NA_real_,
      dev_expl_noSST = NA_real_,
      delta_dev_expl_sst = NA_real_,
      delta_dev_expl_sst_pct = NA_real_,
      r_sq_full = NA_real_,
      r_sq_noSST = NA_real_,
      delta_rsq_sst = NA_real_,
      aic_full = NA_real_,
      aic_noSST = NA_real_,
      delta_aic_sst = NA_real_,
      p_sst_drop = NA_real_,
      status = "Fit failed"
    ))
  }

  s_full <- summary(m_full)
  s_no <- summary(m_no)
  cmp <- tryCatch(anova(m_no, m_full, test = "F"), error = function(e) NULL)
  p_drop <- get_interaction_p(cmp)

  dev_full <- as.numeric(s_full$dev.expl)
  dev_no <- as.numeric(s_no$dev.expl)
  d_dev <- dev_full - dev_no

  tibble(
    response = response,
    n = n_obs,
    species_n = n_sp,
    dev_expl_full = dev_full,
    dev_expl_noSST = dev_no,
    delta_dev_expl_sst = d_dev,
    delta_dev_expl_sst_pct = ifelse(is.finite(dev_full) && dev_full > 0, 100 * d_dev / dev_full, NA_real_),
    r_sq_full = as.numeric(s_full$r.sq),
    r_sq_noSST = as.numeric(s_no$r.sq),
    delta_rsq_sst = as.numeric(s_full$r.sq) - as.numeric(s_no$r.sq),
    aic_full = AIC(m_full),
    aic_noSST = AIC(m_no),
    delta_aic_sst = AIC(m_full) - AIC(m_no),
    p_sst_drop = p_drop,
    status = "OK"
  )
}

sst_share <- bind_rows(lapply(available_responses, function(r) fit_sst_share(data, r)))
save_csv(sst_share, "HGAM_SST_unique_share.csv", row.names = FALSE, category = "main")

pooled_summary <- pooled_summary %>%
  select(-interaction_p) %>%
  left_join(interaction_screen %>% select(response, interaction_p, delta_AIC_interaction, interaction_status = status), by = "response") %>%
  left_join(
    sst_share %>%
      select(
        response,
        delta_dev_expl_sst,
        delta_dev_expl_sst_pct,
        delta_rsq_sst,
        delta_aic_sst,
        p_sst_drop,
        sst_share_status = status
      ),
    by = "response"
  )
save_csv(pooled_summary, "HGAM_pooled_summary.csv", row.names = FALSE, category = "main")

# Basin-specific SST trend disruption test:
# does Azov-Black alter SST shape compared with the global background?
# Basin-specific pooled HGAM main-effect test:
# does Azov-Black differ from the broader background after nonlinear adjustment for SST, PP, S and species?
fit_basin_shift_hgam <- function(df, response, k_base = 5) {
  d <- df %>%
    select(BasinGroup, Species, SST, logPP, S, all_of(response)) %>%
    filter(is.finite(.data[[response]]), is.finite(SST), is.finite(logPP), is.finite(S), !is.na(BasinGroup))

  grp_n <- table(d$BasinGroup)
  if (nrow(d) < 60 || length(grp_n) < 2 || any(grp_n < 20)) {
    return(tibble(
      response = response,
      n = nrow(d),
      n_azov_black = ifelse("Azov-Black" %in% names(grp_n), as.integer(grp_n[["Azov-Black"]]), 0L),
      n_other = ifelse("Other" %in% names(grp_n), as.integer(grp_n[["Other"]]), 0L),
      basin_coef = NA_real_,
      basin_p = NA_real_,
      delta_aic_basin = NA_real_,
      delta_dev_expl_basin = NA_real_,
      status = "Skipped: insufficient basin coverage"
    ))
  }

  k_s <- max(4, min(k_base, floor(nrow(d) / 16)))

  f_red <- as.formula(
    paste0(response, " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')")
  )
  f_full <- as.formula(
    paste0(response, " ~ BasinGroup + s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')")
  )

  m_red <- tryCatch(bam(f_red, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  m_full <- tryCatch(bam(f_full, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)

  if (is.null(m_red) || is.null(m_full)) {
    return(tibble(
      response = response,
      n = nrow(d),
      n_azov_black = ifelse("Azov-Black" %in% names(grp_n), as.integer(grp_n[["Azov-Black"]]), 0L),
      n_other = ifelse("Other" %in% names(grp_n), as.integer(grp_n[["Other"]]), 0L),
      basin_coef = NA_real_,
      basin_p = NA_real_,
      delta_aic_basin = NA_real_,
      delta_dev_expl_basin = NA_real_,
      status = "Fit failed"
    ))
  }

  cs <- tryCatch(summary(m_full)$p.table, error = function(e) NULL)
  basin_coef <- NA_real_
  basin_p <- NA_real_
  if (!is.null(cs)) {
    idx <- grep("^BasinGroup", rownames(cs))
    if (length(idx) > 0) {
      basin_coef <- as.numeric(cs[idx[1], "Estimate"])
      pcol <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(cs))
      if (length(pcol) > 0) basin_p <- as.numeric(cs[idx[1], pcol[1]])
    }
  }

  s_red <- summary(m_red)
  s_full <- summary(m_full)

  tibble(
    response = response,
    n = nrow(d),
    n_azov_black = ifelse("Azov-Black" %in% names(grp_n), as.integer(grp_n[["Azov-Black"]]), 0L),
    n_other = ifelse("Other" %in% names(grp_n), as.integer(grp_n[["Other"]]), 0L),
    basin_coef = basin_coef,
    basin_p = basin_p,
    delta_aic_basin = AIC(m_full) - AIC(m_red),
    delta_dev_expl_basin = as.numeric(s_full$dev.expl) - as.numeric(s_red$dev.expl),
    status = "OK"
  )
}

basin_hgam_shift <- bind_rows(lapply(available_responses, function(r) fit_basin_shift_hgam(data, r)))
save_csv(basin_hgam_shift, "Basin_HGAM_shift_summary.csv", row.names = FALSE, category = "main")

# Basin SST-overlap sensitivity analysis:
# repeat basin HGAMs only within the overlapping SST range of Azov-Black and Other.
basin_overlap_bounds <- data %>%
  filter(!is.na(BasinGroup), is.finite(SST)) %>%
  group_by(BasinGroup) %>%
  summarise(sst_min = min(SST, na.rm = TRUE), sst_max = max(SST, na.rm = TRUE), .groups = "drop")

basin_overlap_summary <- if (nrow(basin_overlap_bounds) == 2) {
  lower <- max(basin_overlap_bounds$sst_min, na.rm = TRUE)
  upper <- min(basin_overlap_bounds$sst_max, na.rm = TRUE)
  tibble(
    overlap_sst_min = lower,
    overlap_sst_max = upper,
    overlap_width = upper - lower,
    status = ifelse(is.finite(lower) & is.finite(upper) & upper > lower, "OK", "No valid SST overlap")
  )
} else {
  tibble(
    overlap_sst_min = NA_real_,
    overlap_sst_max = NA_real_,
    overlap_width = NA_real_,
    status = "Insufficient basin groups"
  )
}
save_csv(basin_overlap_summary, "Basin_SST_overlap_summary.csv", row.names = FALSE, category = "supplementary")

if (nrow(basin_overlap_summary) > 0 && identical(basin_overlap_summary$status[1], "OK")) {
  data_basin_overlap <- data %>%
    filter(is.finite(SST), SST >= basin_overlap_summary$overlap_sst_min[1], SST <= basin_overlap_summary$overlap_sst_max[1])

  basin_hgam_shift_overlap <- bind_rows(lapply(available_responses, function(r) fit_basin_shift_hgam(data_basin_overlap, r))) %>%
    mutate(sensitivity = "overlap_sst")
  save_csv(basin_hgam_shift_overlap, "Basin_HGAM_shift_overlap_summary.csv", row.names = FALSE, category = "supplementary")
} else {
  basin_hgam_shift_overlap <- tibble(
    response = available_responses,
    n = NA_real_,
    n_azov_black = NA_real_,
    n_other = NA_real_,
    basin_coef = NA_real_,
    basin_p = NA_real_,
    delta_aic_basin = NA_real_,
    delta_dev_expl_basin = NA_real_,
    status = basin_overlap_summary$status[1],
    sensitivity = "overlap_sst"
  )
  save_csv(basin_hgam_shift_overlap, "Basin_HGAM_shift_overlap_summary.csv", row.names = FALSE, category = "supplementary")
}

basin_shift_combined <- bind_rows(
  basin_hgam_shift %>% mutate(sensitivity = "full_range"),
  basin_hgam_shift_overlap
)

if (nrow(basin_shift_combined %>% filter(is.finite(basin_coef))) > 0) {
  sig_colors <- c("p < 0.05" = "#2b8cbe", "n.s." = "#bdbdbd")
  p_basin_shift_combined <- basin_shift_combined %>%
    mutate(
      sensitivity = factor(sensitivity, levels = c("full_range", "overlap_sst"), labels = c("Full SST range", "Overlapping SST range")),
      sig = ifelse(is.finite(basin_p) & basin_p < 0.05, "p < 0.05", "n.s."),
      response = factor(response, levels = rev(unique(available_responses)))
    ) %>%
    ggplot(aes(x = response, y = basin_coef, fill = sig)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    facet_wrap(~ sensitivity, ncol = 2) +
    scale_fill_manual(values = sig_colors, drop = FALSE) +
    theme_bw() +
    labs(
      title = "Pooled HGAM basin shift after nonlinear environmental adjustment",
      subtitle = "Comparison of the full dataset and the SST-overlap sensitivity subset",
      x = "Response",
      y = "Adjusted basin coefficient",
      fill = "Significance"
    )
  save_plot(file.path(cfg$paths$figure_dir, "Basin_HGAM_shift_combined.png"), p_basin_shift_combined, width = 11.8, height = 6.3, category = "main")
}

fit_basin_shift_hgam_model <- function(df, response, k_base = 5) {
  d <- df %>%
    select(BasinGroup, Species, SST, logPP, S, all_of(response)) %>%
    filter(is.finite(.data[[response]]), is.finite(SST), is.finite(logPP), is.finite(S), !is.na(BasinGroup))

  grp_n <- table(d$BasinGroup)
  if (nrow(d) < 60 || length(grp_n) < 2 || any(grp_n < 20)) {
    return(list(response = response, status = "Skipped: insufficient basin coverage", full = NULL, reduced = NULL, data_used = d))
  }

  k_s <- max(4, min(k_base, floor(nrow(d) / 16)))
  f_red <- as.formula(
    paste0(response, " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')")
  )
  f_full <- as.formula(
    paste0(response, " ~ BasinGroup + s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ") + s(Species, bs='re')")
  )
  m_red <- tryCatch(bam(f_red, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  m_full <- tryCatch(bam(f_full, data = d, method = "fREML", select = TRUE, discrete = TRUE), error = function(e) NULL)
  if (is.null(m_red) || is.null(m_full)) {
    return(list(response = response, status = "Fit failed", full = NULL, reduced = NULL, data_used = d))
  }
  list(response = response, status = "OK", full = m_full, reduced = m_red, data_used = d)
}

collect_basin_diagnostics <- function(model_obj, analysis_type) {
  if (is.null(model_obj$full) || is.null(model_obj$reduced)) {
    return(tibble(
      analysis = analysis_type,
      response = model_obj$response,
      dev_expl_full = NA_real_,
      dev_expl_reduced = NA_real_,
      delta_dev_expl = NA_real_,
      r_sq_full = NA_real_,
      r_sq_reduced = NA_real_,
      delta_rsq = NA_real_,
      aic_full = NA_real_,
      aic_reduced = NA_real_,
      delta_aic = NA_real_,
      max_cc_observed = NA_real_,
      status = model_obj$status
    ))
  }

  s_full <- summary(model_obj$full)
  s_red <- summary(model_obj$reduced)
  cc <- tryCatch(mgcv::concurvity(model_obj$full, full = TRUE), error = function(e) NULL)
  max_cc_observed <- NA_real_
  if (!is.null(cc) && !is.null(rownames(cc)) && "observed" %in% rownames(cc)) {
    vals <- suppressWarnings(as.numeric(cc["observed", ]))
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0) max_cc_observed <- max(vals, na.rm = TRUE)
  }

  tibble(
    analysis = analysis_type,
    response = model_obj$response,
    dev_expl_full = as.numeric(s_full$dev.expl),
    dev_expl_reduced = as.numeric(s_red$dev.expl),
    delta_dev_expl = as.numeric(s_full$dev.expl) - as.numeric(s_red$dev.expl),
    r_sq_full = as.numeric(s_full$r.sq),
    r_sq_reduced = as.numeric(s_red$r.sq),
    delta_rsq = as.numeric(s_full$r.sq) - as.numeric(s_red$r.sq),
    aic_full = AIC(model_obj$full),
    aic_reduced = AIC(model_obj$reduced),
    delta_aic = AIC(model_obj$full) - AIC(model_obj$reduced),
    max_cc_observed = max_cc_observed,
    status = model_obj$status
  )
}

basin_shift_models <- lapply(available_responses, function(r) fit_basin_shift_hgam_model(data, r))

basin_hgam_diagnostics <- bind_rows(lapply(basin_shift_models, function(x) collect_basin_diagnostics(x, "basin_shift")))
save_csv(basin_hgam_diagnostics, "Basin_HGAM_diagnostics.csv", row.names = FALSE, category = "diagnostics")

for (x in basin_shift_models) {
  if (!identical(x$status, "OK")) next
  save_model_diagnostics(x$full, x$data_used, x$response, prefix = "basin_shift")
}

# Detailed concurvity table per response
concurvity_tbl <- bind_rows(lapply(pooled_models, function(x) {
  if (is.null(x$fit) || is.null(x$concurvity)) return(NULL)
  cc <- as.data.frame(x$concurvity)
  cc$metric <- rownames(cc)
  out <- cc %>%
    pivot_longer(cols = -metric, names_to = "term", values_to = "value") %>%
    mutate(response = x$response)
  out
}))

if (nrow(concurvity_tbl) > 0) {
save_csv(concurvity_tbl, "HGAM_concurvity.csv", row.names = FALSE, category = "diagnostics")
}

# Smooth statistics table
smooth_tbl <- bind_rows(lapply(pooled_models, function(x) {
  if (is.null(x$fit)) return(NULL)
  st <- summary(x$fit)
  s_tab <- as.data.frame(st$s.table)
  s_tab$term <- rownames(st$s.table)
  s_tab$response <- x$response
  as_tibble(s_tab)
}))
if (nrow(smooth_tbl) > 0) save_csv(smooth_tbl, "HGAM_smooth_terms.csv", row.names = FALSE, category = "diagnostics")

# Diagnostic and model figures
make_population_curve <- function(model, response_name, d, file_out) {
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = 180)
  nd <- tibble(
    SST = sst_seq,
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE),
    Species = levels(d$Species)[1]
  )
  if ("SpeciesFS" %in% names(d)) {
    nd$SpeciesFS <- levels(d$SpeciesFS)[1]
  }

  sm_labels <- vapply(model$smooth, function(s) s$label, character(1))
  ex_terms <- sm_labels[grepl("Species", sm_labels)]

  pred <- predict(model, newdata = nd, se.fit = TRUE, type = "response", exclude = ex_terms)
  pr <- tibble(
    SST = sst_seq,
    fit = as.numeric(pred$fit),
    lwr = as.numeric(pred$fit - 1.96 * pred$se.fit),
    upr = as.numeric(pred$fit + 1.96 * pred$se.fit)
  )

  p <- ggplot(pr, aes(SST, fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.22, fill = "#1f77b4") +
    geom_line(size = 1.0, color = "#1f77b4") +
    theme_bw() +
    labs(title = paste0(response_name, ": pooled SST effect"), x = "SST", y = response_name)

  save_plot(file_out, p, width = 8.5, height = 5.2, category = "main")
}

extract_population_env_curves <- function(model, response_name, d, n_points = 180) {
  sm_labels <- vapply(model$smooth, function(s) s$label, character(1))
  env_vars_local <- c("SST", "logPP", "S")

  bind_rows(lapply(env_vars_local, function(var_name) {
    rng <- range(d[[var_name]], na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) <= 0) return(NULL)

    x_seq <- seq(rng[1], rng[2], length.out = n_points)
    nd <- build_prediction_row(d)[rep(1, n_points), , drop = FALSE]
    nd[[var_name]] <- x_seq
    term_pattern <- switch(
      var_name,
      SST = "^s\\(SST\\)$",
      logPP = "^s\\(logPP\\)$",
      S = "^s\\(S\\)$"
    )
    term_label <- sm_labels[grepl(term_pattern, sm_labels)][1]
    if (!is.finite(length(term_label)) || is.na(term_label)) return(NULL)
    pred <- tryCatch(
      predict(model, newdata = nd, type = "terms", terms = term_label, se.fit = TRUE),
      error = function(e) NULL
    )
    if (is.null(pred)) return(NULL)
    fit_vec <- as.numeric(pred$fit[, 1])
    se_vec <- as.numeric(pred$se.fit[, 1])

    tibble(
      response = response_name,
      predictor = factor(var_name, levels = env_vars_local),
      x = x_seq,
      fit = fit_vec,
      lwr = fit_vec - 1.96 * se_vec,
      upr = fit_vec + 1.96 * se_vec
    )
  }))
}

make_species_curves <- function(model, response_name, d, file_out) {
  spp <- levels(d$Species)
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = 120)

  nd <- expand.grid(SST = sst_seq, Species = spp, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(
      Species = factor(Species, levels = spp),
      logPP = median(d$logPP, na.rm = TRUE),
      S = median(d$S, na.rm = TRUE)
    )

  if ("SpeciesFS" %in% names(d)) {
    sf_map <- d %>% distinct(Species, SpeciesFS)
    nd <- nd %>% left_join(sf_map, by = "Species")
  }

  nd$fit <- as.numeric(predict(model, newdata = nd, type = "response"))

  p <- ggplot(nd, aes(SST, fit, color = Species)) +
    geom_line(size = 0.9, alpha = 0.9) +
    theme_bw() +
    labs(title = paste0(response_name, ": species-specific SST responses"), x = "SST", y = response_name)

  save_plot(file_out, p, width = 10.5, height = 6.8, category = "supplementary")
}

extract_species_sst_curves <- function(model, response_name, d, n_points = 120) {
  spp <- levels(d$Species)
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = n_points)
  sm_labels <- vapply(model$smooth, function(s) s$label, character(1))
  term_labels <- sm_labels[grepl("^s\\(SST\\)$|^s\\(SST,SpeciesFS\\)$", sm_labels)]
  if (length(term_labels) == 0) return(NULL)

  nd <- expand.grid(SST = sst_seq, Species = spp, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(
      Species = factor(Species, levels = spp),
      logPP = median(d$logPP, na.rm = TRUE),
      S = median(d$S, na.rm = TRUE)
    )

  if ("SpeciesFS" %in% names(d)) {
    sf_map <- d %>% distinct(Species, SpeciesFS)
    nd <- nd %>% left_join(sf_map, by = "Species")
  } else {
    nd <- nd %>% mutate(SpeciesFS = factor(as.character(Species)))
  }

  pred <- tryCatch(
    predict(model, newdata = nd, type = "terms", terms = term_labels, se.fit = FALSE),
    error = function(e) NULL
  )
  if (is.null(pred)) return(NULL)

  fit_terms <- rowSums(as.matrix(pred), na.rm = TRUE)

  nd %>%
    transmute(
      response = response_name,
      Species = as.character(Species),
      SpeciesFS = as.character(SpeciesFS),
      Species_plot = ifelse(SpeciesFS == "Rare", "Rare", as.character(Species)),
      SST,
      fit = as.numeric(fit_terms)
    ) %>%
    group_by(response, Species_plot, SST) %>%
    summarise(fit = mean(fit, na.rm = TRUE), .groups = "drop")
}

pooled_env_curves <- list()
species_sst_curves <- list()
for (r in available_responses) {
  m <- pooled_models[[r]]$fit
  if (is.null(m)) next

  d_used <- pooled_models[[r]]$data_used
  save_model_diagnostics(m, d_used, r)

  png(file.path(cfg$paths$figure_dir, paste0("HGAM_smooths_", safe_name(r), ".png")),
    width = 1450, height = 1150, res = 170
  )
  plot(m, pages = 1, shade = TRUE, residuals = TRUE, scale = 0)
  dev.off()

  png(file.path(cfg$paths$figure_dir, paste0("HGAM_interaction_SST_logPP_", safe_name(r), ".png")),
    width = 1200, height = 900, res = 170
  )
  try(vis.gam(m, view = c("SST", "logPP"), plot.type = "contour", color = "terrain"), silent = TRUE)
  dev.off()

  make_population_curve(
    model = m,
    response_name = r,
    d = d_used,
    file_out = file.path(cfg$paths$figure_dir, paste0("HGAM_population_SST_curve_", safe_name(r), ".png"))
  )
  pooled_env_curves[[r]] <- extract_population_env_curves(m, r, d_used)

  make_species_curves(
    model = m,
    response_name = r,
    d = d_used,
    file_out = file.path(cfg$paths$figure_dir, paste0("HGAM_species_SST_curves_", safe_name(r), ".png"))
  )
  species_sst_curves[[r]] <- extract_species_sst_curves(m, r, d_used)
}

pooled_env_curves_df <- bind_rows(pooled_env_curves)
if (nrow(pooled_env_curves_df) > 0) {
  predictor_labels <- c(SST = "SST", logPP = "logPP", S = "Salinity")
  p_pooled_env <- ggplot(pooled_env_curves_df, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.22, fill = "#1f77b4") +
    geom_line(linewidth = 0.9, color = "#1f77b4") +
    facet_grid(response ~ predictor, scales = "free", labeller = labeller(predictor = predictor_labels)) +
    theme_bw() +
    labs(
      title = "Smooth terms from pooled HGAMs describing environmental effects on life-history traits",
      x = "Environmental predictor",
      y = "Predicted trait value"
    ) +
    theme(strip.background = element_rect(fill = "grey95"))

  save_plot(
    file.path(cfg$paths$figure_dir, "HGAM_pooled_environmental_smooths_combined.png"),
    p_pooled_env,
    width = 12.5,
    height = 9.0,
    category = "main"
  )
}

species_sst_curves_df <- bind_rows(species_sst_curves)
if (nrow(species_sst_curves_df) > 0) {
  species_levels_plot <- unique(species_sst_curves_df$Species_plot)
  species_palette <- setNames(grDevices::hcl.colors(length(species_levels_plot), "Dark 3"), species_levels_plot)

  p_species_sst <- ggplot(species_sst_curves_df, aes(x = SST, y = fit, color = Species_plot, group = Species_plot)) +
    geom_line(linewidth = 0.85, alpha = 0.95) +
    facet_wrap(~ response, scales = "free_y", ncol = 2) +
    scale_color_manual(values = species_palette, drop = FALSE) +
    theme_bw() +
    labs(
      title = "Species-specific smooth functions describing temperature-trait relationships in pooled HGAMs",
      x = "SST",
      y = "Partial temperature effect",
      color = "Species / group"
    ) +
    theme(
      legend.position = "right",
      strip.background = element_rect(fill = "grey95")
    )

  save_plot(
    file.path(cfg$paths$figure_dir, "HGAM_species_SST_smooths_combined.png"),
    p_species_sst,
    width = 12.8,
    height = 10.0,
    category = "main"
  )
}

p_inter <- pooled_summary %>%
  mutate(interaction_sig = ifelse(is.finite(interaction_p) & interaction_p < 0.05, "p < 0.05", "n.s.")) %>%
  ggplot(aes(x = reorder(response, interaction_p), y = -log10(pmax(interaction_p, 1e-8)), fill = interaction_sig)) +
  geom_col() +
  geom_hline(yintercept = -log10(0.05), linetype = 2) +
  coord_flip() +
  theme_bw() +
  labs(title = "SST x PP interaction support in pooled HGAM", x = "Response", y = "-log10(p)")
save_plot(file.path(cfg$paths$figure_dir, "HGAM_interaction_support.png"), p_inter, width = 8.5, height = 5.5, category = "supplementary")

# ----------------------------
# 3) Species-level GAM/LM (TSR core)
# ----------------------------
# TSR scoring rules requested by author:
# - Linf, Lmax, Lmat, Amax should decrease with SST
# - TL1 should increase with SST
# k and phi_prime are excluded from the workflow because their thermal
# interpretation is not independent of Linf.
expected_sign <- c(
  Linf = -1,
  Lmax = -1,
  TL1 = 1,
  Lmat = -1,
  Amax = -1
)

tsr_scored_traits <- intersect(c("Linf", "Lmax", "TL1", "Lmat", "Amax"), available_responses)
species_model_traits <- tsr_scored_traits

predict_species_temp_delta <- function(model, d, width = standard_temp_width) {
  sst_med <- median(d$SST, na.rm = TRUE)
  sst_rng <- range(d$SST, na.rm = TRUE)
  if (!is.finite(sst_med) || !all(is.finite(sst_rng)) || diff(sst_rng) < width) return(NA_real_)

  nd <- tibble(
    SST = c(sst_med - width / 2, sst_med + width / 2),
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE)
  )
  if (nd$SST[1] < sst_rng[1] || nd$SST[2] > sst_rng[2]) return(NA_real_)

  p <- as.numeric(predict(model, newdata = nd, type = "response"))
  p[2] - p[1]
}

predict_species_temp_pct <- function(model, d, width = standard_temp_width) {
  sst_med <- median(d$SST, na.rm = TRUE)
  sst_rng <- range(d$SST, na.rm = TRUE)
  if (!is.finite(sst_med) || !all(is.finite(sst_rng)) || diff(sst_rng) < width) return(NA_real_)

  base_nd <- tibble(
    SST = sst_med,
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE)
  )
  base_fit <- as.numeric(predict(model, newdata = base_nd, type = "response"))[1]
  if (!is.finite(base_fit) || abs(base_fit) < 1e-8) return(NA_real_)

  100 * predict_species_temp_delta(model, d, width = width) / abs(base_fit)
}

calc_nonlinearity <- function(model, d) {
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = 80)
  if (!all(is.finite(sst_seq)) || length(unique(sst_seq)) < 5) return(NA_real_)

  nd <- tibble(
    SST = sst_seq,
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE)
  )

  pred <- as.numeric(predict(model, newdata = nd, type = "response"))
  if (sum(is.finite(pred)) < 10) return(NA_real_)

  lin <- lm(pred ~ sst_seq)
  fit_lin <- as.numeric(fitted(lin))
  denom <- sd(pred, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) denom <- 1

  sqrt(mean((pred - fit_lin)^2, na.rm = TRUE)) / denom
}

calc_direction_switches <- function(model, d) {
  sst_seq <- seq(min(d$SST, na.rm = TRUE), max(d$SST, na.rm = TRUE), length.out = 100)
  if (!all(is.finite(sst_seq)) || length(unique(sst_seq)) < 6) return(NA_integer_)

  nd <- tibble(
    SST = sst_seq,
    logPP = median(d$logPP, na.rm = TRUE),
    S = median(d$S, na.rm = TRUE)
  )

  pred <- as.numeric(predict(model, newdata = nd, type = "response"))
  if (sum(is.finite(pred)) < 12) return(NA_integer_)

  dp <- diff(pred)
  eps <- 0.02 * sd(pred, na.rm = TRUE)
  if (!is.finite(eps) || eps <= 0) eps <- 1e-8
  sgn <- sign(dp)
  sgn[abs(dp) <= eps] <- 0
  sgn <- sgn[sgn != 0]
  if (length(sgn) <= 1) return(0L)

  as.integer(sum(diff(sgn) != 0))
}

fit_species_model <- function(df, sp, response) {
  d <- df %>%
    filter(Species == sp) %>%
    select(all_of(c("SST", "logPP", "S", response))) %>%
    filter(if_all(everything(), is.finite))

  n <- nrow(d)
  n_sst <- n_distinct(d$SST)

  if (n < 10 || n_sst < 4) {
    return(tibble(
      Species = as.character(sp),
      response = response,
      n = n,
      method = "skip",
      temp_delta = NA_real_,
      temp_delta_pct = NA_real_,
      temp_p = NA_real_,
      dev_expl = NA_real_,
      nonlinearity_index = NA_real_,
      direction_switches = NA_integer_
    ))
  }

  if (n >= 20 && n_sst >= 6) {
    k_s <- max(4, min(5, floor(n / 6)))
    f <- as.formula(paste0(
      response, " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ")"
    ))

    out <- tryCatch({
      m <- gam(f, data = d, method = "REML", select = TRUE)
      st <- summary(m)

      p_sst <- tryCatch({
        idx <- grep("^s\\(SST", rownames(st$s.table))
        if (length(idx) == 0) NA_real_ else as.numeric(st$s.table[idx[1], "p-value"])
      }, error = function(e) NA_real_)

      tibble(
        Species = as.character(sp),
        response = response,
        n = n,
        method = "GAM",
        temp_delta = predict_species_temp_delta(m, d),
        temp_delta_pct = predict_species_temp_pct(m, d),
        temp_p = p_sst,
        dev_expl = st$dev.expl,
        nonlinearity_index = calc_nonlinearity(m, d),
        direction_switches = calc_direction_switches(m, d),
        status = "OK"
      )
    }, error = function(e) {
      tibble(
        Species = as.character(sp),
        response = response,
        n = n,
        method = "GAM",
        temp_delta = NA_real_,
        temp_delta_pct = NA_real_,
        temp_p = NA_real_,
        dev_expl = NA_real_,
        nonlinearity_index = NA_real_,
        direction_switches = NA_integer_,
        status = "Fit failed"
      )
    })

    out
  } else {
    out <- tryCatch({
      m <- lm(as.formula(paste0(response, " ~ SST + logPP + S")), data = d)
      st <- summary(m)

      tibble(
        Species = as.character(sp),
        response = response,
        n = n,
        method = "LM",
        temp_delta = {
          nd <- tibble(
            SST = c(median(d$SST, na.rm = TRUE) - standard_temp_width / 2, median(d$SST, na.rm = TRUE) + standard_temp_width / 2),
            logPP = median(d$logPP, na.rm = TRUE),
            S = median(d$S, na.rm = TRUE)
          )
          sst_rng <- range(d$SST, na.rm = TRUE)
          if (nd$SST[1] < sst_rng[1] || nd$SST[2] > sst_rng[2]) NA_real_ else as.numeric(diff(predict(m, newdata = nd)))
        },
        temp_delta_pct = {
          base_nd <- tibble(
            SST = median(d$SST, na.rm = TRUE),
            logPP = median(d$logPP, na.rm = TRUE),
            S = median(d$S, na.rm = TRUE)
          )
          base_fit <- as.numeric(predict(m, newdata = base_nd))[1]
          delta_fit <- {
            nd <- tibble(
              SST = c(median(d$SST, na.rm = TRUE) - standard_temp_width / 2, median(d$SST, na.rm = TRUE) + standard_temp_width / 2),
              logPP = median(d$logPP, na.rm = TRUE),
              S = median(d$S, na.rm = TRUE)
            )
            sst_rng <- range(d$SST, na.rm = TRUE)
            if (nd$SST[1] < sst_rng[1] || nd$SST[2] > sst_rng[2]) NA_real_ else as.numeric(diff(predict(m, newdata = nd)))
          }
          if (!is.finite(base_fit) || abs(base_fit) < 1e-8 || !is.finite(delta_fit)) NA_real_ else 100 * delta_fit / abs(base_fit)
        },
        temp_p = tryCatch(as.numeric(st$coefficients["SST", "Pr(>|t|)"]), error = function(e) NA_real_),
        dev_expl = st$adj.r.squared,
        nonlinearity_index = 0,
        direction_switches = 0L,
        status = "OK"
      )
    }, error = function(e) {
      tibble(
        Species = as.character(sp),
        response = response,
        n = n,
        method = "LM",
        temp_delta = NA_real_,
        temp_delta_pct = NA_real_,
        temp_p = NA_real_,
        dev_expl = NA_real_,
        nonlinearity_index = NA_real_,
        direction_switches = NA_integer_,
        status = "Fit failed"
      )
    })

    out
  }
}

species_list <- levels(data$Species)
species_effects <- bind_rows(
  lapply(species_list, function(sp) {
    bind_rows(lapply(species_model_traits, function(r) fit_species_model(data, sp, r)))
  })
)

species_effects <- species_effects %>%
  mutate(
    expected_sign = unname(expected_sign[response]),
    observed_sign = sign(temp_delta),
    tsr_rule = ifelse(response %in% tsr_scored_traits, "scored", "descriptive_only"),
    model_supported = ifelse(
      response %in% tsr_scored_traits,
      is.finite(temp_delta_pct) & abs(temp_delta_pct) >= min_relative_effect_pct &
        (
          (is.finite(temp_p) & temp_p < min_supported_p) |
            (is.finite(dev_expl) & dev_expl >= min_supported_dev_expl)
        ),
      NA
    ),
    effect_supported = ifelse(
      response %in% tsr_scored_traits,
      is.finite(temp_delta_pct) & abs(temp_delta_pct) >= min_relative_effect_pct &
        (is.finite(temp_p) & temp_p < min_supported_p),
      NA
    ),
    tsr_support_status = case_when(
      response %in% tsr_scored_traits &
        is.finite(expected_sign) &
        is.finite(observed_sign) &
        effect_supported &
        observed_sign == expected_sign ~ "supports_tsr",
      response %in% tsr_scored_traits &
        is.finite(expected_sign) &
        is.finite(observed_sign) &
        effect_supported &
        observed_sign == -expected_sign ~ "reverse_tsr",
      response %in% tsr_scored_traits ~ "weak_or_undetermined",
      TRUE ~ "descriptive_only"
    ),
    tsr_match = tsr_support_status == "supports_tsr",
    reverse_match = tsr_support_status == "reverse_tsr",
    tsr_signed_effect = ifelse(response %in% tsr_scored_traits, temp_delta_pct * expected_sign, NA_real_),
    nonlinear_flag = ifelse(
      is.finite(nonlinearity_index) | is.finite(direction_switches),
      (ifelse(is.finite(nonlinearity_index), nonlinearity_index, 0) >= 0.35) |
        (ifelse(is.finite(direction_switches), direction_switches, 0) >= 1),
      NA
    ),
    strong_effect = is.finite(temp_delta_pct) & abs(temp_delta_pct) >= min_relative_effect_pct
  ) %>%
  ungroup()

save_csv(species_effects, "Species_TSR_temperature_effects.csv", row.names = FALSE, category = "main")

# Species-specific GAM diagnostics: concurvity and unique environmental share
fit_species_env_concurvity <- function(df, sp, response, focal_var) {
  d <- df %>%
    filter(Species == sp) %>%
    select(all_of(c("SST", "logPP", "S", response))) %>%
    filter(if_all(everything(), is.finite))

  n <- nrow(d)
  if (n < 14 || n_distinct(d$SST) < 5 || n_distinct(d$logPP) < 3 || n_distinct(d$S) < 3) {
    return(tibble(
      Species = as.character(sp),
      response = response,
      focal_var = focal_var,
      n = n,
      k = NA_integer_,
      cc_obs = NA_real_,
      delta_dev_expl = NA_real_,
      delta_dev_expl_pct = NA_real_,
      delta_rsq = NA_real_,
      delta_aic = NA_real_,
      p_drop = NA_real_,
      status = "Skipped: insufficient species-level support"
    ))
  }

  k_s <- max(3, min(4, floor(n / 8)))
  f_full <- as.formula(paste0(
    response, " ~ s(SST, bs='ts', k=", k_s, ") + s(logPP, bs='ts', k=", k_s, ") + s(S, bs='ts', k=", k_s, ")"
  ))
  kept_vars <- setdiff(c("SST", "logPP", "S"), focal_var)
  f_drop <- as.formula(paste0(
    response, " ~ ", paste0("s(", kept_vars, ", bs='ts', k=", k_s, ")", collapse = " + ")
  ))

  m_full <- tryCatch(gam(f_full, data = d, method = "REML", select = TRUE), error = function(e) NULL)
  m_drop <- tryCatch(gam(f_drop, data = d, method = "REML", select = TRUE), error = function(e) NULL)
  if (is.null(m_full) || is.null(m_drop)) {
    return(tibble(
      Species = as.character(sp),
      response = response,
      focal_var = focal_var,
      n = n,
      k = k_s,
      cc_obs = NA_real_,
      delta_dev_expl = NA_real_,
      delta_dev_expl_pct = NA_real_,
      delta_rsq = NA_real_,
      delta_aic = NA_real_,
      p_drop = NA_real_,
      status = "Fit failed"
    ))
  }

  cc <- tryCatch(mgcv::concurvity(m_full, full = TRUE), error = function(e) NULL)
  cc_obs <- NA_real_
  if (!is.null(cc) && !is.null(rownames(cc)) && "observed" %in% rownames(cc)) {
    cn <- colnames(cc)
    i_var <- switch(
      focal_var,
      SST = grep("^s\\(SST", cn),
      logPP = grep("^s\\(logPP", cn),
      S = grep("^s\\(S\\)", cn),
      integer(0)
    )
    if (length(i_var) > 0) cc_obs <- as.numeric(cc["observed", i_var[1]])
  }

  s_full <- summary(m_full)
  s_drop <- summary(m_drop)
  cmp <- tryCatch(anova(m_drop, m_full, test = "F"), error = function(e) NULL)
  p_drop <- get_interaction_p(cmp)

  dev_full <- as.numeric(s_full$dev.expl)
  dev_drop <- as.numeric(s_drop$dev.expl)
  d_dev <- dev_full - dev_drop

  tibble(
    Species = as.character(sp),
    response = response,
    focal_var = focal_var,
    n = n,
    k = k_s,
    cc_obs = cc_obs,
    delta_dev_expl = d_dev,
    delta_dev_expl_pct = ifelse(is.finite(dev_full) && dev_full > 0, 100 * d_dev / dev_full, NA_real_),
    delta_rsq = as.numeric(s_full$r.sq) - as.numeric(s_drop$r.sq),
    delta_aic = AIC(m_full) - AIC(m_drop),
    p_drop = p_drop,
    status = "OK"
  )
}

env_vars_model <- c("SST", "logPP", "S")

species_env_concurvity <- bind_rows(lapply(species_list, function(sp) {
  bind_rows(lapply(tsr_scored_traits, function(r) {
    bind_rows(lapply(env_vars_model, function(v) fit_species_env_concurvity(data, sp, r, v)))
  }))
}))

save_csv(species_env_concurvity, "Species_GAM_env_concurvity_and_share.csv", row.names = FALSE, category = "supplementary")

species_env_summary <- species_env_concurvity %>%
  filter(status == "OK") %>%
  group_by(response, focal_var) %>%
  summarise(
    n_models = n(),
    median_cc_obs = median(cc_obs, na.rm = TRUE),
    median_delta_dev_pct = median(delta_dev_expl_pct, na.rm = TRUE),
    median_delta_aic = median(delta_aic, na.rm = TRUE),
    frac_p_drop_lt_0_05 = mean(p_drop < 0.05, na.rm = TRUE),
    .groups = "drop"
  )
save_csv(species_env_summary, "Species_GAM_env_concurvity_summary.csv", row.names = FALSE, category = "main")

species_sst_concurvity <- species_env_concurvity %>%
  filter(focal_var == "SST") %>%
  transmute(
    Species,
    response,
    n,
    k,
    cc_obs_sst = cc_obs,
    cc_obs_logpp = NA_real_,
    cc_obs_s = NA_real_,
    delta_dev_expl_sst = delta_dev_expl,
    delta_dev_expl_sst_pct = delta_dev_expl_pct,
    delta_rsq_sst = delta_rsq,
    delta_aic_sst = delta_aic,
    p_sst_drop = p_drop,
    status
  )

save_csv(species_sst_concurvity, "Species_GAM_SST_concurvity_and_share.csv", row.names = FALSE, category = "supplementary")

species_sst_summary <- species_sst_concurvity %>%
  filter(status == "OK") %>%
  group_by(response) %>%
  summarise(
    n_models = n(),
    median_cc_obs_sst = median(cc_obs_sst, na.rm = TRUE),
    median_delta_dev_sst_pct = median(delta_dev_expl_sst_pct, na.rm = TRUE),
    median_delta_aic_sst = median(delta_aic_sst, na.rm = TRUE),
    frac_p_sst_drop_lt_0_05 = mean(p_sst_drop < 0.05, na.rm = TRUE),
    .groups = "drop"
  )
save_csv(species_sst_summary, "Species_GAM_SST_concurvity_summary.csv", row.names = FALSE, category = "main")

p_env_share <- species_env_concurvity %>%
  filter(status == "OK") %>%
  mutate(
    focal_var = factor(focal_var, levels = c("SST", "logPP", "S")),
    Species = factor(Species, levels = rev(sort(unique(as.character(Species))))),
    fill_value = ifelse(is.finite(delta_dev_expl_pct), delta_dev_expl_pct, NA_real_)
  ) %>%
  ggplot(aes(x = focal_var, y = Species, fill = fill_value)) +
  geom_tile(color = "white") +
  facet_wrap(~ response, ncol = 3) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0, na.value = "grey85") +
  theme_bw() +
  labs(
    title = "Species-level unique environmental contribution",
    subtitle = "Fill = percent of full-model explained deviance uniquely attributable to each factor",
    x = "Environmental factor",
    y = "Species",
    fill = "Unique share\n(% dev.)"
  )
save_plot(file.path(cfg$paths$figure_dir, "Species_env_unique_share_heatmap.png"), p_env_share, width = 11.5, height = 8.0, category = "main")

total_tsr_traits <- max(1, length(tsr_scored_traits))

species_tsr_score <- species_effects %>%
  filter(response %in% tsr_scored_traits, is.finite(tsr_signed_effect)) %>%
  group_by(Species) %>%
  summarise(
    n_traits = n(),
    n_match = sum(tsr_match, na.rm = TRUE),
    n_reverse = sum(reverse_match, na.rm = TRUE),
    n_strong = sum(strong_effect, na.rm = TRUE),
    n_match_strong = sum(tsr_match & strong_effect, na.rm = TRUE),
    n_reverse_strong = sum(reverse_match & strong_effect, na.rm = TRUE),
    tsr_match_rate = ifelse(n_traits > 0, n_match / n_traits, NA_real_),
    tsr_reverse_rate = ifelse(n_traits > 0, n_reverse / n_traits, NA_real_),
    tsr_match_rate_strong = ifelse(n_strong > 0, n_match_strong / n_strong, NA_real_),
    tsr_reverse_rate_strong = ifelse(n_strong > 0, n_reverse_strong / n_strong, NA_real_),
    coverage = n_traits / total_tsr_traits,
    coverage_strong = n_strong / total_tsr_traits,
    mean_abs_delta = mean(abs(temp_delta_pct), na.rm = TRUE),
    mean_tsr_signed_effect = mean(tsr_signed_effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tsr_match_rate_shrunk = ifelse(n_traits > 0, (n_match + 1) / (n_traits + 2), NA_real_),
    wilson_low = ifelse(
      n_traits > 0,
      {
        z <- 1.96
        p <- tsr_match_rate
        n <- pmax(n_traits, 1)
        (p + z^2 / (2 * n) - z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n)) / (1 + z^2 / n)
      },
      NA_real_
    ),
    wilson_high = ifelse(
      n_traits > 0,
      {
        z <- 1.96
        p <- tsr_match_rate
        n <- pmax(n_traits, 1)
        (p + z^2 / (2 * n) + z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n)) / (1 + z^2 / n)
      },
      NA_real_
    ),
    tsr_score_adjusted = ifelse(is.finite(tsr_match_rate_shrunk), tsr_match_rate_shrunk * coverage, 0)
  ) %>%
  arrange(desc(tsr_score_adjusted), desc(tsr_match_rate_shrunk), desc(mean_abs_delta))

save_csv(species_tsr_score, "Species_TSR_match_scores.csv", row.names = FALSE, category = "supplementary")

p_eff <- ggplot(species_effects %>% filter(method != "skip"), aes(x = response, y = temp_delta_pct, color = Species)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  geom_point(position = position_jitter(width = 0.12, height = 0), size = 2.1, alpha = 0.85) +
  theme_bw() +
  labs(title = "Species-specific temperature effect sizes", x = "Trait", y = "Predicted percent change per 1 C")
save_plot(file.path(cfg$paths$figure_dir, "Species_temperature_effect_sizes.png"), p_eff, width = 11, height = 6.2, category = "supplementary")

## Aggregated TSR score is saved to CSV for supplementary use; no standalone barplot to avoid mixed interpretation with cluster heatmap.

# ----------------------------
# 4) Clustering reaction types
# ----------------------------
species_trait_quality <- species_sst_concurvity %>%
  filter(response %in% tsr_scored_traits) %>%
  transmute(
    Species,
    response,
    sst_model_ok = grepl("^OK", status),
    sst_quality = ifelse(is.finite(cc_obs_sst), pmax(0, 1 - cc_obs_sst), NA_real_),
    sst_unique_support = is.finite(delta_dev_expl_sst_pct) & delta_dev_expl_sst_pct >= 5 &
      is.finite(delta_aic_sst) & delta_aic_sst <= -2 &
      is.finite(p_sst_drop) & p_sst_drop < 0.05
  )

cluster_long <- species_effects %>%
  filter(response %in% tsr_scored_traits, is.finite(temp_delta_pct)) %>%
  left_join(species_trait_quality, by = c("Species", "response")) %>%
  mutate(
    sst_model_ok = replace_na(sst_model_ok, FALSE),
    sst_unique_support = replace_na(sst_unique_support, FALSE),
    sst_quality = replace_na(sst_quality, 0),
    # Trait contributes to clustering only when a strong response has SST-specific support,
    # either from the direct SST term or from the SST unique-support comparison.
    strong_effect_final = (strong_effect & (effect_supported | sst_unique_support)) | sst_unique_support
  ) %>%
  ungroup()

cluster_table <- cluster_long %>%
  group_by(Species) %>%
  summarise(
    n_cluster_traits = n(),
    n_strong = sum(strong_effect_final, na.rm = TRUE),
    n_weak = sum(!strong_effect_final, na.rm = TRUE),
    n_match_strong = sum(tsr_match & strong_effect_final, na.rm = TRUE),
    n_reverse_strong = sum(reverse_match & strong_effect_final, na.rm = TRUE),
    mean_abs_pct_strong = ifelse(sum(strong_effect_final, na.rm = TRUE) > 0, mean(abs(temp_delta_pct[strong_effect_final]), na.rm = TRUE), 0),
    n_sst_supported = sum(sst_unique_support, na.rm = TRUE),
    match_rate = mean(tsr_match, na.rm = TRUE),
    reverse_rate = mean(reverse_match, na.rm = TRUE),
    weak_share = mean(!strong_effect_final, na.rm = TRUE),
    mean_abs_pct = mean(abs(temp_delta_pct), na.rm = TRUE),
    mean_nonlinearity = mean(nonlinearity_index, na.rm = TRUE),
    n_switch_traits = sum(ifelse(is.finite(direction_switches), direction_switches, 0) >= 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(species_tsr_score %>% select(Species, n_traits, coverage, tsr_score_adjusted), by = "Species") %>%
  mutate(
    nonlin_share = n_switch_traits / pmax(1, n_cluster_traits),
    tsr_signal_strength = n_strong / total_tsr_traits,
    tsr_net_strong = n_match_strong - n_reverse_strong,
    tsr_signal_direction = (n_match_strong - n_reverse_strong) / total_tsr_traits,
    cluster_type = case_when(
      weak_share >= 0.8 ~ "weak/undetermined",
      n_strong >= 3 & n_match_strong == n_strong & n_reverse_strong == 0 ~ "support",
      n_strong >= 3 & n_reverse_strong == n_strong & n_match_strong == 0 ~ "reverse",
      n_match_strong > n_reverse_strong ~ "support",
      n_reverse_strong > n_match_strong ~ "reverse",
      TRUE ~ "weak/undetermined"
    )
  ) %>%
  arrange(cluster_type, desc(tsr_net_strong), desc(mean_abs_pct_strong), desc(tsr_signal_strength), desc(tsr_score_adjusted), Species)

all_species_tbl <- tibble(Species = levels(data$Species))
cluster_table <- all_species_tbl %>%
  left_join(cluster_table, by = "Species") %>%
  mutate(
    n_cluster_traits = replace_na(n_cluster_traits, 0L),
    n_strong = replace_na(n_strong, 0L),
    n_weak = replace_na(n_weak, 0L),
    n_match_strong = replace_na(n_match_strong, 0L),
    n_reverse_strong = replace_na(n_reverse_strong, 0L),
    match_rate = replace_na(match_rate, 0),
    reverse_rate = replace_na(reverse_rate, 0),
    weak_share = replace_na(weak_share, 1),
    mean_abs_pct = replace_na(mean_abs_pct, 0),
    mean_abs_pct_strong = replace_na(mean_abs_pct_strong, 0),
    mean_nonlinearity = replace_na(mean_nonlinearity, NA_real_),
    n_switch_traits = replace_na(n_switch_traits, 0L),
    n_sst_supported = replace_na(n_sst_supported, 0L),
    nonlin_share = replace_na(nonlin_share, 0),
    n_traits = replace_na(n_traits, 0L),
    coverage = replace_na(coverage, 0),
    tsr_score_adjusted = replace_na(tsr_score_adjusted, 0),
    tsr_signal_strength = replace_na(tsr_signal_strength, 0),
    tsr_net_strong = replace_na(tsr_net_strong, 0),
    tsr_signal_direction = replace_na(tsr_signal_direction, 0),
    cluster_type = ifelse(is.na(cluster_type), "weak/undetermined", cluster_type)
  ) %>%
  arrange(cluster_type, desc(tsr_net_strong), desc(mean_abs_pct_strong), desc(tsr_signal_strength), desc(tsr_score_adjusted), Species)

save_csv(cluster_table, "Species_response_clusters.csv", row.names = FALSE, category = "main")

p_cluster_scatter <- ggplot(cluster_table, aes(match_rate, reverse_rate, color = cluster_type, size = mean_abs_pct, label = Species)) +
  geom_point(alpha = 0.9) +
  geom_text(size = 2.7, vjust = -0.7, check_overlap = TRUE, show.legend = FALSE) +
  theme_bw() +
  labs(
    title = "Reaction-type classes: support, reverse, weak/undetermined",
    x = "TSR match rate",
    y = "Reverse rate",
    color = "Cluster",
    size = "Mean |%/1 C|"
  )
save_plot(file.path(cfg$paths$figure_dir, "Species_cluster_type_scatter.png"), p_cluster_scatter, width = 9.8, height = 6.6, category = "supplementary")

cluster_order <- cluster_table %>%
  mutate(cluster_type = factor(cluster_type, levels = c("support", "weak/undetermined", "reverse"))) %>%
  arrange(cluster_type, desc(tsr_net_strong), desc(mean_abs_pct_strong), desc(tsr_signal_strength), desc(tsr_score_adjusted), Species)

cluster_labels <- cluster_order %>%
  transmute(
    Species,
    Species_label = paste0(Species, "  [net=", tsr_net_strong, ", %/1C=", sprintf("%.1f", mean_abs_pct_strong), ", sst=", n_sst_supported, "]")
  )
species_label_levels <- rev(cluster_labels$Species_label)

cluster_heat <- cluster_long %>%
  left_join(cluster_table %>% select(Species, cluster_type, tsr_score_adjusted, tsr_signal_strength, tsr_net_strong, mean_abs_pct_strong, tsr_signal_direction, n_sst_supported), by = "Species") %>%
  left_join(cluster_labels, by = "Species") %>%
  mutate(
    cluster_type = factor(cluster_type, levels = c("support", "weak/undetermined", "reverse")),
    tsr_signed_effect = temp_delta_pct * expected_sign,
    Species_label = factor(Species_label, levels = species_label_levels)
  ) %>%
  select(Species, Species_label, cluster_type, response, tsr_signed_effect, strong_effect, strong_effect_final, tsr_score_adjusted, tsr_signal_strength, tsr_net_strong, mean_abs_pct_strong, tsr_signal_direction, n_sst_supported)

p_heat <- ggplot(cluster_heat, aes(x = response, y = Species_label, fill = tsr_signed_effect)) +
  geom_tile(color = "white") +
  geom_point(aes(shape = strong_effect_final), size = 1.8, color = "black", alpha = 0.75, na.rm = TRUE) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0, na.value = "grey85") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  facet_grid(cluster_type ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  labs(
    title = "Species thermal reaction profiles by biologically-defined clusters",
    subtitle = "Quality-gated strong signal: species labels show net strong support, mean absolute percent effect per 1 C, and #traits with SST unique support",
    x = "TSR trait",
    y = "Species [net strong, %/1C, sst-supported traits]",
    fill = "TSR-signed\neffect"
  )
save_plot(file.path(cfg$paths$figure_dir, "Species_response_heatmap.png"), p_heat, width = 10.2, height = 8.2, category = "main")

# ----------------------------
# 5) Save key objects
# ----------------------------
if (isTRUE(cfg$output$save_main)) {
  save(
    data,
    pooled_models,
    pooled_summary,
    species_effects,
    species_env_concurvity,
    species_env_summary,
    species_sst_concurvity,
    species_tsr_score,
    cluster_table,
    basin_hgam_shift,
    basin_overlap_summary,
    basin_hgam_shift_overlap,
    basin_shift_combined,
    basin_hgam_diagnostics,
    file = file.path(cfg$paths$results_dir, "Life history and temp.RData")
  )
}

cat("\nDone. Outputs:\n",
  "- Results/Data_environment_summary.csv\n",
  "- Results/Data_species_coverage.csv\n",
  "- Results/Data_missingness.csv\n",
  "- Results/Data_env_spearman_correlation.csv\n",
  "- Results/Data_env_vif.csv\n",
  "- Results/Basin_coverage_summary.csv\n",
  "- Results/Basin_HGAM_shift_summary.csv\n",
  "- Results/Basin_SST_overlap_summary.csv\n",
  "- Results/Basin_HGAM_shift_overlap_summary.csv\n",
  "- Results/HGAM_pooled_summary.csv\n",
  "- Results/HGAM_concurvity.csv (if available)\n",
  "- Results/HGAM_smooth_terms.csv\n",
  "- Results/Species_TSR_temperature_effects.csv\n",
  "- Results/Species_GAM_env_concurvity_and_share.csv\n",
  "- Results/Species_GAM_env_concurvity_summary.csv\n",
  "- Results/Species_GAM_SST_concurvity_and_share.csv\n",
  "- Results/Species_GAM_SST_concurvity_summary.csv\n",
  "- Results/Species_TSR_match_scores.csv\n",
  "- Results/Species_response_clusters.csv\n",
  "- Results/Life history and temp.RData\n",
  "- Results/figures/Basin_HGAM_shift_combined.png\n",
  "- Results/figures/Species_env_unique_share_heatmap.png\n",
  "- Results/figures/*.png (environment, HGAM, TSR, clustering)\n",
  "- Results/Basin_HGAM_diagnostics.csv\n",
  "- Results/figures/diagnostics/Diag_*_basin_shift_*.png\n",
  "- Results/figures/diagnostics/*.png\n"
)
