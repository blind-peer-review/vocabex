library(tidyverse)
library(lme4)
library(brms)
library(cmdstanr)
library(posterior)
library(readr)

options(contrasts = c("contr.sum", "contr.poly"))

df <- read_csv("data_for_analysis_pca.csv", show_col_types = FALSE)

df <- df %>%
  mutate(
    gram_primary = case_when(
      n_gramcats == 1 ~ gram_primary,
      n_gramcats > 1 ~ "Mixed"
    )
  )

df$error_type[as.character(df$id) %in% c("440", "583")] <- "Synthesis"
df$error_type[as.character(df$id) == "904"] <- "Order"

df <- df %>%
  mutate(
    id = factor(id),
    participant_ID = factor(participant_ID),
    error_type = factor(error_type)
  )

df_clean <- df %>%
  dplyr::select(
    id, participant_ID, text_file, prof_level = level, error_type, exercise_type,
    exercise_format, complete, exercise_score, recognition_imm, correction_imm,
    overall_score_imm, recognition_del, correction_del, overall_score_del,
    time_invested, goal_productive_use_of_targets, goal_meaning_and_sense_elaboration,
    goal_contextual_retrieval, goal_paraphrastic_flexibility,
    goal_conceptual_organization, goal_pragmatic_appropriateness,
    goal_collocational_control, fmt_discrete_choice, fmt_sentence_reconstruction,
    fmt_matching, fmt_prompted_writing, fmt_metalinguistic_reflection, fmt_cloze,
    fmt_accent_marking_and_read_aloud, fmt_text_editing, fmt_paradigmatic_systematization,
    fmt_vocabulary_extraction_and_list_creation, fmt_translation, fmt_syntagmatic_systematization,
    n_goals, n_formats, gram_cat_clean, gram_primary, gram_Preposition, gram_Adjective, gram_Adverb,
    gram_Noun, `gram_Conjunctive phrase`, gram_Verb, gram_Pronoun, gram_Determiner,
    n_gramcats, il_22, il_need_motivation, il_noticing, il_search, il_retrieval,
    il_evaluation_generation, il_retention, Soph_PC1_ex_z, Soph_PC1_imm_z,
    Soph_PC1_del_z, LexDiv_PC1_ex_z, LexDiv_PC1_imm_z, LexDiv_PC1_del_z,
    POS_PC1_ex_z, POS_PC1_imm_z, POS_PC1_del_z, POS_PC2_ex_z, POS_PC2_imm_z,
    POS_PC2_del_z, Tree_PC1_ex_z, Tree_PC1_imm_z, Tree_PC1_del_z, Total_z,
    Total_imm_z, Total_del_z
  ) %>%
  mutate(
    prof_level = relevel(factor(prof_level, ordered = FALSE), ref = "Heritage"),
    gram_primary = as.factor(gram_primary)
  )

ti_il_raw <- c("il_retrieval", "il_evaluation_generation", "il_noticing", "il_retention", "il_search")
le_il_raw <- "il_need_motivation"

df_clean <- df_clean %>%
  mutate(
    ti_il = rowSums(pick(all_of(ti_il_raw)), na.rm = FALSE),
    le_il = rowSums(pick(all_of(le_il_raw)), na.rm = FALSE),
    ti_il_z = as.numeric(scale(ti_il)),
    le_il_z = as.numeric(scale(le_il))
  )

make_long <- function(data, target = c("recognition", "correction")) {
  target <- match.arg(target)
  wide <- paste0(target, c("_imm", "_del"))

  data %>%
    transmute(
      id, participant_ID, prof_level, error_type, complete, exercise_score, ti_il_z,
      Soph_ex = Soph_PC1_ex_z, LexDiv_ex = LexDiv_PC1_ex_z, POS1_ex = POS_PC1_ex_z,
      POS2_ex = POS_PC2_ex_z, Tree_ex = Tree_PC1_ex_z, Total_ex = Total_z,
      Soph_imm = Soph_PC1_imm_z, LexDiv_imm = LexDiv_PC1_imm_z,
      POS1_imm = POS_PC1_imm_z, POS2_imm = POS_PC2_imm_z,
      Tree_imm = Tree_PC1_imm_z, Total_imm = Total_imm_z,
      Soph_del = Soph_PC1_del_z, LexDiv_del = LexDiv_PC1_del_z,
      POS1_del = POS_PC1_del_z, POS2_del = POS_PC2_del_z,
      Tree_del = Tree_PC1_del_z, Total_del = Total_del_z, le_il_z,
      !!sym(wide[1]), !!sym(wide[2])
    ) %>%
    pivot_longer(all_of(wide), names_to = "time", values_to = target) %>%
    mutate(
      time = factor(if_else(grepl("imm", time), "imm", "del"), levels = c("imm", "del")),
      Soph_post = if_else(time == "imm", Soph_imm, Soph_del),
      LexDiv_post = if_else(time == "imm", LexDiv_imm, LexDiv_del),
      POS1_post = if_else(time == "imm", POS1_imm, POS1_del),
      POS2_post = if_else(time == "imm", POS2_imm, POS2_del),
      Tree_post = if_else(time == "imm", Tree_imm, Tree_del),
      Total_post = if_else(time == "imm", Total_imm, Total_del)
    ) %>%
    mutate(
      across(
        c(Soph_ex, LexDiv_ex, POS1_ex, POS2_ex, Tree_ex, Total_ex,
          Soph_post, LexDiv_post, POS1_post, POS2_post, Tree_post, Total_post,
          exercise_score),
        ~ as.numeric(scale(.x)),
        .names = "{.col}_sc"
      )
    )
}

df_cor <- make_long(df_clean, "correction") %>%
  group_by(participant_ID) %>%
  mutate(
    dose_BP_sc = mean(exercise_score_sc, na.rm = TRUE),
    dose_WP_sc = exercise_score_sc - dose_BP_sc
  ) %>%
  ungroup()

m_cor <- glmer(
  correction ~
    time * ti_il_z +
    time * prof_level +
    time * dose_WP_sc + dose_BP_sc +
    error_type + (1 + time | participant_ID) + (1 | id),
  data = df_cor, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

df_brm_cor <- df_clean

to_pm05 <- function(x) if (is.numeric(x)) x - 0.5 else x
goal_vars <- grep("^goal_", names(df_clean), value = TRUE)
fmt_vars <- grep("^fmt_", names(df_clean), value = TRUE)
vars <- union(goal_vars, fmt_vars)

df_brm_cor <- df_brm_cor |>
  mutate(across(all_of(vars), ~ as.numeric(.), .names = "{.col}")) |>
  mutate(across(all_of(vars), to_pm05))

df_brm_cor <- df_brm_cor %>%
  left_join(
    df_cor %>% dplyr::select(
      id, time, dose_BP_sc, dose_WP_sc, correction, Soph_post_sc,
      LexDiv_post_sc, POS1_post_sc, POS2_post_sc, Tree_post_sc, Total_post_sc,
      Soph_ex_sc, LexDiv_ex_sc, POS1_ex_sc, POS2_ex_sc, Tree_ex_sc, Total_ex_sc
    ),
    by = "id"
  ) %>%
  distinct()

f2 <- bf(
  correction ~
    time +
    ti_il_z +
    prof_level +
    Soph_post_sc +
    LexDiv_post_sc +
    POS1_post_sc +
    POS2_post_sc +
    Tree_post_sc +
    Total_post_sc +
    goal_productive_use_of_targets +
    goal_meaning_and_sense_elaboration +
    goal_contextual_retrieval +
    goal_paraphrastic_flexibility +
    goal_conceptual_organization +
    goal_pragmatic_appropriateness +
    goal_collocational_control +
    fmt_discrete_choice +
    fmt_sentence_reconstruction +
    fmt_matching +
    fmt_prompted_writing +
    fmt_metalinguistic_reflection +
    fmt_cloze +
    fmt_accent_marking_and_read_aloud +
    fmt_text_editing +
    fmt_paradigmatic_systematization +
    fmt_vocabulary_extraction_and_list_creation +
    fmt_translation +
    fmt_syntagmatic_systematization +
    Soph_ex_sc + LexDiv_ex_sc + POS1_ex_sc + POS2_ex_sc + Tree_ex_sc + Total_ex_sc +
    (1 | participant_ID) +
    (1 | id) +
    (1 + time +
       fmt_sentence_reconstruction + fmt_matching +
       fmt_text_editing + fmt_translation +
       fmt_prompted_writing + fmt_metalinguistic_reflection +
       goal_paraphrastic_flexibility +
       goal_meaning_and_sense_elaboration +
       goal_conceptual_organization || error_type),
  family = bernoulli(link = "logit"),
  decomp = "QR"
)

add_if_present <- function(gp, class, prior_str, coef = NULL, group = NULL) {
  m <- gp$class == class
  if (!is.null(coef)) m <- m & (gp$coef %in% coef)
  if (!is.null(group)) m <- m & (gp$group %in% group)
  hits <- gp[m, , drop = FALSE]
  if (!nrow(hits)) return(NULL)
  Reduce(c, lapply(seq_len(nrow(hits)), function(i) {
    set_prior(
      prior_str,
      class = hits$class[i],
      coef = if (nzchar(hits$coef[i])) hits$coef[i],
      group = if (nzchar(hits$group[i])) hits$group[i]
    )
  }))
}

gp2 <- get_prior(f2, data = df_brm_cor)

fe <- fixef(m_cor)
se <- sqrt(diag(vcov(m_cor)))
vc <- VarCorr(m_cor)

inflate <- 2
b_names <- subset(gp2, class == "b" & nzchar(coef))$coef
shared <- intersect(setdiff(names(fe), "(Intercept)"), b_names)

pri_shared_list <- lapply(shared, function(nm) {
  mu <- unname(fe[[nm]])
  s <- unname(se[[nm]]) * inflate
  set_prior(sprintf("normal(%0.6f,%0.6f)", mu, s), class = "b", coef = nm)
})
pri_shared <- if (length(pri_shared_list)) do.call(c, pri_shared_list) else NULL

pri_int <- set_prior(sprintf("student_t(3,%0.6f,2)", unname(fe[["(Intercept)"]])), class = "Intercept")

sd_id_int <- tryCatch(attr(vc$id, "stddev")[["(Intercept)"]], error = function(e) NA_real_)
sd_p_int <- tryCatch(attr(vc$participant_ID, "stddev")[["(Intercept)"]], error = function(e) NA_real_)

if (is.finite(sd_id_int)) {
  pri_sd_id <- set_prior(sprintf("exponential(%0.6f)", 1 / sd_id_int), class = "sd", group = "id", coef = "Intercept")
} else {
  pri_sd_id <- set_prior("exponential(1.5)", class = "sd", group = "id", coef = "Intercept")
}

if (is.finite(sd_p_int)) {
  pri_sd_pid <- set_prior(sprintf("exponential(%0.6f)", 1 / sd_p_int), class = "sd", group = "participant_ID", coef = "Intercept")
} else {
  pri_sd_pid <- set_prior("exponential(1.5)", class = "sd", group = "participant_ID", coef = "Intercept")
}

pri_sd_err <- set_prior("exponential(2.0)", class = "sd", group = "error_type", coef = "Intercept")
pri_global_b <- prior(normal(0, 1), class = "b")

error_type_slope_coefs <- c(
  "time1", "fmt_sentence_reconstruction", "fmt_matching", "fmt_text_editing",
  "fmt_translation", "fmt_metalinguistic_reflection", "fmt_prompted_writing",
  "goal_paraphrastic_flexibility", "goal_meaning_and_sense_elaboration",
  "goal_conceptual_organization"
)

pri_err_slopes_informed <- add_if_present(
  gp2, class = "sd", prior_str = "exponential(3)",
  coef = error_type_slope_coefs, group = "error_type"
)

pri2_informed <- do.call(c, Filter(Negate(is.null), list(
  pri_int, pri_shared, pri_sd_id, pri_sd_pid, pri_sd_err, pri_global_b, pri_err_slopes_informed
)))

make_generic_priors <- function(gp, err_slope_coefs) {
  pri_int_generic <- set_prior("student_t(3, 0, 2.5)", class = "Intercept")
  pri_b_generic <- prior(normal(0, 1), class = "b")
  pri_sd_generic <- prior(exponential(1), class = "sd")
  pri_err_slopes_generic <- add_if_present(
    gp, class = "sd", prior_str = "exponential(1)",
    coef = err_slope_coefs, group = "error_type"
  )
  do.call(c, Filter(Negate(is.null), list(
    pri_int_generic, pri_b_generic, pri_sd_generic, pri_err_slopes_generic
  )))
}

pri2_generic <- make_generic_priors(gp2, error_type_slope_coefs)

fit_brm_rq2 <- function(formula, data, priors, seed) {
  brm(
    formula, data = data, prior = priors,
    backend = "cmdstanr", seed = seed,
    chains = 4, cores = 4, threads = threading(6),
    iter = 2000, warmup = 1000,
    control = list(metric = "diag_e", adapt_delta = 0.95, max_treedepth = 12),
    refresh = 50,
    save_pars = save_pars(all = TRUE)
  )
}

fit2_informed <- fit_brm_rq2(f2, df_brm_cor, pri2_informed, seed = 2025)
fit2_generic <- fit_brm_rq2(f2, df_brm_cor, pri2_generic, seed = 2026)

compare_brm_fixed_effects <- function(fit_a, fit_b, label_a = "informed", label_b = "generic") {
  draws_a <- as_draws_df(fit_a) |> summarise_draws()
  draws_b <- as_draws_df(fit_b) |> summarise_draws()

  sum_a <- draws_a |>
    filter(grepl("^b_", variable)) |>
    transmute(term = sub("^b_", "", variable), mean_a = mean, q5_a = q5, q95_a = q95)

  sum_b <- draws_b |>
    filter(grepl("^b_", variable)) |>
    transmute(term = sub("^b_", "", variable), mean_b = mean, q5_b = q5, q95_b = q95)

  full_join(sum_a, sum_b, by = "term") |>
    mutate(
      delta_mean = mean_b - mean_a,
      or_a = exp(mean_a),
      or_b = exp(mean_b),
      delta_or = or_b - or_a,
      same_sign = sign(mean_a) == sign(mean_b),
      a_excludes_zero = !(q5_a <= 0 & q95_a >= 0),
      b_excludes_zero = !(q5_b <= 0 & q95_b >= 0),
      changed_support = a_excludes_zero != b_excludes_zero,
      model_a = label_a,
      model_b = label_b
    ) |>
    arrange(desc(abs(delta_mean)))
}

compare_brm_fit_stats <- function(fit_a, fit_b, label_a = "informed", label_b = "generic") {
  loo_a <- loo(fit_a)
  loo_b <- loo(fit_b)
  vc_a <- VarCorr(fit_a)
  vc_b <- VarCorr(fit_b)

  tibble(
    model = c(label_a, label_b),
    looic = c(loo_a$estimates["looic", "Estimate"], loo_b$estimates["looic", "Estimate"]),
    looic_se = c(loo_a$estimates["looic", "SE"], loo_b$estimates["looic", "SE"]),
    p_loo = c(loo_a$estimates["p_loo", "Estimate"], loo_b$estimates["p_loo", "Estimate"]),
    pareto_k_over_0_7 = c(sum(loo_a$diagnostics$pareto_k > 0.7), sum(loo_b$diagnostics$pareto_k > 0.7)),
    participant_intercept_sd = c(vc_a$participant_ID$sd["Intercept", "Estimate"], vc_b$participant_ID$sd["Intercept", "Estimate"]),
    item_intercept_sd = c(vc_a$id$sd["Intercept", "Estimate"], vc_b$id$sd["Intercept", "Estimate"]),
    error_type_intercept_sd = c(vc_a$error_type$sd["Intercept", "Estimate"], vc_b$error_type$sd["Intercept", "Estimate"])
  )
}

rq2_prior_sensitivity_fixed <- compare_brm_fixed_effects(
  fit2_informed, fit2_generic,
  label_a = "glmm_informed", label_b = "generic_weak"
)

rq2_prior_sensitivity_fit <- compare_brm_fit_stats(
  fit2_informed, fit2_generic,
  label_a = "glmm_informed", label_b = "generic_weak"
)

saveRDS(fit2_informed, "rq2_fit_informed.rds")
saveRDS(fit2_generic, "rq2_fit_generic.rds")
write_csv(rq2_prior_sensitivity_fixed, "rq2_prior_sensitivity_fixed.csv")
write_csv(rq2_prior_sensitivity_fit, "rq2_prior_sensitivity_fit.csv")

print(rq2_prior_sensitivity_fit)
print(head(rq2_prior_sensitivity_fixed, 20))
