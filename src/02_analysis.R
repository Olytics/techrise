# TechRise Scholar Program
# M&E Analysis Script
# Research Questions:
# RQ1. How are scholars experiencing the program across
#      the four Theory of Change pillars?
# RQ2. What factors predict program completion and employment?
# RQ3. Is the program improving across cohorts?
# RQ4. Are refugee scholars experiencing the program
#      differently from their peers?

library(dplyr)
library(ggplot2)
library(tidyr)

# LOAD DATA 
df <- read.csv("techrise_scholar_survey_data.csv",
               stringsAsFactors = FALSE)

df$Q2_cohort   <- factor(df$Q2_cohort,
                          levels = 1:3,
                          labels = c("Cohort 1", "Cohort 2", "Cohort 3"))

df$employed <- ifelse(
  df$Q17_employment_status %in%
    c("Employed full-time", "Employed part-time",
      "Self-employed / Freelancing"), 1, 0)

df_refugee <- df %>%
  filter(Q6_refugee_status %in% c("Yes", "No")) %>%
  mutate(is_refugee = ifelse(Q6_refugee_status == "Yes", 1, 0))

# Shared plot theme
techrise_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14,
                                  colour = "#1F4E79", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, colour = "#595959",
                                  margin = margin(b = 12)),
    axis.title    = element_text(size = 11, colour = "#595959"),
    axis.text     = element_text(size = 10, colour = "#595959"),
    legend.title  = element_text(size = 10, face = "bold"),
    legend.text   = element_text(size = 10),
    panel.grid.minor = element_blank(),
    strip.text    = element_text(face = "bold", colour = "#1F4E79")
  )

pillar_colours <- c(
  "Access & Thrive"    = "#2E75B6",
  "Quality Learning"   = "#1F4E79",
  "Dignified Work"     = "#70AD47",
  "Leadership & Give Back" = "#ED7D31"
)

cohort_colours <- c(
  "Cohort 1" = "#BDD7EE",
  "Cohort 2" = "#2E75B6",
  "Cohort 3" = "#1F4E79"
)

cat("============================================\n")
cat("  TECHRISE SCHOLAR PROGRAM — M&E ANALYSIS\n")
cat("============================================\n\n")


# SURVEY RELIABILITY CRONBACH'S ALPHA
# RQ1: HOW ARE SCHOLARS EXPERIENCING THE PROGRAM?

cat("── RQ1: PILLAR SCORES — DESCRIPTIVE STATISTICS ──\n\n")

# 1a. Overall pillar score summary
pillar_summary <- df %>%
  summarise(
    `Access & Thrive`        = round(mean(access_thrive_score), 2),
    `Quality Learning`       = round(mean(learning_score), 2),
    `Dignified Work`         = round(mean(work_score), 2),
    `Leadership & Give Back` = round(mean(leadership_score), 2)
  ) %>%
  pivot_longer(everything(),
               names_to  = "Pillar",
               values_to = "Mean_Score")

cat("Overall average pillar scores (out of 5):\n")
print(pillar_summary)

# 1b. Individual item means 
cat("\nItem-level means — Access & Thrive:\n")
df %>%
  summarise(
    Device_Internet = round(mean(Q9_device_internet_access), 2),
    Financial       = round(mean(Q10_financial_stability), 2),
    Safety_Belonging = round(mean(Q11_safety_belonging), 2)
  ) %>% print()

cat("\nItem-level means — Quality Learning:\n")
df %>%
  summarise(
    Content_Relevance    = round(mean(Q12_content_relevance), 2),
    Practical_Skills     = round(mean(Q13_practical_skills), 2),
    Training_Satisfaction = round(mean(Q14_training_satisfaction), 2)
  ) %>% print()

cat("\nItem-level means — Dignified Work:\n")
df %>%
  summarise(
    Employment_Confidence = round(mean(Q15_employment_confidence), 2),
    Career_Plan_Clarity   = round(mean(Q16_career_plan_clarity), 2)
  ) %>% print()

cat("\nItem-level means — Leadership & Give Back:\n")
df %>%
  summarise(
    Leadership_Readiness = round(mean(Q19_leadership_readiness), 2),
    GiveBack_Commitment  = round(mean(Q20_giveback_commitment), 2)
  ) %>% print()

cat("\nGive Back participation (%):\n")
print(round(prop.table(table(df$Q21_giveback_active)) * 100, 1))

cat("\nMentorship intention (%):\n")
print(round(prop.table(table(df$Q22_mentorship_intention)) * 100, 1))

cat("\nEmployment status distribution (%):\n")
print(round(prop.table(table(df$Q17_employment_status)) * 100, 1))

cat("\nOverall satisfaction distribution (%):\n")
print(round(prop.table(table(df$Q23_overall_satisfaction)) * 100, 1))

cat(sprintf("\nAverage NPS score: %.1f / 10\n\n",
            mean(df$Q24_nps_score)))


# PLOT 1: Overall pillar scores (horizontal bar) 
p1 <- pillar_summary %>%
  mutate(Pillar = factor(Pillar,
                          levels = c("Access & Thrive", "Quality Learning",
                                     "Dignified Work", "Leadership & Give Back"))) %>%
  ggplot(aes(x = Mean_Score, y = reorder(Pillar, Mean_Score),
             fill = Pillar)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", Mean_Score)),
            hjust = -0.15, size = 4, colour = "#1F4E79", fontface = "bold") +
  scale_fill_manual(values = pillar_colours) +
  scale_x_continuous(limits = c(0, 5.5), breaks = seq(0, 5, 1)) +
  geom_vline(xintercept = 3, linetype = "dashed",
             colour = "#595959", linewidth = 0.4) +
  labs(
    title    = "Average Scholar Experience Scores by Theory of Change Pillar",
    subtitle = "All three cohorts combined | Scale: 1 (Strongly Disagree) – 5 (Strongly Agree)",
    x        = "Average Score (out of 5)",
    y        = NULL
  ) +
  techrise_theme

ggsave("plot1_overall_pillar_scores.png", p1,
       width = 9, height = 5, dpi = 150)
cat("✓ Saved: plot1_overall_pillar_scores.png\n\n")


# RQ2: WHAT PREDICTS PROGRAM COMPLETION AND EMPLOYMENT?

cat("── RQ2: LOGISTIC REGRESSION ──\n\n")

# 2a. Predictors of program completion 
cat("Model 1: Predictors of Program Completion\n")
cat("------------------------------------------\n")

model_completion <- glm(
  program_completion ~
    access_thrive_score +
    learning_score +
    work_score +
    leadership_score +
    Q2_cohort,
  data   = df_refugee,
  family = binomial(link = "logit")
)

completion_summary <- summary(model_completion)
print(completion_summary$coefficients)

# Odds ratios
cat("\nOdds Ratios (Program Completion):\n")
print(round(exp(coef(model_completion)), 3))

# Model fit
cat(sprintf("\nAIC: %.1f\n", AIC(model_completion)))
cat(sprintf("Null deviance: %.1f | Residual deviance: %.1f\n\n",
            model_completion$null.deviance,
            model_completion$deviance))


# 2b. Predictors of employment 
cat("Model 2: Predictors of Employment\n")
cat("-----------------------------------\n")

model_employment <- glm(
  employed ~
    access_thrive_score +
    learning_score +
    work_score +
    leadership_score +
    Q2_cohort,
  data   = df_refugee,
  family = binomial(link = "logit")
)

employment_summary <- summary(model_employment)
print(employment_summary$coefficients)

cat("\nOdds Ratios (Employment):\n")
print(round(exp(coef(model_employment)), 3))
cat(sprintf("\nAIC: %.1f\n\n", AIC(model_employment)))


# PLOT 2: Coefficient plot - employment model 
coef_df <- data.frame(
  Term      = names(coef(model_employment))[-1],
  Estimate  = coef(model_employment)[-1],
  SE        = summary(model_employment)$coefficients[-1, 2]
) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE,
    Significant = ifelse(Lower > 0 | Upper < 0, "Significant", "Not significant"),
    Term  = recode(Term,
      "access_thrive_score"  = "Access & Thrive",
      "learning_score"       = "Quality Learning",
      "work_score"           = "Dignified Work",
      "leadership_score"     = "Leadership & Give Back",
      "Q2_cohortCohort 2"    = "Cohort 2 vs Cohort 1",
      "Q2_cohortCohort 3"    = "Cohort 3 vs Cohort 1"
    )
  )

p2 <- ggplot(coef_df,
             aes(x = Estimate,
                 y = reorder(Term, Estimate),
                 colour = Significant)) +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = 0.25, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "#595959", linewidth = 0.5) +
  scale_colour_manual(values = c("Significant"     = "#1F4E79",
                                  "Not significant" = "#BFBFBF")) +
  labs(
    title    = "Predictors of Scholar Employment After Program Completion",
    subtitle = "Logistic regression coefficients with 95% confidence intervals",
    x        = "Log Odds Coefficient",
    y        = NULL,
    colour   = NULL
  ) +
  techrise_theme

ggsave("plot2_employment_predictors.png", p2,
       width = 9, height = 5, dpi = 150)
cat("✓ Saved: plot2_employment_predictors.png\n\n")


# RQ3: IS THE PROGRAM IMPROVING ACROSS COHORTS?
cat("── RQ3: ANOVA — COHORT COMPARISON ──\n\n")

pillars      <- c("access_thrive_score", "learning_score",
                   "work_score", "leadership_score")
pillar_names <- c("Access & Thrive", "Quality Learning",
                   "Dignified Work", "Leadership & Give Back")

for (i in seq_along(pillars)) {
  cat(sprintf("--- %s ---\n", pillar_names[i]))

  # One-way ANOVA
  formula_str <- as.formula(paste(pillars[i], "~ Q2_cohort"))
  aov_result  <- aov(formula_str, data = df)
  cat("ANOVA result:\n")
  print(summary(aov_result))

  # Tukey post-hoc
  tukey <- TukeyHSD(aov_result)
  cat("Tukey HSD post-hoc test:\n")
  print(tukey)
  cat("\n")
}

# Cohort means for all pillars
cat("Average pillar scores by cohort:\n")
df %>%
  group_by(Q2_cohort) %>%
  summarise(
    `Access & Thrive`        = round(mean(access_thrive_score), 2),
    `Quality Learning`       = round(mean(learning_score), 2),
    `Dignified Work`         = round(mean(work_score), 2),
    `Leadership & Give Back` = round(mean(leadership_score), 2),
    .groups = "drop"
  ) %>% print()

cat("\nGive Back participation by cohort (%):\n")
print(round(tapply(df$Q21_giveback_active == "Yes",
                   df$Q2_cohort, mean) * 100, 1))

cat("\nProgram completion by cohort (%):\n")
print(round(tapply(df$program_completion,
                   df$Q2_cohort, mean) * 100, 1))


# PLOT 3: Grouped bar chart — pillar scores by cohort 
cohort_pillar <- df %>%
  group_by(Q2_cohort) %>%
  summarise(
    `Access & Thrive`        = mean(access_thrive_score),
    `Quality Learning`       = mean(learning_score),
    `Dignified Work`         = mean(work_score),
    `Leadership & Give Back` = mean(leadership_score),
    .groups = "drop"
  ) %>%
  pivot_longer(-Q2_cohort,
               names_to  = "Pillar",
               values_to = "Score") %>%
  mutate(Pillar = factor(Pillar,
                          levels = c("Access & Thrive", "Quality Learning",
                                     "Dignified Work", "Leadership & Give Back")))

p3 <- ggplot(cohort_pillar,
             aes(x = Pillar, y = Score, fill = Q2_cohort)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", Score)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.2, colour = "#1F4E79") +
  scale_fill_manual(values = cohort_colours, name = "Cohort") +
  scale_y_continuous(limits = c(0, 5.3), breaks = seq(0, 5, 1)) +
  geom_hline(yintercept = 3, linetype = "dashed",
             colour = "#595959", linewidth = 0.4) +
  labs(
    title    = "Scholar Experience Scores Across Three Cohorts",
    subtitle = "Scores improve consistently from Cohort 1 to Cohort 3 across all pillars",
    x        = NULL,
    y        = "Average Score (out of 5)"
  ) +
  techrise_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("plot3_cohort_comparison.png", p3,
       width = 10, height = 5.5, dpi = 150)
cat("\n✓ Saved: plot3_cohort_comparison.png\n\n")


# PLOT 4: Give Back and completion trend by cohort 
trend_df <- df %>%
  group_by(Q2_cohort) %>%
  summarise(
    `Give Back Active (%)`     = round(mean(Q21_giveback_active == "Yes") * 100, 1),
    `Program Completion (%)`   = round(mean(program_completion) * 100, 1),
    `Employment Rate (%)`      = round(mean(employed) * 100, 1),
    .groups = "drop"
  ) %>%
  pivot_longer(-Q2_cohort,
               names_to  = "Metric",
               values_to = "Percentage")

p4 <- ggplot(trend_df,
             aes(x = Q2_cohort, y = Percentage,
                 colour = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(Percentage, "%")),
            vjust = -0.8, size = 3.5, fontface = "bold") +
  scale_colour_manual(values = c(
    "Give Back Active (%)"   = "#ED7D31",
    "Program Completion (%)" = "#1F4E79",
    "Employment Rate (%)"    = "#70AD47"
  )) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Key Outcome Metrics Across Three Cohorts",
    subtitle = "Program completion, employment, and Give Back participation rates",
    x        = "Cohort",
    y        = "Percentage (%)",
    colour   = NULL
  ) +
  techrise_theme

ggsave("plot4_outcome_trends.png", p4,
       width = 9, height = 5, dpi = 150)
cat("✓ Saved: plot4_outcome_trends.png\n\n")


# RQ4: ARE REFUGEE SCHOLARS EXPERIENCING THE PROGRAM
#       DIFFERENTLY FROM THEIR PEERS?

cat("── RQ4: REFUGEE vs NON-REFUGEE COMPARISON ──\n\n")

# 4a. Independent samples t-tests 
cat("Independent Samples T-Tests — Refugee vs Non-Refugee:\n")
cat("-------------------------------------------------------\n\n")

for (i in seq_along(pillars)) {
  t_result <- t.test(
    as.formula(paste(pillars[i], "~ Q6_refugee_status")),
    data = df_refugee
  )
  cat(sprintf("%s:\n", pillar_names[i]))
  cat(sprintf("  Non-refugee mean: %.2f | Refugee mean: %.2f\n",
              mean(df_refugee[df_refugee$Q6_refugee_status == "No",
                               pillars[i]]),
              mean(df_refugee[df_refugee$Q6_refugee_status == "Yes",
                               pillars[i]])))
  cat(sprintf("  t(%.0f) = %.3f, p = %.4f %s\n\n",
              t_result$parameter,
              t_result$statistic,
              t_result$p.value,
              ifelse(t_result$p.value < 0.05, "★ Significant", "")))
}

# 4b. Employment and Give Back cross-tabulation 
cat("Employment status by refugee status (%):\n")
emp_cross <- df_refugee %>%
  group_by(Q6_refugee_status, Q17_employment_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Q6_refugee_status) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(emp_cross)

cat("\nGive Back active by refugee status (%):\n")
giveback_cross <- df_refugee %>%
  group_by(Q6_refugee_status, Q21_giveback_active) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Q6_refugee_status) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(giveback_cross)


# PLOT 5: Refugee vs non-refugee pillar score comparison
refugee_pillar <- df_refugee %>%
  group_by(Q6_refugee_status) %>%
  summarise(
    `Access & Thrive`        = mean(access_thrive_score),
    `Quality Learning`       = mean(learning_score),
    `Dignified Work`         = mean(work_score),
    `Leadership & Give Back` = mean(leadership_score),
    .groups = "drop"
  ) %>%
  pivot_longer(-Q6_refugee_status,
               names_to  = "Pillar",
               values_to = "Score") %>%
  mutate(
    Pillar = factor(Pillar,
                     levels = c("Access & Thrive", "Quality Learning",
                                "Dignified Work", "Leadership & Give Back")),
    Q6_refugee_status = factor(Q6_refugee_status,
                                levels = c("No", "Yes"),
                                labels = c("Non-refugee", "Refugee / Displaced"))
  )

p5 <- ggplot(refugee_pillar,
             aes(x = Pillar, y = Score, fill = Q6_refugee_status)) +
  geom_col(position = position_dodge(width = 0.70), width = 0.60) +
  geom_text(aes(label = sprintf("%.2f", Score)),
            position = position_dodge(width = 0.70),
            vjust = -0.4, size = 3.2, colour = "#1F4E79") +
  scale_fill_manual(
    values = c("Non-refugee"           = "#2E75B6",
               "Refugee / Displaced"   = "#ED7D31"),
    name = "Scholar Status"
  ) +
  scale_y_continuous(limits = c(0, 5.3), breaks = seq(0, 5, 1)) +
  geom_hline(yintercept = 3, linetype = "dashed",
             colour = "#595959", linewidth = 0.4) +
  labs(
    title    = "Program Experience Scores — Refugee vs Non-Refugee Scholars",
    subtitle = "Refugee scholars score lower on Access & Thrive despite similar outcomes on other pillars",
    x        = NULL,
    y        = "Average Score (out of 5)"
  ) +
  techrise_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("plot5_refugee_comparison.png", p5,
       width = 10, height = 5.5, dpi = 150)
cat("\n✓ Saved: plot5_refugee_comparison.png\n\n")


# EXPORT SUMMARY TABLE FOR TABLEAU / REPORTING

summary_export <- df %>%
  group_by(Q2_cohort) %>%
  summarise(
    n_scholars              = n(),
    access_thrive_avg       = round(mean(access_thrive_score), 2),
    learning_avg            = round(mean(learning_score), 2),
    work_avg                = round(mean(work_score), 2),
    leadership_avg          = round(mean(leadership_score), 2),
    overall_satisfaction    = round(mean(Q23_overall_satisfaction), 2),
    avg_nps                 = round(mean(Q24_nps_score), 1),
    completion_rate_pct     = round(mean(program_completion) * 100, 1),
    employment_rate_pct     = round(mean(employed) * 100, 1),
    giveback_active_pct     = round(mean(Q21_giveback_active == "Yes") * 100, 1),
    .groups = "drop"
  )

write.csv(summary_export,
          "techrise_cohort_summary.csv",
          row.names = FALSE)

cat("✓ Saved: techrise_cohort_summary.csv\n")
cat("  (Use this file in Tableau for KPI dashboard)\n\n")

cat("============================================\n")
cat("  ANALYSIS COMPLETE\n")
cat("  Output files:\n")
cat("  - plot1_overall_pillar_scores.png\n")
cat("  - plot2_employment_predictors.png\n")
cat("  - plot3_cohort_comparison.png\n")
cat("  - plot4_outcome_trends.png\n")
cat("  - plot5_refugee_comparison.png\n")
cat("  - techrise_cohort_summary.csv\n")
cat("============================================\n")
