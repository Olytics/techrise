# TechRise Scholar Program
# Scholar Experience & Outcomes Assessment Simulation
# Survey version: 26 questions (endline only)
# Cohorts: 3 | Scholars per cohort: 100 | Total: 300


set.seed(123)
library(dplyr)

n_total      <- 300
n_per_cohort <- 100

# HELPER FUNCTIONS

cohort_boost <- function(cohort, boost = 0.15) (cohort - 1) * boost

refugee_penalty <- function(refugee, penalty = 0.35) {
  ifelse(refugee == "Yes", -penalty, 0)
}
clamp <- function(x) pmin(5, pmax(1, x))


# SECTION 1: DEMOGRAPHICS (Q1–Q8) 

countries <- c("Nigeria", "Ghana", "Kenya", "Ethiopia", "Uganda",
               "Rwanda", "Tanzania", "Senegal", "Cameroon", "Zimbabwe")

tracks <- c("Data Analysis", "Product Design", "Cybersecurity",
            "Front-End Development", "Digital Marketing")

df <- data.frame(

  Q1_scholar_id       = sprintf("SCH-%03d", 1:n_total),

  Q2_cohort           = rep(1:3, each = n_per_cohort),

  Q3_training_track   = sample(tracks, n_total, replace = TRUE,
                               prob = c(0.25, 0.20, 0.20, 0.20, 0.15)),

  Q4_country          = sample(countries, n_total, replace = TRUE,
                               prob = c(0.20, 0.12, 0.12, 0.10, 0.10,
                                        0.08, 0.08, 0.08, 0.07, 0.05)),

  Q5_gender           = sample(c("Male", "Female",
                                  "Non-binary", "Prefer not to say"),
                               n_total, replace = TRUE,
                               prob = c(0.44, 0.52, 0.02, 0.02)),

  Q6_refugee_status   = sample(c("Yes", "No", "Prefer not to say"),
                               n_total, replace = TRUE,
                               prob = c(0.20, 0.75, 0.05)),
  
  Q7_prior_education  = sample(
    c("No formal education", "Primary school", "Secondary school",
      "Vocational / Technical certificate", "Undergraduate degree or higher"),
    n_total, replace = TRUE,
    prob = c(0.05, 0.10, 0.35, 0.25, 0.25)),

  # Q8 — Age range
  Q8_age_range        = sample(c("18-24", "25-30", "31-35", "36 and above"),
                               n_total, replace = TRUE,
                               prob = c(0.40, 0.35, 0.15, 0.10)),

  stringsAsFactors = FALSE
)


# SECTION 2: ACCESS & THRIVE (Q9–Q11) 

at_base <- 3.55 +
  cohort_boost(df$Q2_cohort, 0.18) +
  refugee_penalty(df$Q6_refugee_status, 0.35)

df$Q9_device_internet_access  <- round(clamp(rnorm(n_total, at_base + 0.10, 0.55)), 0)

df$Q10_financial_stability     <- round(clamp(rnorm(n_total, at_base - 0.15, 0.60)), 0)

df$Q11_safety_belonging        <- round(clamp(rnorm(n_total, at_base + 0.20, 0.50)), 0)

df$access_thrive_score <- round(
  rowMeans(df[, c("Q9_device_internet_access",
                   "Q10_financial_stability",
                   "Q11_safety_belonging")]), 2)


# SECTION 3: QUALITY & RELEVANT LEARNING (Q12–Q14)
ql_base <- 3.90 +
  cohort_boost(df$Q2_cohort, 0.20)

df$Q12_content_relevance    <- round(clamp(rnorm(n_total, ql_base + 0.10, 0.50)), 0)

df$Q13_practical_skills     <- round(clamp(rnorm(n_total, ql_base + 0.15, 0.48)), 0)

df$Q14_training_satisfaction <- round(clamp(rnorm(n_total, ql_base + 0.12, 0.50)), 0)

df$learning_score <- round(
  rowMeans(df[, c("Q12_content_relevance",
                   "Q13_practical_skills",
                   "Q14_training_satisfaction")]), 2)


# SECTION 4: DIGNIFIED & FULFILLING WORK (Q15–Q18) 

dw_base <- 3.55 +
  cohort_boost(df$Q2_cohort, 0.22)

df$Q15_employment_confidence <- round(clamp(rnorm(n_total, dw_base + 0.15, 0.58)), 0)

df$Q16_career_plan_clarity   <- round(clamp(rnorm(n_total, dw_base + 0.05, 0.60)), 0)

df$work_score <- round(
  rowMeans(df[, c("Q15_employment_confidence",
                   "Q16_career_plan_clarity")]), 2)

df$Q17_employment_status <- sapply(1:n_total, function(i) {
  cohort_adj <- (df$Q2_cohort[i] - 1) * 0.05
  sample(
    c("Employed full-time", "Employed part-time",
      "Self-employed / Freelancing",
      "Actively seeking employment",
      "Not currently seeking employment"),
    1, prob = c(0.25 + cohort_adj, 0.15, 0.20,
                0.30 - cohort_adj, 0.10))
})

df$Q18_employment_relevance <- sapply(1:n_total, function(i) {
  is_working <- df$Q17_employment_status[i] %in%
    c("Employed full-time", "Employed part-time",
      "Self-employed / Freelancing")
  if (is_working) {
    sample(c("Directly related", "Somewhat related", "Not related"),
           1, prob = c(0.50, 0.35, 0.15))
  } else {
    "Not applicable"
  }
})


# SECTION 5: LEADERSHIP & GIVE BACK (Q19–Q22) 

lb_base <- 3.30 +
  cohort_boost(df$Q2_cohort, 0.25)

df$Q19_leadership_readiness  <- round(clamp(rnorm(n_total, lb_base + 0.10, 0.62)), 0)

df$Q20_giveback_commitment   <- round(clamp(rnorm(n_total, lb_base + 0.15, 0.60)), 0)

df$leadership_score <- round(
  rowMeans(df[, c("Q19_leadership_readiness",
                   "Q20_giveback_commitment")]), 2)

df$Q21_giveback_active <- sapply(1:n_total, function(i) {
  p_yes <- 0.50 + (df$Q2_cohort[i] - 1) * 0.12
  sample(c("Yes", "No"), 1,
         prob = c(min(p_yes, 0.90), max(1 - p_yes, 0.10)))
})

df$Q22_mentorship_intention <- sapply(df$Q2_cohort, function(c) {
  p_yes  <- 0.35 + 0.10 * (c / 3)
  p_no   <- max(0.10, 0.25 - 0.08 * (c / 3))
  p_poss <- 1 - p_yes - p_no
  sample(c("Yes, definitely", "Possibly", "No"),
         1, prob = c(p_yes, p_poss, p_no))
})


# SECTION 6: OVERALL EXPERIENCE (Q23–Q26) 

df$Q23_overall_satisfaction <- round(clamp(
  rowMeans(df[, c("access_thrive_score", "learning_score",
                   "work_score", "leadership_score")]) +
    rnorm(n_total, 0, 0.20)), 0)

df$Q24_nps_score <- round(pmin(10, pmax(0,
  df$Q23_overall_satisfaction * 2 - 0.5 +
    rnorm(n_total, 0, 0.80))))

df$Q25_expectations_met <- sapply(df$Q23_overall_satisfaction, function(s) {
  if (s >= 5)
    sample(c("Exceeded my expectations",
             "Met my expectations"), 1, prob = c(0.60, 0.40))
  else if (s == 4)
    sample(c("Met my expectations",
             "Partially met my expectations"), 1, prob = c(0.65, 0.35))
  else if (s == 3)
    sample(c("Partially met my expectations",
             "Met my expectations"), 1, prob = c(0.65, 0.35))
  else
    sample(c("Partially met my expectations",
             "Did not meet my expectations"), 1, prob = c(0.50, 0.50))
})

improvement_responses <- c(
  "More hands-on projects and real-world case studies.",
  "Better internet access support for scholars in rural areas.",
  "More mentorship sessions with industry professionals.",
  "The program should provide stipends to cover basic living costs.",
  "More career placement support after graduation.",
  "Increase the duration of the program — 4 months feels too short.",
  "Provide more emotional and mental health support resources.",
  "More networking opportunities with alumni and employers.",
  "Better support for refugee scholars navigating documentation issues.",
  "The curriculum should be updated more regularly to reflect industry trends."
)
df$Q26_improvement_suggestion <- sample(improvement_responses,
                                         n_total, replace = TRUE)



completion_prob <- plogis(
  -1.0
  + 0.60 * df$learning_score
  + 0.25 * df$access_thrive_score
  + 0.15 * (df$Q2_cohort - 1)
  + ifelse(df$Q6_refugee_status == "Yes", -0.10, 0)
)
df$program_completion <- rbinom(n_total, 1, completion_prob)


# QUALITY CHECK 

cat("=============================================\n")
cat("  TECHRISE SCHOLAR PROGRAM — DATA CHECK\n")
cat("=============================================\n\n")

cat("Total records:", nrow(df), "\n")
cat("Total columns:", ncol(df), "\n")

cat("\nCohort distribution:\n")
print(table(df$Q2_cohort))

cat("\nTraining track distribution:\n")
print(table(df$Q3_training_track))

cat("\nRefugee status:\n")
print(table(df$Q6_refugee_status))

cat("\nAverage pillar scores by cohort:\n")
df %>%
  group_by(Q2_cohort) %>%
  summarise(
    Access_Thrive = round(mean(access_thrive_score), 2),
    Learning      = round(mean(learning_score), 2),
    Work          = round(mean(work_score), 2),
    Leadership    = round(mean(leadership_score), 2),
    .groups       = "drop"
  ) %>% print()

cat("\nRefugee vs Non-refugee — Access & Thrive scores:\n")
df %>%
  filter(Q6_refugee_status != "Prefer not to say") %>%
  group_by(Q6_refugee_status) %>%
  summarise(
    Avg_Access_Thrive = round(mean(access_thrive_score), 2),
    n = n(),
    .groups = "drop"
  ) %>% print()

cat("\nGive Back active by cohort (%):\n")
print(round(tapply(df$Q21_giveback_active == "Yes",
                   df$Q2_cohort, mean) * 100, 1))

cat("\nMentorship intention by cohort:\n")
print(table(df$Q2_cohort, df$Q22_mentorship_intention))

cat("\nEmployment status distribution:\n")
print(table(df$Q17_employment_status))

cat("\nProgram completion rate by cohort (%):\n")
print(round(tapply(df$program_completion, df$Q2_cohort, mean) * 100, 1))

cat("\nExpectations met distribution:\n")
print(table(df$Q25_expectations_met))

cat("\nMissing values:\n")
print(colSums(is.na(df)))


# EXPORT 

write.csv(df,
          "techrise_scholar_survey_data.csv",
          row.names = FALSE,
          na = "")

cat("\n✓ Dataset saved to: techrise_scholar_survey_data.csv\n")
cat("  Rows:", nrow(df), "\n")
cat("  Columns:", ncol(df), "\n")
cat("\nAll column names:\n")
print(names(df))
