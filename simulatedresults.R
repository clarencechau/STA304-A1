# ---- STEMBuddies Survey Data Simulation ----

set.seed(123)   # reproducibility
n <- 200        # number of respondents

# -------------------------------
# 1. Demographics
# -------------------------------
Grade <- sample(
  c("Grade 9", "Grade 10", "Grade 11", "Grade 12"),
  size = n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)
)

Gender <- sample(
  c("Female", "Male", "Other", "Prefer not to say"),
  size = n, replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)
)

# -------------------------------
# 2. Underrepresented STEM groups (multi-select; OPTIONAL)
# -------------------------------
underrep_options <- c(
  "Women in STEM",
  "Racial Minority",
  "2SLGBTQ+",
  "Low-income/First-gen",
  "New Immigrant (ESL)",
  "Disabled",
  "Prefer not to say"
)

# Share of respondents who SKIP this question entirely (tweak as needed)
p_skip <- 0.20
skipped_underrep <- rbinom(n, 1, p_skip) == 1

# Simulate raw multi-selects (1/0) for everyone first
underrep_raw <- replicate(length(underrep_options), rbinom(n, 1, 0.2))
colnames(underrep_raw) <- underrep_options
underrep_raw <- as.data.frame(underrep_raw, check.names = FALSE)

# Apply "Prefer not to say" logic ONLY for those who answered
answered_idx <- !skipped_underrep
if ("Prefer not to say" %in% names(underrep_raw)) {
  pns_idx <- answered_idx & (underrep_raw[["Prefer not to say"]] == 1)
  other_cols <- setdiff(names(underrep_raw), "Prefer not to say")
  underrep_raw[pns_idx, other_cols] <- 0L
}

# For respondents who skipped the question, set ALL underrep columns to NA
underrep_raw[skipped_underrep, ] <- NA_integer_

# Add a flag column indicating they didn't answer the underrep question
Underrep_NotAnswered <- as.integer(skipped_underrep)

# -------------------------------
# 3. Needs Assessment (Likert 1–5)
# -------------------------------
likert_gen <- function(n, probs = c(0.05, 0.10, 0.20, 0.35, 0.30)) {
  sample(1:5, size = n, replace = TRUE, prob = probs)
}

needs_assessment <- data.frame(
  Scholarship_info   = likert_gen(n),
  Course_planning    = likert_gen(n),
  Univ_admissions    = likert_gen(n),
  Extracurriculars   = likert_gen(n, probs = c(0.08, 0.15, 0.27, 0.27, 0.23)),
  Mental_health      = likert_gen(n, probs = c(0.07, 0.12, 0.21, 0.27, 0.33))
)

# Convert to ordered factors with labels "1"…"5"
needs_assessment[] <- lapply(needs_assessment, function(x) {
  factor(x, levels = 1:5, ordered = TRUE, labels = c("1","2","3","4","5"))
})

# -------------------------------
# 4. Combine all parts
# -------------------------------
survey_data <- data.frame(
  Grade = Grade,
  Gender = Gender,
  needs_assessment,
  Underrep_NotAnswered = Underrep_NotAnswered,
  underrep_raw,
  check.names = FALSE
)

# -------------------------------
# 5. Preview
# -------------------------------
str(survey_data)
head(survey_data, 10)

saveRDS(survey_data, "survey_data.rds")
