library(dplyr)
library(tidyr)
library(ggplot2)

# =========================
# Demographics frequency table
# =========================
demographics_table <- survey_data %>%
  group_by(Grade, Gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Gender, values_from = n, values_fill = 0)

demographics_table


# Reshape demographics into long format
demo_long <- survey_data %>%
  count(Grade, Gender)

# Grouped bar plot
ggplot(demo_long, aes(x = Grade, y = n, fill = Gender)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Figure 1: Demographic Breakdown by Grade and Gender",
    x = "Grade",
    y = "Number of Respondents",
    fill = "Gender"
  ) +
  theme_minimal()


# =========================
# Likert distributions (stacked)
# =========================
likert_long <- survey_data %>%
  select(Scholarship_info, Course_planning, Univ_admissions, 
         Extracurriculars, Mental_health) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# ensure 1..5 order is respected, even if a level is missing in the sample
likert_long <- likert_long %>%
  mutate(Response = factor(Response, levels = c("1","2","3","4","5"), ordered = TRUE))

ggplot(likert_long, aes(x = Question, fill = Response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(drop = FALSE) +
  labs(x = "Survey Question", y = "Proportion", 
       title = "Figure 2: Distribution of Needs Assessment Responses",
       fill = "Response") +
  theme_minimal()


# =========================
# Underrepresented groups
# =========================

# Helpful counts
n_total   <- nrow(survey_data)
n_skipped <- sum(survey_data$Underrep_NotAnswered == 1, na.rm = TRUE)
denom_ans <- n_total - n_skipped

# ---- Figure 2: OVERALL view (counts skips as 0) + explicit 'Did not answer' bar
underrep_overall <- survey_data %>%
  select(`Women in STEM`:`Prefer not to say`) %>%
  # treat NA (skips) as 0 for each selection column
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) %>%
  summarise(across(everything(), ~ mean(.x) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Percent")

skip_row <- tibble(Group = "Did not answer", Percent = 100 * n_skipped / n_total)

underrep_summary_overall <- bind_rows(underrep_overall, skip_row)

ggplot(underrep_summary_overall, aes(x = reorder(Group, Percent), y = Percent)) +
  geom_col() +
  coord_flip() +
  labs(x = "Group", y = "Percent of ALL respondents",
       title = "Figure 3: Underrepresented Groups — Overall (incl. non-response)") +
  theme_minimal()
