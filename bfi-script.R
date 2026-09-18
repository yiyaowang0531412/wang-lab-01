library(tidyverse)
library(psych)
library(broom)
library(gt)
library(performance)

# Load the example BFI data included with the psych package
data("bfi", package = "psych")

# Standard scoring keys supplied for the BFI data
bfi_keys <- list(
  Agreeableness = c("-A1", "A2", "A3", "A4", "A5"),
  Conscientiousness = c("C1", "C2", "C3", "-C4", "-C5"),
  Extraversion = c("-E1", "-E2", "E3", "E4", "E5"),
  Neuroticism = c("N1", "N2", "N3", "N4", "N5"),
  Openness = c("O1", "-O2", "O3", "O4", "-O5")
)

# Score the five BFI scales.
# scoreItems() also provides scale reliability estimates.
bfi_scoring <- psych::scoreItems(
  keys = bfi_keys,
  items = bfi,
  min = 1,
  max = 6,
  impute = "none"
)

# Convert the generated scale scores to a tibble and retain demographics.
bfi_analysis <- bfi_scoring$scores |>
  as_tibble() |>
  bind_cols(
    bfi |>
      select(gender, education, age)
  ) |>
  mutate(
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

scale_names <- names(bfi_keys)

# psych::describe() returns a large set of descriptive statistics.
# Keep only the quantities useful for this report.
descriptive_table <- bfi_analysis |>
  select(all_of(scale_names)) |>
  psych::describe() |>
  as.data.frame() |>
  rownames_to_column("Scale") |>
  as_tibble() |>
  select(
    Scale,
    n,
    mean,
    sd,
    median,
    min,
    max,
    skew
  )

# scoreItems() provides coefficient alpha for each scored scale.
reliability_table <- tibble(
  Scale = names(bfi_keys),
  Alpha = as.numeric(bfi_scoring$alpha)
)

scale_summary <- descriptive_table |>
  left_join(reliability_table, by = "Scale")

scale_summary |>
  gt() |>
  tab_header(
    title = "Descriptive Statistics and Reliability",
    subtitle = "Big Five personality scale scores"
  ) |>
  cols_label(
    Scale = "Scale",
    n = "N",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    min = "Minimum",
    max = "Maximum",
    skew = "Skewness",
    Alpha = "α"
  ) |>
  fmt_number(
    columns = c(mean, sd, median, min, max, skew, Alpha),
    decimals = 2
  ) |>
  opt_row_striping()

model_data <- bfi_analysis |>
  select(
    Neuroticism,
    age,
    gender,
    Extraversion,
    Conscientiousness
  ) |>
  drop_na()

model_1 <- lm(
  Extraversion ~ age + gender,
  data = model_data
)

model_2 <- lm(
  Extraversion ~ age + gender + Neuroticism + Conscientiousness,
  data = model_data
)

model_comparison <- bind_rows(
  glance(model_1) |> mutate(Model = "Model 1: Age + gender"),
  glance(model_2) |> mutate(
    Model = "Model 2: Age + gender + extraversion + neuroticism"
  )
) |>
  select(Model, r.squared, adj.r.squared, AIC, BIC)

model_comparison |>
  gt() |>
  tab_header(
    title = "Comparison of Candidate Regression Models"
  ) |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    AIC = "AIC",
    BIC = "BIC"
  ) |>
  fmt_number(
    columns = c(r.squared, adj.r.squared),
    decimals = 3
  ) |>
  fmt_number(
    columns = c(AIC, BIC),
    decimals = 1
  ) |>
  opt_row_striping()

model_test <- anova(model_1, model_2) |>
  tidy()

model_test |>
  gt() |>
  tab_header(
    title = "Nested Model Comparison"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  )

diagnostic_plots <- performance::check_model(
  model_2,
  check = c(
    "qq",
    "linearity",
    "homogeneity",
    "outliers",
    "vif"
  ),
  panel = FALSE,
  base_size = 13
)

plot(diagnostic_plots)

final_model_table <- tidy(
  model_2,
  conf.int = TRUE,
  conf.level = .95
) |>
  mutate(
    term = recode(
      term,
      `(Intercept)` = "Intercept",
      age = "Age",
      genderFemale = "Gender: Female",
      Neuroticism = "Neuroticism",
      Conscientiousness = "Conscientiousness"
    ),
    p = case_when(
      p.value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  )

final_model_table |>
  select(
    term,
    estimate,
    std.error,
    statistic,
    conf.low,
    conf.high,
    p
  ) |>
  gt() |>
  tab_header(
    title = "Linear Regression Predicting Extraversion",
    subtitle = paste0(
      "N = ", nobs(model_2),
      "; R² = ", round(glance(model_2)$r.squared, 3)
    )
  ) |>
  cols_label(
    term = "Predictor",
    estimate = "b",
    std.error = "SE",
    statistic = "t",
    conf.low = "95% CI Low",
    conf.high = "95% CI High",
    p = "p"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      statistic,
      conf.low,
      conf.high
    ),
    decimals = 2
  ) |>
  opt_row_striping()