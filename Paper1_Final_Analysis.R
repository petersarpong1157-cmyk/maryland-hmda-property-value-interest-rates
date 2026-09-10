# ============================================================
# Mortgage Interest Rates and Residential Property Values:
# Evidence from Linked Maryland HMDA and SDAT Data
#
# Author: Peter Sarpong
# Year: 2026
#
# FINAL REPRODUCTION SCRIPT
#
# This script contains the final analysis used in the revised
# manuscript. It excludes loan amount and loan-to-value ratio
# from all preferred property-value regression specifications.
# HMDA debt-to-income ratio is modeled categorically as publicly
# reported; interval values are NOT converted to numeric midpoints.
#
# Required input files in the working directory:
#   1. loan_purposes_1_state_MD.csv
#   2. SDAT_2025_Market_Sales_With_Census_Tracts.csv
#
# The second file is the cleaned 2025 Maryland SDAT residential
# transaction file with Census tract identifiers and the property
# fields used below.
# ============================================================

# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(tibble)
library(lmtest)
library(sandwich)
library(ggplot2)

# Start from a clean workspace so stale objects from earlier runs cannot
# make a failed run appear successful.
rm(list = setdiff(ls(), character(0)))


# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

hmda_recovered <- read_csv(
  "loan_purposes_1_state_MD.csv",
  show_col_types = FALSE
)

sdat_recovered <- read_csv(
  "SDAT_2025_Market_Sales_With_Census_Tracts.csv",
  show_col_types = FALSE
)

cat("\nRAW INPUT CHECKS\n")
cat("HMDA rows:", nrow(hmda_recovered), "\n")
cat("SDAT rows:", nrow(sdat_recovered), "\n")


# ------------------------------------------------------------
# 2. BUILD 2025 MARYLAND ORIGINATED-LOAN MATCHING POPULATION
# ------------------------------------------------------------
#
# Keep originated HMDA loans with a valid numeric property value.
# Census tract is retained when available but is not required for
# inclusion in the full HMDA matching population.
# ------------------------------------------------------------

hmda_match_df <- hmda_recovered %>%
  filter(action_taken == 1) %>%
  mutate(
    property_value_num =
      suppressWarnings(as.numeric(property_value)),
    
    loan_amount_num =
      suppressWarnings(as.numeric(loan_amount)),
    
    census_tract_chr =
      if_else(
        is.na(census_tract),
        NA_character_,
        sprintf("%.0f", as.numeric(census_tract))
      ),
    
    county_code_chr =
      if_else(
        is.na(county_code),
        NA_character_,
        sprintf("%.0f", as.numeric(county_code))
      )
  ) %>%
  filter(!is.na(property_value_num)) %>%
  mutate(hmda_id = row_number())

cat("\nHMDA MATCHING POPULATION\n")
cat("Total HMDA matching population:", nrow(hmda_match_df), "\n")
cat("HMDA records missing tract:",
    sum(is.na(hmda_match_df$census_tract_chr)), "\n")


# ------------------------------------------------------------
# 3. PREPARE SDAT MATCHING POPULATION
# ------------------------------------------------------------

sdat_match_df <- sdat_recovered %>%
  mutate(
    sdat_id = as.character(account_id_mdp_field_acctid),
    GEOID = as.character(GEOID),
    sale_price_clean = as.numeric(sale_price_clean)
  )

cat("\nSDAT MATCHING POPULATION\n")
cat("Total SDAT records:", nrow(sdat_match_df), "\n")
cat("Distinct SDAT tracts:", n_distinct(sdat_match_df$GEOID), "\n")


# ------------------------------------------------------------
# 4. EXACT TRACT + EXACT VALUE CANDIDATE LINKS
# ------------------------------------------------------------
#
# High-confidence linkage uses exact Census tract plus exact equality
# between the disclosed HMDA property value and SDAT sale price.
# The final linked sample further requires uniqueness in both
# directions: one SDAT candidate for the HMDA record and one HMDA
# candidate for the SDAT property.
# ------------------------------------------------------------

exact_price_candidates <- hmda_match_df %>%
  filter(!is.na(census_tract_chr)) %>%
  select(
    hmda_id,
    census_tract_chr,
    property_value_num
  ) %>%
  inner_join(
    sdat_match_df %>%
      select(
        sdat_id,
        GEOID,
        sale_price_clean
      ),
    by = c(
      "census_tract_chr" = "GEOID",
      "property_value_num" = "sale_price_clean"
    ),
    relationship = "many-to-many"
  )

exact_hmda_counts <- exact_price_candidates %>%
  count(hmda_id, name = "n_sdat_per_hmda")

exact_sdat_counts <- exact_price_candidates %>%
  count(sdat_id, name = "n_hmda_per_sdat")

exact_bidirectional_pairs <- exact_price_candidates %>%
  left_join(exact_hmda_counts, by = "hmda_id") %>%
  left_join(exact_sdat_counts, by = "sdat_id") %>%
  filter(
    n_sdat_per_hmda == 1,
    n_hmda_per_sdat == 1
  )

cat("\nBIDIRECTIONAL EXACT LINKAGE CHECKS\n")
cat("All exact tract/value candidate pairs:",
    nrow(exact_price_candidates), "\n")
cat("Unique HMDA records with any exact candidate:",
    n_distinct(exact_price_candidates$hmda_id), "\n")
cat("Exact bidirectional pairs:",
    nrow(exact_bidirectional_pairs), "\n")
cat("Unique HMDA IDs:",
    n_distinct(exact_bidirectional_pairs$hmda_id), "\n")
cat("Unique SDAT IDs:",
    n_distinct(exact_bidirectional_pairs$sdat_id), "\n")
cat("Duplicate HMDA IDs:",
    sum(duplicated(exact_bidirectional_pairs$hmda_id)), "\n")
cat("Duplicate SDAT IDs:",
    sum(duplicated(exact_bidirectional_pairs$sdat_id)), "\n")

write_csv(
  exact_bidirectional_pairs,
  "HMDA_SDAT_Exact_Bidirectional_4789.csv"
)

write_csv(
  hmda_match_df,
  "HMDA_2025_Purchase_Originated_Matching_Population_62305.csv"
)


# ------------------------------------------------------------
# 5. LINKAGE-SELECTION DIAGNOSTICS
# ------------------------------------------------------------
#
# Loan amount is used here only to describe selection into the
# linked sample. It is NOT a regressor in any preferred model.
# ------------------------------------------------------------

exact_matched_hmda_ids <- exact_bidirectional_pairs %>%
  distinct(hmda_id)

exact_selection_diag <- hmda_match_df %>%
  mutate(
    exact_linked = hmda_id %in% exact_matched_hmda_ids$hmda_id
  ) %>%
  group_by(exact_linked) %>%
  summarise(
    n = n(),
    mean_property_value = mean(property_value_num, na.rm = TRUE),
    median_property_value = median(property_value_num, na.rm = TRUE),
    mean_loan_amount = mean(loan_amount_num, na.rm = TRUE),
    median_loan_amount = median(loan_amount_num, na.rm = TRUE),
    .groups = "drop"
  )

print(exact_selection_diag, width = Inf)

write_csv(
  exact_selection_diag,
  "HMDA_SDAT_Exact_4789_Selection_Diagnostic.csv"
)


# ------------------------------------------------------------
# 6. PROPERTY-CHARACTERISTIC COVERAGE AND STRUCTURAL COMPARISON
# ------------------------------------------------------------

exact_property_sample <- exact_bidirectional_pairs %>%
  left_join(
    sdat_match_df %>%
      select(
        sdat_id,
        sqft_clean,
        year_built_clean,
        grade_clean,
        dwelling_type,
        dwelling_units
      ),
    by = "sdat_id"
  )

structural_comparison <- bind_rows(
  sdat_match_df %>%
    summarise(
      sample = "Full SDAT",
      n = n(),
      sqft_available = sum(!is.na(sqft_clean)),
      sqft_mean = mean(sqft_clean, na.rm = TRUE),
      sqft_median = median(sqft_clean, na.rm = TRUE),
      year_available = sum(!is.na(year_built_clean)),
      year_mean = mean(year_built_clean, na.rm = TRUE),
      year_median = median(year_built_clean, na.rm = TRUE),
      grade_available = sum(!is.na(grade_clean) & grade_clean != ""),
      dwelling_type_available =
        sum(!is.na(dwelling_type) & dwelling_type != "")
    ),
  
  exact_property_sample %>%
    summarise(
      sample = "Exact linked 4,789",
      n = n(),
      sqft_available = sum(!is.na(sqft_clean)),
      sqft_mean = mean(sqft_clean, na.rm = TRUE),
      sqft_median = median(sqft_clean, na.rm = TRUE),
      year_available = sum(!is.na(year_built_clean)),
      year_mean = mean(year_built_clean, na.rm = TRUE),
      year_median = median(year_built_clean, na.rm = TRUE),
      grade_available = sum(!is.na(grade_clean) & grade_clean != ""),
      dwelling_type_available =
        sum(!is.na(dwelling_type) & dwelling_type != "")
    )
)

structural_comparison_final <- structural_comparison %>%
  mutate(
    sqft_coverage_pct = 100 * sqft_available / n,
    year_coverage_pct = 100 * year_available / n,
    grade_coverage_pct = 100 * grade_available / n,
    dwelling_type_coverage_pct = 100 * dwelling_type_available / n
  ) %>%
  select(
    sample,
    n,
    sqft_mean,
    sqft_median,
    sqft_coverage_pct,
    year_mean,
    year_median,
    year_coverage_pct,
    grade_coverage_pct,
    dwelling_type_coverage_pct
  )

print(structural_comparison_final, width = Inf)

write_csv(
  structural_comparison_final,
  "HMDA_SDAT_Exact_Linkage_Structural_Comparison.csv"
)


# ------------------------------------------------------------
# 7. BUILD FINAL FULL-HMDA ANALYSIS DATASET
# ------------------------------------------------------------
#
# DTI is retained exactly as publicly reported and converted to a
# factor. No midpoint conversion is used.
# ------------------------------------------------------------

primary_analysis <- hmda_match_df %>%
  mutate(
    interest_rate_num =
      suppressWarnings(as.numeric(interest_rate)),
    
    income_thousands =
      suppressWarnings(as.numeric(income)),
    
    loan_term_num =
      suppressWarnings(as.numeric(loan_term)),
    
    log_property_value =
      log(property_value_num),
    
    log_income =
      log(
        if_else(
          income_thousands > 0,
          income_thousands,
          NA_real_
        )
      ),
    
    dti_factor =
      factor(debt_to_income_ratio),
    
    occupancy_factor =
      factor(occupancy_type),
    
    construction_method_factor =
      factor(construction_method),
    
    total_units_factor =
      factor(total_units),
    
    county_factor =
      factor(county_code_chr)
  )


# ------------------------------------------------------------
# 8. BUILD FINAL LINKED PROPERTY-CONTROL DATASET
# ------------------------------------------------------------

property_analysis <- exact_bidirectional_pairs %>%
  select(hmda_id, sdat_id) %>%
  left_join(
    hmda_match_df,
    by = "hmda_id"
  ) %>%
  left_join(
    sdat_match_df %>%
      select(
        sdat_id,
        sqft_clean,
        year_built_clean,
        grade_clean,
        dwelling_type,
        dwelling_units
      ),
    by = "sdat_id"
  ) %>%
  mutate(
    interest_rate_num =
      suppressWarnings(as.numeric(interest_rate)),
    
    income_thousands =
      suppressWarnings(as.numeric(income)),
    
    loan_term_num =
      suppressWarnings(as.numeric(loan_term)),
    
    log_property_value =
      log(property_value_num),
    
    log_income =
      log(
        if_else(
          income_thousands > 0,
          income_thousands,
          NA_real_
        )
      ),
    
    dti_factor =
      factor(debt_to_income_ratio),
    
    property_age =
      2025 - year_built_clean,
    
    log_sqft =
      log(
        if_else(
          sqft_clean > 0,
          sqft_clean,
          NA_real_
        )
      ),
    
    occupancy_factor =
      factor(occupancy_type),
    
    construction_method_factor =
      factor(construction_method),
    
    total_units_factor =
      factor(total_units),
    
    grade_factor =
      factor(grade_clean),
    
    dwelling_type_factor =
      factor(dwelling_type),
    
    county_factor =
      factor(county_code_chr)
  )

cat("\nFINAL DATASET CHECKS\n")
cat("Primary HMDA dataset:", nrow(primary_analysis), "\n")
cat("Property-control linked dataset:", nrow(property_analysis), "\n")
cat("Unique HMDA IDs in linked dataset:",
    n_distinct(property_analysis$hmda_id), "\n")
cat("Unique SDAT IDs in linked dataset:",
    n_distinct(property_analysis$sdat_id), "\n")


# ------------------------------------------------------------
# 9. COMPLETE-CASE ANALYSIS SAMPLES
# ------------------------------------------------------------

primary_complete <- primary_analysis %>%
  filter(
    !is.na(log_property_value),
    !is.na(interest_rate_num),
    !is.na(log_income),
    !is.na(dti_factor),
    !is.na(loan_term_num),
    !is.na(occupancy_factor),
    !is.na(construction_method_factor),
    !is.na(total_units_factor),
    !is.na(county_factor)
  )

property_complete <- property_analysis %>%
  filter(
    !is.na(log_property_value),
    !is.na(interest_rate_num),
    !is.na(log_income),
    !is.na(dti_factor),
    !is.na(loan_term_num),
    !is.na(occupancy_factor),
    !is.na(construction_method_factor),
    !is.na(total_units_factor),
    !is.na(county_factor),
    !is.na(log_sqft),
    !is.na(property_age),
    !is.na(grade_factor),
    !is.na(dwelling_type_factor)
  )

cat("\nCOMPLETE-CASE SAMPLE SIZES\n")
cat("Primary complete-case N:", nrow(primary_complete), "\n")
cat("Primary retained %:",
    round(100 * nrow(primary_complete) / nrow(primary_analysis), 1),
    "%\n")
cat("Property-control complete-case N:",
    nrow(property_complete), "\n")
cat("Property-control retained %:",
    round(100 * nrow(property_complete) / nrow(property_analysis), 1),
    "%\n")


# ------------------------------------------------------------
# 10. PRIMARY FULL-HMDA MODEL
# ------------------------------------------------------------

model_primary <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    county_factor,
  data = primary_complete
)

primary_hc3 <- coeftest(
  model_primary,
  vcov. = vcovHC(model_primary, type = "HC3")
)


# ------------------------------------------------------------
# 11. SAME-SAMPLE LINKED MODEL: HMDA CONTROLS ONLY
# ------------------------------------------------------------

model_linked_base <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    county_factor,
  data = property_complete
)

base_hc3 <- coeftest(
  model_linked_base,
  vcov. = vcovHC(model_linked_base, type = "HC3")
)


# ------------------------------------------------------------
# 12. SAME-SAMPLE LINKED MODEL: ADD SDAT PROPERTY CONTROLS
# ------------------------------------------------------------

model_linked_property <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    county_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor,
  data = property_complete
)

property_hc3 <- coeftest(
  model_linked_property,
  vcov. = vcovHC(model_linked_property, type = "HC3")
)


# ------------------------------------------------------------
# 13. CORE INTEREST-RATE RESULTS
# ------------------------------------------------------------

extract_interest_result <- function(model, hc3_table, model_name) {
  # Calculate model statistics before tibble() so the output column named
  # `model` cannot mask the fitted-model function argument.
  model_n <- nobs(model)
  model_adj_r2 <- summary(model)$adj.r.squared
  
  tibble(
    model = model_name,
    n = model_n,
    interest_beta = hc3_table["interest_rate_num", "Estimate"],
    robust_se = hc3_table["interest_rate_num", "Std. Error"],
    p_value = hc3_table["interest_rate_num", "Pr(>|t|)"],
    exact_percent_association =
      100 * (exp(hc3_table["interest_rate_num", "Estimate"]) - 1),
    adjusted_r2 = model_adj_r2
  )
}

primary_interest_result <- extract_interest_result(
  model_primary,
  primary_hc3,
  "Primary full HMDA sample"
)

linked_base_interest_result <- extract_interest_result(
  model_linked_base,
  base_hc3,
  "Linked sample: HMDA controls"
)

linked_property_interest_result <- extract_interest_result(
  model_linked_property,
  property_hc3,
  "Linked sample: + property controls"
)

final_core_models <- bind_rows(
  primary_interest_result,
  linked_base_interest_result,
  linked_property_interest_result
)

cat("\nFINAL CORE MODEL COMPARISON\n")
print(final_core_models, width = Inf)

write_csv(
  final_core_models,
  "Paper1_Final_Core_Model_Comparison_With_Percent_Effects.csv"
)

# Keep the original final comparison filename as well.
write_csv(
  final_core_models %>%
    select(
      model,
      n,
      interest_beta,
      robust_se,
      p_value,
      adjusted_r2
    ),
  "Paper1_Final_Core_Model_Comparison.csv"
)


# ------------------------------------------------------------
# 14. EXPORT FULL HC3 COEFFICIENT TABLES
# ------------------------------------------------------------

hc3_to_tibble <- function(hc3_table) {
  tibble(
    term = rownames(hc3_table),
    estimate = hc3_table[, "Estimate"],
    robust_se = hc3_table[, "Std. Error"],
    t_value = hc3_table[, "t value"],
    p_value = hc3_table[, "Pr(>|t|)"]
  )
}

primary_coefficients <- hc3_to_tibble(primary_hc3)
linked_base_coefficients <- hc3_to_tibble(base_hc3)
linked_property_coefficients <- hc3_to_tibble(property_hc3)

write_csv(
  primary_coefficients,
  "Paper1_Final_Primary_HC3_Coefficients.csv"
)

write_csv(
  linked_base_coefficients,
  "Paper1_Final_Linked_Base_HC3_Coefficients.csv"
)

write_csv(
  linked_property_coefficients,
  "Paper1_Final_Linked_Property_HC3_Coefficients.csv"
)

write_csv(
  bind_rows(
    linked_base_interest_result,
    linked_property_interest_result
  ) %>%
    select(
      model,
      n,
      interest_beta,
      robust_se,
      p_value,
      adjusted_r2
    ),
  "HMDA_SDAT_Final_Same_Sample_Nested_Models.csv"
)


# ------------------------------------------------------------
# 15. FINAL VERIFICATION
# ------------------------------------------------------------
#
# These checks stop the script if the reconstructed samples do not
# match the final manuscript analysis.
# ------------------------------------------------------------

stopifnot(nrow(hmda_match_df) == 62305)
stopifnot(nrow(exact_bidirectional_pairs) == 4789)
stopifnot(nrow(primary_complete) == 58587)
stopifnot(nrow(property_complete) == 4167)
stopifnot(nobs(model_linked_base) == nobs(model_linked_property))

cat("\nFINAL VERIFIED SAMPLE COUNTS\n")
cat("HMDA matching population: 62,305\n")
cat("Bidirectionally unique exact links: 4,789\n")
cat("Primary regression sample: 58,587\n")
cat("Linked same-sample regressions: 4,167\n")

cat("\nFINAL INTEREST-RATE RESULTS\n")
print(final_core_models, width = Inf)

cat("\nOUTPUT FILE CHECKS\n")
output_files <- c(
  "HMDA_SDAT_Exact_Bidirectional_4789.csv",
  "HMDA_2025_Purchase_Originated_Matching_Population_62305.csv",
  "HMDA_SDAT_Exact_4789_Selection_Diagnostic.csv",
  "HMDA_SDAT_Exact_Linkage_Structural_Comparison.csv",
  "HMDA_SDAT_Final_Same_Sample_Nested_Models.csv",
  "Paper1_Final_Core_Model_Comparison.csv",
  "Paper1_Final_Core_Model_Comparison_With_Percent_Effects.csv",
  "Paper1_Final_Primary_HC3_Coefficients.csv",
  "Paper1_Final_Linked_Base_HC3_Coefficients.csv",
  "Paper1_Final_Linked_Property_HC3_Coefficients.csv"
)

print(
  tibble(
    file = output_files,
    exists = file.exists(output_files)
  ),
  n = Inf
)


# ------------------------------------------------------------
# 16. SESSION INFORMATION
# ------------------------------------------------------------

cat("\nSESSION INFORMATION\n")
print(sessionInfo())

cat("\nFINAL ANALYSIS COMPLETE\n")

# ============================================================
# 17. PUBLICATION FIGURE: INTEREST-RATE ASSOCIATION BY MODEL
# ============================================================

# Build 95% confidence intervals from the HC3 robust standard errors.
figure_data <- final_core_models %>%
  mutate(
    ci_low_beta = interest_beta - 1.96 * robust_se,
    ci_high_beta = interest_beta + 1.96 * robust_se,
    ci_low_percent = 100 * (exp(ci_low_beta) - 1),
    ci_high_percent = 100 * (exp(ci_high_beta) - 1),
    model_label = factor(
      model,
      levels = c(
        "Linked sample: HMDA controls",
        "Linked sample: + property controls",
        "Primary full HMDA sample"
      ),
      labels = c(
        "Linked: HMDA controls",
        "Linked: + SDAT property controls",
        "Primary HMDA"
      )
    )
  )

interest_rate_plot <- ggplot(
  figure_data,
  aes(x = exact_percent_association, y = model_label)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = ci_low_percent, xmax = ci_high_percent),
    height = 0.16,
    linewidth = 0.7
  ) +
  geom_point(size = 3) +
  labs(
    title = "Interest-Rate Association with Residential Property Value",
    subtitle = "Exact semi-log percentage association with HC3 95% confidence intervals",
    x = "Percent association for a 1 percentage-point higher mortgage interest rate",
    y = NULL,
    caption = "Note: Estimates are conditional associations, not causal effects."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )

print(interest_rate_plot)

ggsave(
  "Paper1_Interest_Rate_Model_Comparison.png",
  plot = interest_rate_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  "Paper1_Interest_Rate_Model_Comparison.pdf",
  plot = interest_rate_plot,
  width = 9,
  height = 5.5
)

cat("\nPUBLICATION FIGURE FILES\n")
print(
  tibble(
    file = c(
      "Paper1_Interest_Rate_Model_Comparison.png",
      "Paper1_Interest_Rate_Model_Comparison.pdf"
    ),
    exists = file.exists(c(
      "Paper1_Interest_Rate_Model_Comparison.png",
      "Paper1_Interest_Rate_Model_Comparison.pdf"
    ))
  )
)

# ============================================================
# 18. PUBLICATION FIGURE: LINKED-SAMPLE MODEL FIT COMPARISON
# ============================================================

linked_fit_data <- final_core_models %>%
  filter(model %in% c(
    "Linked sample: HMDA controls",
    "Linked sample: + property controls"
  )) %>%
  mutate(
    model_label = factor(
      model,
      levels = c(
        "Linked sample: HMDA controls",
        "Linked sample: + property controls"
      ),
      labels = c(
        "HMDA controls",
        "+ SDAT property controls"
      )
    )
  )

linked_fit_plot <- ggplot(
  linked_fit_data,
  aes(x = model_label, y = adjusted_r2)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.3f", adjusted_r2)),
    vjust = -0.5,
    size = 4.5
  ) +
  coord_cartesian(ylim = c(0, 0.95)) +
  labs(
    title = "Model Fit in the Linked HMDA-SDAT Sample",
    subtitle = "Same 4,167 observations in both specifications",
    x = NULL,
    y = "Adjusted R²",
    caption = "The specification including SDAT property controls has an adjusted R² of 0.856, compared with 0.703 for the HMDA-controls specification."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10)
  )

print(linked_fit_plot)

ggsave(
  "Paper1_Linked_Model_Fit_Comparison.png",
  plot = linked_fit_plot,
  width = 8,
  height = 5.5,
  dpi = 300
)

ggsave(
  "Paper1_Linked_Model_Fit_Comparison.pdf",
  plot = linked_fit_plot,
  width = 8,
  height = 5.5
)

cat("\nPUBLICATION FIGURE FILES\n")
figure_files <- c(
  "Paper1_Interest_Rate_Model_Comparison.png",
  "Paper1_Interest_Rate_Model_Comparison.pdf",
  "Paper1_Linked_Model_Fit_Comparison.png",
  "Paper1_Linked_Model_Fit_Comparison.pdf"
)

print(
  tibble(
    file = figure_files,
    exists = file.exists(figure_files)
  ),
  n = Inf
)

