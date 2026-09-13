# ============================================================
# Property, Mortgage, Lender, and Location Heterogeneity in the
# Mortgage Rate–Property Value Relationship:
# Evidence from Linked HMDA and Assessment Data
#
# Author: Peter Sarpong
# Year: 2026
#
# QREF VERSION 2 ANALYSIS AND REPRODUCTION SCRIPT.
#
# This script contains the analysis used in the revised
# Version 2 manuscript. It examines specification sensitivity
# across property, mortgage-product, lender, and geographic
# controls, including a linkage-selection sensitivity analysis.
#
# Loan amount and loan-to-value ratio are excluded from the
# preferred property-value regression specifications.
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

ls()

dim(property_complete)

names(property_complete)

c(
  "lei",
  "census_tract",
  "county_code"
) %in% names(property_complete)


property_complete %>%
  summarise(
    observations = n(),
    lenders = n_distinct(lei),
    census_tracts = n_distinct(census_tract),
    counties = n_distinct(county_code)
  )

names(property_complete)


property_complete %>%
  count(lei, name = "n") %>%
  summarise(
    min_per_lender = min(n),
    median_per_lender = median(n),
    mean_per_lender = mean(n),
    max_per_lender = max(n),
    lenders_with_1 = sum(n == 1),
    lenders_with_5plus = sum(n >= 5),
    lenders_with_10plus = sum(n >= 10)
  )

property_complete %>%
  count(census_tract, name = "n") %>%
  summarise(
    min_per_tract = min(n),
    median_per_tract = median(n),
    mean_per_tract = mean(n),
    max_per_tract = max(n),
    tracts_with_1 = sum(n == 1),
    tracts_with_5plus = sum(n >= 5),
    tracts_with_10plus = sum(n >= 10)
  )

# Show the full lender and tract counts
property_complete %>%
  count(lei, name = "n") %>%
  summarise(
    lenders = n(),
    lenders_with_1 = sum(n == 1),
    lenders_with_5plus = sum(n >= 5),
    lenders_with_10plus = sum(n >= 10),
    lenders_with_20plus = sum(n >= 20)
  )

property_complete %>%
  count(census_tract, name = "n") %>%
  summarise(
    tracts = n(),
    tracts_with_1 = sum(n == 1),
    tracts_with_5plus = sum(n >= 5),
    tracts_with_10plus = sum(n >= 10)
  )

# Interest-rate variation within lenders
lender_rate_variation <- property_complete %>%
  group_by(lei) %>%
  summarise(
    n = n(),
    unique_rates = n_distinct(interest_rate_num),
    sd_rate = sd(interest_rate_num),
    .groups = "drop"
  )

lender_rate_variation %>%
  summarise(
    lenders = n(),
    lenders_with_rate_variation = sum(unique_rates > 1),
    lenders_without_rate_variation = sum(unique_rates == 1),
    pct_with_variation = 100 * mean(unique_rates > 1)
  )

# Interest-rate variation within census tracts
tract_rate_variation <- property_complete %>%
  group_by(census_tract) %>%
  summarise(
    n = n(),
    unique_rates = n_distinct(interest_rate_num),
    sd_rate = sd(interest_rate_num),
    .groups = "drop"
  )

tract_rate_variation %>%
  summarise(
    tracts = n(),
    tracts_with_rate_variation = sum(unique_rates > 1),
    tracts_without_rate_variation = sum(unique_rates == 1),
    pct_with_variation = 100 * mean(unique_rates > 1)
  )

# Mortgage product variables
property_complete %>%
  count(loan_type, sort = TRUE)

property_complete %>%
  count(derived_loan_product_type, sort = TRUE)

property_complete %>%
  count(conforming_loan_limit, sort = TRUE)

property_complete %>%
  count(lien_status, sort = TRUE)

# Other potentially relevant loan/lender variables
property_complete %>%
  count(purchaser_type, sort = TRUE)

property_complete %>%
  count(initially_payable_to_institution, sort = TRUE)

property_complete %>%
  count(applicant_credit_score_type, sort = TRUE)


property_complete %>%
  summarise(
    tract_income_min = min(tract_to_msa_income_percentage, na.rm = TRUE),
    tract_income_median = median(tract_to_msa_income_percentage, na.rm = TRUE),
    tract_income_max = max(tract_to_msa_income_percentage, na.rm = TRUE),
    
    minority_pct_min = min(tract_minority_population_percent, na.rm = TRUE),
    minority_pct_median = median(tract_minority_population_percent, na.rm = TRUE),
    minority_pct_max = max(tract_minority_population_percent, na.rm = TRUE),
    
    housing_age_min = min(tract_median_age_of_housing_units, na.rm = TRUE),
    housing_age_median = median(tract_median_age_of_housing_units, na.rm = TRUE),
    housing_age_max = max(tract_median_age_of_housing_units, na.rm = TRUE)
  )

model_v2_loan <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor +
    county_factor +
    factor(loan_type) +
    factor(conforming_loan_limit),
  data = property_complete
)

v2_loan_hc3 <- lmtest::coeftest(
  model_v2_loan,
  vcov. = sandwich::vcovHC(model_v2_loan, type = "HC3")
)

v2_loan_hc3["interest_rate_num", ]

summary(model_v2_loan)$adj.r.squared
nobs(model_v2_loan)


model_v2_lender <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor +
    county_factor +
    factor(loan_type) +
    factor(conforming_loan_limit) +
    factor(lei),
  data = property_complete
)

v2_lender_hc3 <- lmtest::coeftest(
  model_v2_lender,
  vcov. = sandwich::vcovHC(model_v2_lender, type = "HC3")
)

v2_lender_hc3["interest_rate_num", ]

summary(model_v2_lender)$adj.r.squared
nobs(model_v2_lender)


v2_lender_cluster <- lmtest::coeftest(
  model_v2_lender,
  vcov. = sandwich::vcovCL(
    model_v2_lender,
    cluster = property_complete$lei,
    type = "HC1"
  )
)

v2_lender_cluster["interest_rate_num", ]

lender_counts <- property_complete %>%
  count(lei, name = "lender_n")

property_lender2 <- property_complete %>%
  left_join(lender_counts, by = "lei") %>%
  filter(lender_n >= 2)

dim(property_lender2)

n_distinct(property_lender2$lei)


model_v2_lender2 <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor +
    county_factor +
    factor(loan_type) +
    factor(conforming_loan_limit) +
    factor(lei),
  data = property_lender2
)

v2_lender2_cluster <- lmtest::coeftest(
  model_v2_lender2,
  vcov. = sandwich::vcovCL(
    model_v2_lender2,
    cluster = property_lender2$lei,
    type = "HC1"
  )
)

v2_lender2_cluster["interest_rate_num", ]

summary(model_v2_lender2)$adj.r.squared
nobs(model_v2_lender2)

property_complete %>%
  summarise(
    tract_income_min =
      min(tract_to_msa_income_percentage, na.rm = TRUE),
    tract_income_median =
      median(tract_to_msa_income_percentage, na.rm = TRUE),
    tract_income_max =
      max(tract_to_msa_income_percentage, na.rm = TRUE),
    
    housing_age_min =
      min(tract_median_age_of_housing_units, na.rm = TRUE),
    housing_age_median =
      median(tract_median_age_of_housing_units, na.rm = TRUE),
    housing_age_max =
      max(tract_median_age_of_housing_units, na.rm = TRUE),
    
    missing_tract_income =
      sum(is.na(tract_to_msa_income_percentage)),
    missing_housing_age =
      sum(is.na(tract_median_age_of_housing_units))
  ) %>%
  print(width = Inf)

property_complete %>%
  summarise(
    owner_occ_min =
      min(tract_owner_occupied_units, na.rm = TRUE),
    owner_occ_median =
      median(tract_owner_occupied_units, na.rm = TRUE),
    owner_occ_max =
      max(tract_owner_occupied_units, na.rm = TRUE),
    
    one_four_min =
      min(tract_one_to_four_family_homes, na.rm = TRUE),
    one_four_median =
      median(tract_one_to_four_family_homes, na.rm = TRUE),
    one_four_max =
      max(tract_one_to_four_family_homes, na.rm = TRUE),
    
    missing_owner_occ =
      sum(is.na(tract_owner_occupied_units)),
    missing_one_four =
      sum(is.na(tract_one_to_four_family_homes))
  ) %>%
  print(width = Inf)

model_v2_geo <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor +
    county_factor +
    factor(loan_type) +
    factor(conforming_loan_limit) +
    tract_to_msa_income_percentage +
    tract_median_age_of_housing_units +
    factor(lei),
  data = property_complete
)

v2_geo_cluster <- lmtest::coeftest(
  model_v2_geo,
  vcov. = sandwich::vcovCL(
    model_v2_geo,
    cluster = property_complete$lei,
    type = "HC1"
  )
)

v2_geo_cluster["interest_rate_num", ]

summary(model_v2_geo)$adj.r.squared
nobs(model_v2_geo)

summary(model_v2_geo)$adj.r.squared
nobs(model_v2_geo)

v2_geo_cluster[
  c(
    "interest_rate_num",
    "tract_to_msa_income_percentage",
    "tract_median_age_of_housing_units"
  ),
]


model_v2_tract <- lm(
  log_property_value ~
    interest_rate_num +
    log_income +
    dti_factor +
    loan_term_num +
    occupancy_factor +
    construction_method_factor +
    total_units_factor +
    log_sqft +
    property_age +
    grade_factor +
    dwelling_type_factor +
    factor(loan_type) +
    factor(conforming_loan_limit) +
    factor(lei) +
    factor(census_tract),
  data = property_complete
)

coef(model_v2_tract)["interest_rate_num"]

summary(model_v2_tract)$adj.r.squared

nobs(model_v2_tract)

df.residual(model_v2_tract)

sum(is.na(coef(model_v2_tract)))

names(coef(model_v2_tract))[is.na(coef(model_v2_tract))]

v2_tract_cluster <- lmtest::coeftest(
  model_v2_tract,
  vcov. = sandwich::vcovCL(
    model_v2_tract,
    cluster = property_complete$census_tract,
    type = "HC1"
  )
)

v2_tract_cluster["interest_rate_num", ]

v2_tract_twoway <- lmtest::coeftest(
  model_v2_tract,
  vcov. = sandwich::vcovCL(
    model_v2_tract,
    cluster = ~ lei + census_tract,
    type = "HC1"
  )
)

v2_tract_twoway["interest_rate_num", ]

dim(hmda_match_df)
names(hmda_match_df)
class(exact_matched_hmda_ids)
length(exact_matched_hmda_ids)
head(exact_matched_hmda_ids)

dim(exact_bidirectional_pairs)
names(exact_bidirectional_pairs)
head(exact_bidirectional_pairs)


hmda_match_df %>%
  summarise(
    observations = n(),
    unique_hmda_ids = n_distinct(hmda_id),
    duplicate_ids = n() - n_distinct(hmda_id)
  )

hmda_selection <- hmda_match_df %>%
  mutate(
    linked = if_else(
      hmda_id %in% exact_matched_hmda_ids$hmda_id,
      1L,
      0L
    )
  )

hmda_selection %>%
  count(linked) %>%
  mutate(percent = 100 * n / sum(n))

selection_summary <- hmda_selection %>%
  group_by(linked) %>%
  summarise(
    n = n(),
    
    mean_property_value =
      mean(property_value_num, na.rm = TRUE),
    
    median_property_value =
      median(property_value_num, na.rm = TRUE),
    
    mean_loan_amount =
      mean(loan_amount_num, na.rm = TRUE),
    
    median_loan_amount =
      median(loan_amount_num, na.rm = TRUE),
    
    mean_income =
      mean(as.numeric(income), na.rm = TRUE),
    
    median_income =
      median(as.numeric(income), na.rm = TRUE),
    
    mean_interest_rate =
      mean(as.numeric(interest_rate), na.rm = TRUE),
    
    median_interest_rate =
      median(as.numeric(interest_rate), na.rm = TRUE),
    
    mean_tract_income_pct =
      mean(tract_to_msa_income_percentage, na.rm = TRUE),
    
    mean_tract_minority_pct =
      mean(tract_minority_population_percent, na.rm = TRUE),
    
    .groups = "drop"
  )

print(selection_summary, width = Inf)

hmda_selection %>%
  count(linked, loan_type) %>%
  group_by(linked) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(linked, loan_type)

hmda_selection %>%
  count(linked, conforming_loan_limit) %>%
  group_by(linked) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(linked, conforming_loan_limit)

hmda_selection %>%
  count(linked, occupancy_type) %>%
  group_by(linked) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(linked, occupancy_type)


selection_model_df <- hmda_selection %>%
  mutate(
    interest_rate_num = suppressWarnings(as.numeric(interest_rate)),
    income_num = suppressWarnings(as.numeric(income)),
    loan_term_num = suppressWarnings(as.numeric(loan_term)),
    
    log_property_value = log(property_value_num),
    log_income = if_else(
      !is.na(income_num) & income_num > 0,
      log(income_num),
      NA_real_
    ),
    
    loan_type_factor = factor(loan_type),
    conforming_factor = factor(conforming_loan_limit),
    occupancy_factor_sel = factor(occupancy_type),
    construction_factor_sel = factor(construction_method),
    units_factor_sel = factor(total_units),
    county_factor_sel = factor(county_code),
    dti_factor_sel = factor(debt_to_income_ratio)
  )

selection_model_df %>%
  summarise(
    total = n(),
    complete = sum(
      complete.cases(
        linked,
        log_property_value,
        log_income,
        interest_rate_num,
        dti_factor_sel,
        loan_type_factor,
        conforming_factor,
        occupancy_factor_sel,
        construction_factor_sel,
        units_factor_sel,
        county_factor_sel,
        tract_to_msa_income_percentage,
        tract_minority_population_percent,
        tract_median_age_of_housing_units
      )
    ),
    linked_complete = sum(
      linked == 1 &
        complete.cases(
          log_property_value,
          log_income,
          interest_rate_num,
          dti_factor_sel,
          loan_type_factor,
          conforming_factor,
          occupancy_factor_sel,
          construction_factor_sel,
          units_factor_sel,
          county_factor_sel,
          tract_to_msa_income_percentage,
          tract_minority_population_percent,
          tract_median_age_of_housing_units
        )
    )
  )



selection_model_df <- hmda_selection %>%
  mutate(
    interest_rate_num = suppressWarnings(as.numeric(interest_rate)),
    income_num = suppressWarnings(as.numeric(income)),
    
    log_property_value = log(property_value_num),
    log_income = log(if_else(income_num > 0, income_num, NA_real_)),
    
    loan_type_factor = factor(loan_type),
    conforming_factor = factor(conforming_loan_limit),
    occupancy_factor_sel = factor(occupancy_type),
    construction_factor_sel = factor(construction_method),
    units_factor_sel = factor(total_units),
    county_factor_sel = factor(county_code),
    dti_factor_sel = factor(debt_to_income_ratio)
  )

link_model <- glm(
  linked ~
    log_property_value +
    log_income +
    interest_rate_num +
    dti_factor_sel +
    loan_type_factor +
    conforming_factor +
    occupancy_factor_sel +
    construction_factor_sel +
    units_factor_sel +
    county_factor_sel +
    tract_to_msa_income_percentage +
    tract_minority_population_percent +
    tract_median_age_of_housing_units,
  data = selection_model_df,
  family = binomial()
)

summary(link_model)

selection_model_df$pscore <- predict(
  link_model,
  newdata = selection_model_df,
  type = "response"
)

selection_model_df$pscore <- NA_real_

model_rows <- as.integer(rownames(link_model$model))

selection_model_df$pscore[model_rows] <- fitted(link_model)

selection_model_df %>%
  summarise(
    predicted = sum(!is.na(pscore)),
    linked_predicted = sum(linked == 1 & !is.na(pscore))
  )

selection_model_df %>%
  filter(linked == 1, !is.na(pscore)) %>%
  summarise(
    n = n(),
    min_pscore = min(pscore),
    p1 = quantile(pscore, 0.01),
    p5 = quantile(pscore, 0.05),
    median_pscore = median(pscore),
    p95 = quantile(pscore, 0.95),
    p99 = quantile(pscore, 0.99),
    max_pscore = max(pscore)
  )

selection_model_df %>%
  filter(linked == 1, !is.na(pscore)) %>%
  mutate(ipw = 1 / pscore) %>%
  summarise(
    min_weight = min(ipw),
    p50 = median(ipw),
    p95 = quantile(ipw, 0.95),
    p99 = quantile(ipw, 0.99),
    max_weight = max(ipw)
  )

link_rate_complete <- mean(
  selection_model_df$linked[!is.na(selection_model_df$pscore)]
)

link_rate_complete


selection_weights <- selection_model_df %>%
  filter(linked == 1, !is.na(pscore)) %>%
  transmute(
    hmda_id,
    pscore,
    ipw_raw = 1 / pscore,
    ipw_stabilized = link_rate_complete / pscore
  )

property_ipw <- property_complete %>%
  left_join(
    selection_weights,
    by = "hmda_id"
  )

property_ipw %>%
  summarise(
    total = n(),
    with_weight = sum(!is.na(ipw_stabilized)),
    missing_weight = sum(is.na(ipw_stabilized))
  )

property_ipw %>%
  filter(!is.na(ipw_stabilized)) %>%
  summarise(
    min_weight = min(ipw_stabilized),
    median_weight = median(ipw_stabilized),
    p95 = quantile(ipw_stabilized, .95),
    p99 = quantile(ipw_stabilized, .99),
    max_weight = max(ipw_stabilized),
    effective_n =
      sum(ipw_stabilized)^2 /
      sum(ipw_stabilized^2)
  )

formula(model_v2_geo)

model_v2_geo_ipw <- lm(
  formula(model_v2_geo),
  data = property_ipw,
  weights = ipw_stabilized
)

nobs(model_v2_geo_ipw)
coef(model_v2_geo_ipw)["interest_rate"]
library(sandwich)
library(lmtest)

ipw_geo_cluster <- coeftest(
  model_v2_geo_ipw,
  vcov = vcovCL(
    model_v2_geo_ipw,
    cluster = ~ lei,
    type = "HC1"
  )
)

coef(model_v2_geo_ipw)["interest_rate_num"]
ipw_geo_cluster["interest_rate_num", ]
100 * (exp(coef(model_v2_geo_ipw)["interest_rate_num"]) - 1)
names(coef(model_v2_geo_ipw))[1:20]

# ============================================================
# EXPORT VERSION 2 MODEL COMPARISON
# ============================================================

# Models A and B: HC3
vcov_A <- sandwich::vcovHC(model_linked_base, type = "HC3")
vcov_B <- sandwich::vcovHC(model_linked_property, type = "HC3")

# Models C: HC3
vcov_C <- sandwich::vcovHC(model_v2_loan, type = "HC3")

# Models D and E: lender-clustered
vcov_D <- sandwich::vcovCL(
  model_v2_lender,
  cluster = ~ lei,
  type = "HC1"
)

vcov_E <- sandwich::vcovCL(
  model_v2_geo,
  cluster = ~ lei,
  type = "HC1"
)

# Model F: tract-clustered
vcov_F <- sandwich::vcovCL(
  model_v2_tract,
  cluster = ~ census_tract,
  type = "HC1"
)

# IPW Model E: lender-clustered
vcov_IPW <- sandwich::vcovCL(
  model_v2_geo_ipw,
  cluster = ~ lei,
  type = "HC1"
)

get_result <- function(model, vcov_matrix, label) {
  
  beta <- coef(model)["interest_rate_num"]
  se <- sqrt(diag(vcov_matrix))["interest_rate_num"]
  p <- 2 * pnorm(abs(beta / se), lower.tail = FALSE)
  
  data.frame(
    model = label,
    n = nobs(model),
    interest_beta = beta,
    robust_se = se,
    p_value = p,
    adjusted_r2 = summary(model)$adj.r.squared,
    pct_change_per_1pp_rate = 100 * (exp(beta) - 1)
  )
}

version2_results <- dplyr::bind_rows(
  get_result(model_linked_base, vcov_A, "Model A - HMDA controls"),
  get_result(model_linked_property, vcov_B, "Model B - Property controls"),
  get_result(model_v2_loan, vcov_C, "Model C - Mortgage-product controls"),
  get_result(model_v2_lender, vcov_D, "Model D - Lender fixed effects"),
  get_result(model_v2_geo, vcov_E, "Model E - Local-market controls"),
  get_result(model_v2_tract, vcov_F, "Model F - Census tract fixed effects"),
  get_result(model_v2_geo_ipw, vcov_IPW, "Model E - IPW sensitivity")
)

print(version2_results)

write.csv(
  version2_results,
  "Paper1_QREF_Version2_Model_Results.csv",
  row.names = FALSE
)

names(coef(model_linked_base))



get_result <- function(model, vcov_matrix, label) {
  
  ct <- lmtest::coeftest(model, vcov. = vcov_matrix)
  
  beta <- coef(model)["interest_rate_num"]
  se <- ct["interest_rate_num", 2]
  p <- ct["interest_rate_num", 4]
  
  data.frame(
    model = label,
    n = nobs(model),
    interest_beta = beta,
    robust_se = se,
    p_value = p,
    adjusted_r2 = summary(model)$adj.r.squared,
    pct_change_per_1pp_rate = 100 * (exp(beta) - 1)
  )
}
  
version2_results <- dplyr::bind_rows(
  get_result(model_linked_base, vcov_A, "Model A - HMDA controls"),
  get_result(model_linked_property, vcov_B, "Model B - Property controls"),
  get_result(model_v2_loan, vcov_C, "Model C - Mortgage-product controls"),
  get_result(model_v2_lender, vcov_D, "Model D - Lender fixed effects"),
  get_result(model_v2_geo, vcov_E, "Model E - Local-market controls"),
  get_result(model_v2_tract, vcov_F, "Model F - Census tract fixed effects"),
  get_result(model_v2_geo_ipw, vcov_IPW, "Model E - IPW sensitivity")
)

print(version2_results)


write.csv(
  version2_results,
  "Paper1_QREF_Version2_Model_Results.csv",
  row.names = FALSE
)

file.exists("Paper1_QREF_Version2_Model_Results.csv")
getwd()
