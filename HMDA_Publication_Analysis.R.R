# ============================================================
# Interest Rate and Structural Determinants of Residential
# Property Value: A Cross-Sectional Analysis of Maryland HMDA Data
#
# Author: Peter Sarpong
# Data: FFIEC/CFPB HMDA National Loan-Level Dataset, Maryland, 2025
#
# PRIMARY SPECIFICATION: originated loans only (action_taken == 1),
# full control set (income, loan amount, DTI, LTV, loan term).
# Robustness checks (top-2% trim; all-applications sample) reported
# separately and labeled honestly, not presented as equally valid
# alternatives to the primary result.
# ============================================================

library(dplyr)
library(tidyr)
library(car)         # vif()
library(lmtest)      # bptest(), coeftest(), dwtest()
library(sandwich)    # vcovHC()
library(stargazer)   # publication tables

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
raw_data <- read.csv("C:/Users/peter/Downloads/loan_purposes_1_state_MD.csv",
                      stringsAsFactors = FALSE)
cat("Raw rows:", nrow(raw_data), "\n")
cat("action_taken breakdown (1 = originated/funded loan):\n")
print(table(raw_data$action_taken))

# ------------------------------------------------------------
# 2. HELPER: convert bucketed DTI strings to numeric midpoints
# HMDA reports debt_to_income_ratio as exact values only at the
# extremes; the middle of the distribution is banded into ranges
# (e.g., "30%-<36%") for applicant privacy. We use bucket midpoints.
# ------------------------------------------------------------
dti_to_numeric <- function(val) {
  if (is.na(val) || val == "Exempt") return(NA_real_)
  if (val == "<20%") return(15)
  if (val == ">60%") return(65)
  nums <- regmatches(val, gregexpr("[0-9]+", val))[[1]]
  if (length(nums) == 2) return((as.numeric(nums[1]) + as.numeric(nums[2])) / 2)
  suppressWarnings(as.numeric(val))
}

# ------------------------------------------------------------
# 3. BUILD PRIMARY SAMPLE: originated loans, full controls
# ------------------------------------------------------------
originated <- raw_data %>%
  filter(action_taken == 1) %>%
  mutate(
    property_value      = suppressWarnings(as.numeric(property_value)),
    interest_rate        = suppressWarnings(as.numeric(interest_rate)),
    loan_to_value_ratio   = suppressWarnings(as.numeric(loan_to_value_ratio)),
    loan_term             = suppressWarnings(as.numeric(loan_term)),
    income_thousands       = as.numeric(income),
    dti_numeric              = sapply(debt_to_income_ratio, dti_to_numeric)
  ) %>%
  select(property_value, interest_rate, loan_amount, income_thousands,
         dti_numeric, loan_to_value_ratio, loan_term) %>%
  drop_na() %>%
  mutate(log_property_value = log(property_value))

cat("\nPrimary sample (originated loans, complete cases): N =", nrow(originated), "\n")


# 3A. TABLE 1: DESCRIPTIVE STATISTICS
# Primary Sample: Originated Loans, Complete Cases
# --------------------------------------------------
# ------------------------------------------------------------
# TABLE 1: DESCRIPTIVE STATISTICS
# Primary Sample: Originated Loans, Complete Cases
# ------------------------------------------------------------

desc_stats <- data.frame(
  Variable = c(
    "Property Value",
    "Interest Rate",
    "Income",
    "Loan Amount",
    "Debt-to-Income Ratio",
    "Loan-to-Value Ratio",
    "Loan Term"
  ),
  
  N = c(
    length(originated$property_value),
    length(originated$interest_rate),
    length(originated$income_thousands),
    length(originated$loan_amount),
    length(originated$dti_numeric),
    length(originated$loan_to_value_ratio),
    length(originated$loan_term)
  ),
  
  Mean = c(
    mean(originated$property_value),
    mean(originated$interest_rate),
    mean(originated$income_thousands),
    mean(originated$loan_amount),
    mean(originated$dti_numeric),
    mean(originated$loan_to_value_ratio),
    mean(originated$loan_term)
  ),
  
  SD = c(
    sd(originated$property_value),
    sd(originated$interest_rate),
    sd(originated$income_thousands),
    sd(originated$loan_amount),
    sd(originated$dti_numeric),
    sd(originated$loan_to_value_ratio),
    sd(originated$loan_term)
  ),
  
  Median = c(
    median(originated$property_value),
    median(originated$interest_rate),
    median(originated$income_thousands),
    median(originated$loan_amount),
    median(originated$dti_numeric),
    median(originated$loan_to_value_ratio),
    median(originated$loan_term)
  ),
  
  Min = c(
    min(originated$property_value),
    min(originated$interest_rate),
    min(originated$income_thousands),
    min(originated$loan_amount),
    min(originated$dti_numeric),
    min(originated$loan_to_value_ratio),
    min(originated$loan_term)
  ),
  
  Max = c(
    max(originated$property_value),
    max(originated$interest_rate),
    max(originated$income_thousands),
    max(originated$loan_amount),
    max(originated$dti_numeric),
    max(originated$loan_to_value_ratio),
    max(originated$loan_term)
  )
)

# Round numerical statistics for display
desc_stats$Mean   <- round(desc_stats$Mean, 2)
desc_stats$SD     <- round(desc_stats$SD, 2)
desc_stats$Median <- round(desc_stats$Median, 2)
desc_stats$Min    <- round(desc_stats$Min, 2)
desc_stats$Max    <- round(desc_stats$Max, 2)

cat("\n============================================================\n")
cat("TABLE 1: DESCRIPTIVE STATISTICS\n")
cat("Primary Sample: Originated Loans, Complete Cases\n")
cat("============================================================\n\n")

print(desc_stats, row.names = FALSE)

cat("\nObservations in primary sample:", nrow(originated), "\n")



# ------------------------------------------------------------
# 4. MODEL 1 (restricted) vs MODEL 2 (full) -- nested comparison
# ------------------------------------------------------------
model_restricted <- lm(log_property_value ~ interest_rate + loan_amount + income_thousands,
                        data = originated)
model_full <- lm(log_property_value ~ interest_rate + income_thousands + loan_amount +
                    dti_numeric + loan_to_value_ratio + loan_term,
                  data = originated)

cat("\n========== MODEL 1: Restricted (interest_rate + loan_amount + income) ==========\n")
print(summary(model_restricted))

cat("\n========== MODEL 2: Full Controls (PRIMARY SPECIFICATION) ==========\n")
print(summary(model_full))

cat("\n---- Model selection: AIC / BIC ----\n")
cat("Model 1 (restricted) AIC:", AIC(model_restricted), " BIC:", BIC(model_restricted), "\n")
cat("Model 2 (full)       AIC:", AIC(model_full), " BIC:", BIC(model_full), "\n")
cat("(Lower AIC/BIC indicates better fit adjusted for model complexity.)\n")

cat("\n---- Nested model comparison: partial F-test ----\n")
print(anova(model_restricted, model_full))

# ------------------------------------------------------------
# 5. DIAGNOSTICS on the primary model
# ------------------------------------------------------------
cat("\n========== DIAGNOSTICS: Model 2 (Primary) ==========\n")

cat("\n---- VIF (multicollinearity) ----\n")
print(vif(model_full))

cat("\n---- Breusch-Pagan test (heteroscedasticity) ----\n")
print(bptest(model_full))

cat("\n---- Robust (HC3) standard errors ----\n")
print(coeftest(model_full, vcov = vcovHC(model_full, type = "HC3")))

cat("\n---- Shapiro-Wilk normality test on residuals (sampled) ----\n")
set.seed(1)
print(shapiro.test(sample(residuals(model_full), min(5000, nrow(originated)))))

cat("\n---- Cook's distance / influential points ----\n")
cooksd <- cooks.distance(model_full)
threshold <- 4 / nrow(originated)
n_influential <- sum(cooksd > threshold)
cat("Flagged influential (Cook's D >", round(threshold, 6), "):", n_influential,
    "of", nrow(originated), "(", round(100 * n_influential / nrow(originated), 2), "%)\n")
cat("Most influential observation:\n")
print(originated[which.max(cooksd), ])

cat("\nNote on autocorrelation: Durbin-Watson is not reported, as this is a\n")
cat("cross-sectional sample with no meaningful row ordering; DW is designed\n")
cat("for time-series or spatially ordered data and is not applicable here.\n")

# ------------------------------------------------------------
# 6. ROBUSTNESS CHECK A: exclude top 2% by property value
# ------------------------------------------------------------
cutoff_98 <- quantile(originated$property_value, 0.98)
trimmed <- originated %>% filter(property_value <= cutoff_98)
model_trimmed <- lm(log_property_value ~ interest_rate + income_thousands + loan_amount +
                       dti_numeric + loan_to_value_ratio + loan_term,
                     data = trimmed)

cat("\n========== ROBUSTNESS CHECK A: Excl. Top 2% by Property Value ==========\n")
cat("Cutoff: $", round(cutoff_98), " | N =", nrow(trimmed),
    "(excluded", nrow(originated) - nrow(trimmed), "observations)\n")
print(coeftest(model_trimmed, vcov = vcovHC(model_trimmed, type = "HC3")))
cat("Adjusted R-squared:", summary(model_trimmed)$adj.r.squared, "\n")

# ------------------------------------------------------------
# 7. ROBUSTNESS CHECK B: all applications (incl. denied/withdrawn)
# Reported for transparency. NOT the primary result -- interest_rate
# and property_value may not reflect a completed transaction for
# applications that were not originated.
# ------------------------------------------------------------
all_apps <- raw_data %>%
  mutate(
    property_value      = suppressWarnings(as.numeric(property_value)),
    interest_rate        = suppressWarnings(as.numeric(interest_rate)),
    loan_to_value_ratio   = suppressWarnings(as.numeric(loan_to_value_ratio)),
    loan_term             = suppressWarnings(as.numeric(loan_term)),
    income_thousands       = as.numeric(income),
    dti_numeric              = sapply(debt_to_income_ratio, dti_to_numeric)
  ) %>%
  select(property_value, interest_rate, loan_amount, income_thousands,
         dti_numeric, loan_to_value_ratio, loan_term) %>%
  drop_na() %>%
  mutate(log_property_value = log(property_value))

model_all_apps <- lm(log_property_value ~ interest_rate + income_thousands + loan_amount +
                        dti_numeric + loan_to_value_ratio + loan_term,
                      data = all_apps)

cat("\n========== ROBUSTNESS CHECK B: All Applications (incl. non-originated) ==========\n")
cat("N =", nrow(all_apps), "\n")
print(coeftest(model_all_apps, vcov = vcovHC(model_all_apps, type = "HC3")))



# ------------------------------------------------------------
# 8. PUBLICATION TABLES WITH HC3 ROBUST STANDARD ERRORS
#    AND HC3-ROBUST F / WALD TESTS
# ------------------------------------------------------------
# ------------------------------------------------------------
# A. CREATE HC3 COVARIANCE MATRICES
# ------------------------------------------------------------

vcov_restricted <- vcovHC(model_restricted, type = "HC3")
vcov_full       <- vcovHC(model_full, type = "HC3")
vcov_trimmed    <- vcovHC(model_trimmed, type = "HC3")
vcov_all_apps   <- vcovHC(model_all_apps, type = "HC3")


# ------------------------------------------------------------
# B. EXTRACT HC3 STANDARD ERRORS
# ------------------------------------------------------------

se_restricted <- sqrt(diag(vcov_restricted))
se_full       <- sqrt(diag(vcov_full))
se_trimmed    <- sqrt(diag(vcov_trimmed))
se_all_apps   <- sqrt(diag(vcov_all_apps))


# ------------------------------------------------------------
# C. EXTRACT HC3 P-VALUES
# ------------------------------------------------------------

p_restricted <- coeftest(
  model_restricted,
  vcov. = vcov_restricted
)[, 4]

p_full <- coeftest(
  model_full,
  vcov. = vcov_full
)[, 4]

p_trimmed <- coeftest(
  model_trimmed,
  vcov. = vcov_trimmed
)[, 4]

p_all_apps <- coeftest(
  model_all_apps,
  vcov. = vcov_all_apps
)[, 4]


# ------------------------------------------------------------
# D. HC3-ROBUST JOINT F-TEST FOR EACH MODEL
#
# Null hypothesis:
# All slope coefficients = 0
#
# Intercept is NOT tested
# ------------------------------------------------------------

hyp_restricted <- paste0(
  names(coef(model_restricted))[-1],
  " = 0"
)

hyp_full <- paste0(
  names(coef(model_full))[-1],
  " = 0"
)

hyp_trimmed <- paste0(
  names(coef(model_trimmed))[-1],
  " = 0"
)

hyp_all_apps <- paste0(
  names(coef(model_all_apps))[-1],
  " = 0"
)


wald_restricted <- linearHypothesis(
  model_restricted,
  hyp_restricted,
  vcov. = vcov_restricted,
  test = "F"
)

wald_full <- linearHypothesis(
  model_full,
  hyp_full,
  vcov. = vcov_full,
  test = "F"
)

wald_trimmed <- linearHypothesis(
  model_trimmed,
  hyp_trimmed,
  vcov. = vcov_trimmed,
  test = "F"
)

wald_all_apps <- linearHypothesis(
  model_all_apps,
  hyp_all_apps,
  vcov. = vcov_all_apps,
  test = "F"
)


# Extract HC3 robust F-statistics

F_restricted <- wald_restricted[2, "F"]
F_full       <- wald_full[2, "F"]
F_trimmed    <- wald_trimmed[2, "F"]
F_all_apps   <- wald_all_apps[2, "F"]


# Extract HC3 robust F-test p-values

Fp_restricted <- wald_restricted[2, "Pr(>F)"]
Fp_full       <- wald_full[2, "Pr(>F)"]
Fp_trimmed    <- wald_trimmed[2, "Pr(>F)"]
Fp_all_apps   <- wald_all_apps[2, "Pr(>F)"]


# ------------------------------------------------------------
# E. HC3-ROBUST JOINT TEST OF CONTROLS ADDED
#    TO THE FULL PRIMARY MODEL
#
# H0:
# dti_numeric = 0
# loan_to_value_ratio = 0
# loan_term = 0
# ------------------------------------------------------------

robust_added_controls <- linearHypothesis(
  model_full,
  c(
    "dti_numeric = 0",
    "loan_to_value_ratio = 0",
    "loan_term = 0"
  ),
  vcov. = vcov_full,
  test = "F"
)

print(robust_added_controls)

F_added_controls  <- robust_added_controls[2, "F"]
Fp_added_controls <- robust_added_controls[2, "Pr(>F)"]


# ------------------------------------------------------------
# F. GENERAL FORMATTING FUNCTIONS
# ------------------------------------------------------------

format_f <- function(x) {
  formatC(
    x,
    format = "f",
    digits = 2,
    big.mark = ","
  )
}


format_p <- function(x) {
  
  if (is.na(x)) {
    
    return("")
    
  } else if (x < 0.001) {
    
    return("<0.001")
    
  } else {
    
    return(
      formatC(
        x,
        format = "f",
        digits = 3
      )
    )
  }
}


stars <- function(p) {
  
  if (p < 0.01) {
    
    return("***")
    
  } else if (p < 0.05) {
    
    return("**")
    
  } else if (p < 0.10) {
    
    return("*")
    
  } else {
    
    return("")
  }
}


# ------------------------------------------------------------
# TABLE 1
# RESTRICTED VS FULL PRIMARY MODEL
# ------------------------------------------------------------

stargazer(
  model_restricted,
  model_full,
  
  type = "text",
  style = "aer",
  
  se = list(
    se_restricted,
    se_full
  ),
  
  p = list(
    p_restricted,
    p_full
  ),
  
  column.labels = c(
    "Restricted",
    "Full Controls (Primary)"
  ),
  
  dep.var.labels = "ln(Property Value)",
  
  covariate.labels = c(
    "Interest Rate",
    "Loan Amount",
    "Debt-to-Income",
    "Loan-to-Value",
    "Loan Term",
    "Income ($000s)"
  ),
  
  title = paste0(
    "Table 1: Interest Rate and Structural Determinants ",
    "of Property Value (Originated Loans, Maryland)"
  ),
  
  notes = paste0(
    "HC3 heteroscedasticity-robust standard errors are reported ",
    "in parentheses. Model-wide F-tests are calculated using ",
    "HC3 robust covariance estimates."
  ),
  
  notes.append = TRUE,
  
  digits = 5,
  digits.extra = 5,
  
  omit.stat = "f",
  
  add.lines = list(
    
    c(
      "HC3 Robust F Statistic",
      format_f(F_restricted),
      format_f(F_full)
    ),
    
    c(
      "HC3 Robust F p-value",
      format_p(Fp_restricted),
      format_p(Fp_full)
    )
  ),
  
  out = "C:/Users/peter/Downloads/Table1_Primary_Models_HC3.txt"
)


# ------------------------------------------------------------
# TABLE 2
# FULL SAMPLE VS EXCLUDING TOP 2%
#
# IMPORTANT:
# This table is printed directly from the actual model objects,
# HC3 SE vectors, and HC3 p-values.
#
# Stargazer is NOT used here because it was rounding the
# trimmed Loan Amount HC3 SE to 0.00000.
# ------------------------------------------------------------


# Exact coefficient order from the fitted models

table2_vars <- c(
  "interest_rate",
  "income_thousands",
  "loan_amount",
  "dti_numeric",
  "loan_to_value_ratio",
  "loan_term",
  "(Intercept)"
)


# Publication labels corresponding to those exact variables

table2_labels <- c(
  "Interest Rate",
  "Income (000s)",
  "Loan Amount",
  "Debt-to-Income",
  "Loan-to-Value",
  "Loan Term",
  "Constant"
)


# ------------------------------------------------------------
# TABLE 2 NUMBER FORMATTING
#
# Loan Amount is printed in scientific notation ONLY because
# its coefficient and HC3 SE are extremely small.
#
# The values themselves still come directly from R.
# ------------------------------------------------------------

format_table2_coef <- function(x, variable) {
  
  if (variable == "loan_amount") {
    
    return(
      formatC(
        x,
        format = "e",
        digits = 4
      )
    )
    
  } else {
    
    return(
      formatC(
        x,
        format = "f",
        digits = 5
      )
    )
  }
}


format_table2_se <- function(x, variable) {
  
  if (variable == "loan_amount") {
    
    return(
      paste0(
        "(",
        formatC(
          x,
          format = "e",
          digits = 4
        ),
        ")"
      )
    )
    
  } else {
    
    return(
      paste0(
        "(",
        formatC(
          x,
          format = "f",
          digits = 5
        ),
        ")"
      )
    )
  }
}


# ------------------------------------------------------------
# EXTRACT ACTUAL TABLE 2 MODEL RESULTS
# ------------------------------------------------------------

coef_full_table2 <- coef(model_full)
coef_trim_table2 <- coef(model_trimmed)


# ------------------------------------------------------------
# BUILD TABLE 2 BODY
# ------------------------------------------------------------

table2_output <- character()


table2_output <- c(
  table2_output,
  "Table 2: Sensitivity to Luxury-Property Truncation",
  "",
  sprintf(
    "%-25s %25s %25s",
    "",
    "Originated, Full Sample",
    "Originated, Excl. Top 2%"
  ),
  sprintf(
    "%-25s %25s %25s",
    "",
    "(1)",
    "(2)"
  ),
  paste(rep("-", 80), collapse = "")
)


for (i in seq_along(table2_vars)) {
  
  v <- table2_vars[i]
  
  coef_full_text <- paste0(
    format_table2_coef(
      coef_full_table2[v],
      v
    ),
    stars(
      p_full[v]
    )
  )
  
  coef_trim_text <- paste0(
    format_table2_coef(
      coef_trim_table2[v],
      v
    ),
    stars(
      p_trimmed[v]
    )
  )
  
  
  se_full_text <- format_table2_se(
    se_full[v],
    v
  )
  
  se_trim_text <- format_table2_se(
    se_trimmed[v],
    v
  )
  
  
  table2_output <- c(
    table2_output,
    
    sprintf(
      "%-25s %25s %25s",
      table2_labels[i],
      coef_full_text,
      coef_trim_text
    ),
    
    sprintf(
      "%-25s %25s %25s",
      "",
      se_full_text,
      se_trim_text
    ),
    
    ""
  )
}


# ------------------------------------------------------------
# TABLE 2 MODEL STATISTICS
# ------------------------------------------------------------

table2_output <- c(
  
  table2_output,
  
  sprintf(
    "%-25s %25s %25s",
    "HC3 Robust F Statistic",
    format_f(F_full),
    format_f(F_trimmed)
  ),
  
  sprintf(
    "%-25s %25s %25s",
    "HC3 Robust F p-value",
    format_p(Fp_full),
    format_p(Fp_trimmed)
  ),
  
  sprintf(
    "%-25s %25s %25s",
    "Observations",
    formatC(
      nobs(model_full),
      format = "d",
      big.mark = ","
    ),
    formatC(
      nobs(model_trimmed),
      format = "d",
      big.mark = ","
    )
  ),
  
  sprintf(
    "%-25s %25s %25s",
    "R2",
    formatC(
      summary(model_full)$r.squared,
      format = "f",
      digits = 5
    ),
    formatC(
      summary(model_trimmed)$r.squared,
      format = "f",
      digits = 5
    )
  ),
  
  sprintf(
    "%-25s %25s %25s",
    "Adjusted R2",
    formatC(
      summary(model_full)$adj.r.squared,
      format = "f",
      digits = 5
    ),
    formatC(
      summary(model_trimmed)$adj.r.squared,
      format = "f",
      digits = 5
    )
  ),
  
  sprintf(
    "%-25s %25s %25s",
    "Residual Std. Error",
    formatC(
      sigma(model_full),
      format = "f",
      digits = 5
    ),
    formatC(
      sigma(model_trimmed),
      format = "f",
      digits = 5
    )
  ),
  
  paste(rep("-", 80), collapse = ""),
  
  "Notes:",
  "*** Significant at the 1 percent level.",
  "** Significant at the 5 percent level.",
  "* Significant at the 10 percent level.",
  paste0(
    "HC3 heteroscedasticity-robust standard errors are reported ",
    "in parentheses. Model-wide F-tests are calculated using ",
    "HC3 robust covariance estimates. ",
    "Top 2% cutoff = $1,545,000."
  )
)


# ------------------------------------------------------------
# PRINT TABLE 2 TO R CONSOLE
# ------------------------------------------------------------

cat(
  paste(
    table2_output,
    collapse = "\n"
  )
)


# ------------------------------------------------------------
# SAVE TABLE 2 TO DOWNLOADS
# ------------------------------------------------------------

writeLines(
  table2_output,
  "C:/Users/peter/Downloads/Table2_Robustness_HC3.txt"
)


# ------------------------------------------------------------
# TABLE 3
# ORIGINATED LOANS VS ALL APPLICATIONS
# ------------------------------------------------------------

stargazer(
  model_full,
  model_all_apps,
  
  type = "text",
  style = "aer",
  
  se = list(
    se_full,
    se_all_apps
  ),
  
  p = list(
    p_full,
    p_all_apps
  ),
  
  column.labels = c(
    "Originated Loans",
    "All Applications"
  ),
  
  covariate.labels = c(
    "Interest Rate",
    "Income ($000s)",
    "Loan Amount",
    "Debt-to-Income",
    "Loan-to-Value",
    "Loan Term"
  ),
  
  dep.var.labels = "ln(Property Value)",
  
  title = "Table 3: Robustness to Alternative Sample Definition",
  
  notes = paste0(
    "HC3 heteroscedasticity-robust standard errors are reported ",
    "in parentheses. Model-wide F-tests are calculated using ",
    "HC3 robust covariance estimates."
  ),
  
  notes.append = TRUE,
  
  digits = 5,
  digits.extra = 5,
  
  omit.stat = "f",
  
  add.lines = list(
    
    c(
      "HC3 Robust F Statistic",
      format_f(F_full),
      format_f(F_all_apps)
    ),
    
    c(
      "HC3 Robust F p-value",
      format_p(Fp_full),
      format_p(Fp_all_apps)
    )
  ),
  
  out = "C:/Users/peter/Downloads/Table3_All_Applications_HC3.txt"
)


# ------------------------------------------------------------
# G. DISPLAY HC3 ROBUST JOINT TEST OF ADDED CONTROLS
# ------------------------------------------------------------

cat("\n\n============================================================\n")
cat("HC3-ROBUST JOINT TEST OF ADDED CONTROLS\n")
cat("H0: DTI = LTV = Loan Term = 0\n")
cat("============================================================\n")

print(robust_added_controls)

cat(
  "\nHC3 Robust F Statistic:",
  format_f(F_added_controls),
  "\n"
)

cat(
  "HC3 Robust p-value:",
  format_p(Fp_added_controls),
  "\n"
)


# ------------------------------------------------------------
# H. CONFIRM TABLE FILES SAVED
# ------------------------------------------------------------

cat(
  "\nDONE. HC3 publication tables saved to Downloads folder.\n"
)




# ------------------------------------------------------------
# ------------------------------------------------------------
# 9. DIAGNOSTIC PLOT: residuals vs. fitted (primary model)
# ------------------------------------------------------------

plot(
  fitted(model_full),
  residuals(model_full),
  main = "Residuals vs. Fitted Values\nPrimary Model, Originated Loans",
  xlab = "Fitted Values (Predicted ln(Property Value))",
  ylab = "Residuals",
  col = rgb(0, 0, 0.5, 0.08),
  pch = 16
)

abline(
  h = 0,
  col = "red",
  lwd = 2,
  lty = 2
)




# ------------------------------------------------------------
# 10. SENSITIVITY CHART: primary spec, full vs. trimmed
# ------------------------------------------------------------

ci_full <- coefci(
  model_full,
  vcov. = vcovHC(model_full, type = "HC3")
)["interest_rate", ]

ci_trimmed <- coefci(
  model_trimmed,
  vcov. = vcovHC(model_trimmed, type = "HC3")
)["interest_rate", ]

est_full <- coef(model_full)["interest_rate"]
est_trimmed <- coef(model_trimmed)["interest_rate"]

estimates <- c(est_full, est_trimmed)

lower_ci <- c(
  ci_full[1],
  ci_trimmed[1]
)

upper_ci <- c(
  ci_full[2],
  ci_trimmed[2]
)

plot(
  1:2,
  estimates,
  ylim = c(
    min(lower_ci) - 0.003,
    max(upper_ci) + 0.003
  ),
  xlim = c(0.5, 2.5),
  pch = 18,
  cex = 2,
  col = c("firebrick", "darkblue"),
  xaxt = "n",
  xlab = "Model Specification (Originated Loans Only, Full Controls)",
  ylab = "Interest Rate Coefficient on ln(Property Value)",
  main = "Estimated Interest Rate Coefficients\nAcross Alternative Sample Specifications"
)

segments(
  x0 = 1:2,
  y0 = lower_ci,
  x1 = 1:2,
  y1 = upper_ci,
  col = c("firebrick", "darkblue"),
  lwd = 2
)

segments(
  x0 = c(0.96, 1.96),
  y0 = lower_ci,
  x1 = c(1.04, 2.04),
  y1 = lower_ci,
  col = c("firebrick", "darkblue"),
  lwd = 2
)

segments(
  x0 = c(0.96, 1.96),
  y0 = upper_ci,
  x1 = c(1.04, 2.04),
  y1 = upper_ci,
  col = c("firebrick", "darkblue"),
  lwd = 2
)

points(
  1:2,
  estimates,
  pch = 18,
  cex = 2,
  col = c("firebrick", "darkblue")
)

axis(
  1,
  at = 1:2,
  labels = c(
    paste0("Full Sample\n(N=", nrow(originated), ")"),
    paste0("Excluding Top 2% of Property Values\n(N=", nrow(trimmed), ")")
  )
)

abline(
  h = 0,
  col = "red",
  lty = 2
)

