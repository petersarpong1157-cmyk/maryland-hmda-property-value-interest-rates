# Mortgage Interest Rates and Residential Property Values: Evidence from Linked Maryland HMDA and SDAT Data

## Overview

This repository contains the R code supporting the research paper:

**Mortgage Interest Rates and Residential Property Values: Evidence from Linked Maryland HMDA and SDAT Data**

**Author:** Peter Sarpong  
**Affiliation:** Independent Researcher, Gaithersburg, Maryland, USA

The study examines the cross-sectional association between mortgage interest rates and residential property values using 2025 Maryland Home Mortgage Disclosure Act (HMDA) data.

A linked-sample robustness analysis incorporates property characteristics from Maryland State Department of Assessments and Taxation (SDAT) residential transaction data.

The analysis is observational and cross-sectional. Estimated coefficients are interpreted as conditional associations rather than causal effects.

## Data

### HMDA

The analysis uses the 2025 Maryland HMDA loan-level dataset made publicly available through the Federal Financial Institutions Examination Council (FFIEC) and Consumer Financial Protection Bureau (CFPB).

The raw Maryland HMDA file contains **120,115 observations**.

After restricting the data to originated loans with a valid numeric reported property value, the HMDA matching population contains **62,305 observations**.

The complete-case sample used in the primary regression contains **58,587 observations**.

### Maryland SDAT

The property-control analysis uses a processed Maryland SDAT residential transaction file with Census tract assignments.

The processed SDAT matching file contains **63,304 residential transaction records** across **1,439 Census tracts**.

The current analysis script begins with the processed, tract-assigned SDAT file rather than the original raw SDAT source files.

The source HMDA and SDAT datasets are not redistributed in this repository.

## HMDA-SDAT Linkage

HMDA and SDAT records are linked using Census tract and property value/sale price information.

The analysis first identifies records with an exact match on:

- Census tract
- HMDA reported property value and SDAT sale price

The linkage then requires uniqueness in both directions: an HMDA record must correspond to exactly one SDAT candidate, and that SDAT record must correspond to exactly one HMDA candidate.

This procedure produces **4,789 bidirectionally unique exact tract/value links**, with no duplicate HMDA or SDAT identifiers in the linked sample.

Because HMDA reported property value and SDAT sale price are not conceptually identical measures in every transaction, these records are treated as **high-confidence links rather than definitively identified property matches**.

After requiring complete information for the linked regression specifications, the final same-sample property-control analysis contains **4,167 observations**.

## Primary Analysis

The dependent variable is the natural logarithm of reported property value.

The preferred full-HMDA specification includes:

- Mortgage interest rate
- Log applicant income
- Debt-to-income ratio as a categorical variable
- Loan term
- Occupancy type
- Construction method
- Total units
- County fixed effects

Loan amount and loan-to-value ratio are **not included** in the preferred property-value specification.

The model is estimated using ordinary least squares (OLS), with **HC3 heteroskedasticity-robust standard errors**.

The primary model uses **58,587 observations**.

The estimated interest-rate coefficient is **-0.0369**. This corresponds to an exact semi-log association of approximately **-3.62%** for a one-percentage-point higher observed mortgage interest rate, conditional on the included covariates.

The adjusted R-squared is **0.637**.

## Linked Property-Control Analysis

The linked-sample analysis estimates two models on the same **4,167 observations**.

The first uses the HMDA controls from the primary specification. The estimated interest-rate coefficient is **-0.0809**, corresponding to approximately **-7.77%**, with an adjusted R-squared of **0.703**.

The second adds the following SDAT property characteristics:

- Log square footage
- Property age
- Property grade
- Dwelling type

After adding these property controls, the estimated interest-rate coefficient is **-0.0405**, corresponding to approximately **-3.97%**, and the adjusted R-squared increases to **0.856**.

The reduction in the magnitude of the interest-rate coefficient after adding observed property characteristics indicates that property characteristics account for a meaningful portion of the simpler linked-sample association.

The linked sample is used as a robustness analysis rather than as the primary sample because the linkage procedure selects a subset of HMDA observations and the linked records tend to represent larger and higher-value properties.

## Code

The final analysis script is:

`Paper1_Final_Analysis.R`

The script performs:

- HMDA sample construction
- HMDA-SDAT exact tract/value linkage
- Bidirectional uniqueness checks
- Linked-sample selection diagnostics
- Structural property comparisons
- Primary HMDA regression
- Same-sample linked regressions
- HC3 robust inference
- Final result and coefficient exports
- Reproducibility checks

## Required R Packages

The final analysis uses:

- `readr`
- `dplyr`
- `tibble`
- `lmtest`
- `sandwich`

## Reproducibility

To reproduce the final analysis:

1. Obtain the 2025 Maryland HMDA loan-level data.
2. Prepare the Maryland SDAT residential transaction data with Census tract assignments using the preprocessing procedures described in the research project.
3. Place the following two files in the R working directory:

   `loan_purposes_1_state_MD.csv`

   `SDAT_2025_Market_Sales_With_Census_Tracts.csv`

4. Place `Paper1_Final_Analysis.R` in the same working directory.
5. Install the required R packages if necessary.
6. Run the complete R script.

The final script was verified to reproduce the principal sample counts, linkage counts, model estimates, and output files reported in the revised analysis.

## Main Output Files

The script produces the following principal result files:

- `Paper1_Final_Core_Model_Comparison.csv`
- `Paper1_Final_Core_Model_Comparison_With_Percent_Effects.csv`
- `Paper1_Final_Primary_HC3_Coefficients.csv`
- `Paper1_Final_Linked_Base_HC3_Coefficients.csv`
- `Paper1_Final_Linked_Property_HC3_Coefficients.csv`
- `HMDA_SDAT_Final_Same_Sample_Nested_Models.csv`

## License

The code in this repository is released under the MIT License.

## Citation

Sarpong, P. (2026). *Mortgage Interest Rates and Residential Property Values: Evidence from Linked Maryland HMDA and SDAT Data*.
