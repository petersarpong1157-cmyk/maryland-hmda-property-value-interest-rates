# Property, Mortgage, Lender, and Location Heterogeneity in the Mortgage Rate–Property Value Relationship

## Evidence from Linked HMDA and Assessment Data

This repository contains the replication materials for the revised Version 2 analysis of the relationship between mortgage interest rates and residential property values using linked 2025 Maryland HMDA and Maryland State Department of Assessments and Taxation (SDAT) data.

The revised analysis emphasizes **specification sensitivity rather than causal interpretation**. It examines how the estimated mortgage rate–property value association changes as property characteristics, mortgage-product characteristics, lender heterogeneity, local-market controls, and Census tract fixed effects are introduced.

## Data

The analysis uses two source files:

1. `loan_purposes_1_state_MD.csv` — 2025 Maryland HMDA loan-level data.
2. `SDAT_2025_Market_Sales_With_Census_Tracts.csv` — cleaned 2025 Maryland SDAT residential transaction data with Census tract identifiers.

The raw source data are not redistributed in this repository. Users should obtain the underlying public data from the original data providers and place the required files in the R working directory before running the reproduction script.

## Record Linkage

The HMDA–SDAT linkage procedure uses Census tract and rounded property value as blocking variables, together with county and occupancy restrictions. A high-confidence statistical link is retained only when the candidate pair is unique in both directions within the tract/property-value block.

Key linkage counts are:

- 62,305 HMDA observations in the matching population.
- 62,112 tract-eligible HMDA observations.
- 54,874 observations with at least one compatible SDAT candidate (88.1% of tract-eligible observations).
- 18,568 observations with exactly one compatible candidate (29.8%).
- 36,306 observations with multiple compatible candidates (58.3%).
- 4,789 exact bidirectional high-confidence statistical links (7.69% of the matching population).
- 4,167 observations in the complete linked regression sample.

## Version 2 Specification Ladder

The revised analysis estimates a sequence of nested specifications on the same 4,167-observation linked sample:

- **Model A — HMDA controls:** borrower income, debt-to-income category, applicant credit-score type, loan purpose, occupancy, and county fixed effects.
- **Model B — Property controls:** Model A plus living area, lot size, property age, bedrooms, bathrooms, and property-type indicators.
- **Model C — Mortgage-product controls:** Model B plus loan-type and conforming-loan-limit indicators.
- **Model D — Lender fixed effects:** Model C plus lender fixed effects.
- **Model E — Local-market controls:** Model D plus tract income and housing-age measures.
- **Model F — Census tract fixed effects:** a geographic stress-test specification replacing the local-market variables with Census tract fixed effects.

Loan amount and loan-to-value ratio are excluded from the preferred property-value specifications because they are mechanically or jointly related to the property-value outcome and mortgage pricing.

## Main Results

The estimated coefficient on mortgage interest rate changes materially across specifications:

| Model | Interest-rate coefficient | Approximate value difference for +1 percentage point |
|---|---:|---:|
| A — HMDA controls | -0.0809 | -7.77% |
| B — Property controls | -0.0405 | -3.97% |
| C — Mortgage-product controls | -0.0483 | -4.72% |
| D — Lender fixed effects | -0.0471 | -4.61% |
| E — Local-market controls | -0.0427 | -4.18% |
| F — Census tract fixed effects | -0.0356 | -3.50% |

The largest change occurs when independently measured property characteristics are added between Models A and B. Mortgage-product controls increase the magnitude somewhat, while lender and geographic controls subsequently attenuate the conditional association. The estimates are interpreted as **conditional associations**, not causal effects.

## Linkage-Selection Sensitivity

Because only a subset of HMDA records can be linked uniquely to SDAT transactions, the revised analysis also estimates an observable linkage-selection model and constructs stabilized inverse-probability weights.

The stabilized weights have a median of approximately 0.90, a 99th percentile of approximately 2.25, and a maximum of approximately 3.85. The effective weighted sample size is approximately 3,561 observations.

Applying these weights to Model E produces an interest-rate coefficient of approximately **-0.0374**, corresponding to about **3.67% lower property value** for a one-percentage-point higher mortgage rate. This is smaller in magnitude than the unweighted Model E estimate of -0.0427. The weighting exercise is an **observable-selection sensitivity analysis only** and does not establish representativeness or correct for unobserved selection.

## Inference

- Models A–C use HC3 heteroskedasticity-robust standard errors.
- Models D and E use lender-clustered HC1 standard errors.
- Model F uses Census-tract-clustered HC1 standard errors.
- The linkage-weighted Model E sensitivity uses lender-clustered HC1 standard errors.

## Reproducibility

Run `Paper1_QREF_Version2_Reproduction.R` in R with the two required input files in the working directory. The script reproduces the record linkage, linked analysis sample, Version 2 specification ladder, lender and tract diagnostics, linkage-selection model, inverse-probability weighting sensitivity, and final model-result export.

The final model results are provided in `Paper1_QREF_Version2_Model_Results.csv`.

## Repository Files

- `Paper1_QREF_Version2_Reproduction.R` — Version 2 analysis and reproduction script.
- `Paper1_QREF_Version2_Model_Results.csv` — final Version 2 model results.
- `README.md` — documentation for the replication package.
- `LICENSE` — repository license.

## Citation

Version 2.1 of these replication materials is archived on Zenodo.

**DOI:** 10.5281/zenodo.22740270

## Interpretation

The analysis is designed to evaluate how the mortgage rate–property value relationship changes as richer information is incorporated into the empirical specification. The results should not be interpreted as identifying the causal effect of mortgage interest rates on property values.
