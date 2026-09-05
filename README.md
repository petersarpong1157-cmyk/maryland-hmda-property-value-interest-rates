# Interest Rate and Structural Determinants of Residential Property Value

## Overview

This repository contains the R code supporting the research paper:

**Interest Rate and Structural Determinants of Residential Property Value: A Cross-Sectional Analysis of Maryland HMDA Data**

**Author:** Peter Sarpong  
**Affiliation:** Independent Researcher, Washington, DC, USA

The study examines the cross-sectional association between mortgage interest rates and residential property values using 2025 Maryland Home Mortgage Disclosure Act (HMDA) loan-level data.

## Data

The analysis uses the 2025 Maryland HMDA loan-level dataset made publicly available through the Federal Financial Institutions Examination Council (FFIEC) and Consumer Financial Protection Bureau (CFPB).

The raw HMDA dataset is not redistributed in this repository.

The original dataset contains **120,115 observations and 99 variables**.

The primary analysis is restricted to originated loans (`action_taken == 1`) with complete information on the variables used in the primary model, resulting in an analytical sample of **57,138 loans**.

## Analysis

The primary outcome is the natural logarithm of reported property value.

The primary model estimates the association between property value and:

- Interest rate
- Applicant income
- Loan amount
- Debt-to-income ratio
- Loan-to-value ratio
- Loan term

Ordinary least squares (OLS) regression is used. Because the Breusch-Pagan test indicated heteroskedasticity, inference for the primary model uses **HC3 heteroskedasticity-robust standard errors**.

The analysis also includes robustness checks using a restricted specification, exclusion of the highest 2% of reported property values, and a broader sample of mortgage applications.

The analysis is observational and cross-sectional. Estimated coefficients are interpreted as conditional associations rather than causal effects.

## Code

The main analysis script is:

`HMDA_Publication_Analysis.R`

The script contains the data preparation, model estimation, diagnostic testing, and robustness analyses used for the study.

## Reproducibility

To reproduce the analysis:

1. Obtain the 2025 Maryland HMDA loan-level dataset from the official FFIEC/CFPB HMDA data platform.
2. Place the dataset in the appropriate local working directory.
3. Update the file path or working directory in `HMDA_Publication_Analysis.R` if necessary.
4. Run the R script.

The HMDA source data are not included in this repository.

## License

The code in this repository is released under the **MIT License**.

## Citation

Sarpong, P. (2026). *Interest Rate and Structural Determinants of Residential Property Value: A Cross-Sectional Analysis of Maryland HMDA Data.*
