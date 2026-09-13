# Property, Mortgage, Lender, and Location Heterogeneity in the Mortgage Rate–Property Value Relationship

## Evidence from Linked HMDA and Assessment Data

**Author:** Peter Sarpong  
**Year:** 2026

## Overview

This repository contains the analysis code and supporting outputs for the study:

**“Property, Mortgage, Lender, and Location Heterogeneity in the Mortgage Rate–Property Value Relationship: Evidence from Linked HMDA and Assessment Data.”**

The study uses 2025 Maryland Home Mortgage Disclosure Act (HMDA) data linked to Maryland State Department of Assessments and Taxation (SDAT) property transaction records.

The analysis examines how the estimated relationship between mortgage interest rates and residential property values changes as increasingly detailed information about property characteristics, mortgage products, lenders, and location is incorporated into the regression specification.

The analysis is designed as a specification-sensitivity exercise. The estimated coefficients are interpreted as conditional associations and not as causal effects of mortgage interest rates on property values.

## Data

The analysis uses two primary data sources:

1. **2025 Maryland HMDA data**
2. **2025 Maryland SDAT residential property transaction data**

The HMDA data contain information on mortgage applications, originations, borrower characteristics, loan characteristics, lenders, and Census tracts.

The SDAT data provide independently measured property information, including transaction prices and physical property characteristics.

Raw HMDA and SDAT data are not redistributed in this repository.

## Record Linkage

HMDA mortgage records are linked to SDAT property transactions using Census tract and property-value information.

The linkage procedure identifies high-confidence statistical links by requiring compatibility between the HMDA-reported property-value interval and the SDAT transaction price and applying bidirectional uniqueness restrictions.

The matching population contains **62,305 HMDA originations**.

Among **62,112 tract-eligible observations**:

- **54,874 (88.1%)** had at least one compatible SDAT candidate.
- **18,568 (29.8%)** had exactly one compatible candidate.
- **36,306 (58.3%)** had multiple compatible candidates.

The final linkage procedure identifies **4,789 high-confidence statistical links**, representing **7.69%** of the HMDA matching population.

After applying complete-case requirements for the regression variables, the main linked analysis sample contains **4,167 observations**.

## Specification Framework

The analysis estimates a sequence of nested specifications.

**Model A — HMDA controls**

Controls for borrower income, debt-to-income category, loan term, occupancy, construction method, number of units, and county.

**Model B — Property characteristics**

Adds independently measured property characteristics, including square footage, property age, grade, and dwelling type.

**Model C — Mortgage-product characteristics**

Adds loan type and conforming-loan status.

**Model D — Lender heterogeneity**

Adds lender fixed effects.

**Model E — Local-market characteristics**

Adds tract-to-MSA income percentage and median age of the housing stock.

**Model F — Census tract fixed effects**

Uses Census tract fixed effects as a high-dimensional geographic specification while retaining property, mortgage-product, and lender controls.

## Main Results

The estimated coefficient on mortgage interest rates changes materially across specifications:

| Model | Rate coefficient | Approx. % association |
|------|-----------------:|----------------------:|
| A | -0.0809 | -7.77% |
| B | -0.0405 | -3.97% |
| C | -0.0483 | -4.72% |
| D | -0.0471 | -4.61% |
| E | -0.0427 | -4.18% |
| F | -0.0356 | -3.50% |

The largest change occurs when independently measured property characteristics are introduced between Models A and B.

The coefficient does not decline monotonically across every specification. In particular, adding mortgage-product controls in Model C increases the magnitude relative to Model B. Subsequent lender and geographic specifications produce further changes in the estimated association.

Across all specifications, the mortgage-rate coefficient remains negative, but its magnitude is sensitive to the information included in the model.

## Linkage-Selection Sensitivity Analysis

Because only a subset of HMDA observations can be linked with high confidence to SDAT property records, the study also evaluates observable linkage selection.

A logistic model estimates the probability that an HMDA observation is successfully linked using observable borrower, mortgage, property-value, and Census-tract characteristics.

Stabilized inverse-probability weights are then applied to Model E as a sensitivity analysis.

The linkage-weighted Model E produces an interest-rate coefficient of approximately **-0.0374**, corresponding to an estimated **3.67% lower property value per one-percentage-point higher mortgage rate**, conditional on the included covariates.

The unweighted Model E coefficient is approximately **-0.0427**.

This weighting exercise is interpreted only as a sensitivity analysis for observable linkage selection. It does not establish representativeness or correct for selection on unobserved characteristics.

## Statistical Inference

Heteroskedasticity-consistent HC3 standard errors are used for Models A–C.

Models D and E use lender-clustered standard errors.

Model F uses Census-tract-clustered standard errors.

The linkage-weighted Model E uses lender-clustered standard errors.

## Repository Files

### `Paper1_QREF_Version2_Analysis.R`

Main R analysis and reproduction script used for the Version 2 study. The script contains the data preparation, record linkage, regression specifications, lender and geographic analyses, and linkage-selection sensitivity analysis.

Additional CSV and figure files in the repository contain supporting model outputs and visualizations generated during the analysis.

## Software

The analysis was conducted in R.

Principal R packages used include:

- `readr`
- `dplyr`
- `tibble`
- `lmtest`
- `sandwich`

## Reproducibility

The analysis script requires the original HMDA and cleaned Maryland SDAT input files.

Because the raw source datasets are not redistributed in this repository, users must obtain the underlying data separately and place the required input files in the R working directory before running the analysis.

Required input filenames:

- `loan_purposes_1_state_MD.csv`
- `SDAT_2025_Market_Sales_With_Census_Tracts.csv`

## Interpretation

The results should not be interpreted as identifying the causal effect of mortgage interest rates on property values.

Instead, the analysis demonstrates that the estimated mortgage rate–property value relationship is sensitive to observed property characteristics, mortgage-product characteristics, lender heterogeneity, and geographic controls.

This specification sensitivity is the central empirical focus of the study.

## License

This repository is distributed under the MIT License.

## Citation

A permanent citation and DOI will be added after the Version 2 repository is archived through Zenodo.
