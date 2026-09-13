# Food Assistance and Refugee Welfare — South Sudan

Observational estimates of the relationship between WFP food assistance and household welfare among refugees in South Sudan, using propensity score matching to construct a comparison group from survey data.

Assistance is not randomly allocated. Households receiving general food distribution differ from those that do not on characteristics that independently predict welfare outcomes: household size, displacement history, location, assets. A naive comparison of outcomes between recipients and non-recipients measures selection as much as it measures effect. Matching on the estimated probability of receiving assistance, conditional on observed covariates, narrows that gap by comparing households that were similarly likely to be selected.

These are still observational estimates. Matching balances what was measured and cannot balance what was not. Where covariate balance after matching is poor, or where common support is thin, that is reported rather than smoothed over.

A second strand constructs a safety net dependency measure: the share of a household's total consumption accounted for by transfers. I drop credit from the denominator and express the result as a rate rather than a ratio, because the untransformed distribution is too skewed to interpret. That is a judgement call and is documented in the script.

## Methods

Propensity score matching (`MatchIt`), covariate balance diagnostics, covariate balancing propensity scores (`CBPS`), survey-weighted estimation, quantile regression.

## Files

| File | Purpose |
|---|---|
| `fsnms-psm-gfd-welfare.R` | Main analysis: matching on FSNMS data, welfare outcomes under general food distribution |
| `yida-psm-camp.R` | Parallel analysis for Yida camp |
| `safety-net-dependency-ratio.R` | Safety net dependency rate construction and distributional analysis |

Note that `safety-net-dependency-ratio.R` expects a merged FSNMS dataset produced by an upstream merge step that is not included in this repository.

## Data

These analyses use WFP Food Security and Nutrition Monitoring System household survey data and Yida camp survey data. Neither is redistributed here, for data protection reasons — this repository holds the analysis code only. To run it, place the SPSS files in `data/`:

| File | Description |
|---|---|
| `fsnms.23.sav` | FSNMS household survey round |
| `yida.sav` | Yida camp household survey |

## Reproducing

Paths resolve from the repository root through the `here` package.

```r
install.packages(c("tidyverse", "haven", "MatchIt", "CBPS", "survey",
                   "quantreg", "Hmisc", "data.table", "psych", "skimr",
                   "GGally", "DataExplorer", "ggthemes", "here"))

source("fsnms-psm-gfd-welfare.R")
```

## License

MIT — see [LICENSE](LICENSE).
