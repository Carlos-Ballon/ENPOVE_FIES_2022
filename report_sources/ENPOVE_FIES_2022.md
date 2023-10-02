ENPOVE FIES 2022
================
Carlos Ballon-Salcedo
2023-09-20

- [Set global knitr options](#set-global-knitr-options)
- [Load packages](#load-packages)
- [gtsummary themes](#gtsummary-themes)
- [Import data](#import-data)
- [Process data](#process-data)
  - [Recode and relevel dataset](#recode-and-relevel-dataset)
  - [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Produce outputs](#produce-outputs)
  - [Characteristics of Venezuelan adults living in
    Peru](#characteristics-of-venezuelan-adults-living-in-peru)
    - [Unweighted descriptive table](#unweighted-descriptive-table)
    - [Expansion factor weighted descriptive
      table](#expansion-factor-weighted-descriptive-table)
    - [Confirm with `dplyr` package](#confirm-with-dplyr-package)
  - [Characteristics of Venezuelan adults according to Food Insecurity
    Experience Scale
    (FIES)](#characteristics-of-venezuelan-adults-according-to-food-insecurity-experience-scale-fies)
    - [Bivariate analysis](#bivariate-analysis)
    - [Confirm with `dplyr` package](#confirm-with-dplyr-package-1)
  - [Unadjusted models](#unadjusted-models)
    - [Proportional odds logistic regression model with
      `svyolr()`](#proportional-odds-logistic-regression-model-with-svyolr)
    - [Proportional odds logistic regression model with
      `polr()`](#proportional-odds-logistic-regression-model-with-polr)
    - [Proportional odds logistic regression model with
      `vglm`](#proportional-odds-logistic-regression-model-with-vglm)
    - [Confirm the traditional way](#confirm-the-traditional-way)
  - [Adjusted model](#adjusted-model)
    - [Proportional odds logistic regression model with
      `svyolr`](#proportional-odds-logistic-regression-model-with-svyolr-1)
    - [Proportional odds logistic regression model with
      `polr`](#proportional-odds-logistic-regression-model-with-polr-1)
    - [Proportional odds logistic regression model with
      `vglm`](#proportional-odds-logistic-regression-model-with-vglm-1)
    - [Multinomial logistic
      regression](#multinomial-logistic-regression)
- [Save outputs](#save-outputs)

# Set global knitr options

# Load packages

``` r
pacman::p_load(rio,
               here,
               tidyverse,
               survey,
               gtsummary,
               finalfit,
               flextable,
               kableExtra,
               sjPlot,
               broom,
               car,
               MASS, 
               VGAM,
               nnet)
```

# gtsummary themes

``` r
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_language(language = "en")
theme_gtsummary_compact()
```

# Import data

``` r
enpove <- import(here("data", "basefinal-enpove.dta"))
enpove <- janitor::clean_names(enpove)   #standardize the syntax of column names
```

# Process data

## Recode and relevel dataset

``` r
# Select variables
data <- enpove |>
  dplyr::select(insecat, estrato, alquilaviv, p204, edadcat, p402, p601, p501, 
                p501b, p501a, necalimsalud, sitmigratoria, residencia, p410, 
                estadocivil, factorfinal)

# Recode, relevel, and relabel
data_clear <- data |>
  mutate(NIVEL_EDUCATIVO = case_when(p501b == 1 ~ "No formal education or primary",
                                     p501b == 2 ~ "No formal education or primary",
                                     p501b == 3 ~ "No formal education or primary",
                                     p501b == 4 ~ "No formal education or primary",
                                     p501b == 5 ~ "No formal education or primary",
                                     p501b == 6 ~ "Secondary",
                                     p501b == 8 ~ "Secondary",
                                     p501b == 9 ~ "Higher",
                                     p501b == 10 ~ "Secondary",
                                     p501b == 11 ~ "Higher",
                                     p501b == 12 ~ "Higher",
                                     p501 == 1 ~ "No formal education or primary",
                                     p501 == 2 ~ "No formal education or primary",
                                     p501 == 3 ~ "No formal education or primary",
                                     p501 == 4 ~ "No formal education or primary",
                                     p501 == 5 ~ "No formal education or primary",
                                     p501 == 6 ~ "Secondary",
                                     p501 == 7 ~ "Secondary",
                                     p501 == 8 ~ "Higher",
                                     p501 == 9 ~ "Secondary",
                                     p501 == 10 ~ "Higher",
                                     p501 == 11 ~ "Higher",
                                     p501a == 3 ~ "No formal education or primary")) |>
  
  mutate(sexo = factor(p204) |>
           fct_recode("Male" = "1",
                      "Female" = "2") |>
           fct_relevel("Male", "Female") |>
           ff_label("Sex"),
         
         edad = factor(edadcat) |>
           fct_recode("18 to 25" = "1",
                      "26 to 35" = "2",
                      "36 to 50" = "3",
                      "50 to 65" = "4",
                      ">65" = "5") |>
           fct_relevel("18 to 25", "26 to 35", "36 to 50", "50 to 65", ">65") |>
           ff_label("Age (years)"),
         
         estrato = factor(estrato) |>
           fct_recode("Low" = "1",
                      "Medium" = "2",
                      "Medium" = "3",
                      "High" = "4",
                      "High" = "5") |>
           fct_relevel("High", "Medium", "Low") |>
           ff_label("Socioeconomic status"),
         
         educacion = factor(NIVEL_EDUCATIVO) |>
           fct_relevel("Higher", "Secondary", "No formal education or primary") |>
           ff_label("Educational level"),
         
         estadocivil = factor(estadocivil) |>
           fct_recode("Single" = "0",
                      "With a partner" = "1") |>
           fct_relevel("Single", "With a partner") |>
           ff_label("Marital status"),
         
         sitmigratoria = factor(sitmigratoria) |>
           fct_recode("Legal" = "0",
                      "Illegal" = "1") |>
           fct_relevel("Legal", "Illegal") |>
           ff_label("Migratory status"),
         
         residencia = factor(residencia) |>
           fct_recode("0-6" = "1",
                      "7-12" = "2",
                      "13-24" = "3",
                      ">24" = "4") |>
           fct_relevel(">24",  "13-24", "7-12", "0-6") |>
           ff_label("Residence (months)"),
         
         vivienda = factor(alquilaviv) |>
           fct_recode("Own" = "0",
                      "Rented" = "1") |>
           fct_relevel("Own", "Rented") |>
           ff_label("Housing"),
         
         enf_cronica = factor(p402) |>
           fct_recode("Yes" = "1",
                      "No" = "2") |>
           fct_relevel("Yes", "No") |>
           ff_label("Chronic disease"),
         
         seguro = factor(necalimsalud) |>
           fct_recode("Yes" = "0",
                      "No" = "1") |>
           fct_relevel("Yes", "No") |>
           ff_label("Health insurance"),
         
         COVID19 = factor(p410) |>
           fct_recode("Current" = "1",
                      "Previously" = "2",
                      "No" = "3",
                      "Do not know" = "4") |>
           fct_relevel("No", "Current", "Previously", "Do not know") |>
           ff_label("COVID-19"), 
         
         trabajo = factor(p601) |>
           fct_recode("Yes" = "1",
                      "No" = "2") |>
           fct_relevel("Yes", "No") |>
           ff_label("Economic income"),
         
         FIES = factor(insecat) |>
           fct_recode("No" = "0",
                      "Slight" = "1",
                      "Moderate" = "2",
                      "Severe" = "3") |>
           fct_relevel("No", "Slight", "Moderate", "Severe") |>
           ff_label("FIES")
         ) |>
  
  # Select variables
  dplyr::select(sexo, edad, estrato, educacion, estadocivil, sitmigratoria, 
                residencia, vivienda, enf_cronica, seguro, COVID19, trabajo, 
                FIES, factorfinal)
```

## Exploratory Data Analysis (EDA)

``` r
# Structure
glimpse(data_clear)

# Unique values
lapply(data_clear, function(x) unique(x))

# Missing values
lapply(data_clear, function(x) sum(is.na(x)))
lapply(data_clear, function(x) sum(complete.cases(x)))

# Tables
lapply(data_clear[1:13], function(x) table(x))
```

# Produce outputs

## Characteristics of Venezuelan adults living in Peru

In this subsection we use `tbl_summary()` to make the unweighted
descriptive analysis,

### Unweighted descriptive table

``` r
data_clear |>
  tbl_summary(percent = "column", include = c(sexo:FIES),
              statistic = list(all_categorical() ~ "{n} ({p})"),
              type   = all_categorical() ~ "categorical",
              digits = all_categorical() ~ c(0, 1),
              missing_text = "(Missing)") |>
  bold_labels() |>
  add_ci(statistic = list(all_categorical() ~ "{conf.low} - {conf.high}"), 
         style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 1))) |>
  modify_caption("Table S1. Characteristics of Venezuelan adults living in Peru (N = {N})") |>
  
  # HTML table with flextable
  as_flex_table() |>
  theme_booktabs() |>
  font(part = "all", fontname = "Segoe UI") |>
  fontsize(size = 10, part = "all")
```

### Expansion factor weighted descriptive table

Use the `show_header_names()` to see the column names that can be
modified

``` r
tbl_svysummary <-
  survey::svydesign(ids = ~1, 
                    data = data_clear, 
                    weights = ~factorfinal) |>
  tbl_svysummary(percent = "column", include = c(sexo:FIES), 
                 statistic = list(all_categorical() ~ "{n_unweighted} ({p})"),
                 type   = all_categorical() ~ "categorical",
                 digits = all_categorical() ~ c(0, 1),
                 missing_text = "(Missing)") |>
  add_ci(method = list(all_categorical() ~ "svyprop.logit"),
         statistic = list(all_categorical() ~ "{conf.low} - {conf.high}"),
         style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 1))) |>
  bold_labels() |>
  modify_column_alignment(columns = everything(), align = "left") |>
  modify_caption("**Table 1**. Characteristics of Venezuelan adults living in Peru (N = 7727)") |>
  modify_header(label = "**Characteristic**", 
                stat_0 ~ "**n (%)**", 
                ci_stat_0 = "**95% CI**") |>
  modify_spanning_header(c("stat_0", "ci_stat_0") ~ "**Weighted proportion**") |>
  modify_footnote(stat_0 ~ "n (%) = count unweighted (weighted percentage)")

# View
tbl_svysummary
```

### Confirm with `dplyr` package

``` r
data_clear |>
  dplyr::select(sexo, edad, estrato, educacion, estadocivil, sitmigratoria, residencia, 
                vivienda, enf_cronica, seguro, COVID19, trabajo, FIES, factorfinal) |>
  pivot_longer(cols = c(sexo:trabajo), names_to = "Variable", values_to = "Valor") |>
  group_by(Variable, Valor) |>
  summarise(n = n(),
            weighted_prop = sum(factorfinal)) |>
  mutate(weighted_prop = round(weighted_prop/sum(weighted_prop)*100, 2))
```

## Characteristics of Venezuelan adults according to Food Insecurity Experience Scale (FIES)

Package `survey` and `gtsummary` are required.

### Bivariate analysis

Chi-squared test with Rao & Scott’s second-order correction and
Holm-Bonferroni method to adjust for multiple tests. Use (García-Pérez
2023) to understand the goodness of fit.

``` r
tbl_svysummary_bivariate <-
  survey::svydesign(ids = ~1, 
                    data = data_clear,
                    weights = ~factorfinal) |>
  tbl_svysummary(by = FIES, 
                 percent = "row", 
                 include = c(sexo:FIES), 
                 statistic = list(all_categorical() ~ "{p}"),
                 type   = all_categorical() ~ "categorical",
                 digits = all_categorical() ~ 2,
                 missing_text = "(Missing)") |>
  add_ci(method = list(all_categorical() ~ "svyprop.logit"),
         statistic = list(all_categorical() ~ "{conf.low} - {conf.high}"),
         style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 2)),
         conf.level = 0.95) |>
  bold_labels() |>
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) |>
  add_q(method = "holm") |>
  bold_p(t = 0.05) |>
  modify_column_alignment(columns = everything(), align = "left") |>
  modify_caption("**Table 2**. Characteristics of Venezuelan adults according to FIES") |>
  modify_header(all_stat_cols() ~ "**{level}**, n = {n_unweighted} ({style_percent(p)}%)")

# View
tbl_svysummary_bivariate
```

### Confirm with `dplyr` package

``` r
# one by one
data_clear |>
  dplyr::select(sexo, edad, estrato, educacion, estadocivil, sitmigratoria, residencia, 
                vivienda, enf_cronica, seguro, COVID19, trabajo, FIES, factorfinal) |>
  group_by(FIES) |>
  summarise(n = n(),
            weighted_prop = sum(factorfinal)) |>
  mutate(weighted_prop = round(weighted_prop/sum(weighted_prop)*100, 2))

# whole
data_clear |>
  dplyr::select(sexo, edad, estrato, educacion, estadocivil, sitmigratoria, residencia, 
                vivienda, enf_cronica, seguro, COVID19, trabajo, FIES, factorfinal) |>
  pivot_longer(cols = c(sexo:trabajo), names_to = "Variable", values_to = "Valor") %>%
  group_by(Variable, Valor, FIES) %>%
  summarise(n = n(),
            weighted_prop = sum(factorfinal)) |>
  mutate(weighted_prop = round(weighted_prop/sum(weighted_prop)*100, 2))

# Create a vector of independent variable names
independent_vars <- c("sexo", "edad", "estrato", "educacion", "estadocivil", "sitmigratoria", 
                      "residencia", "vivienda", "enf_cronica", "seguro", "COVID19", "trabajo")

# Loop over the independent variables and perform chi-squared test
lapply(data_clear[, independent_vars], function(x) chisq.test(data_clear$FIES, x))
```

## Unadjusted models

We performed unadjusted ordinal logistic regression models with
proportional odds

### Proportional odds logistic regression model with `svyolr()`

Let’s use `svyolr()` from `survey` package to fit cumulative link
models, specifically proportional odds sample-weighted

``` r
# survey design
design <- svydesign(ids = ~1, data = data_clear, weights = ~factorfinal)

# Univariate regression models
univ_tab <-
  tbl_uvregression(
    data = design,
    method = svyolr,
    method.args = list(method = "logistic"),
    include = c(sexo:trabajo),
    y = FIES,
    hide_n = TRUE,
    exponentiate = TRUE,
    conf.int = TRUE,
    ci_method = "wald",
    tidy_fun = broom.helpers::tidy_parameters,
    add_estimate_to_reference_rows = FALSE,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun = ~style_number(.x, digits = 2)) |>
  bold_labels() |>
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE)
```

### Proportional odds logistic regression model with `polr()`

Let’s use `polr()` from `MASS` package to fit proportional odds logistic
regression models sample-weighted. However, `polr()` function is
supported by `tbl_regression()`, but is not optimized for
`tbl_uvregression()`. Alternatively, it’s possible to create multiple
univariate models using loops and a tidy tibble object by creating a
function that includes `tidy()` from the `broom` package.

Using the function created is not possible to obtain p-values, so this
test must be run using other functions such as `dropterm()`, `drop1()`,
`Anova()` and `lrtest()`. It’s not feasible to extract the p-values to
generate a new column and append it to the tidy tibble object. Although
it is possible to extract the p-value using the `$` or `[row, column]`
operators, it varies considerably even after assigning the results to a
tibble, data.frame, or list.

``` r
# Univariate regression models with tbl_regression
polr(FIES ~ sexo, 
     weights = factorfinal,
     data = data_clear, 
     Hess = TRUE, 
     method = "logistic") |>
  tbl_regression(exponentiate = TRUE,
                 conf.int = TRUE,
                 ci_method = "wald",
                 tidy_fun = broom.helpers::tidy_parameters,
                 add_estimate_to_reference_rows = FALSE,
                 pvalue_fun = ~style_pvalue(.x, digits = 3),
                 estimate_fun = ~style_number(.x, digits = 2))

# Univariate regression models with tidy

## Build models
exp1 <- polr(FIES ~ sexo, weights = factorfinal, data = data_clear, Hess = TRUE, method = "logistic")
exp2 <- polr(FIES ~ edad, weights = factorfinal, data = data_clear, Hess = TRUE, method = "logistic")
exp3 <- polr(FIES ~ estrato, weights = factorfinal, data = data_clear, Hess = TRUE, method = "logistic")

## Gathering models in a list
polr_results <- list(exp1, exp2, exp3)

## Create loop function
tidy_table <- function(polr_results) {
  lapply(polr_results,  function(x) {
    broom::tidy(x, conf.int = TRUE, exponentiate = TRUE) |>
      dplyr::filter(coef.type == "coefficient") |> # Filter coefficients
      dplyr::mutate(across(where(is.numeric), round, digits = 2))
  })
}

## Run the function on the univariate regression models
tidy_table(list(exp1, exp2, exp3)) |>
  bind_rows()

# p-values

## Create loop function with drop1
polr_analysis <- function(polr_results) {
  drop1(polr_results, test = "Chisq")
}

## Run the function on the univariate regression models
lapply(polr_results, polr_analysis)

## Short form
lapply(polr_results, function(x) drop1(x, test = "Chisq"))
```

### Proportional odds logistic regression model with `vglm`

Let’s use `vglm` from `VGAM` package to fit proportional odds logistic
regression models sample-weighted. `VGAM::vglm` has limited support by
`tbl_regression()` and `tbl_uvregression()`. Instead, the functions
`summary()`, `coef()`, `confint()`, and `exp()` can be used to extract
the coefficients and their confidence intervals, but the
`sjPlot::tab_model()` function provides a better visualization.

``` r
# Build models
univ_vglm <- vglm(FIES ~ sexo, weights = factorfinal, data = data_clear, family = propodds)

# Table
tab_model(univ_vglm, show.intercept = F)
```

### Confirm the traditional way

``` r
# Select explanatory variables and weights
data_loop = data_clear |>
  dplyr::select(FIES, sexo, edad, estrato, educacion, estadocivil, sitmigratoria, 
                residencia, vivienda, enf_cronica, COVID19, trabajo)

weights = data_clear$factorfinal

# Loop to confirm results from svyolr
lapply(colnames(data_loop)[-1], 
       function(x) svyolr(as.formula(paste("FIES ~", x)), design = design) |>
         coef() |>
         exp())

# Loop to confirm results from polr
map(colnames(data_loop)[-1], 
    ~ polr(as.formula(paste("FIES ~", .x)), weights = weights, data = data_loop, method = "logistic") |>
      coef() |>
      exp())
```

## Adjusted model

### Proportional odds logistic regression model with `svyolr`

``` r
# survey design
design <- svydesign(ids = ~1, data = data_clear, weights = ~factorfinal)

# Multivariable regression model
mv_tab <- svyolr(FIES ~ sexo + estrato + educacion + estadocivil + sitmigratoria + 
                   residencia + vivienda + enf_cronica + seguro + trabajo, 
                 method = "logistic", design = design) |> 
  tbl_regression(exponentiate = TRUE,
                 conf.int = TRUE,
                 tidy_fun = broom.helpers::tidy_parameters,
                 pvalue_fun = ~style_pvalue(.x, digits = 3),
                 estimate_fun = ~style_number(.x, digits = 2)) |>
  add_vif() |>
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE)
```

### Proportional odds logistic regression model with `polr`

``` r
# Multivariable regression model
mv_tab_polr <- polr(FIES ~ sexo + estrato + educacion + estadocivil + sitmigratoria + 
                      residencia + vivienda + enf_cronica + seguro + trabajo, 
                    data = data_clear, weights = factorfinal, Hess = TRUE, method = "logistic") |>
  tbl_regression(exponentiate = TRUE, 
                 conf.int = TRUE,
                 tidy_fun = broom.helpers::tidy_parameters,
                 pvalue_fun = ~style_pvalue(.x, digits = 3),
                 estimate_fun = ~style_number(.x, digits = 2)) |>
  add_vif() |>
  modify_column_alignment(columns = everything(), align = "left") |>
  modify_caption("Table S2. Factors associated with FIES by proportional odds logistic regression")
```

### Proportional odds logistic regression model with `vglm`

Alternatively, you can create a tibble object using the `tidy()`
function, which is supported by `vglm()`.

``` r
mv_vglm <- vglm(FIES ~ sexo + estrato + educacion + estadocivil + sitmigratoria + 
                      residencia + vivienda + enf_cronica + seguro + trabajo, 
                      data = data_clear, weights = factorfinal, family = propodds)

tab_model(mv_vglm, show.intercept = F)
```

``` r
# Modifications
tbl_merge <-
  gtsummary::tbl_merge(tbls = list(univ_tab, mv_tab),
            tab_spanner = c("**Unadjusted Analysis**", "**Adjusted Analysis**")) |>
  modify_header(estimate_1 = "**cOR** **(95% CI)**",
                estimate_2 = "**aOR** **(95% CI)**",
                p.value_1 = "**p-value**",
                p.value_2 = "**p-value**",
                GVIF_2 = "**GVIF**",
                aGVIF_2 = "**aGVIF**") |>
  modify_footnote(estimate_1 = "cOR = Crude Odds Ratio, CI = Confidence Interval",
                  estimate_2 = "aOR = Adjusted Odds Ratio, CI = Confidence Interval",
                  GVIF_2 = "GVIF = Generalized Variance Inflation Factor",
                  aGVIF_2 = "aVIF = Adjusted GVIF") |>
  modify_caption("**Table 3.** Factors associated with FIES by Ordinal Logistic Regression")

# View
tbl_merge
```

### Multinomial logistic regression

``` r
# Multivariable regression model
mv_multinom_tab <- multinom(FIES ~ sexo + edad + estrato + educacion + estadocivil + sitmigratoria + 
                              residencia + vivienda + enf_cronica + seguro + trabajo, 
            data = data_clear, weights = factorfinal) |>
  tbl_regression(exponentiate = TRUE, 
                 pvalue_fun = ~style_pvalue(., digits = 3)) |>
  modify_caption("Table S3. Factors associated with FIES by multinomial logistic regression")

# HTML table with kableExtra
mv_multinom_tab |>
  as_kable_extra() |>
  kable_styling(bootstrap_options = c("hover", "striped", "condensed"), 
                position = "center", fixed_thead = TRUE, font_size = 14) |>
  kable_classic_2("hover", full_width = FALSE, html_font = "Segoe UI") |>
  row_spec(0, bold = TRUE, align = "left")
```

# Save outputs

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-garcía-pérez2023" class="csl-entry">

García-Pérez, Miguel A. 2023. “Use and Misuse of Corrections for
Multiple Testing.” *Methods in Psychology* 8 (November): 100120.
<https://doi.org/10.1016/j.metip.2023.100120>.

</div>

</div>
