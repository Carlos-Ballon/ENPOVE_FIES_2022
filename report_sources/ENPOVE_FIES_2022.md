ENPOVE FIES 2022
================
Carlos Ballon-Salcedo
2023-09-19

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
               sjPlot,
               broom,
               car,
               MASS, 
               VGAM,
               nnet)
```

# gtsummary themes

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
  modify_caption("Table S1. Characteristics of Venezuelan adults living in Peru (N = {N})")
```

<div id="ohdtfhfyuo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}
&#10;#ohdtfhfyuo .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ohdtfhfyuo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ohdtfhfyuo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ohdtfhfyuo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ohdtfhfyuo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ohdtfhfyuo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ohdtfhfyuo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ohdtfhfyuo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ohdtfhfyuo .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ohdtfhfyuo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ohdtfhfyuo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ohdtfhfyuo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ohdtfhfyuo .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ohdtfhfyuo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ohdtfhfyuo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ohdtfhfyuo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ohdtfhfyuo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ohdtfhfyuo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ohdtfhfyuo .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ohdtfhfyuo .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ohdtfhfyuo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 1px;
  padding-right: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ohdtfhfyuo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ohdtfhfyuo .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ohdtfhfyuo .gt_left {
  text-align: left;
}
&#10;#ohdtfhfyuo .gt_center {
  text-align: center;
}
&#10;#ohdtfhfyuo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ohdtfhfyuo .gt_font_normal {
  font-weight: normal;
}
&#10;#ohdtfhfyuo .gt_font_bold {
  font-weight: bold;
}
&#10;#ohdtfhfyuo .gt_font_italic {
  font-style: italic;
}
&#10;#ohdtfhfyuo .gt_super {
  font-size: 65%;
}
&#10;#ohdtfhfyuo .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}
&#10;#ohdtfhfyuo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ohdtfhfyuo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ohdtfhfyuo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ohdtfhfyuo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ohdtfhfyuo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ohdtfhfyuo .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption>Table S1. Characteristics of Venezuelan adults living in Peru (N = 7727)</caption>
  &#10;  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 7,727&lt;/strong&gt;"><strong>N = 7,727</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Sex, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_0" class="gt_row gt_center">3,737 (48.4)</td>
<td headers="ci_stat_0" class="gt_row gt_center">47.2 - 49.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_0" class="gt_row gt_center">3,990 (51.6)</td>
<td headers="ci_stat_0" class="gt_row gt_center">50.5 - 52.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age (years), n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 to 25</td>
<td headers="stat_0" class="gt_row gt_center">1,945 (25.2)</td>
<td headers="ci_stat_0" class="gt_row gt_center">24.2 - 26.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    26 to 35</td>
<td headers="stat_0" class="gt_row gt_center">3,006 (38.9)</td>
<td headers="ci_stat_0" class="gt_row gt_center">37.8 - 40.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    36 to 50</td>
<td headers="stat_0" class="gt_row gt_center">1,941 (25.1)</td>
<td headers="ci_stat_0" class="gt_row gt_center">24.2 - 26.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    50 to 65</td>
<td headers="stat_0" class="gt_row gt_center">701 (9.1)</td>
<td headers="ci_stat_0" class="gt_row gt_center">8.45 - 9.74</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    >65</td>
<td headers="stat_0" class="gt_row gt_center">134 (1.7)</td>
<td headers="ci_stat_0" class="gt_row gt_center">1.46 - 2.06</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Socioeconomic status, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High</td>
<td headers="stat_0" class="gt_row gt_center">1,671 (21.6)</td>
<td headers="ci_stat_0" class="gt_row gt_center">20.7 - 22.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium</td>
<td headers="stat_0" class="gt_row gt_center">5,141 (66.5)</td>
<td headers="ci_stat_0" class="gt_row gt_center">65.5 - 67.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Low</td>
<td headers="stat_0" class="gt_row gt_center">915 (11.8)</td>
<td headers="ci_stat_0" class="gt_row gt_center">11.1 - 12.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Educational level, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Higher</td>
<td headers="stat_0" class="gt_row gt_center">2,250 (29.1)</td>
<td headers="ci_stat_0" class="gt_row gt_center">28.1 - 30.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Secondary</td>
<td headers="stat_0" class="gt_row gt_center">3,479 (45.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">43.9 - 46.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No formal education or primary</td>
<td headers="stat_0" class="gt_row gt_center">1,998 (25.9)</td>
<td headers="ci_stat_0" class="gt_row gt_center">24.9 - 26.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Marital status, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single</td>
<td headers="stat_0" class="gt_row gt_center">2,769 (35.8)</td>
<td headers="ci_stat_0" class="gt_row gt_center">34.8 - 36.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    With a partner</td>
<td headers="stat_0" class="gt_row gt_center">4,958 (64.2)</td>
<td headers="ci_stat_0" class="gt_row gt_center">63.1 - 65.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Migratory status, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Legal</td>
<td headers="stat_0" class="gt_row gt_center">4,866 (63.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">61.9 - 64.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Illegal</td>
<td headers="stat_0" class="gt_row gt_center">2,861 (37.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">35.9 - 38.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Residence (months), n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    >24</td>
<td headers="stat_0" class="gt_row gt_center">5,866 (75.9)</td>
<td headers="ci_stat_0" class="gt_row gt_center">74.9 - 76.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    13-24</td>
<td headers="stat_0" class="gt_row gt_center">656 (8.5)</td>
<td headers="ci_stat_0" class="gt_row gt_center">7.88 - 9.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7-12</td>
<td headers="stat_0" class="gt_row gt_center">434 (5.6)</td>
<td headers="ci_stat_0" class="gt_row gt_center">5.12 - 6.16</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0-6</td>
<td headers="stat_0" class="gt_row gt_center">771 (10.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">9.32 - 10.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Housing, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="stat_0" class="gt_row gt_center">476 (6.2)</td>
<td headers="ci_stat_0" class="gt_row gt_center">5.64 - 6.73</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rented</td>
<td headers="stat_0" class="gt_row gt_center">7,251 (93.8)</td>
<td headers="ci_stat_0" class="gt_row gt_center">93.3 - 94.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Chronic disease, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_center">1,334 (17.3)</td>
<td headers="ci_stat_0" class="gt_row gt_center">16.4 - 18.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">6,393 (82.7)</td>
<td headers="ci_stat_0" class="gt_row gt_center">81.9 - 83.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Health insurance, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_center">903 (11.7)</td>
<td headers="ci_stat_0" class="gt_row gt_center">11.0 - 12.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">6,824 (88.3)</td>
<td headers="ci_stat_0" class="gt_row gt_center">87.6 - 89.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">COVID-19, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">4,708 (60.9)</td>
<td headers="ci_stat_0" class="gt_row gt_center">59.8 - 62.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Current</td>
<td headers="stat_0" class="gt_row gt_center">99 (1.3)</td>
<td headers="ci_stat_0" class="gt_row gt_center">1.05 - 1.56</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Previously</td>
<td headers="stat_0" class="gt_row gt_center">2,277 (29.5)</td>
<td headers="ci_stat_0" class="gt_row gt_center">28.5 - 30.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Do not know</td>
<td headers="stat_0" class="gt_row gt_center">643 (8.3)</td>
<td headers="ci_stat_0" class="gt_row gt_center">7.72 - 8.96</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Economic income, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_center">5,830 (75.4)</td>
<td headers="ci_stat_0" class="gt_row gt_center">74.5 - 76.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">1,897 (24.6)</td>
<td headers="ci_stat_0" class="gt_row gt_center">23.6 - 25.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">FIES, n (%)</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="ci_stat_0" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">394 (5.1)</td>
<td headers="ci_stat_0" class="gt_row gt_center">4.62 - 5.62</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Slight</td>
<td headers="stat_0" class="gt_row gt_center">3,632 (47.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">45.9 - 48.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Moderate</td>
<td headers="stat_0" class="gt_row gt_center">2,850 (36.9)</td>
<td headers="ci_stat_0" class="gt_row gt_center">35.8 - 38.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Severe</td>
<td headers="stat_0" class="gt_row gt_center">851 (11.0)</td>
<td headers="ci_stat_0" class="gt_row gt_center">10.3 - 11.7</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

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
tbl_svysummary
```

<div id="bkkgdoonby" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}
&#10;#bkkgdoonby .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bkkgdoonby .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#bkkgdoonby .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#bkkgdoonby .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#bkkgdoonby .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#bkkgdoonby .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bkkgdoonby .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bkkgdoonby .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#bkkgdoonby .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#bkkgdoonby .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#bkkgdoonby .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bkkgdoonby .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bkkgdoonby .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#bkkgdoonby .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bkkgdoonby .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#bkkgdoonby .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bkkgdoonby .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bkkgdoonby .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bkkgdoonby .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bkkgdoonby .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bkkgdoonby .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 1px;
  padding-right: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bkkgdoonby .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bkkgdoonby .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bkkgdoonby .gt_left {
  text-align: left;
}
&#10;#bkkgdoonby .gt_center {
  text-align: center;
}
&#10;#bkkgdoonby .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bkkgdoonby .gt_font_normal {
  font-weight: normal;
}
&#10;#bkkgdoonby .gt_font_bold {
  font-weight: bold;
}
&#10;#bkkgdoonby .gt_font_italic {
  font-style: italic;
}
&#10;#bkkgdoonby .gt_super {
  font-size: 65%;
}
&#10;#bkkgdoonby .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}
&#10;#bkkgdoonby .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bkkgdoonby .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bkkgdoonby .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bkkgdoonby .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bkkgdoonby .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bkkgdoonby .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Table 1</strong>. Characteristics of Venezuelan adults living in Peru (N = 7727)</caption>
  &#10;  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Weighted proportion&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Weighted proportion</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;n (%)&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>n (%)</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Sex</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_0" class="gt_row gt_left">3,737 (48.4)</td>
<td headers="ci_stat_0" class="gt_row gt_left">46.9 - 49.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_0" class="gt_row gt_left">3,990 (51.6)</td>
<td headers="ci_stat_0" class="gt_row gt_left">50.1 - 53.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age (years)</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 to 25</td>
<td headers="stat_0" class="gt_row gt_left">1,945 (23.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">22.5 - 25.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    26 to 35</td>
<td headers="stat_0" class="gt_row gt_left">3,006 (40.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">39.1 - 42.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    36 to 50</td>
<td headers="stat_0" class="gt_row gt_left">1,941 (25.3)</td>
<td headers="ci_stat_0" class="gt_row gt_left">24.1 - 26.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    50 to 65</td>
<td headers="stat_0" class="gt_row gt_left">701 (8.9)</td>
<td headers="ci_stat_0" class="gt_row gt_left">8.09 - 9.70</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;65</td>
<td headers="stat_0" class="gt_row gt_left">134 (1.6)</td>
<td headers="ci_stat_0" class="gt_row gt_left">1.33 - 1.96</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Socioeconomic status</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High</td>
<td headers="stat_0" class="gt_row gt_left">1,671 (23.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">22.4 - 25.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium</td>
<td headers="stat_0" class="gt_row gt_left">5,141 (64.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">63.1 - 65.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Low</td>
<td headers="stat_0" class="gt_row gt_left">915 (11.8)</td>
<td headers="ci_stat_0" class="gt_row gt_left">10.9 - 12.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Educational level</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Higher</td>
<td headers="stat_0" class="gt_row gt_left">2,250 (31.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">30.4 - 33.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Secondary</td>
<td headers="stat_0" class="gt_row gt_left">3,479 (47.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">46.0 - 48.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No formal education or primary</td>
<td headers="stat_0" class="gt_row gt_left">1,998 (20.8)</td>
<td headers="ci_stat_0" class="gt_row gt_left">19.7 - 22.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Marital status</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single</td>
<td headers="stat_0" class="gt_row gt_left">2,769 (36.2)</td>
<td headers="ci_stat_0" class="gt_row gt_left">34.8 - 37.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    With a partner</td>
<td headers="stat_0" class="gt_row gt_left">4,958 (63.8)</td>
<td headers="ci_stat_0" class="gt_row gt_left">62.5 - 65.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Migratory status</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Legal</td>
<td headers="stat_0" class="gt_row gt_left">4,866 (70.3)</td>
<td headers="ci_stat_0" class="gt_row gt_left">69.0 - 71.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Illegal</td>
<td headers="stat_0" class="gt_row gt_left">2,861 (29.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">28.4 - 31.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Residence (months)</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;24</td>
<td headers="stat_0" class="gt_row gt_left">5,866 (77.6)</td>
<td headers="ci_stat_0" class="gt_row gt_left">76.3 - 78.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    13-24</td>
<td headers="stat_0" class="gt_row gt_left">656 (8.4)</td>
<td headers="ci_stat_0" class="gt_row gt_left">7.60 - 9.26</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7-12</td>
<td headers="stat_0" class="gt_row gt_left">434 (5.0)</td>
<td headers="ci_stat_0" class="gt_row gt_left">4.40 - 5.68</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0-6</td>
<td headers="stat_0" class="gt_row gt_left">771 (9.0)</td>
<td headers="ci_stat_0" class="gt_row gt_left">8.24 - 9.87</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Housing</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="stat_0" class="gt_row gt_left">476 (5.3)</td>
<td headers="ci_stat_0" class="gt_row gt_left">4.69 - 5.99</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rented</td>
<td headers="stat_0" class="gt_row gt_left">7,251 (94.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">94.0 - 95.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Chronic disease</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_left">1,334 (16.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">15.5 - 17.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_left">6,393 (83.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">82.4 - 84.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Health insurance</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_left">903 (11.2)</td>
<td headers="ci_stat_0" class="gt_row gt_left">10.4 - 12.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_left">6,824 (88.8)</td>
<td headers="ci_stat_0" class="gt_row gt_left">87.8 - 89.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">COVID-19</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_left">4,708 (59.6)</td>
<td headers="ci_stat_0" class="gt_row gt_left">58.2 - 61.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Current</td>
<td headers="stat_0" class="gt_row gt_left">99 (1.3)</td>
<td headers="ci_stat_0" class="gt_row gt_left">1.03 - 1.67</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Previously</td>
<td headers="stat_0" class="gt_row gt_left">2,277 (31.4)</td>
<td headers="ci_stat_0" class="gt_row gt_left">30.0 - 32.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Do not know</td>
<td headers="stat_0" class="gt_row gt_left">643 (7.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">6.94 - 8.48</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Economic income</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_left">5,830 (76.4)</td>
<td headers="ci_stat_0" class="gt_row gt_left">75.1 - 77.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_left">1,897 (23.6)</td>
<td headers="ci_stat_0" class="gt_row gt_left">22.4 - 24.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">FIES</td>
<td headers="stat_0" class="gt_row gt_left"></td>
<td headers="ci_stat_0" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_left">394 (5.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">4.87 - 6.16</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Slight</td>
<td headers="stat_0" class="gt_row gt_left">3,632 (49.5)</td>
<td headers="ci_stat_0" class="gt_row gt_left">48.1 - 51.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Moderate</td>
<td headers="stat_0" class="gt_row gt_left">2,850 (34.7)</td>
<td headers="ci_stat_0" class="gt_row gt_left">33.3 - 36.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Severe</td>
<td headers="stat_0" class="gt_row gt_left">851 (10.3)</td>
<td headers="ci_stat_0" class="gt_row gt_left">9.45 - 11.2</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> n (%) = count unweighted (weighted percentage)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">2</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

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
Holm-Bonferroni method to adjust for multiple tests.

Use (García-Pérez 2023) to understand the goodness of fit

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
tbl_svysummary_bivariate
```

<div id="aqormwjtqa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}
&#10;#aqormwjtqa .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#aqormwjtqa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#aqormwjtqa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#aqormwjtqa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#aqormwjtqa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#aqormwjtqa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#aqormwjtqa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#aqormwjtqa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#aqormwjtqa .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#aqormwjtqa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#aqormwjtqa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#aqormwjtqa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#aqormwjtqa .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#aqormwjtqa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aqormwjtqa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#aqormwjtqa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#aqormwjtqa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aqormwjtqa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#aqormwjtqa .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aqormwjtqa .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#aqormwjtqa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 1px;
  padding-right: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aqormwjtqa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#aqormwjtqa .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aqormwjtqa .gt_left {
  text-align: left;
}
&#10;#aqormwjtqa .gt_center {
  text-align: center;
}
&#10;#aqormwjtqa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#aqormwjtqa .gt_font_normal {
  font-weight: normal;
}
&#10;#aqormwjtqa .gt_font_bold {
  font-weight: bold;
}
&#10;#aqormwjtqa .gt_font_italic {
  font-style: italic;
}
&#10;#aqormwjtqa .gt_super {
  font-size: 65%;
}
&#10;#aqormwjtqa .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}
&#10;#aqormwjtqa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#aqormwjtqa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#aqormwjtqa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#aqormwjtqa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#aqormwjtqa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#aqormwjtqa .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Table 2</strong>. Characteristics of Venezuelan adults according to FIES</caption>
  &#10;  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;No&lt;/strong&gt;, n = 394 (5.5%)&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>No</strong>, n = 394 (5.5%)<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Slight&lt;/strong&gt;, n = 3632 (50%)&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Slight</strong>, n = 3632 (50%)<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Moderate&lt;/strong&gt;, n = 2850 (35%)&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Moderate</strong>, n = 2850 (35%)<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Severe&lt;/strong&gt;, n = 851 (10%)&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Severe</strong>, n = 851 (10%)<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;3&lt;/sup&gt;"><strong>p-value</strong><sup class="gt_footnote_marks">3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;q-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;4&lt;/sup&gt;"><strong>q-value</strong><sup class="gt_footnote_marks">4</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Sex</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left">0.426</td>
<td headers="q.value" class="gt_row gt_left">0.85</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_1" class="gt_row gt_left">5.35</td>
<td headers="ci_stat_1" class="gt_row gt_left">41.29 - 53.20</td>
<td headers="stat_2" class="gt_row gt_left">50.71</td>
<td headers="ci_stat_2" class="gt_row gt_left">47.49 - 51.67</td>
<td headers="stat_3" class="gt_row gt_left">33.65</td>
<td headers="ci_stat_3" class="gt_row gt_left">44.54 - 49.36</td>
<td headers="stat_4" class="gt_row gt_left">10.29</td>
<td headers="ci_stat_4" class="gt_row gt_left">43.70 - 52.86</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_1" class="gt_row gt_left">5.61</td>
<td headers="ci_stat_1" class="gt_row gt_left">46.80 - 58.71</td>
<td headers="stat_2" class="gt_row gt_left">48.37</td>
<td headers="ci_stat_2" class="gt_row gt_left">48.33 - 52.51</td>
<td headers="stat_3" class="gt_row gt_left">35.68</td>
<td headers="ci_stat_3" class="gt_row gt_left">50.64 - 55.46</td>
<td headers="stat_4" class="gt_row gt_left">10.34</td>
<td headers="ci_stat_4" class="gt_row gt_left">47.14 - 56.30</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age (years)</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left">0.548</td>
<td headers="q.value" class="gt_row gt_left">0.85</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 to 25</td>
<td headers="stat_1" class="gt_row gt_left">4.57</td>
<td headers="ci_stat_1" class="gt_row gt_left">15.45 - 24.93</td>
<td headers="stat_2" class="gt_row gt_left">48.31</td>
<td headers="ci_stat_2" class="gt_row gt_left">21.44 - 24.94</td>
<td headers="stat_3" class="gt_row gt_left">36.31</td>
<td headers="ci_stat_3" class="gt_row gt_left">22.79 - 26.98</td>
<td headers="stat_4" class="gt_row gt_left">10.81</td>
<td headers="ci_stat_4" class="gt_row gt_left">21.11 - 29.06</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    26 to 35</td>
<td headers="stat_1" class="gt_row gt_left">5.93</td>
<td headers="ci_stat_1" class="gt_row gt_left">37.88 - 49.91</td>
<td headers="stat_2" class="gt_row gt_left">50.81</td>
<td headers="ci_stat_2" class="gt_row gt_left">39.52 - 43.67</td>
<td headers="stat_3" class="gt_row gt_left">33.02</td>
<td headers="ci_stat_3" class="gt_row gt_left">36.23 - 40.93</td>
<td headers="stat_4" class="gt_row gt_left">10.24</td>
<td headers="ci_stat_4" class="gt_row gt_left">35.76 - 44.86</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    36 to 50</td>
<td headers="stat_1" class="gt_row gt_left">5.62</td>
<td headers="ci_stat_1" class="gt_row gt_left">21.06 - 31.45</td>
<td headers="stat_2" class="gt_row gt_left">48.77</td>
<td headers="ci_stat_2" class="gt_row gt_left">23.15 - 26.77</td>
<td headers="stat_3" class="gt_row gt_left">34.94</td>
<td headers="ci_stat_3" class="gt_row gt_left">23.42 - 27.63</td>
<td headers="stat_4" class="gt_row gt_left">10.68</td>
<td headers="ci_stat_4" class="gt_row gt_left">22.47 - 30.29</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    50 to 65</td>
<td headers="stat_1" class="gt_row gt_left">5.28</td>
<td headers="ci_stat_1" class="gt_row gt_left">6.001 - 11.99</td>
<td headers="stat_2" class="gt_row gt_left">48.65</td>
<td headers="ci_stat_2" class="gt_row gt_left">7.627 - 9.924</td>
<td headers="stat_3" class="gt_row gt_left">37.88</td>
<td headers="ci_stat_3" class="gt_row gt_left">8.343 - 11.19</td>
<td headers="stat_4" class="gt_row gt_left">8.18</td>
<td headers="ci_stat_4" class="gt_row gt_left">5.095 - 9.626</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;65</td>
<td headers="stat_1" class="gt_row gt_left">6.75</td>
<td headers="ci_stat_1" class="gt_row gt_left">0.937 - 4.154</td>
<td headers="stat_2" class="gt_row gt_left">50.64</td>
<td headers="ci_stat_2" class="gt_row gt_left">1.252 - 2.166</td>
<td headers="stat_3" class="gt_row gt_left">31.81</td>
<td headers="ci_stat_3" class="gt_row gt_left">1.043 - 2.088</td>
<td headers="stat_4" class="gt_row gt_left">10.80</td>
<td headers="ci_stat_4" class="gt_row gt_left">0.940 - 3.008</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Socioeconomic status</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High</td>
<td headers="stat_1" class="gt_row gt_left">3.94</td>
<td headers="ci_stat_1" class="gt_row gt_left">12.75 - 22.30</td>
<td headers="stat_2" class="gt_row gt_left">51.21</td>
<td headers="ci_stat_2" class="gt_row gt_left">22.68 - 26.39</td>
<td headers="stat_3" class="gt_row gt_left">33.53</td>
<td headers="ci_stat_3" class="gt_row gt_left">20.84 - 25.05</td>
<td headers="stat_4" class="gt_row gt_left">11.32</td>
<td headers="ci_stat_4" class="gt_row gt_left">22.06 - 30.35</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium</td>
<td headers="stat_1" class="gt_row gt_left">6.06</td>
<td headers="ci_stat_1" class="gt_row gt_left">65.47 - 76.52</td>
<td headers="stat_2" class="gt_row gt_left">47.13</td>
<td headers="ci_stat_2" class="gt_row gt_left">59.34 - 63.48</td>
<td headers="stat_3" class="gt_row gt_left">36.59</td>
<td headers="ci_stat_3" class="gt_row gt_left">65.72 - 70.31</td>
<td headers="stat_4" class="gt_row gt_left">10.22</td>
<td headers="ci_stat_4" class="gt_row gt_left">59.36 - 68.31</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Low</td>
<td headers="stat_1" class="gt_row gt_left">5.43</td>
<td headers="ci_stat_1" class="gt_row gt_left">8.419 - 16.01</td>
<td headers="stat_2" class="gt_row gt_left">59.10</td>
<td headers="ci_stat_2" class="gt_row gt_left">12.64 - 15.65</td>
<td headers="stat_3" class="gt_row gt_left">26.68</td>
<td headers="ci_stat_3" class="gt_row gt_left">7.790 - 10.53</td>
<td headers="stat_4" class="gt_row gt_left">8.79</td>
<td headers="ci_stat_4" class="gt_row gt_left">7.577 - 13.22</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Educational level</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Higher</td>
<td headers="stat_1" class="gt_row gt_left">6.33</td>
<td headers="ci_stat_1" class="gt_row gt_left">31.07 - 42.55</td>
<td headers="stat_2" class="gt_row gt_left">54.54</td>
<td headers="ci_stat_2" class="gt_row gt_left">32.99 - 36.99</td>
<td headers="stat_3" class="gt_row gt_left">31.16</td>
<td headers="ci_stat_3" class="gt_row gt_left">26.33 - 30.77</td>
<td headers="stat_4" class="gt_row gt_left">7.98</td>
<td headers="ci_stat_4" class="gt_row gt_left">20.83 - 28.69</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Secondary</td>
<td headers="stat_1" class="gt_row gt_left">5.81</td>
<td headers="ci_stat_1" class="gt_row gt_left">44.33 - 56.32</td>
<td headers="stat_2" class="gt_row gt_left">49.04</td>
<td headers="ci_stat_2" class="gt_row gt_left">44.93 - 49.12</td>
<td headers="stat_3" class="gt_row gt_left">34.52</td>
<td headers="ci_stat_3" class="gt_row gt_left">44.82 - 49.65</td>
<td headers="stat_4" class="gt_row gt_left">10.62</td>
<td headers="ci_stat_4" class="gt_row gt_left">44.30 - 53.47</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No formal education or primary</td>
<td headers="stat_1" class="gt_row gt_left">3.44</td>
<td headers="ci_stat_1" class="gt_row gt_left">9.648 - 17.41</td>
<td headers="stat_2" class="gt_row gt_left">42.88</td>
<td headers="ci_stat_2" class="gt_row gt_left">16.51 - 19.62</td>
<td headers="stat_3" class="gt_row gt_left">40.50</td>
<td headers="ci_stat_3" class="gt_row gt_left">22.32 - 26.33</td>
<td headers="stat_4" class="gt_row gt_left">13.18</td>
<td headers="ci_stat_4" class="gt_row gt_left">22.85 - 30.67</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Marital status</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;">0.019</td>
<td headers="q.value" class="gt_row gt_left">0.058</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single</td>
<td headers="stat_1" class="gt_row gt_left">5.61</td>
<td headers="ci_stat_1" class="gt_row gt_left">31.44 - 42.98</td>
<td headers="stat_2" class="gt_row gt_left">46.81</td>
<td headers="ci_stat_2" class="gt_row gt_left">32.24 - 36.18</td>
<td headers="stat_3" class="gt_row gt_left">35.87</td>
<td headers="ci_stat_3" class="gt_row gt_left">35.09 - 39.71</td>
<td headers="stat_4" class="gt_row gt_left">11.71</td>
<td headers="ci_stat_4" class="gt_row gt_left">36.59 - 45.62</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    With a partner</td>
<td headers="stat_1" class="gt_row gt_left">5.41</td>
<td headers="ci_stat_1" class="gt_row gt_left">57.02 - 68.56</td>
<td headers="stat_2" class="gt_row gt_left">51.03</td>
<td headers="ci_stat_2" class="gt_row gt_left">63.82 - 67.76</td>
<td headers="stat_3" class="gt_row gt_left">34.03</td>
<td headers="ci_stat_3" class="gt_row gt_left">60.29 - 64.91</td>
<td headers="stat_4" class="gt_row gt_left">9.53</td>
<td headers="ci_stat_4" class="gt_row gt_left">54.38 - 63.41</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Migratory status</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Legal</td>
<td headers="stat_1" class="gt_row gt_left">6.43</td>
<td headers="ci_stat_1" class="gt_row gt_left">77.69 - 86.38</td>
<td headers="stat_2" class="gt_row gt_left">52.14</td>
<td headers="ci_stat_2" class="gt_row gt_left">72.16 - 75.81</td>
<td headers="stat_3" class="gt_row gt_left">32.96</td>
<td headers="ci_stat_3" class="gt_row gt_left">64.50 - 68.97</td>
<td headers="stat_4" class="gt_row gt_left">8.47</td>
<td headers="ci_stat_4" class="gt_row gt_left">53.19 - 62.16</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Illegal</td>
<td headers="stat_1" class="gt_row gt_left">3.24</td>
<td headers="ci_stat_1" class="gt_row gt_left">13.62 - 22.31</td>
<td headers="stat_2" class="gt_row gt_left">43.28</td>
<td headers="ci_stat_2" class="gt_row gt_left">24.19 - 27.84</td>
<td headers="stat_3" class="gt_row gt_left">38.81</td>
<td headers="ci_stat_3" class="gt_row gt_left">31.03 - 35.50</td>
<td headers="stat_4" class="gt_row gt_left">14.67</td>
<td headers="ci_stat_4" class="gt_row gt_left">37.84 - 46.81</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Residence (months)</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;24</td>
<td headers="stat_1" class="gt_row gt_left">6.09</td>
<td headers="ci_stat_1" class="gt_row gt_left">81.28 - 90.03</td>
<td headers="stat_2" class="gt_row gt_left">50.65</td>
<td headers="ci_stat_2" class="gt_row gt_left">77.63 - 81.02</td>
<td headers="stat_3" class="gt_row gt_left">33.98</td>
<td headers="ci_stat_3" class="gt_row gt_left">73.84 - 77.99</td>
<td headers="stat_4" class="gt_row gt_left">9.27</td>
<td headers="ci_stat_4" class="gt_row gt_left">65.37 - 73.80</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    13-24</td>
<td headers="stat_1" class="gt_row gt_left">5.09</td>
<td headers="ci_stat_1" class="gt_row gt_left">4.888 - 12.20</td>
<td headers="stat_2" class="gt_row gt_left">45.11</td>
<td headers="ci_stat_2" class="gt_row gt_left">6.601 - 8.849</td>
<td headers="stat_3" class="gt_row gt_left">34.56</td>
<td headers="ci_stat_3" class="gt_row gt_left">7.092 - 9.836</td>
<td headers="stat_4" class="gt_row gt_left">15.24</td>
<td headers="ci_stat_4" class="gt_row gt_left">9.487 - 16.07</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7-12</td>
<td headers="stat_1" class="gt_row gt_left">2.77</td>
<td headers="ci_stat_1" class="gt_row gt_left">1.022 - 6.100</td>
<td headers="stat_2" class="gt_row gt_left">46.52</td>
<td headers="ci_stat_2" class="gt_row gt_left">3.870 - 5.698</td>
<td headers="stat_3" class="gt_row gt_left">37.78</td>
<td headers="ci_stat_3" class="gt_row gt_left">4.449 - 6.648</td>
<td headers="stat_4" class="gt_row gt_left">12.93</td>
<td headers="ci_stat_4" class="gt_row gt_left">4.521 - 8.637</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0-6</td>
<td headers="stat_1" class="gt_row gt_left">2.10</td>
<td headers="ci_stat_1" class="gt_row gt_left">2.108 - 5.614</td>
<td headers="stat_2" class="gt_row gt_left">45.38</td>
<td headers="ci_stat_2" class="gt_row gt_left">7.212 - 9.469</td>
<td headers="stat_3" class="gt_row gt_left">39.29</td>
<td headers="ci_stat_3" class="gt_row gt_left">8.848 - 11.77</td>
<td headers="stat_4" class="gt_row gt_left">13.23</td>
<td headers="ci_stat_4" class="gt_row gt_left">9.053 - 14.68</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Housing</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="stat_1" class="gt_row gt_left">8.52</td>
<td headers="ci_stat_1" class="gt_row gt_left">5.217 - 12.79</td>
<td headers="stat_2" class="gt_row gt_left">58.80</td>
<td headers="ci_stat_2" class="gt_row gt_left">5.313 - 7.449</td>
<td headers="stat_3" class="gt_row gt_left">23.34</td>
<td headers="ci_stat_3" class="gt_row gt_left">2.886 - 4.399</td>
<td headers="stat_4" class="gt_row gt_left">9.34</td>
<td headers="ci_stat_4" class="gt_row gt_left">3.338 - 6.866</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rented</td>
<td headers="stat_1" class="gt_row gt_left">5.31</td>
<td headers="ci_stat_1" class="gt_row gt_left">87.21 - 94.78</td>
<td headers="stat_2" class="gt_row gt_left">48.99</td>
<td headers="ci_stat_2" class="gt_row gt_left">92.55 - 94.69</td>
<td headers="stat_3" class="gt_row gt_left">35.33</td>
<td headers="ci_stat_3" class="gt_row gt_left">95.60 - 97.11</td>
<td headers="stat_4" class="gt_row gt_left">10.37</td>
<td headers="ci_stat_4" class="gt_row gt_left">93.13 - 96.66</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Chronic disease</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;">0.003</td>
<td headers="q.value" class="gt_row gt_left">0.013</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_left">4.58</td>
<td headers="ci_stat_1" class="gt_row gt_left">10.15 - 18.59</td>
<td headers="stat_2" class="gt_row gt_left">44.44</td>
<td headers="ci_stat_2" class="gt_row gt_left">13.43 - 16.40</td>
<td headers="stat_3" class="gt_row gt_left">39.90</td>
<td headers="ci_stat_3" class="gt_row gt_left">17.22 - 20.99</td>
<td headers="stat_4" class="gt_row gt_left">11.08</td>
<td headers="ci_stat_4" class="gt_row gt_left">14.72 - 21.29</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_left">5.66</td>
<td headers="ci_stat_1" class="gt_row gt_left">81.41 - 89.85</td>
<td headers="stat_2" class="gt_row gt_left">50.51</td>
<td headers="ci_stat_2" class="gt_row gt_left">83.60 - 86.57</td>
<td headers="stat_3" class="gt_row gt_left">33.67</td>
<td headers="ci_stat_3" class="gt_row gt_left">79.01 - 82.78</td>
<td headers="stat_4" class="gt_row gt_left">10.16</td>
<td headers="ci_stat_4" class="gt_row gt_left">78.71 - 85.28</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Health insurance</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_left">7.20</td>
<td headers="ci_stat_1" class="gt_row gt_left">10.97 - 19.60</td>
<td headers="stat_2" class="gt_row gt_left">73.35</td>
<td headers="ci_stat_2" class="gt_row gt_left">15.18 - 18.25</td>
<td headers="stat_3" class="gt_row gt_left">15.82</td>
<td headers="ci_stat_3" class="gt_row gt_left">4.223 - 6.208</td>
<td headers="stat_4" class="gt_row gt_left">3.63</td>
<td headers="ci_stat_4" class="gt_row gt_left">2.489 - 6.246</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_left">5.26</td>
<td headers="ci_stat_1" class="gt_row gt_left">80.40 - 89.03</td>
<td headers="stat_2" class="gt_row gt_left">46.49</td>
<td headers="ci_stat_2" class="gt_row gt_left">81.75 - 84.82</td>
<td headers="stat_3" class="gt_row gt_left">37.09</td>
<td headers="ci_stat_3" class="gt_row gt_left">93.79 - 95.78</td>
<td headers="stat_4" class="gt_row gt_left">11.16</td>
<td headers="ci_stat_4" class="gt_row gt_left">93.75 - 97.51</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">COVID-19</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;">0.005</td>
<td headers="q.value" class="gt_row gt_left">0.021</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_left">5.01</td>
<td headers="ci_stat_1" class="gt_row gt_left">48.40 - 60.47</td>
<td headers="stat_2" class="gt_row gt_left">48.72</td>
<td headers="ci_stat_2" class="gt_row gt_left">56.59 - 60.74</td>
<td headers="stat_3" class="gt_row gt_left">35.41</td>
<td headers="ci_stat_3" class="gt_row gt_left">58.49 - 63.17</td>
<td headers="stat_4" class="gt_row gt_left">10.86</td>
<td headers="ci_stat_4" class="gt_row gt_left">58.22 - 67.09</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Current</td>
<td headers="stat_1" class="gt_row gt_left">11.15</td>
<td headers="ci_stat_1" class="gt_row gt_left">1.376 - 5.137</td>
<td headers="stat_2" class="gt_row gt_left">50.63</td>
<td headers="ci_stat_2" class="gt_row gt_left">0.935 - 1.932</td>
<td headers="stat_3" class="gt_row gt_left">31.24</td>
<td headers="ci_stat_3" class="gt_row gt_left">0.802 - 1.746</td>
<td headers="stat_4" class="gt_row gt_left">6.98</td>
<td headers="ci_stat_4" class="gt_row gt_left">0.361 - 2.179</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Previously</td>
<td headers="stat_1" class="gt_row gt_left">6.59</td>
<td headers="ci_stat_1" class="gt_row gt_left">31.87 - 43.96</td>
<td headers="stat_2" class="gt_row gt_left">49.86</td>
<td headers="ci_stat_2" class="gt_row gt_left">29.68 - 33.61</td>
<td headers="stat_3" class="gt_row gt_left">33.09</td>
<td headers="ci_stat_3" class="gt_row gt_left">27.79 - 32.18</td>
<td headers="stat_4" class="gt_row gt_left">10.45</td>
<td headers="ci_stat_4" class="gt_row gt_left">27.64 - 36.30</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Do not know</td>
<td headers="stat_1" class="gt_row gt_left">3.65</td>
<td headers="ci_stat_1" class="gt_row gt_left">3.297 - 7.812</td>
<td headers="stat_2" class="gt_row gt_left">53.96</td>
<td headers="ci_stat_2" class="gt_row gt_left">7.246 - 9.629</td>
<td headers="stat_3" class="gt_row gt_left">36.30</td>
<td headers="ci_stat_3" class="gt_row gt_left">6.825 - 9.415</td>
<td headers="stat_4" class="gt_row gt_left">6.10</td>
<td headers="ci_stat_4" class="gt_row gt_left">3.177 - 6.438</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Economic income</td>
<td headers="stat_1" class="gt_row gt_left"></td>
<td headers="ci_stat_1" class="gt_row gt_left"></td>
<td headers="stat_2" class="gt_row gt_left"></td>
<td headers="ci_stat_2" class="gt_row gt_left"></td>
<td headers="stat_3" class="gt_row gt_left"></td>
<td headers="ci_stat_3" class="gt_row gt_left"></td>
<td headers="stat_4" class="gt_row gt_left"></td>
<td headers="ci_stat_4" class="gt_row gt_left"></td>
<td headers="p.value" class="gt_row gt_left" style="font-weight: bold;"><0.001</td>
<td headers="q.value" class="gt_row gt_left"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_left">5.47</td>
<td headers="ci_stat_1" class="gt_row gt_left">70.56 - 81.05</td>
<td headers="stat_2" class="gt_row gt_left">51.42</td>
<td headers="ci_stat_2" class="gt_row gt_left">77.59 - 80.99</td>
<td headers="stat_3" class="gt_row gt_left">33.60</td>
<td headers="ci_stat_3" class="gt_row gt_left">71.79 - 76.05</td>
<td headers="stat_4" class="gt_row gt_left">9.51</td>
<td headers="ci_stat_4" class="gt_row gt_left">66.11 - 74.42</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_left">5.53</td>
<td headers="ci_stat_1" class="gt_row gt_left">18.95 - 29.44</td>
<td headers="stat_2" class="gt_row gt_left">43.31</td>
<td headers="ci_stat_2" class="gt_row gt_left">19.01 - 22.41</td>
<td headers="stat_3" class="gt_row gt_left">38.25</td>
<td headers="ci_stat_3" class="gt_row gt_left">23.95 - 28.21</td>
<td headers="stat_4" class="gt_row gt_left">12.91</td>
<td headers="ci_stat_4" class="gt_row gt_left">25.58 - 33.89</td>
<td headers="p.value" class="gt_row gt_left"></td>
<td headers="q.value" class="gt_row gt_left"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="11"><sup class="gt_footnote_marks">1</sup> %</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="11"><sup class="gt_footnote_marks">2</sup> CI = Confidence Interval</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="11"><sup class="gt_footnote_marks">3</sup> chi-squared test with Rao &amp; Scott's second-order correction</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="11"><sup class="gt_footnote_marks">4</sup> Holm correction for multiple testing</td>
    </tr>
  </tfoot>
</table>
</div>

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
design <- svydesign(ids = ~1, data = data_clear, weights = ~factorfinal)

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
# tbl_regression
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

# tidy

## Univariate regression models
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
univ_vglm <- vglm(FIES ~ sexo, weights = factorfinal, data = data_clear, family = propodds)
tab_model(univ_vglm, show.intercept = F)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
FIES
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Sex: Female
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.05 – 1.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
7727
</td>
</tr>
</table>

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
      exp() |>
      tab_model)
```

## Adjusted model

### Proportional odds logistic regression model with `svyolr`

``` r
design <- svydesign(ids = ~1, data = data_clear, weights = ~factorfinal)

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
mv_tab_polr <- polr(FIES ~ sexo + estrato + educacion + estadocivil + sitmigratoria + 
                      residencia + vivienda + enf_cronica + seguro + trabajo, 
                    data = data_clear, weights = factorfinal, Hess = TRUE, method = "logistic") |>
  tbl_regression(exponentiate = TRUE, 
                 conf.int = TRUE,
                 tidy_fun = broom.helpers::tidy_parameters,
                 pvalue_fun = ~style_pvalue(.x, digits = 3),
                 estimate_fun = ~style_number(.x, digits = 2)) |>
  add_vif()
mv_tab_polr
```

<div id="xdmthtepac" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}
&#10;#xdmthtepac .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xdmthtepac .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xdmthtepac .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xdmthtepac .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xdmthtepac .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xdmthtepac .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xdmthtepac .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xdmthtepac .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xdmthtepac .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xdmthtepac .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xdmthtepac .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xdmthtepac .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xdmthtepac .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xdmthtepac .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xdmthtepac .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xdmthtepac .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xdmthtepac .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xdmthtepac .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xdmthtepac .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xdmthtepac .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xdmthtepac .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 1px;
  padding-right: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xdmthtepac .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xdmthtepac .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xdmthtepac .gt_left {
  text-align: left;
}
&#10;#xdmthtepac .gt_center {
  text-align: center;
}
&#10;#xdmthtepac .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xdmthtepac .gt_font_normal {
  font-weight: normal;
}
&#10;#xdmthtepac .gt_font_bold {
  font-weight: bold;
}
&#10;#xdmthtepac .gt_font_italic {
  font-style: italic;
}
&#10;#xdmthtepac .gt_super {
  font-size: 65%;
}
&#10;#xdmthtepac .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}
&#10;#xdmthtepac .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xdmthtepac .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xdmthtepac .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xdmthtepac .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xdmthtepac .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xdmthtepac .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  &#10;  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt; &lt;strong&gt;(95% CI)&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>OR</strong> <strong>(95% CI)</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;GVIF&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>GVIF</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Adjusted GVIF&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2,1&lt;/sup&gt;"><strong>Adjusted GVIF</strong><sup class="gt_footnote_marks">2,1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.2</td>
<td headers="aGVIF" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">0.95 (0.94 to 0.96)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Socioeconomic status</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.0</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium</td>
<td headers="estimate" class="gt_row gt_center">1.04 (1.03 to 1.05)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Low</td>
<td headers="estimate" class="gt_row gt_center">0.84 (0.83 to 0.86)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Educational level</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.1</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Higher</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Secondary</td>
<td headers="estimate" class="gt_row gt_center">1.12 (1.11 to 1.14)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No formal education or primary</td>
<td headers="estimate" class="gt_row gt_center">1.37 (1.36 to 1.39)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Marital status</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.0</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    With a partner</td>
<td headers="estimate" class="gt_row gt_center">0.87 (0.86 to 0.88)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Migratory status</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.2</td>
<td headers="aGVIF" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Legal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Illegal</td>
<td headers="estimate" class="gt_row gt_center">1.42 (1.41 to 1.44)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Residence (months)</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.2</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;24</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    13-24</td>
<td headers="estimate" class="gt_row gt_center">1.14 (1.12 to 1.16)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7-12</td>
<td headers="estimate" class="gt_row gt_center">1.08 (1.06 to 1.10)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0-6</td>
<td headers="estimate" class="gt_row gt_center">1.18 (1.16 to 1.19)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Housing</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.0</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rented</td>
<td headers="estimate" class="gt_row gt_center">1.36 (1.33 to 1.39)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Chronic disease</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.1</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate" class="gt_row gt_center">0.85 (0.84 to 0.86)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Health insurance</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.0</td>
<td headers="aGVIF" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate" class="gt_row gt_center">2.68 (2.64 to 2.72)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Economic income</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center">1.2</td>
<td headers="aGVIF" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate" class="gt_row gt_center">1.16 (1.15 to 1.17)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td>
<td headers="GVIF" class="gt_row gt_center"></td>
<td headers="aGVIF" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">1</sup> OR = Odds Ratio, CI = Confidence Interval, GVIF = Generalized Variance Inflation Factor</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">2</sup> GVIF^[1/(2*df)]</td>
    </tr>
  </tfoot>
</table>
</div>

### Proportional odds logistic regression model with `vglm`

Alternatively, you can create a tibble object using the `tidy()`
function, which is supported by `vglm()`.

``` r
mv_vglm <- vglm(FIES ~ sexo + estrato + educacion + estadocivil + sitmigratoria + 
                      residencia + vivienda + enf_cronica + seguro + trabajo, 
                      data = data_clear, weights = factorfinal, family = propodds)
tab_model(mv_vglm, show.intercept = F)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
FIES
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Sex: Female
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.95
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.94 – 0.96
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Socioeconomic status:<br>Medium
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.03 – 1.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Socioeconomic status: Low
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.84
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.83 – 0.86
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Educational level:<br>Secondary
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.11 – 1.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Educational level: No<br>formal education or<br>primary
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.36 – 1.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Marital status: With a<br>partner
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.87
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.86 – 0.88
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Migratory status: Illegal
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.42
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.41 – 1.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
residencia13-24
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.12 – 1.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
residencia7-12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.08
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.06 – 1.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
residencia0-6
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.18
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.16 – 1.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Housing: Rented
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.33 – 1.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Chronic disease: No
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.84 – 0.86
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Health insurance: No
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.68
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.64 – 2.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Economic income: No
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.15 – 1.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
7727
</td>
</tr>
</table>

``` r
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
tbl_merge
```

<div id="rkvioabowt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}
&#10;#rkvioabowt .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rkvioabowt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#rkvioabowt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#rkvioabowt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#rkvioabowt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#rkvioabowt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rkvioabowt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rkvioabowt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#rkvioabowt .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#rkvioabowt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#rkvioabowt .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rkvioabowt .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rkvioabowt .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#rkvioabowt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkvioabowt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#rkvioabowt .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rkvioabowt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkvioabowt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rkvioabowt .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkvioabowt .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rkvioabowt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 1px;
  padding-right: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkvioabowt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rkvioabowt .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkvioabowt .gt_left {
  text-align: left;
}
&#10;#rkvioabowt .gt_center {
  text-align: center;
}
&#10;#rkvioabowt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rkvioabowt .gt_font_normal {
  font-weight: normal;
}
&#10;#rkvioabowt .gt_font_bold {
  font-weight: bold;
}
&#10;#rkvioabowt .gt_font_italic {
  font-style: italic;
}
&#10;#rkvioabowt .gt_super {
  font-size: 65%;
}
&#10;#rkvioabowt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}
&#10;#rkvioabowt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rkvioabowt .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rkvioabowt .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rkvioabowt .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rkvioabowt .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rkvioabowt .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Table 3.</strong> Factors associated with FIES by Ordinal Logistic Regression</caption>
  &#10;  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Unadjusted Analysis&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Unadjusted Analysis</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;Adjusted Analysis&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Adjusted Analysis</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;cOR&lt;/strong&gt; &lt;strong&gt;(95% CI)&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>cOR</strong> <strong>(95% CI)</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;aOR&lt;/strong&gt; &lt;strong&gt;(95% CI)&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>aOR</strong> <strong>(95% CI)</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;GVIF&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;3&lt;/sup&gt;"><strong>GVIF</strong><sup class="gt_footnote_marks">3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;aGVIF&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;4&lt;/sup&gt;"><strong>aGVIF</strong><sup class="gt_footnote_marks">4</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Sex</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate_1" class="gt_row gt_center">1.06 (0.95 to 1.18)</td>
<td headers="p.value_1" class="gt_row gt_center">0.297</td>
<td headers="estimate_2" class="gt_row gt_center">0.95 (0.85 to 1.07)</td>
<td headers="p.value_2" class="gt_row gt_center">0.408</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age (years)</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 to 25</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    26 to 35</td>
<td headers="estimate_1" class="gt_row gt_center">0.86 (0.74 to 0.99)</td>
<td headers="p.value_1" class="gt_row gt_center">0.036</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    36 to 50</td>
<td headers="estimate_1" class="gt_row gt_center">0.93 (0.80 to 1.09)</td>
<td headers="p.value_1" class="gt_row gt_center">0.392</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    50 to 65</td>
<td headers="estimate_1" class="gt_row gt_center">0.92 (0.75 to 1.12)</td>
<td headers="p.value_1" class="gt_row gt_center">0.393</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;65</td>
<td headers="estimate_1" class="gt_row gt_center">0.83 (0.56 to 1.25)</td>
<td headers="p.value_1" class="gt_row gt_center">0.378</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Socioeconomic status</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium</td>
<td headers="estimate_1" class="gt_row gt_center">1.00 (0.87 to 1.14)</td>
<td headers="p.value_1" class="gt_row gt_center">>0.999</td>
<td headers="estimate_2" class="gt_row gt_center">1.04 (0.90 to 1.19)</td>
<td headers="p.value_2" class="gt_row gt_center">0.609</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Low</td>
<td headers="estimate_1" class="gt_row gt_center">0.70 (0.57 to 0.85)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.84 (0.69 to 1.03)</td>
<td headers="p.value_2" class="gt_row gt_center">0.100</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Educational level</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.2</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Higher</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Secondary</td>
<td headers="estimate_1" class="gt_row gt_center">1.27 (1.12 to 1.44)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.12 (0.99 to 1.28)</td>
<td headers="p.value_2" class="gt_row gt_center">0.081</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No formal education or primary</td>
<td headers="estimate_1" class="gt_row gt_center">1.78 (1.54 to 2.07)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.37 (1.17 to 1.61)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Marital status</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.0</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    With a partner</td>
<td headers="estimate_1" class="gt_row gt_center">0.86 (0.77 to 0.96)</td>
<td headers="p.value_1" class="gt_row gt_center">0.009</td>
<td headers="estimate_2" class="gt_row gt_center">0.87 (0.77 to 0.98)</td>
<td headers="p.value_2" class="gt_row gt_center">0.021</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Migratory status</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.3</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Legal</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Illegal</td>
<td headers="estimate_1" class="gt_row gt_center">1.69 (1.50 to 1.91)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.42 (1.25 to 1.63)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Residence (months)</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.2</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt;24</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    13-24</td>
<td headers="estimate_1" class="gt_row gt_center">1.38 (1.10 to 1.72)</td>
<td headers="p.value_1" class="gt_row gt_center">0.005</td>
<td headers="estimate_2" class="gt_row gt_center">1.14 (0.91 to 1.44)</td>
<td headers="p.value_2" class="gt_row gt_center">0.258</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7-12</td>
<td headers="estimate_1" class="gt_row gt_center">1.41 (1.10 to 1.81)</td>
<td headers="p.value_1" class="gt_row gt_center">0.006</td>
<td headers="estimate_2" class="gt_row gt_center">1.08 (0.84 to 1.39)</td>
<td headers="p.value_2" class="gt_row gt_center">0.537</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0-6</td>
<td headers="estimate_1" class="gt_row gt_center">1.51 (1.27 to 1.81)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.18 (0.98 to 1.41)</td>
<td headers="p.value_2" class="gt_row gt_center">0.081</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Housing</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rented</td>
<td headers="estimate_1" class="gt_row gt_center">1.65 (1.28 to 2.12)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.36 (1.05 to 1.77)</td>
<td headers="p.value_2" class="gt_row gt_center">0.022</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Chronic disease</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate_1" class="gt_row gt_center">0.78 (0.68 to 0.90)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.85 (0.73 to 0.98)</td>
<td headers="p.value_2" class="gt_row gt_center">0.029</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Health insurance</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.0</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate_1" class="gt_row gt_center">2.92 (2.49 to 3.41)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">2.68 (2.28 to 3.14)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">COVID-19</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Current</td>
<td headers="estimate_1" class="gt_row gt_center">0.64 (0.39 to 1.04)</td>
<td headers="p.value_1" class="gt_row gt_center">0.074</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Previously</td>
<td headers="estimate_1" class="gt_row gt_center">0.89 (0.78 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.060</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Do not know</td>
<td headers="estimate_1" class="gt_row gt_center">0.85 (0.71 to 1.02)</td>
<td headers="p.value_1" class="gt_row gt_center">0.079</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Economic income</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center">1.1</td>
<td headers="aGVIF_2" class="gt_row gt_center">1.1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate_1" class="gt_row gt_center">1.35 (1.18 to 1.54)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.16 (1.01 to 1.34)</td>
<td headers="p.value_2" class="gt_row gt_center">0.041</td>
<td headers="GVIF_2" class="gt_row gt_center"></td>
<td headers="aGVIF_2" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><sup class="gt_footnote_marks">1</sup> cOR = Crude Odds Ratio, CI = Confidence Interval</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="7"><sup class="gt_footnote_marks">2</sup> aOR = Adjusted Odds Ratio, CI = Confidence Interval</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="7"><sup class="gt_footnote_marks">3</sup> GVIF = Generalized Variance Inflation Factor</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="7"><sup class="gt_footnote_marks">4</sup> aVIF = Adjusted GVIF</td>
    </tr>
  </tfoot>
</table>
</div>

### Multinomial logistic regression

``` r
mv_multinom_tab <- multinom(FIES ~ sexo + edad + estrato + educacion + estadocivil + sitmigratoria + 
              residencia + vivienda + enf_cronica + seguro + COVID19 + trabajo, 
            data = data_clear, weights = factorfinal) |>
  tbl_regression(exponentiate = TRUE, 
                 pvalue_fun = ~style_pvalue(., digits = 3)) |>
  modify_caption("**Table 4**. Factors associated with FIES by Multinomial logistic regression")
mv_multinom_tab
```

# Save outputs

``` r
# Table 1
tbl1 <- as_flex_table(tbl_svysummary)

# Table 2
tbl2 <- as_flex_table(tbl_svysummary_bivariate)

# Table 3
tbl3 <- as_flex_table(tbl_merge)

# Save
save_as_docx(tbl1, path = "/Academic/ENPOVE/ENPOVE_FIES_2022/outputs/Table_1.docx", align = "center")
save_as_docx(tbl2, path = "/Academic/ENPOVE/ENPOVE_FIES_2022/outputs/Table_2.docx", align = "center") 
save_as_docx(tbl3, path = "/Academic/ENPOVE/ENPOVE_FIES_2022/outputs/Table_3.docx", align = "center")
```

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-garcía-pérez2023" class="csl-entry">

García-Pérez, Miguel A. 2023. “Use and Misuse of Corrections for
Multiple Testing.” *Methods in Psychology* 8 (November): 100120.
<https://doi.org/10.1016/j.metip.2023.100120>.

</div>

</div>
