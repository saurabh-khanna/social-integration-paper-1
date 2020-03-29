Analysis for SN Paper 1 - Inference
================
Saurabh Khanna
2020-03-29

``` r
# Libraries
library(tidyverse)
library(haven)
library(stargazer)
library(plm)
library(sandwich)
library(lmtest)

# Parameters
data_file <- here::here("data/stu_admin_all_with_netvars.Rds")
trends_file <- here::here("data/trends.csv")
```

Read main data file:

``` r
df <-
  data_file %>%
  read_rds() %>% 
  filter(stu_merge == 3) %>% 
  mutate_at(vars(contains("ea_seats_reser")), replace_na, replace = 0) %>% 
  mutate(
    female = gender - 1,
    age = 2017 - lubridate::year(b_birthdate),
    stu_reserved = ea_seats_reserved_students_obc + ea_seats_reserved_students_sc + ea_seats_reserved_students_st,
    fac_reserved = ea_seats_reserved_faculty_obc + ea_seats_reserved_faculty_sc + ea_seats_reserved_faculty_st
  )
```

Models:

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age, 
    data = df %>% filter(reservation == "Reservation")
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age, 
    data = df %>% filter(reservation == "Non-reservation")
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4, 
    data = df %>% filter(reservation == "Reservation")
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4, 
    data = df %>% filter(reservation == "Non-reservation")
  )

lm3_r <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6, 
  data = df %>% filter(reservation == "Reservation")
)

lm3_nr <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6, 
  data = df %>% filter(reservation == "Non-reservation")
)

lm4_r <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6 + stu_reserved, 
  data = df %>% filter(reservation == "Reservation")
)

lm4_nr <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6 + stu_reserved, 
  data = df %>% filter(reservation == "Non-reservation")
)

lm5_r <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6 + stu_reserved + fac_reserved, 
  data = df %>% filter(reservation == "Reservation")
)

lm5_nr <- lm(
  e_seg_studymate ~ b_seg_studymate + female + age + ea_polic_integration_students4 + ea_polic_integration_students6 + stu_reserved + fac_reserved, 
  data = df %>% filter(reservation == "Non-reservation")
)

rob_se <- 
  list(
    sqrt(diag(vcovHC(lm1_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(lm2_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm2_nr, type = "HC1"))),
    sqrt(diag(vcovHC(lm3_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm3_nr, type = "HC1"))),
    sqrt(diag(vcovHC(lm4_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm4_nr, type = "HC1"))),
    sqrt(diag(vcovHC(lm5_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm5_nr, type = "HC1")))
  )

stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.labels   = "Segregation - End of year 2",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  type = "html",
  out = "testing.html"
)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="10"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="10" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td colspan="10">Segregation - End of year 2</td></tr>
    ## <tr><td style="text-align:left"></td><td>Reservation</td><td>Non-reservation</td><td>Reservation</td><td>Non-reservation</td><td>Reservation</td><td>Non-reservation</td><td>Reservation</td><td>Non-reservation</td><td>Reservation</td><td>Non-reservation</td></tr>
    ## <tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">b_seg_studymate</td><td>0.492<sup>***</sup></td><td>0.557<sup>***</sup></td><td>0.493<sup>***</sup></td><td>0.556<sup>***</sup></td><td>0.493<sup>***</sup></td><td>0.548<sup>***</sup></td><td>0.487<sup>***</sup></td><td>0.540<sup>***</sup></td><td>0.487<sup>***</sup></td><td>0.540<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td><td>(0.010)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">female</td><td>0.001</td><td>-0.023<sup>***</sup></td><td>0.001</td><td>-0.023<sup>***</sup></td><td>0.001</td><td>-0.022<sup>***</sup></td><td>0.001</td><td>-0.023<sup>***</sup></td><td>0.001</td><td>-0.023<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">age</td><td>0.002</td><td>0.005<sup>***</sup></td><td>0.002</td><td>0.005<sup>***</sup></td><td>0.002</td><td>0.005<sup>**</sup></td><td>0.002</td><td>0.004<sup>**</sup></td><td>0.002</td><td>0.004<sup>**</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td><td>(0.002)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">ea_polic_integration_students4</td><td></td><td></td><td>0.014<sup>***</sup></td><td>-0.015<sup>***</sup></td><td>0.013<sup>**</sup></td><td>-0.007</td><td>0.014<sup>***</sup></td><td>-0.014<sup>***</sup></td><td>0.014<sup>***</sup></td><td>-0.016<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.006)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">ea_polic_integration_students6</td><td></td><td></td><td></td><td></td><td>0.004</td><td>-0.026<sup>***</sup></td><td>-0.002</td><td>-0.019<sup>***</sup></td><td>-0.002</td><td>-0.019<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td><td>(0.005)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">stu_reserved</td><td></td><td></td><td></td><td></td><td></td><td></td><td>0.001<sup>***</sup></td><td>-0.001<sup>***</sup></td><td>0.001<sup>***</sup></td><td>-0.001<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td>(0.0001)</td><td>(0.0002)</td><td>(0.0002)</td><td>(0.0002)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">fac_reserved</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>-0.00002</td><td>0.0001</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>(0.0001)</td><td>(0.0001)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Constant</td><td>0.259<sup>***</sup></td><td>0.162<sup>***</sup></td><td>0.244<sup>***</sup></td><td>0.173<sup>***</sup></td><td>0.241<sup>***</sup></td><td>0.197<sup>***</sup></td><td>0.198<sup>***</sup></td><td>0.268<sup>***</sup></td><td>0.197<sup>***</sup></td><td>0.271<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.033)</td><td>(0.037)</td><td>(0.034)</td><td>(0.037)</td><td>(0.034)</td><td>(0.037)</td><td>(0.034)</td><td>(0.038)</td><td>(0.034)</td><td>(0.038)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>7,074</td><td>6,785</td><td>7,074</td><td>6,785</td><td>7,074</td><td>6,785</td><td>7,074</td><td>6,785</td><td>7,074</td><td>6,785</td></tr>
    ## <tr><td style="text-align:left">R<sup>2</sup></td><td>0.293</td><td>0.354</td><td>0.294</td><td>0.355</td><td>0.294</td><td>0.357</td><td>0.298</td><td>0.363</td><td>0.298</td><td>0.363</td></tr>
    ## <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.293</td><td>0.354</td><td>0.294</td><td>0.354</td><td>0.294</td><td>0.357</td><td>0.298</td><td>0.362</td><td>0.297</td><td>0.362</td></tr>
    ## <tr><td style="text-align:left">Residual Std. Error</td><td>0.200 (df = 7070)</td><td>0.199 (df = 6781)</td><td>0.200 (df = 7069)</td><td>0.199 (df = 6780)</td><td>0.200 (df = 7068)</td><td>0.199 (df = 6779)</td><td>0.200 (df = 7067)</td><td>0.198 (df = 6778)</td><td>0.200 (df = 7066)</td><td>0.198 (df = 6777)</td></tr>
    ## <tr><td style="text-align:left">F Statistic</td><td>978.692<sup>***</sup> (df = 3; 7070)</td><td>1,238.254<sup>***</sup> (df = 3; 6781)</td><td>736.536<sup>***</sup> (df = 4; 7069)</td><td>931.770<sup>***</sup> (df = 4; 6780)</td><td>589.319<sup>***</sup> (df = 5; 7068)</td><td>753.457<sup>***</sup> (df = 5; 6779)</td><td>500.352<sup>***</sup> (df = 6; 7067)</td><td>642.701<sup>***</sup> (df = 6; 6778)</td><td>428.817<sup>***</sup> (df = 7; 7066)</td><td>551.174<sup>***</sup> (df = 7; 6777)</td></tr>
    ## <tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="10" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    ## </table>
