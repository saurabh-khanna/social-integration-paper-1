Analysis for SN Paper 1 - Inference
================
Saurabh Khanna
2020-04-01

  - [Integration v Policy](#integration-v-policy)
      - [Y1 to Y2](#y1-to-y2)
      - [Y3 to Y4](#y3-to-y4)
      - [Elite (Y1 to Y2)](#elite-y1-to-y2)
      - [Elite (Y3 to Y4)](#elite-y3-to-y4)
      - [Non-elite (Y1 to Y2)](#non-elite-y1-to-y2)
      - [Non-elite (Y3 to Y4)](#non-elite-y3-to-y4)
  - [Reciprocity v Policy](#reciprocity-v-policy)

``` r
# Libraries
library(tidyverse)
library(haven)
library(stargazer)
library(AER)
library(sandwich)
library(lmtest)

# Parameters
data_file <- here::here("data/stu_admin_all_with_netvars.Rds")
trends_file <- here::here("data/trends.csv")
fac_res_file <- here::here("data/fac_res_prop.csv")
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
    reservation = dplyr::recode(reservation, "Non-reservation" = 0L, "Reservation" = 1L, .default = NA_integer_),
    stu_res_official = (ea_seats_reserved_students_obc + ea_seats_reserved_students_sc + ea_seats_reserved_students_st) / 100,
    fac_res_official = (ea_seats_reserved_faculty_obc + ea_seats_reserved_faculty_sc + ea_seats_reserved_faculty_st) / 100,
    z_seg_g1 = if_else(grade == 2, b_seg_studymate, NA_real_),
    z_seg_g2 = if_else(grade == 2, e_seg_studymate, NA_real_),
    z_seg_g3 = if_else(grade == 4, b_seg_studymate, NA_real_),
    z_seg_g4 = if_else(grade == 4, e_seg_studymate, NA_real_)
  ) %>% 
  left_join(
    read_csv(fac_res_file),
    by = "department_id"
  )
```

    ## Warning: Column `department_id` has different attributes on LHS and RHS of join

``` r
df <-
  df %>% 
  left_join(
    df %>% 
      group_by(department_id) %>% 
      summarize(stu_res_actual = mean(reservation, na.rm = TRUE)),
    by = "department_id"
  ) %>% 
  mutate_at(
    vars(starts_with(c("z_seg_g", "stu_res_", "fac_res_")), age),
    ~ scale(.) %>% as.vector
  )

df %>% 
  select(starts_with(c("z_seg_g", "stu_res_", "fac_res_")), age) %>% 
  summary()
```

    ##     z_seg_g1         z_seg_g2         z_seg_g3         z_seg_g4     
    ##  Min.   :-2.139   Min.   :-2.383   Min.   :-2.304   Min.   :-2.372  
    ##  1st Qu.:-0.688   1st Qu.:-0.750   1st Qu.:-0.719   1st Qu.:-0.736  
    ##  Median : 0.037   Median : 0.067   Median : 0.073   Median : 0.083  
    ##  Mean   : 0.000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.000  
    ##  3rd Qu.: 0.762   3rd Qu.: 0.793   3rd Qu.: 0.777   3rd Qu.: 0.810  
    ##  Max.   : 1.487   Max.   : 1.700   Max.   : 1.658   Max.   : 1.720  
    ##  NA's   :9166     NA's   :9012     NA's   :7982     NA's   :8036    
    ##  stu_res_official  stu_res_actual     fac_res_official  fac_res_actual    
    ##  Min.   :-2.7480   Min.   :-2.89283   Min.   :-0.7363   Min.   :-1.95182  
    ##  1st Qu.:-0.4253   1st Qu.:-0.51947   1st Qu.:-0.7363   1st Qu.:-0.57578  
    ##  Median : 0.1702   Median : 0.05462   Median :-0.7363   Median : 0.05729  
    ##  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000  
    ##  3rd Qu.: 0.2298   3rd Qu.: 0.65206   3rd Qu.: 0.8199   3rd Qu.: 0.70852  
    ##  Max.   : 3.2075   Max.   : 2.72941   Max.   : 3.2540   Max.   : 2.48208  
    ##                                                                           
    ##       age          
    ##  Min.   :-2.86187  
    ##  1st Qu.:-0.70296  
    ##  Median : 0.01667  
    ##  Mean   : 0.00000  
    ##  3rd Qu.: 0.73631  
    ##  Max.   :11.53086  
    ##  NA's   :154

Checking IV stage-1 assumption:

``` r
df %>% 
  lm(stu_res_actual ~  z_seg_g1 + female + age + ses + stu_res_official + reservation + elite, data = .) %>%
  linearHypothesis(c("stu_res_official = 0"), vcov = vcovHC, type = "HC1")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## stu_res_official = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: stu_res_actual ~ z_seg_g1 + female + age + ses + stu_res_official + 
    ##     reservation + elite
    ## 
    ## Note: Coefficient covariance matrix supplied.
    ## 
    ##   Res.Df Df      F    Pr(>F)    
    ## 1   6708                        
    ## 2   6707  1 478.84 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df %>% 
  lm(fac_res_actual ~  z_seg_g1 + female + age + ses + fac_res_official + reservation + elite, data = .) %>%
  linearHypothesis(c("fac_res_official = 0"), vcov = vcovHC, type = "HC1")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## fac_res_official = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: fac_res_actual ~ z_seg_g1 + female + age + ses + fac_res_official + 
    ##     reservation + elite
    ## 
    ## Note: Coefficient covariance matrix supplied.
    ## 
    ##   Res.Df Df      F    Pr(>F)    
    ## 1   6708                        
    ## 2   6707  1 18.763 1.502e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df %>% 
  lm(stu_res_actual ~  z_seg_g3 + female + age + ses + stu_res_official + reservation + elite, data = .) %>%
  linearHypothesis(c("stu_res_official = 0"), vcov = vcovHC, type = "HC1")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## stu_res_official = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: stu_res_actual ~ z_seg_g3 + female + age + ses + stu_res_official + 
    ##     reservation + elite
    ## 
    ## Note: Coefficient covariance matrix supplied.
    ## 
    ##   Res.Df Df      F    Pr(>F)    
    ## 1   7853                        
    ## 2   7852  1 702.62 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df %>% 
  lm(fac_res_actual ~  z_seg_g3 + female + age + ses + fac_res_official + reservation + elite, data = .) %>%
  linearHypothesis(c("fac_res_official = 0"), vcov = vcovHC, type = "HC1")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## fac_res_official = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: fac_res_actual ~ z_seg_g3 + female + age + ses + fac_res_official + 
    ##     reservation + elite
    ## 
    ## Note: Coefficient covariance matrix supplied.
    ## 
    ##   Res.Df Df      F    Pr(>F)    
    ## 1   7853                        
    ## 2   7852  1 150.53 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Integration v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0)
  )

lm2_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1)
  )

lm2_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0)
  )

lm3_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1)
  )

lm3_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0)
  )

lm4_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1)
  )

lm4_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0)
  )

lm5_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1)
  )

lm5_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0)
  )

iv1_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv1_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0)
  )

iv2_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv2_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )

stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing.html",
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

### Y3 to Y4

``` r
lm1_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0)
  )

lm2_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1)
  )

lm2_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0)
  )

lm3_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1)
  )

lm3_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0)
  )

lm4_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1)
  )

lm4_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0)
  )

lm5_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1)
  )

lm5_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0)
  )

iv1_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv1_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0)
  )

iv2_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv2_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )

stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing.html",
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

### Elite (Y1 to Y2)

``` r
lm1_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm1_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm2_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm2_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm3_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm3_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm4_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm4_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm5_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm5_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

iv1_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

iv1_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

iv2_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

iv2_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )

stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  omit.stat = c("all"),
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

### Elite (Y3 to Y4)

``` r
lm1_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm1_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm2_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm2_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm3_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm3_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm4_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm4_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm5_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm5_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

iv1_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

iv1_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

iv2_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

iv2_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 1)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )


stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  omit.stat = c("all"),
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

### Non-elite (Y1 to Y2)

``` r
lm1_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm1_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm2_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm2_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm3_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm3_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm4_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm4_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm5_r <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm5_nr <- 
  lm(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

iv1_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

iv1_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + stu_res_actual |
      z_seg_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

iv2_r <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

iv2_nr <- 
  ivreg(
    z_seg_g2 ~ z_seg_g1 + female + age + ses + fac_res_actual |
      z_seg_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )

stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  omit.stat = c("all"),
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

### Non-elite (Y3 to Y4)

``` r
lm1_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm1_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm2_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm2_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm3_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm3_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm4_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm4_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

lm5_r <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm5_nr <- 
  lm(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

iv1_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

iv1_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + stu_res_actual |
      z_seg_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
  )

iv2_r <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

iv2_nr <- 
  ivreg(
    z_seg_g4 ~ z_seg_g3 + female + age + ses + fac_res_actual |
      z_seg_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 0, elite == 0)
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
    sqrt(diag(vcovHC(lm5_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_r, type = "HC1"))),
    sqrt(diag(vcovHC(iv2_nr, type = "HC1")))
  )


stargazer(
  lm1_r, lm1_nr, lm2_r, lm2_nr, lm3_r, lm3_nr, lm4_r, lm4_nr, lm5_r, lm5_nr, iv1_r, iv1_nr, iv2_r, iv2_nr,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  omit.stat = c("all"),
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

## Reciprocity v Policy

``` r
df <-
  df %>%
  mutate(
    diff_homophily = e_homophily - b_homophily,
    diff_reciprocity = e_reciprocity - b_reciprocity,
    diff_transitivity = e_transitivity - b_transitivity,
    diff_seg_studymate = e_seg_studymate - b_seg_studymate,
    diff_ct = e_ct_score_perc - b_ct_score_perc,
    diff_ql = e_ql_score_perc - b_ql_score_perc,
    diff_math = e_math_g3_score_perc - b_math_g1_score_perc,
    diff_physics = e_physics_g3_score_perc - b_physics_g1_score_perc
  ) %>%
  mutate_at(
    vars(starts_with("diff_")),
    ~ scale(.) %>% as.vector
  )

  
lm1_r <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 0)
  )

lm1_r_e <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm1_nr_e <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm1_r_ne <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm1_nr_ne <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 0, elite == 0)
  )


rob_se <- 
  list(
    sqrt(diag(vcovHC(lm1_r, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_nr, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_r_e, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_nr_e, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_r_ne, type = "HC1"))),
    sqrt(diag(vcovHC(lm1_nr_ne, type = "HC1")))
  )


stargazer(
  lm1_r, lm1_nr, lm1_r_e, lm1_nr_e, lm1_r_ne, lm1_nr_ne,
  se = rob_se,
  header = F,
  digits = 3,
  model.numbers = F,
  dep.var.caption  = "Change in Segregation",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation elite", "Non-reservation elite", "Reservation non-elite", "Non-reservation non-elite"),
  covariate.labels = c("Change in homophily", "Change in reciprocity", "Change in transitivity", "Constant"),
  keep = c("diff_", "Constant"),
  type = "html",
  out = "testing.html"
#  omit.stat = c("all")
)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="6">Change in Segregation</td></tr>
    ## <tr><td></td><td colspan="6" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td>Reservation</td><td>Non-reservation</td><td>Reservation elite</td><td>Non-reservation elite</td><td>Reservation non-elite</td><td>Non-reservation non-elite</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Change in homophily</td><td>0.104<sup>***</sup></td><td>0.106<sup>***</sup></td><td>0.117<sup>***</sup></td><td>0.112<sup>***</sup></td><td>0.098<sup>***</sup></td><td>0.112<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.013)</td><td>(0.014)</td><td>(0.029)</td><td>(0.030)</td><td>(0.014)</td><td>(0.016)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Change in reciprocity</td><td>-0.052<sup>***</sup></td><td>0.046<sup>**</sup></td><td>-0.117<sup>**</sup></td><td>0.206<sup>***</sup></td><td>-0.043<sup>**</sup></td><td>0.026</td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.018)</td><td>(0.020)</td><td>(0.050)</td><td>(0.053)</td><td>(0.020)</td><td>(0.022)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Change in transitivity</td><td>0.059<sup>***</sup></td><td>-0.031</td><td>0.113<sup>**</sup></td><td>-0.103<sup>*</sup></td><td>0.051<sup>**</sup></td><td>-0.018</td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.019)</td><td>(0.021)</td><td>(0.056)</td><td>(0.058)</td><td>(0.021)</td><td>(0.023)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Constant</td><td>0.040<sup>***</sup></td><td>-0.042<sup>***</sup></td><td>-0.021</td><td>0.053</td><td>0.048<sup>***</sup></td><td>-0.053<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.012)</td><td>(0.012)</td><td>(0.036)</td><td>(0.037)</td><td>(0.013)</td><td>(0.013)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>7,117</td><td>6,815</td><td>1,077</td><td>1,080</td><td>6,040</td><td>5,735</td></tr>
    ## <tr><td style="text-align:left">R<sup>2</sup></td><td>0.011</td><td>0.011</td><td>0.024</td><td>0.027</td><td>0.009</td><td>0.010</td></tr>
    ## <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.010</td><td>0.011</td><td>0.021</td><td>0.025</td><td>0.008</td><td>0.010</td></tr>
    ## <tr><td style="text-align:left">Residual Std. Error</td><td>1.016 (df = 7113)</td><td>0.971 (df = 6811)</td><td>1.113 (df = 1073)</td><td>1.060 (df = 1076)</td><td>0.997 (df = 6036)</td><td>0.953 (df = 5731)</td></tr>
    ## <tr><td style="text-align:left">F Statistic</td><td>26.095<sup>***</sup> (df = 3; 7113)</td><td>26.080<sup>***</sup> (df = 3; 6811)</td><td>8.833<sup>***</sup> (df = 3; 1073)</td><td>10.033<sup>***</sup> (df = 3; 1076)</td><td>17.517<sup>***</sup> (df = 3; 6036)</td><td>19.938<sup>***</sup> (df = 3; 5731)</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="6" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    ## </table>

``` r
df %>% 
  summarize_at(vars(diff_ct, diff_ql, diff_math, diff_physics), ~ sum(!is.na(.)))
```

    ## # A tibble: 1 x 4
    ##   diff_ct diff_ql diff_math diff_physics
    ##     <int>   <int>     <int>        <int>
    ## 1    3987    3006      3762         3754
