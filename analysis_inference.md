Analysis for SN Paper 1 - Inference
================
Saurabh Khanna
2020-04-07

  - [Integration v Policy](#integration-v-policy)
      - [Y1 to Y2](#y1-to-y2)
      - [Y3 to Y4](#y3-to-y4)
      - [Elite (Y1 to Y2)](#elite-y1-to-y2)
      - [Elite (Y3 to Y4)](#elite-y3-to-y4)
      - [Non-elite (Y1 to Y2)](#non-elite-y1-to-y2)
      - [Non-elite (Y3 to Y4)](#non-elite-y3-to-y4)
  - [Number of Studymates v Policy](#number-of-studymates-v-policy)
      - [Y1 to Y2](#y1-to-y2-1)
      - [Y3 to Y4](#y3-to-y4-1)
  - [Homophily v Policy](#homophily-v-policy)
      - [Y1 to Y2](#y1-to-y2-2)
      - [Y3 to Y4](#y3-to-y4-2)
  - [Reciprocity v Policy](#reciprocity-v-policy)
      - [Y1 to Y2](#y1-to-y2-3)
      - [Y3 to Y4](#y3-to-y4-3)
  - [Transitivity v Policy](#transitivity-v-policy)
      - [Y1 to Y2](#y1-to-y2-4)
      - [Y3 to Y4](#y3-to-y4-4)
  - [Student fixed effects](#student-fixed-effects)

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
n_studymates_file <- here::here("data/n_studymates.csv")
```

Read main data file:

``` r
df <-
  data_file %>%
  read_rds() %>% 
  filter(stu_merge == 3) %>% 
  mutate_at(vars(contains("ea_seats_reser")), replace_na, replace = 0) %>% 
  left_join(
    read_csv(fac_res_file),
    by = "department_id"
  ) %>% 
  left_join(
    read_csv(n_studymates_file),
    by = "stdid"
  ) %>% 
  mutate(
    female = gender - 1,
    age = 2017 - lubridate::year(b_birthdate),
    reservation = dplyr::recode(reservation, "Non-reservation" = 0L, "Reservation" = 1L, .default = NA_integer_),
    stu_res_official = (ea_seats_reserved_students_obc + ea_seats_reserved_students_sc + ea_seats_reserved_students_st) / 100,
    fac_res_official = (ea_seats_reserved_faculty_obc + ea_seats_reserved_faculty_sc + ea_seats_reserved_faculty_st) / 100,
    z_seg_g1 = if_else(grade == 2, b_seg_studymate, NA_real_),
    z_seg_g2 = if_else(grade == 2, e_seg_studymate, NA_real_),
    z_seg_g3 = if_else(grade == 4, b_seg_studymate, NA_real_),
    z_seg_g4 = if_else(grade == 4, e_seg_studymate, NA_real_),
    z_homophily_g1 = if_else(grade == 2, b_homophily, NA_real_),
    z_homophily_g2 = if_else(grade == 2, e_homophily, NA_real_),
    z_homophily_g3 = if_else(grade == 4, b_homophily, NA_real_),
    z_homophily_g4 = if_else(grade == 4, e_homophily, NA_real_),
    z_reciprocity_g1 = if_else(grade == 2, b_reciprocity, NA_real_),
    z_reciprocity_g2 = if_else(grade == 2, e_reciprocity, NA_real_),
    z_reciprocity_g3 = if_else(grade == 4, b_reciprocity, NA_real_),
    z_reciprocity_g4 = if_else(grade == 4, e_reciprocity, NA_real_),
    z_transitivity_g1 = if_else(grade == 2, b_transitivity, NA_real_),
    z_transitivity_g2 = if_else(grade == 2, e_transitivity, NA_real_),
    z_transitivity_g3 = if_else(grade == 4, b_transitivity, NA_real_),
    z_transitivity_g4 = if_else(grade == 4, e_transitivity, NA_real_),
    z_n_studymates_g1 = if_else(grade == 2, b_n_studymates, NA_real_),
    z_n_studymates_g2 = if_else(grade == 2, e_n_studymates, NA_real_),
    z_n_studymates_g3 = if_else(grade == 4, b_n_studymates, NA_real_),
    z_n_studymates_g4 = if_else(grade == 4, e_n_studymates, NA_real_),
  ) 

df <-
  df %>% 
  left_join(
    df %>% 
      group_by(department_id) %>% 
      summarize(stu_res_actual = mean(reservation, na.rm = TRUE)),
    by = "department_id"
  ) %>% 
  mutate_at(
    vars(starts_with(c("z_seg_g", "z_homophily", "z_reciprocity", "z_transitivity", "z_n_stud", "stu_res_", "fac_res_")), age),
    ~ scale(.) %>% as.vector
  )

df %>% 
  select(starts_with(c("z_seg_g", "z_homophily", "z_reciprocity", "z_transitivity", "z_n_stud", "stu_res_", "fac_res_")), age) %>% 
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
    ##  z_homophily_g1   z_homophily_g2   z_homophily_g3   z_homophily_g4  
    ##  Min.   :-2.528   Min.   :-1.987   Min.   :-2.766   Min.   :-6.665  
    ##  1st Qu.:-0.635   1st Qu.:-0.512   1st Qu.:-0.631   1st Qu.:-0.569  
    ##  Median :-0.115   Median :-0.093   Median :-0.108   Median :-0.111  
    ##  Mean   : 0.000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.000  
    ##  3rd Qu.: 0.364   3rd Qu.: 0.304   3rd Qu.: 0.574   3rd Qu.: 0.399  
    ##  Max.   : 5.747   Max.   : 6.211   Max.   : 3.775   Max.   : 3.405  
    ##  NA's   :8513     NA's   :8513     NA's   :7437     NA's   :7443    
    ##  z_reciprocity_g1 z_reciprocity_g2 z_reciprocity_g3 z_reciprocity_g4
    ##  Min.   :-3.080   Min.   :-4.010   Min.   :-3.622   Min.   :-1.822  
    ##  1st Qu.:-0.459   1st Qu.:-0.782   1st Qu.:-0.576   1st Qu.:-0.674  
    ##  Median : 0.130   Median : 0.092   Median : 0.176   Median :-0.003  
    ##  Mean   : 0.000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.000  
    ##  3rd Qu.: 0.780   3rd Qu.: 0.699   3rd Qu.: 0.678   3rd Qu.: 0.761  
    ##  Max.   : 4.478   Max.   : 5.046   Max.   : 4.634   Max.   : 5.319  
    ##  NA's   :8513     NA's   :8513     NA's   :7439     NA's   :7439    
    ##  z_transitivity_g1 z_transitivity_g2 z_transitivity_g3 z_transitivity_g4
    ##  Min.   :-2.405    Min.   :-3.961    Min.   :-1.634    Min.   :-1.551   
    ##  1st Qu.:-0.449    1st Qu.:-0.638    1st Qu.:-0.724    1st Qu.:-0.619   
    ##  Median :-0.098    Median :-0.218    Median :-0.044    Median :-0.198   
    ##  Mean   : 0.000    Mean   : 0.000    Mean   : 0.000    Mean   : 0.000   
    ##  3rd Qu.: 0.548    3rd Qu.: 0.565    3rd Qu.: 0.443    3rd Qu.: 0.513   
    ##  Max.   : 6.755    Max.   : 7.840    Max.   : 7.312    Max.   : 7.261   
    ##  NA's   :8513      NA's   :8513      NA's   :7439      NA's   :7439     
    ##  z_n_studymates_g1 z_n_studymates_g2 z_n_studymates_g3 z_n_studymates_g4
    ##  Min.   :-2.091    Min.   :-2.868    Min.   :-2.681    Min.   :-3.060   
    ##  1st Qu.:-0.610    1st Qu.:-0.407    1st Qu.:-0.615    1st Qu.:-0.501   
    ##  Median : 0.131    Median : 0.413    Median : 0.211    Median : 0.352   
    ##  Mean   : 0.000    Mean   : 0.000    Mean   : 0.000    Mean   : 0.000   
    ##  3rd Qu.: 0.871    3rd Qu.: 0.823    3rd Qu.: 0.625    3rd Qu.: 0.779   
    ##  Max.   : 1.242    Max.   : 0.823    Max.   : 1.038    Max.   : 0.779   
    ##  NA's   :9159      NA's   :8976      NA's   :7976      NA's   :7997     
    ##  stu_res_official  stu_res_actual     fac_res_actual     fac_res_official 
    ##  Min.   :-2.7480   Min.   :-2.89283   Min.   :-1.95182   Min.   :-0.7363  
    ##  1st Qu.:-0.4253   1st Qu.:-0.51947   1st Qu.:-0.57578   1st Qu.:-0.7363  
    ##  Median : 0.1702   Median : 0.05462   Median : 0.05729   Median :-0.7363  
    ##  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000  
    ##  3rd Qu.: 0.2298   3rd Qu.: 0.65206   3rd Qu.: 0.70852   3rd Qu.: 0.8199  
    ##  Max.   : 3.2075   Max.   : 2.72941   Max.   : 2.48208   Max.   : 3.2540  
    ##                                                                           
    ##       age          
    ##  Min.   :-2.86187  
    ##  1st Qu.:-0.70296  
    ##  Median : 0.01667  
    ##  Mean   : 0.00000  
    ##  3rd Qu.: 0.73631  
    ##  Max.   :11.53086  
    ##  NA's   :154

``` r
# Preparing classid level data for homophily/reciprocity/transitivity at elite/non-elite colleges
df_e <- df %>% distinct(classid, .keep_all = TRUE) %>% filter(elite == 1)
df_ne <- df %>% distinct(classid, .keep_all = TRUE) %>% filter(elite == 0)
```

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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing1.html",
  #report = "vcp",
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing1.html",
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing2.html",
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing2.html",
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing3.html",
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_seg", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  type = "html",
  out = "testing3.html",
  omit.stat = c("all"),
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

## Number of Studymates v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0)
  )

lm2_r <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1)
  )

lm2_nr <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0)
  )

lm3_r <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1)
  )

lm3_nr <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0)
  )

lm4_r <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1)
  )

lm4_nr <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0)
  )

lm5_r <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1)
  )

lm5_nr <- 
  lm(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0)
  )

iv1_r <- 
  ivreg(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_actual |
      z_n_studymates_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv1_nr <- 
  ivreg(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + stu_res_actual |
      z_n_studymates_g1 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0)
  )

iv2_r <- 
  ivreg(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + fac_res_actual |
      z_n_studymates_g1 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv2_nr <- 
  ivreg(
    z_n_studymates_g2 ~ z_n_studymates_g1 + female + age + ses + fac_res_actual |
      z_n_studymates_g1 + female + age + ses + fac_res_official, 
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Number of studymates - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Number of studymates - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_n_stud", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing1.html",
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
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official, 
    data = df %>% filter(reservation == 0)
  )

lm2_r <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 1)
  )

lm2_nr <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df %>% filter(reservation == 0)
  )

lm3_r <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1)
  )

lm3_nr <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 0)
  )

lm4_r <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1)
  )

lm4_nr <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 0)
  )

lm5_r <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1)
  )

lm5_nr <- 
  lm(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0)
  )

iv1_r <- 
  ivreg(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_actual |
      z_n_studymates_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv1_nr <- 
  ivreg(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + stu_res_actual |
      z_n_studymates_g3 + female + age + ses + stu_res_official, 
    data = df %>% filter(reservation == 0)
  )

iv2_r <- 
  ivreg(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + fac_res_actual |
      z_n_studymates_g3 + female + age + ses + fac_res_official, 
    data = df %>% filter(reservation == 1)
  )

iv2_nr <- 
  ivreg(
    z_n_studymates_g4 ~ z_n_studymates_g3 + female + age + ses + fac_res_actual |
      z_n_studymates_g3 + female + age + ses + fac_res_official, 
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Number of studymates - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Number of studymates - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_n_stud", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing2.html",
  notes = c(
    "All OLS models control for <i>proportion of seats officially reserved for students and faculty, student gender, student socioeconomic status,</i> and <i>student age</i>.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty. The model also controls for <i>student gender, student socioeconomic status,</i> and <i>student age</i>."
  )
)
```

## Homophily v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_actual |
      z_homophily_g1 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_homophily_g2 ~ z_homophily_g1 + stu_res_actual |
      z_homophily_g1 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_homophily_g2 ~ z_homophily_g1 + fac_res_actual |
      z_homophily_g1 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_homophily_g2 ~ z_homophily_g1 + fac_res_actual |
      z_homophily_g1 + fac_res_official, 
    data = df_ne
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
  dep.var.caption  = "Homophily - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Homophily - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_homophily", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

### Y3 to Y4

``` r
lm1_r <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_actual |
      z_homophily_g3 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_homophily_g4 ~ z_homophily_g3 + stu_res_actual |
      z_homophily_g3 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_homophily_g4 ~ z_homophily_g3 + fac_res_actual |
      z_homophily_g3 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_homophily_g4 ~ z_homophily_g3 + fac_res_actual |
      z_homophily_g3 + fac_res_official, 
    data = df_ne
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
  dep.var.caption  = "Homophily - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Homophily - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_homophily", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

## Reciprocity v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_actual |
      z_reciprocity_g1 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_reciprocity_g2 ~ z_reciprocity_g1 + stu_res_actual |
      z_reciprocity_g1 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_reciprocity_g2 ~ z_reciprocity_g1 + fac_res_actual |
      z_reciprocity_g1 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_reciprocity_g2 ~ z_reciprocity_g1 + fac_res_actual |
      z_reciprocity_g1 + fac_res_official, 
    data = df_ne
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
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Reciprocity - End of year 2 (by college status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Reciprocity - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_reciprocity", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing1.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

### Y3 to Y4

``` r
lm1_r <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_actual |
      z_reciprocity_g3 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_reciprocity_g4 ~ z_reciprocity_g3 + stu_res_actual |
      z_reciprocity_g3 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_reciprocity_g4 ~ z_reciprocity_g3 + fac_res_actual |
      z_reciprocity_g3 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_reciprocity_g4 ~ z_reciprocity_g3 + fac_res_actual |
      z_reciprocity_g3 + fac_res_official, 
    data = df_ne
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
  title = "Table 12",
  se = rob_se,
  header = F,
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Reciprocity - End of year 4 (by college status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Reciprocity - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_reciprocity", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing2.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

## Transitivity v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_actual |
      z_transitivity_g1 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_transitivity_g2 ~ z_transitivity_g1 + stu_res_actual |
      z_transitivity_g1 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_transitivity_g2 ~ z_transitivity_g1 + fac_res_actual |
      z_transitivity_g1 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_transitivity_g2 ~ z_transitivity_g1 + fac_res_actual |
      z_transitivity_g1 + fac_res_official, 
    data = df_ne
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
  title = "Table 13",
  se = rob_se,
  header = F,
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Transitivity - End of year 2 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Transitivity - Start of year 1", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_transitivity", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing1.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

### Y3 to Y4

``` r
lm1_r <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official, 
    data = df_e
  )

lm1_nr <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official, 
    data = df_ne
  )

lm2_r <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_e
  )

lm2_nr <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3, 
    data = df_ne
  )

lm3_r <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_e
  )

lm3_nr <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4, 
    data = df_ne
  )

lm4_r <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_e
  )

lm4_nr <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df_ne
  )

lm5_r <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_e
  )

lm5_nr <- 
  lm(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_official + fac_res_official + ea_polic_integration_students3 + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df_ne
  )

iv1_r <- 
  ivreg(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_actual |
      z_transitivity_g3 + stu_res_official, 
    data = df_e
  )

iv1_nr <- 
  ivreg(
    z_transitivity_g4 ~ z_transitivity_g3 + stu_res_actual |
      z_transitivity_g3 + stu_res_official, 
    data = df_ne
  )

iv2_r <- 
  ivreg(
    z_transitivity_g4 ~ z_transitivity_g3 + fac_res_actual |
      z_transitivity_g3 + fac_res_official, 
    data = df_e
  )

iv2_nr <- 
  ivreg(
    z_transitivity_g4 ~ z_transitivity_g3 + fac_res_actual |
      z_transitivity_g3 + fac_res_official, 
    data = df_ne
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
  title = "Table 14",
  se = rob_se,
  header = F,
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Transitivity - End of year 4 (by student reservation status)",
  dep.var.labels.include  = F,
  column.labels   = c("Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite", "Elite", "Non-elite"),
  covariate.labels = c("Transitivity - Start of year 3", "Mentoring program", "Diverse hostels", "Diverse course sections", "Integration courses", "Proportion of reservation students", "Proportion of reservation faculty", "Constant"),
  keep = c("z_transitivity", "stu_res_act", "fac_res_act", "ea_", "Constant"),
  omit.stat = c("all"),
  type = "html",
  out = "testing2.html",
  notes = c(
    "All OLS models control for proportion of seats officially reserved for students and faculty.",
    "Instruments for IV models: Proportion of seats officially reserved for students and faculty."
  )
)
```

## Student fixed effects

``` r
df <-
  df %>%
  mutate(
    diff_homophily = e_homophily - b_homophily,
    diff_reciprocity = e_reciprocity - b_reciprocity,
    diff_transitivity = e_transitivity - b_transitivity,
    diff_seg_studymate = e_seg_studymate - b_seg_studymate,
    diff_n_studymate = e_n_studymates - b_n_studymates,
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
    diff_seg_studymate ~ diff_homophily + diff_n_studymate + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1)
  )

lm1_nr <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_n_studymate + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 0)
  )

lm1_r_e <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_n_studymate + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1, elite == 1)
  )

lm1_nr_e <-
  lm(
    diff_seg_studymate ~ diff_n_studymate + diff_homophily + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 0, elite == 1)
  )

lm1_r_ne <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_n_studymate + diff_reciprocity + diff_transitivity, 
    data = df %>% filter(reservation == 1, elite == 0)
  )

lm1_nr_ne <-
  lm(
    diff_seg_studymate ~ diff_homophily + diff_n_studymate + diff_reciprocity + diff_transitivity, 
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
  title = "Table 9: Gains in segregation controlling for student fixed effects",
  se = rob_se,
  header = F,
  digits = 2,
  model.numbers = F,
  dep.var.caption  = "Gains in Segregation",
  dep.var.labels.include  = F,
  column.labels   = c("Reservation", "Non-reservation", "Reservation elite", "Non-reservation elite", "Reservation non-elite", "Non-reservation non-elite"),
  covariate.labels = c("Gains in class homophily", "Gains in studymates", "Gains in class reciprocity", "Gains in class transitivity", "Constant"),
  keep = c("diff_", "Constant"),
  type = "html",
  out = "testing3.html",
  notes = "All OLS models control for student fixed effects."
)
```
