Analysis for SN Paper 1 - Inference
================
Saurabh Khanna
2020-03-29

  - [Integration v Policy](#integration-v-policy)
      - [Y1 to Y2](#y1-to-y2)
      - [Y3 to Y4](#y3-to-y4)
      - [Elite (Y1 to Y2)](#elite-y1-to-y2)
      - [Elite (Y3 to Y4)](#elite-y3-to-y4)
      - [Non-elite (Y1 to Y2)](#non-elite-y1-to-y2)
      - [Non-elite (Y3 to Y4)](#non-elite-y3-to-y4)

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
    stu_reserved = (ea_seats_reserved_students_obc + ea_seats_reserved_students_sc + ea_seats_reserved_students_st) / 100,
    fac_reserved = (ea_seats_reserved_faculty_obc + ea_seats_reserved_faculty_sc + ea_seats_reserved_faculty_st) / 100
  )
```

## Integration v Policy

### Y1 to Y2

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 2)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 2)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 2)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 2)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 2)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 2)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, grade == 2)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 2)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 2)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 2)
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
  dep.var.caption  = "Segregation - End of year 2",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age."
)
```

### Y3 to Y4

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 4)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 4)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 4)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 4)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 4)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 4)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5, 
    data = df %>% filter(reservation == 1, grade == 4)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 4)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 4)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 4)
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
  dep.var.caption  = "Segregation - End of year 4",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age."
)
```

### Elite (Y1 to Y2)

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 1)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 1)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 1)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 1)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 1)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 2, elite == 1)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 1, grade == 2, elite == 1)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 2, elite == 1)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 1)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 1)
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
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 2",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age.",
  omit.table.layout = "sn"
)
```

### Elite (Y3 to Y4)

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 1)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 1)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 1)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 1)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 1)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 4, elite == 1)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 1, grade == 4, elite == 1)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 4, elite == 1)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 1)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 1)
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
  dep.var.caption  = "Segregation at <i>elite</i> colleges - End of year 4",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age.",
  omit.table.layout = "sn"
)
```

### Non-elite (Y1 to Y2)

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 0)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 0)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 0)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 0)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 0)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 2, elite == 0)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 1, grade == 2, elite == 0)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 2, elite == 0)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 2, elite == 0)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 2, elite == 0)
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
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 2",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 1", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age.",
  omit.table.layout = "sn"
)
```

### Non-elite (Y3 to Y4)

``` r
lm1_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 0)
  )

lm1_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 0)
  )

lm2_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 0)
  )

lm2_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 0)
  )

lm3_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 0)
  )

lm3_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4,
    data = df %>% filter(reservation == 0, grade == 4, elite == 0)
  )

lm4_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 1, grade == 4, elite == 0)
  )

lm4_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5,
    data = df %>% filter(reservation == 0, grade == 4, elite == 0)
  )

lm5_r <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 1, grade == 4, elite == 0)
  )

lm5_nr <- 
  lm(
    e_seg_studymate ~ b_seg_studymate + female + age + ses + stu_reserved + ea_polic_integration_students4 + ea_polic_integration_students5 + ea_polic_integration_students6, 
    data = df %>% filter(reservation == 0, grade == 4, elite == 0)
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
  dep.var.caption  = "Segregation at <i>non-elite</i> colleges - End of year 4",
  dep.var.labels  = "Student reservation status",
  column.labels   = c("Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation", "Reservation", "Non-reservation"),
  covariate.labels = c("Segregation - Start of year 3", "Proportion of student seats reserved", "Diverse hostels", "Diverse course sections", "Integration courses taught", "Constant"),
  keep = c("b_seg", "stu_", "ea_", "Constant"),
  type = "html",
  out = "testing.html",
  notes = "All models control for student gender, socioeconomic status, and age.",
  omit.table.layout = "sn"
)
```