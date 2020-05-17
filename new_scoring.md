Analysis for SN Paper 1 - New Scoring
================
Saurabh Khanna
2020-05-17

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
baseline_file <- here::here("data/new_scoring/b_file.dta")
endline_file <- here::here("data/new_scoring/e_file.dta")
```

Class info:

``` r
class_info <-
  read_rds(data_file) %>% 
  select(classid, stdid, reservation) %>% 
  mutate(
    reservation = dplyr::recode(reservation, "Non-reservation" = 0L, "Reservation" = 1L, .default = NA_integer_)
  ) %>% 
  drop_na() %>% 
  count(classid, reservation) %>% 
  pivot_wider(
    names_from = reservation,
    values_from = n,
    names_prefix = "res_class_"
  ) %>% 
  mutate(
    res_class_0 = if_else(is.na(res_class_0), 0L, res_class_0),
    res_class_1 = if_else(is.na(res_class_1), 0L, res_class_1)
  )
```

Student info:

``` r
read_dta(baseline_file) %>% 
  drop_na(res_stdid, res_b_alter) %>% 
  count(classid, stdid, res_stdid, res_b_alter) %>% 
  pivot_wider(
    names_from = res_b_alter,
    values_from = n,
    names_prefix = "res_stu_"
  ) %>% 
  mutate(
    res_stu_0 = if_else(is.na(res_stu_0), 0L, res_stu_0),
    res_stu_1 = if_else(is.na(res_stu_1), 0L, res_stu_1)
  ) %>% 
  inner_join(class_info, by = "classid") %>% 
  mutate(
    b_seg_score = if_else(
      res_stdid == 0,
      (res_stu_0 / res_stu_1) * ((res_class_1 - res_stu_1) / (res_class_0 - res_stu_0 - 1)),
      (res_stu_1 / res_stu_0) * ((res_class_0 - res_stu_0) / (res_class_1 - res_stu_1 - 1))
    ),
    log = log(b_seg_score)
  )
```

    ## # A tibble: 16,325 x 9
    ##    classid stdid res_stdid res_stu_0 res_stu_1 res_class_0 res_class_1
    ##    <chr>   <chr>     <dbl>     <int>     <int>       <int>       <int>
    ##  1 IR001C… IR00…         0         4         6          29          20
    ##  2 IR001C… IR00…         1         2         2          29          20
    ##  3 IR001C… IR00…         1         3         1          29          20
    ##  4 IR001C… IR00…         0         5         5          29          20
    ##  5 IR001C… IR00…         0         5         5          29          20
    ##  6 IR001C… IR00…         0         4         5          29          20
    ##  7 IR001C… IR00…         1         0         1          29          20
    ##  8 IR001C… IR00…         0         1         3          29          20
    ##  9 IR001C… IR00…         1         3         0          29          20
    ## 10 IR001C… IR00…         0         4         0          29          20
    ## # … with 16,315 more rows, and 2 more variables: b_seg_score <dbl>, log <dbl>
