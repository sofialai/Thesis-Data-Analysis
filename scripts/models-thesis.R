dat_1 <- dat %>% 
  mutate(
    ubi_300 = gsub("Stop working", "Work less", ubi_300), 
    ubi_500 = gsub("Stop working", "Work less", ubi_500),
    ubi_500 = gsub("Stop working", "Work less", ubi_800)
  ) %>% 
  mutate(
    ubi_300 = gsub("Work less", "Work less/Stop working", ubi_300),
    ubi_500 = gsub("Work less", "Work less/Stop working", ubi_500),
    ubi_800 = gsub("Work less", "Work less/Stop working", ubi_800)
  ) %>% 
  mutate(income_cat = case_when( 
    income == "0-15,000" ~ "Low", 
    income == "15,000-30,000" ~ "Middle",
    income == "30,000-45,000" ~ "Middle",
    income == "45,000-60,000" ~ "High",
    income == "60,000+" ~ "High")) %>% 
  mutate(hours = case_when( 
    hours == "0-10" ~ "Less than 20", 
    hours == "10-20" ~ "Less than 20",
    hours == "20-40" ~ "More than 20",
    hours == "40+" ~ "More than 20")) %>% 
  mutate(ubi_300 = case_when(
    ubi_300 == "Work less/Stop working" ~ 0, 
    ubi_300 == "Work the same amount as now" ~ 1), 
    ubi_500 = case_when(
      ubi_500 == "Work less/Stop working" ~ 0, 
      ubi_500 == "Work the same amount as now" ~ 1), 
    ubi_800 = case_when(
      ubi_800 == "Work less/Stop working" ~ 0, 
      ubi_800 == "Work the same amount as now" ~ 1))

dat_1$hours <- as.factor(dat_1$hours)
dat_1$ubi_300 <- as.factor(dat_1$ubi_300)
dat_1$ubi_500 <- as.factor(dat_1$ubi_500)
dat_1$ubi_800 <- as.factor(dat_1$ubi_800)
dat_1$change_500 <- as.factor(dat_1$change_500)
dat_1$change_800 <- as.factor(dat_1$change_800)
dat_1$income_cat <- as.factor(dat_1$income_cat)
dat_1$income_cat <- relevel(dat_1$income_cat, ref = "Middle")


#behavior under UBI = 300 and hours worked 
#UBI ref category = work less. So Work less = 1 and continue working = 0. 

output_300 <- glm(ubi_300 ~ hours + income_cat + age + job_ashamed + job_satisfied + job_interesting + job_repetitive + job_boring, data = dat_1, family=binomial())

tbl_regression(output_300, exponentiate = TRUE, include = c(hours, income_cat),
               label = list(hours ~ "Weekly work hours", 
                            income_cat ~ "Income category")) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  as_gt() %>%
  gt::tab_header(title = "Attitudes under UBI = EUR 300")

#behavior under UBI = 500 and hours worked
output_500 <- glm(ubi_500 ~ hours + income_cat + age + job_satisfied + job_interesting + job_repetitive + job_boring, data = dat_1, family=binomial())
tbl_regression(output_500, exponentiate = TRUE, include = c(hours, income_cat),
               label = list(hours ~ "Weekly work hours", 
                            income_cat ~ "Income category")) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  as_gt() %>%
  gt::tab_header(title = "Weekly work hours and UBI = 500")


#behavior under UBI = 800 and hours worked
output_800 <- glm(ubi_800 ~ hours + income_cat + age + job_satisfied + job_interesting + job_repetitive + job_boring, data = dat_1, family=binomial())
tbl_regression(output_800, exponentiate = TRUE, include = c(hours, income_cat),
               label = list(hours ~ "Weekly work hours",
                            income_cat ~ "Income category")) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  as_gt() %>%
  gt::tab_header(title = "Weekly work hours and UBI = 800")

dat_1$ubi_300 <- as.numeric(dat_1$ubi_300)
dat_1$ubi_500 <- as.numeric(dat_1$ubi_500)
dat_1$ubi_800 <- as.numeric(dat_1$ubi_800)

#behavior under UBI and job scales
output_scales_300 <- lm(ubi_300 ~ job_good +  hours + gender + income_cat, data = dat_1)
tab_scales_300 <- tbl_regression(output_scales_300, include = job_good)

output_scales_500 <- lm(ubi_500 ~ job_good + hours + gender + income_cat, data = dat_1)
tab_scales_500 <- tbl_regression(output_scales_500, include = job_good)

output_scales_800 <- lm(ubi_800 ~ job_good + hours + gender + income_cat, data = dat_1)
tab_scales_800 <- tbl_regression(output_scales_800, include = job_good)

tab_scales_merged <- tbl_merge(tbls = list(tab_scales_300, tab_scales_500, tab_scales_800),
                               tab_spanner = c("**UBI = EUR 300**", "**UBI = EUR 500**", "**UBI = EUR 800**"))


#Interaction job_good:hours
table1::label(dat_1$hours) <- "Weekly work hours"
output_scales_300_int <- lm(ubi_300 ~ job_good:hours + gender + income_cat, data = dat_1)
tab_scales_300_int <- tbl_regression(output_scales_300_int, include = -c(gender, income_cat))

output_scales_500_int <- lm(ubi_500 ~ job_good:hours + gender + income_cat, data = dat_1)
tab_scales_500_int <- tbl_regression(output_scales_500_int, include = -c(gender, income_cat))

output_scales_800_int <- lm(ubi_800 ~ job_good:hours + gender + income_cat, data = dat_1)
tab_scales_800_int <- tbl_regression(output_scales_800_int, include = -c(gender, income_cat))

tab_scales_merged_int <- tbl_merge(tbls = list(tab_scales_300_int, tab_scales_500_int, tab_scales_800_int),
                               tab_spanner = c("**UBI = EUR 300**", "**UBI = EUR 500**", "**UBI = EUR 800**"))
