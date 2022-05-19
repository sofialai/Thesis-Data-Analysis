#Labels
table1::label(dat$age) <- "Age"
table1::label(dat$gender) <- "Gender"
table1::label(dat$income) <- "Income"
table1::label(dat$hours) <- "Weekly work hours"
table1::label(dat$ubi_300) <- "Behavior with monthly UBI = EUR 300"
table1::label(dat$ubi_500) <- "Behavior with monthly UBI = EUR 500"
table1::label(dat$ubi_800) <- "Behavior with monthly UBI = EUR 800"
table1::label(dat$job_ashamed) <- "Ashamed of own job"
table1::label(dat$job_boring) <- "Perceives own job as boring"
table1::label(dat$job_interesting) <- "Perceives own job as interesting"
table1::label(dat$job_repetitive) <- "Job consists of repetitive tasks"
table1::label(dat$job_stimulating) <- "Perceives own job as stimulating"
table1::label(dat$job_social_status) <- "Perceives own job as status-giving"
table1::label(dat$job_satisfied) <- "Satisfied of own job"
table1::label(dat$job_unsatisfied) <- "Unsatisfied of own job"
table1::label(dat$change_500) <- "Change of behavior if UBI increases to EUR 500"
table1::label(dat$change_800) <- "Change of behavior if UBI increases to EUR 800"
table1::label(dat$job_good) <- "Positive job perception"
table1::label(dat$job_bad) <- "Negative job perception"

#Sample stats 
summary_stats <- table1(~ ubi_300 + ubi_500 + ubi_800 + age + gender + hours + income, data = dat)
t1flex(summary_stats) %>% 
  save_as_docx(path="summary_stats.docx")

#summary stats by income category 
dat_2 <- dat %>% 
  mutate(income_cat = case_when( 
    income == "0-15,000" ~ "1. Low (0-15,000 EUR)", 
    income == "15,000-30,000" ~ "2. Middle (15,000-45,000 EUR)",
    income == "30,000-45,000" ~ "2. Middle (15,000-45,000 EUR)",
    income == "45,000-60,000" ~ "3. High (45,000+)",
    income == "60,000+" ~ "3. High (45,000+)"))
table1::label(dat_2$income_cat) <- "Income category"

summary_stats_income_cat <- table1(~ ubi_300 + ubi_500 + ubi_800 | income_cat, data = dat_2) 
t1flex(summary_stats_income_cat) %>% 
  save_as_docx(path="summary_stats_income_cat.docx")

#Job satisfaction and attitudes with UBI = EUR 300
tab_job_satis_300 <- table1(~ job_interesting + job_repetitive + job_stimulating + job_social_status + job_satisfied | ubi_300, data = dat)
t1flex(tab_job_satis_300) %>% 
  save_as_docx(path="tab_job_satis_300.docx")

#Job satisfaction and attitudes with UBI = EUR 500
tab_job_satis_500 <- table1(~ job_interesting + job_repetitive + job_stimulating + job_social_status + job_satisfied | ubi_500, data = dat)
t1flex(tab_job_satis_500) %>% 
  save_as_docx(path="tab_job_satis_500.docx")

#Job satisfaction 
tab_job_satis <- table1(~ job_ashamed + job_boring + job_interesting + job_repetitive + job_stimulating + job_social_status + job_satisfied + job_unsatisfied, data = dat)
t1flex(tab_job_satis) %>% 
  save_as_docx(path="tab_job_satis.docx")

tab_ubi_job_stim <- table1(~ ubi_300 + ubi_500 + ubi_800 | job_stimulating, data = dat) 
t1flex(tab_ubi_job_stim) %>% 
  save_as_docx(path="tab_ubi_job_stim.docx")
tab_ubi_job_satis <- table1(~ ubi_300 + ubi_500 + ubi_800 | job_satisfied, data = dat) 
t1flex(tab_ubi_job_satis) %>% 
  save_as_docx(path="tab_ubi_job_satis.docx")

#Summary job satisfaction scales 
tab_job_scales <- table1(~ job_good + job_bad, data = dat)
t1flex(tab_job_scales) %>% 
  save_as_docx(path="tab_job_scales.docx")


#Summary attitudes on UBI by income
table1(~ ubi_300 + ubi_500 + ubi_800 | income, data = dat) 

#Summary attitudes on UBI by work hours 
table1(~ ubi_300 + ubi_500 + ubi_800 | hours, data = dat)

#Summary attitudes on changing the response on UBI, 300 to 500 
table1(~ change_500 + change_800 | ubi_300, data = dat)

#Summary attitudes on changing the response on UBI, 500 to 800
table1(~ change_500 + change_800 | ubi_500, data = dat)

#Summary attitudes for "what would you do with your additional income? 
dat <- dat %>% mutate(what_income_500 = as.factor(what_income_500))
table1::label(dat$what_income_300) <- "What would you do with your additional income? (UBI = EUR 300)"
table1::label(dat$what_income_500) <- "What would you do with your additional income? (UBI = EUR 500)"
table1::label(dat$what_income_800) <- "What would you do with your additional income? (UBI = EUR 800)"

dat_add_income_300 <- dat %>% 
  filter(!is.na(what_income_300))

tab_add_income_300 <- table1(~ what_income_300, data = dat_add_income_300)
t1flex(tab_add_income_300) %>% 
  save_as_docx(path="tab_add_income_300.docx")

#tables on use of income for "work the same amount as now"
dat_500 <- dat %>% 
  filter(ubi_500 == "Work the same amount as now") %>% 
  mutate(what_income_300 = as.character(what_income_300), 
         what_income_500 = as.character(what_income_500))
  
dat_500$what_income_500[is.na(dat_500$what_income_500)] <- dat_500$what_income_300[is.na(dat_500$what_income_500)]

dat_500 <- dat_500 %>% 
  mutate(what_income_300 = as.factor(what_income_300), 
         what_income_500 = as.factor(what_income_500))

tab_add_income_500 <- table1(~ what_income_500, data = dat_500)
t1flex(tab_add_income_500) %>% 
  save_as_docx(path="tab_add_income_500.docx")

dat_800 <- dat_500 %>% 
  filter(ubi_800 == "Work the same amount as now") %>% 
  mutate(what_income_300 = as.character(what_income_300), 
         what_income_500 = as.character(what_income_500),
         what_income_800 = as.character(what_income_800))

dat_800$what_income_800[is.na(dat_800$what_income_800)] <- dat_800$what_income_500[is.na(dat_800$what_income_800)]

dat_800 <- dat_800 %>% 
  mutate(what_income_300 = as.factor(what_income_300), 
         what_income_500 = as.factor(what_income_500),
         what_income_800 = as.factor(what_income_800))

tab_add_income_800 <- table1(~ what_income_800, data = dat_800)
t1flex(tab_add_income_800) %>% 
  save_as_docx(path="tab_add_income_800.docx")


#Tables on use of time for "work less"

table1::label(dat$what_time_500) <- "How would you use your additional time? (UBI = EUR 500)"
table1::label(dat$what_time_800) <- "How would you use your additional time? (UBI = EUR 800)"

#if change_500 = 0 => ubi_500 = ubi_300 => uf ubi_500 = Work less & what_time_500 = NA => what_time_500 = what_time_300 

dat$what_time_500 <- as.factor(dat$what_time_500)
dat$what_time_800 <- as.factor(dat$what_time_800)

dat_add_time_work_less_300 <- dat %>% 
  filter(!is.na(what_time_300) & ubi_300 == "Work less") 

dat_add_time_work_less_300$what_time_300 <- as.factor(dat_add_time_work_less_300$what_time_300) 
table1::label(dat_add_time_work_less_300$what_time_300) <- "How would you use your additional time? (UBI = EUR 300)"

tab_add_time_work_less_300 <- table1(~ what_time_300, data = dat_add_time_work_less_300)
t1flex(tab_add_time_work_less_300) %>% 
  save_as_docx(path="tab_add_time_work_less_300.docx") 

dat_add_time_work_less_500 <- dat %>% 
  select(ubi_300, ubi_500, change_500, what_time_300, what_time_500) %>% 
  filter(ubi_300 == "Work less" | ubi_500 == "Work less") %>% 
  mutate(what_time_500 = case_when( 
    ubi_500 == "Work less" & change_500 == 0 ~ what_time_300, 
    ubi_500 == "Work less" & change_500 == 1 ~ what_time_500))


dat_add_time_work_less_500$what_time_500 <- as.factor(dat_add_time_work_less_500$what_time_500) 
table1::label(dat_add_time_work_less_500$what_time_500) <- "How would you use your additional time? (UBI = EUR 500)"

tab_add_time_work_less_500 <- table1(~ what_time_500, data = dat_add_time_work_less_500)
t1flex(tab_add_time_work_less_500) %>% 
  save_as_docx(path="tab_add_time_work_less_500.docx") 


dat_add_time_work_less_800 <- dat %>% 
  select(ubi_300, ubi_500, ubi_800, change_500, change_800, what_time_300, what_time_500, what_time_800) %>% 
  filter(ubi_300 == "Work less" | ubi_500 == "Work less" | ubi_800 == "Work less") %>% 
  mutate(what_time_500 = case_when( 
    ubi_500 == "Work less" & change_500 == 0 ~ what_time_300, 
    ubi_500 == "Work less" & change_500 == 1 ~ what_time_500),
    what_time_800 = case_when( 
      ubi_800 == "Work less" & change_800 == 0 ~ what_time_500, 
      ubi_800 == "Work less" & change_800 == 1 ~ what_time_800)) %>% 
  filter(ubi_800 == "Work less")

dat_add_time_work_less_800$what_time_800 <- as.factor(dat_add_time_work_less_800$what_time_800) 
table1::label(dat_add_time_work_less_800$what_time_800) <- "How would you use your additional time? (UBI = EUR 800)"

tab_add_time_work_less_800 <- table1(~ what_time_800, data = dat_add_time_work_less_800)
t1flex(tab_add_time_work_less_800) %>% 
  save_as_docx(path="tab_add_time_work_less_800.docx") 

# same but with "Stop working", only for UBI = 800
dat_add_time_stop_working_800 <- dat %>% 
  select(ubi_300, ubi_500, ubi_800, change_500, change_800, what_time_300, what_time_500, what_time_800) %>% 
  filter(ubi_500 == "Stop working" | ubi_800 == "Stop working") %>% 
  mutate(
    what_time_800 = case_when( 
      ubi_800 == "Stop working" & change_800 == 0 ~ what_time_500, 
      ubi_800 == "Stop working" & change_800 == 1 ~ what_time_800)) %>% 
  filter(ubi_800 == "Stop working")
  

dat_add_time_stop_working_800$what_time_800 <- as.factor(dat_add_time_stop_working_800$what_time_800) 
table1::label(dat_add_time_stop_working_800$what_time_800) <- "How would you use your additional time? (UBI = EUR 800)"

tab_add_time_stop_working_800 <- table1(~ what_time_800, data = dat_add_time_stop_working_800)
t1flex(tab_add_time_stop_working_800) %>% 
  save_as_docx(path="tab_add_time_stop_working_800.docx") 


#Distribution of eduction in work hours
dat_less <- dat %>% 
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
    hours == "40+" ~ "More than 20"))
table1::label(dat_less$what_less_300) <- "Weekly hour reduction under UBI = EUR 300)"
table1::label(dat_less$what_less_500) <- "Weekly hour reduction under UBI = EUR 500)"
table1::label(dat_less$what_less_800) <- "Weekly hour reduction under UBI = EUR 800)"

dat_less_300 <- dat_less %>% filter(!is.na(what_less_300))
dat_less_500 <- dat_less %>% filter(!is.na(what_less_500))
dat_less_800 <- dat_less %>% filter(!is.na(what_less_800))

tab_hour_reduction_300 <- table1(~ what_less_300 | hours, data = dat_less_300)
t1flex(tab_hour_reduction_300) %>% 
  save_as_docx(path="tab_hour_reduction_300.docx") 

dat_less_500 <- dat_less_500 %>% 
  mutate(what_less_500 = 
           gsub("20-30% reduction", "20-30%", what_less_500))

tab_hour_reduction_500 <- table1(~ what_less_500 | hours, data = dat_less_500)
t1flex(tab_hour_reduction_500) %>% 
  save_as_docx(path="tab_hour_reduction_500.docx") 

dat_less_800 <- dat_less_800 %>% 
  mutate(what_less_800 = 
           gsub("10-30%", "10-20%", what_less_800), 
         what_less_800 = 
           gsub("20-20%", "20-30%", what_less_800))

tab_hour_reduction_800 <- table1(~ what_less_800 | hours, data = dat_less_800)
t1flex(tab_hour_reduction_800) %>% 
  save_as_docx(path="tab_hour_reduction_800.docx") 





