#load libraries

pacman::p_load(haven, tidyverse, ggplot2, table1, readxl, gtsummary, writexl, stargazer, questionr, flextable, magrittr)

#read and clean data from German survey 

dat_de <- read_excel("responses-de.xlsx")

dat_de_1 <- dat_de %>% 
  rename("empl" = "Sind Sie derzeit beschäftigt oder waren Sie in den letzten 12 Monaten beschäftigt?", 
         "ger" = "Leben Sie derzeit in Deutschland?", 
         "income" = "Was ist Ihr derzeitiges Bruttojahreseinkommen (EUR)?", 
         "gender" = "Was ist Ihr Geschlecht?",
         "age" = "Was ist Ihr Alter?",
         "hours" = "Wie viele Stunden arbeiten Sie durchschnittlich pro Woche?", 
         "job_boring" = "Ich finde meinen Job langweilig",
         "job_interesting" = "Ich finde meinen Job interessant", 
         "job_repetitive" = "Mein Job besteht aus sich wiederholenden Aufgaben", 
         "job_stimulating" = "Ich empfinde meine Arbeit als anregend", 
         "job_social_status" = "Mein Beruf verleiht mir sozialen Status", 
         "job_ashamed" = "Ich schäme mich für meine Arbeit", 
         "job_satisfied" = "Ich bin mit meiner Arbeit zufrieden", 
         "job_unsatisfied" = "Ich bin mit meiner Arbeit nicht zufrieden", 
         "ubi_300" = "Das bedingungslose Grundeinkommen ist definiert als ein monatlicher Geldtransfer, der unabhängig von jeglichen Anforderungen ist. Was würden Sie mit einem Grundeinkommen von 300 EUR tun?", 
         "ubi_500" = "Was würden Sie mit einem Grundeinkommen von 500 EUR tun?",
         "ubi_800" = "Was würden Sie mit einem Grundeinkommen von 800 EUR tun?",
         "what_income_300" = "Wie würden Sie Ihr zusätzliches Einkommen verwenden?...17",
         "what_income_500" = "Wie würden Sie Ihr zusätzliches Einkommen verwenden?...22", 
         "what_income_800" = "Wie würden Sie Ihr zusätzliches Einkommen verwenden?...27",
         "what_less_300" = "Um wie viel Prozent würden Sie Ihre Wochenarbeitszeit idealerweise reduzieren wollen?...18", 
         "what_less_500" = "Um wie viel Prozent würden Sie Ihre Wochenarbeitszeit idealerweise reduzieren wollen?...23",
         "what_less_800" = "Um wie viel Prozent würden Sie Ihre Wochenarbeitszeit idealerweise reduzieren wollen?...28",
         "what_time_300" = "Was würden Sie mit Ihrer zusätzlichen Freizeit machen?...19",
         "what_time_500" = "Was würden Sie mit Ihrer zusätzlichen Freizeit machen?...24",
         "what_time_800" = "Was würden Sie mit Ihrer zusätzlichen Freizeit machen?...29",
         "change_500" = "Stellen Sie sich nun ein monatliches Grundeinkommen von 500 EUR vor. Würden sich Ihre bisherigen Antworten ändern?",
         "change_800" = "Stellen Sie sich nun ein monatliches Grundeinkommen von 800 EUR vor. Würden sich Ihre bisherigen Antworten ändern?", 
         "time_change" = 'Angenommen, Sie erhalten ab morgen ein Grundeinkommen. Über welchen Zeitraum erwarten Sie, dass sich Ihre Chancen auf einen Arbeitsplatz verbessern werden?', 
         "expect" = "Wählen Sie aus, welche der folgenden Punkte auf Sie zutreffen.")

dat_de_1 <- dat_de_1 %>% 
  filter(empl == 1)

dat_de_2 <- dat_de_1 %>%  
  replace_na(list("job_boring" = 0, 
                  "job_interesting" = 0, 
                  "job_repetitive" = 0, 
                  "job_stimulating" = 0, 
                  "job_social_status" = 0, 
                  "job_ashamed" = 0, 
                  "job_satisfied" = 0, 
                  "job_unsatisfied" = 0)) %>% 
  mutate("job_boring" = factor(ifelse(job_boring == 0, 0, 1)),
         "job_interesting" = factor(ifelse(job_interesting == 0, 0, 1)),
         "job_repetitive" = factor(ifelse(job_repetitive == 0, 0, 1)),
         "job_stimulating" = factor(ifelse(job_stimulating == 0, 0, 1)),
         "job_social_status" = factor(ifelse(job_social_status == 0, 0, 1)),
         "job_ashamed" = factor(ifelse(job_ashamed == 0, 0, 1)),
         "job_satisfied" = factor(ifelse(job_satisfied == 0, 0, 1)),
         "job_unsatisfied" = factor(ifelse(job_unsatisfied == 0, 0, 1)),
         hours = sub("Weniger als 10", "0-10", hours))

dat_de_3 <- dat_de_2 %>% 
  mutate(ubi_300 = case_when( 
    ubi_300 == "Gleich viel arbeiten wie jetzt" ~ "Work the same amount as now", 
    ubi_300 == "Weniger arbeiten" ~ "Work less", 
    ubi_300 == "Aufhören zu arbeiten" ~ "Stop working"), 
    ubi_500 = case_when( 
      ubi_500 == "Gleich viel arbeiten wie jetzt" ~ "Work the same amount as now", 
      ubi_500 == "Weniger arbeiten" ~ "Work less", 
      ubi_500 == "Aufhören zu arbeiten" ~ "Stop working"), 
    ubi_800 = case_when( 
      ubi_800 == "Gleich viel arbeiten wie jetzt" ~ "Work the same amount as now", 
      ubi_800 == "Weniger arbeiten" ~ "Work less", 
      ubi_800 == "Aufhören zu arbeiten" ~ "Stop working"), 
    gender = case_when(
      gender == "Männlich" ~ "Male", 
      gender == "Weiblich" ~ "Female"), 
    what_income_300 = case_when(
      what_income_300 == "Es investieren" ~ "Invest it", 
      what_income_300 == "Es sparen" ~ "Save it", 
      what_income_300 == "Sonstiges" ~ "Other", 
      what_income_300 == "Es für die berufliche Entwicklung ausgeben" ~ "Spend it for career development training", 
      what_income_300 == "Es für materielle Güter ausgeben" ~ "Spend it for material goods"),
    what_income_500 = case_when(
      what_income_500 == "Es investieren" ~ "Invest it", 
      what_income_500 == "Es sparen" ~ "Save it", 
      what_income_500 == "Sonstiges" ~ "Other", 
      what_income_500 == "Es für die berufliche Entwicklung ausgeben" ~ "Spend it for career development training", 
      what_income_500 == "Es für materielle Güter ausgeben" ~ "Spend it for material goods"), 
    what_income_800 = case_when(
      what_income_800 == "Es investieren" ~ "Invest it", 
      what_income_800 == "Es sparen" ~ "Save it", 
      what_income_800 == "Sonstiges" ~ "Other", 
      what_income_800 == "Es für die berufliche Entwicklung ausgeben" ~ "Spend it for career development training", 
      what_income_800 == "Es für materielle Güter ausgeben" ~ "Spend it for material goods"),
    what_time_300 = case_when(
      what_time_300 == "Training zur Karriereentwicklung" ~ "Career development training", 
      what_time_300 == "Bildung" ~ "Education", 
      what_time_300 == "Sonstiges" ~ "Other"),
    what_time_500 = case_when(
      what_time_500 == "Training zur Karriereentwicklung" ~ "Career development training", 
      what_time_500 == "Bildung" ~ "Education", 
      what_time_500 == "Sonstiges" ~ "Other"),
    what_time_800 = case_when(
      what_time_800 == "Training zur Karriereentwicklung" ~ "Career development training", 
      what_time_800 == "Bildung" ~ "Education", 
      what_time_800 == "Sonstiges" ~ "Other"),
    time_change = case_when( 
      time_change == "Innerhalb der nächsten 3 Monate." ~ "Over the next few months", 
      time_change == "Innerhalb der nächsten 12 Monate." ~ "Within the next 12 months", 
      time_change == "Länger als ein Jahr." ~ "In more than a year"),
    expect = case_when(
      expect == "Ich bin derzeit teilzeitbeschäftigt, und das Grundeinkommen würde meine Chancen erhöhen, eine Vollzeitbeschäftigung zu finden." ~ "Employed part-time, more chances of full-time employment with UBI", 
      expect == "Keiner der oben genannten Punkte. " ~ "No change with UBI", 
      expect == "Ich bin derzeit arbeitslos und das Grundeinkommen würde meine Chancen auf eine Teilzeitbeschäftigung erhöhen." ~ "Unemployed, more chances of part-time employment with UBI", 
      expect == "Ich bin derzeit arbeitslos und das Grundeinkommen würde meine Chancen auf eine Vollzeitbeschäftigung erhöhen." ~ "Unemployed, more chances of full-time employment with UBI"
    )) %>% 
  replace_na(list("expect" = "No change with UBI."))

#When change_500 = 0 => ubi_500 = NA, it means that the response would not change from the previous one, hence: when change_NA = 0 => ubi_500 = ubi_300 
#when change_800 = 0 => ubi_800 = NA => ubi_800 = ubi_500 

dat_de_4 <- dat_de_3 #duplicate data
dat_de_4$ubi_500[is.na(dat_de_4$ubi_500)] <- dat_de_4$ubi_300[is.na(dat_de_4$ubi_500)]#replace missing values of ubi_500 with values from ubi_300

dat_de_5 <- dat_de_4
dat_de_5$ubi_800[is.na(dat_de_5$ubi_800)] <- dat_de_5$ubi_500[is.na(dat_de_5$ubi_800)] #replace missing values of ubi_800 with values from ubi_500

dat_de_5 <- dat_de_5 %>% select(- '#')
dat_de_5 <- dat_de_5[, 1:30]

#arrange alphabetically 
dat_de_5 <- dat_de_5 %>% select(order(colnames(dat_de_5)))

dat_de_6 <- dat_de_5 %>%
  mutate(age = gsub(" ", "", age), 
         income = gsub(" ", "", income), 
         hours = gsub(" ", "", hours))

#load and clean data from survey in English 

dat1 <- read_excel("responses-eng.xlsx")

dat1 <- dat1 %>% 
  rename("empl" = "Are you currently employed or have been employed during the last 12 months?", 
         "ger" = "Are you currently living in Germany?", 
         "income" = "What is your current gross annual income (EUR)?", 
         "gender" = "What is your gender?",
         "age" = "How old are you?",
         "hours" = "How many weekly hours are you currently working?", 
         "job_boring" = "I consider my job boring",
         "job_interesting" = "I consider my job interesting", 
         "job_repetitive" = "My job consists of repetitive tasks", 
         "job_stimulating" = "I perceive my job as stimulating", 
         "job_social_status" = "My job gives me social status", 
         "job_ashamed" = "I am ashamed of my job", 
         "job_satisfied" = "I am satisfied with my job", 
         "job_unsatisfied" = "I am not satisfied with my job", 
         "ubi_300" = "Universal basic income (UBI) is defined as a monthly transfer of money, independent of any requirements. What would you do with a UBI of EUR 300?", 
         "what_income_300" = "How would you use your additional income?...16", 
         "what_less_300" = "Ideally, around what percentage would you like to reduce your weekly working hours?...17", 
         "what_time_300" = "What would you do with the extra free time?", 
         "change_500" = "Consider now a monthly UBI of EUR 500. Would your previous answers change?", 
         "ubi_500" = "What would you do with a monthly UBI of EUR 500?", 
         "what_less_500" = "Ideally, around what percentage would you like to reduce your weekly working hours?...21", 
         "what_time_500" = "What would you do with your additional free time?...22", 
         "what_income_500" = "What would you do with your additional income?", 
         "change_800" = "Finally, consider a monthly UBI of EUR 800. Would this change your previous answers?", 
         "ubi_800" = "What would you do with a monthly UBI of EUR 800?", 
         "what_income_800" = "How would you use your additional income?...26", 
         "what_less_800" = "Ideally, around what percentage would you like to reduce your weekly working hours?...27", 
         "what_time_800" = "What would you do with your additional free time?...28", 
         "time_change" = "Assume you start receiving UBI from tomorrow. Over what period of time do you expect your chances of finding employment to improve?", 
         "expect" = "Select which of the following applies to you.")  %>% 
  replace_na(list("job_boring" = 0, 
                  "job_interesting" = 0, 
                  "job_repetitive" = 0, 
                  "job_stimulating" = 0, 
                  "job_social_status" = 0, 
                  "job_ashamed" = 0, 
                  "job_satisfied" = 0, 
                  "job_unsatisfied" = 0)) %>% 
  mutate("job_boring" = factor(ifelse(job_boring == 0, 0, 1)),
         "job_interesting" = factor(ifelse(job_interesting == 0, 0, 1)),
         "job_repetitive" = factor(ifelse(job_repetitive == 0, 0, 1)),
         "job_stimulating" = factor(ifelse(job_stimulating == 0, 0, 1)),
         "job_social_status" = factor(ifelse(job_social_status == 0, 0, 1)),
         "job_ashamed" = factor(ifelse(job_ashamed == 0, 0, 1)),
         "job_satisfied" = factor(ifelse(job_satisfied == 0, 0, 1)),
         "job_unsatisfied" = factor(ifelse(job_unsatisfied == 0, 0, 1)),
         hours = sub("Less than 10", "0-10", hours))


dat_1 <- dat1 %>% 
  mutate(expect = case_when(
    expect == "I am currently employed part-time and UBI would increase my chances of finding full-time employment." ~ "Employed part-time, more chances of full-time employment with UBI", 
    expect == "None of the above" ~ "No change with UBI", 
    expect == "I am currently unemployed and UBI would increase my chances of finding part-time employment." ~ "Unemployed, more chances of part-time employment with UBI", 
    expect == "I am currently unemployed and UBI would increase my chances of finding full-time employment." ~ "Unemployed, more chances of full-time employment with UBI"
  )) %>% 
  replace_na(list("expect" = "No change with UBI."))

#Removing invalid responses (unemployed people)
dat_1 <- dat_1 %>% 
  filter(empl == 1)

#When change_500 = 0 => ubi_500 = NA, it means that the response would not change from the previous one, hence: when change_NA = 0 => ubi_500 = ubi_300 
#when change_800 = 0 => ubi_800 = NA => ubi_800 = ubi_500 

dat2 <- dat_1 #duplicate data
dat2$ubi_500[is.na(dat2$ubi_500)] <- dat2$ubi_300[is.na(dat2$ubi_500)] #replace missing values of ubi_500 with values from ubi_300

dat3 <- dat2 #duplicate data
dat3$ubi_800[is.na(dat3$ubi_800)] <- dat3$ubi_500[is.na(dat3$ubi_800)] #replace missing values of ubi_800 with values from ubi_500


#Ensuring response "Stop working" is included even if N=0 
dat3$ubi_300 <- fct_expand(dat3$ubi_300, "Stop working")
dat3$ubi_300 <- relevel(dat3$ubi_300, ref = "Work the same amount as now")
dat3$ubi_500 <- fct_expand(dat3$ubi_500, "Stop working")
dat3$ubi_500 <- relevel(dat3$ubi_500, ref = "Work the same amount as now")
dat3$ubi_800 <- fct_expand(dat3$ubi_800, "Stop working")
dat3$ubi_800 <- relevel(dat3$ubi_800, ref = "Work the same amount as now")

#arrange columns alphabetically 
dat3 <- dat3 %>% select(order(colnames(dat3)))

##remove spaces between - in levels
dat4 <- dat3 %>% 
  mutate(age = gsub(" ", "", age), 
         income = gsub(" ", "", income),
         hours = gsub(" ", "", hours))

#merge both datasets
dat <- rbind(dat_de_6, dat4)

#Further data cleaning
#formatting income levels, fixing typos
dat<- dat %>% 
  mutate(
    income = gsub("0-15.000", "0-15,000", income),
    income = gsub("15.000-30.000", "15,000-30,000", income),
    income = gsub("30.000-45.000", "30,000-45,000", income),
    income = gsub("45.000-60.000" ,  "45,000-60,000", income),
    income = gsub("60.000", "60,000", income))


dat <- dat %>% 
  mutate(
    what_income_300 = gsub("Spend it in material goods", "Spend it for material goods", what_income_300),
    what_income_500 = gsub("Spent it in material goods", "Spend it for material goods", what_income_500),
    what_income_500 = gsub("Spend it in material goods", "Spend it for material goods", what_income_500), 
    what_income_800 = gsub("Spent it in material goods", "Spend it for material goods", what_income_800), 
    what_income_800 = gsub("Spend it in material goods", "Spend it for material goods", what_income_800),
    what_income_800 = gsub("Spent it for material goods", "Spend it for material goods", what_income_800), 
    what_income_300 = gsub("Spend it for career training", "Spend it for career development training", what_income_300), 
    what_income_500 = gsub("Spend it for career training", "Spend it for career development training", what_income_500),
    what_income_800 = gsub("Spend it for career training", "Spend it for career development training", what_income_800))

#all variables as factors
dat$income <- as.factor(dat$income)
dat$age <- as.factor(dat$age)
dat$gender <- as.factor(dat$gender)
dat$hours <- as.factor(dat$hours)
dat$ubi_300 <- as.factor(dat$ubi_300)
dat$ubi_500 <- as.factor(dat$ubi_500)
dat$ubi_800 <- as.factor(dat$ubi_800)
dat$what_income_300 <- as.factor(dat$what_income_300)
dat$what_income_500 <- as.factor(dat$what_income_500)
dat$what_income_800 <- as.factor(dat$what_income_800)

dat <- dat %>% replace_na(list(c("what_less_300" = "No reduction"),
                               ("what_less_500" = "No reduction"),
                               ("what_less_800" = "No reduction")))


# job_good : job_interesting, job_satisfied, job_social_status, job_stimulating (when sum > 2)
# job_bad : job_boring, job_ashamed, job_repetitive, job_unsatisfied 
dat <- dat %>% mutate(job_interesting = as.numeric(job_interesting), 
                      job_satisfied = as.numeric(job_satisfied),  
                      job_social_status = as.numeric(job_social_status), 
                      job_stimulating = as.numeric(job_stimulating), 
                      job_boring = as.numeric(job_boring), 
                      job_ashamed = as.numeric(job_ashamed), 
                      job_repetitive = as.numeric(job_repetitive), 
                      job_unsatisfied = as.numeric(job_unsatisfied)) 
#1 = 0, 2 = 1 
dat <- dat %>% 
  mutate(job_interesting = case_when(
    job_interesting == 1 ~ 0, 
    job_interesting == 2 ~ 1), 
    job_satisfied = case_when(
      job_satisfied == 1 ~ 0,
      job_satisfied == 2 ~ 1),
    job_social_status = case_when(
      job_social_status == 1 ~ 0,
      job_social_status == 2 ~ 1),
    job_stimulating = case_when(
      job_stimulating == 1 ~ 0, 
      job_stimulating == 2 ~ 1),
    job_boring = case_when(
      job_boring == 1 ~ 0,
      job_boring == 2 ~ 1), 
    job_ashamed = case_when(
      job_ashamed == 1 ~ 0,
      job_ashamed == 2 ~ 1), 
    job_repetitive = case_when(
      job_repetitive == 1 ~ 0,
      job_repetitive == 2 ~ 1), 
    job_unsatisfied = case_when(
      job_unsatisfied == 1 ~ 0,
      job_unsatisfied == 2 ~ 1)) 

#0 = min satisfaction/unsatisfaction, 4 = max satisfaction/unsatisfaction i
dat <- dat %>% 
  mutate(job_good =
           (job_interesting + job_satisfied + job_social_status + job_stimulating), 
         job_bad = (job_boring + job_ashamed + job_repetitive + job_unsatisfied))

#SUMMARY TABLES 
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
dat$what_time_300 <- as.factor(dat$what_time_300)
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

#MODELS

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










