pacman::p_load("haven", "tidyverse", "ggplot2", "table1", "readxl", "gtsummary")

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




