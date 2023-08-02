library(tidyverse)
library(scales)
library(haven)

hse_2019 <- read_spss("Z:/IA_HealthSurveyEngland/Data/HSE 2019 SL 20220330.sav")
hse_2018 <- read_spss("Z:/IA_HealthSurveyEngland/Data/HSE 2018 SL 20220412.sav")
hse_2017 <- read_dta("Z:/IA_HealthSurveyEngland/Data/hse2017_eul.dta")
hse_2016 <- read_spss("Z:/IA_HealthSurveyEngland/Data/hse2016_eul.sav")

# economic activity status by BMI
 hse_2018 %>% 
  filter(age16g5 >= 2 & # working age
           age16g5<12,
         BMIOK == 1) %>% # 18+ with valid BMI
  mutate(BMI_cat = case_when(BMI>18.5 & BMI<25 ~ 'healthy',
                              BMI>30 ~ 'obese'),
         Economic_Status = case_when(HRPactIv2 == 1 ~ 'healthy', # activity status
                                     HRPactIv2 == 2 ~ 'healthy',
                                     HRPactIv2 == 3 ~ 'healthy',
                                     HRPactIv2 == 4 ~ 'healthy',
                                     HRPactIv2 == 5 ~ 'healthy',
                                     HRPactIv2 == 6 ~ 'healthy',
                                     HRPactIv2 == 7 ~ 'long-term sick',
                                     HRPactIv2 == 8 ~ 'healthy',
                                     HRPactIv2 == 9 ~ 'healthy')) %>% 
  filter(!is.na(Economic_Status)) %>% # remove other activity
  group_by(BMI_cat,Economic_Status) %>% 
  summarise(num = sum(wt_int)) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(BMI_cat) %>% 
  mutate(p = num / sum(num)) %>% 
   filter(Economic_Status == 'long-term sick')

 # economic activity status by BMI
 hse_2019 %>% 
   filter(age16g5 >= 2 & # working age
            age16g5<12,
          BMIOK == 1) %>% # 18+ with valid BMI
   mutate(BMI_cat = case_when(BMI>18.5 & BMI<25 ~ 'healthy',
                               BMI>30 ~ 'obese'),
          Economic_Status = case_when(HRPactIv3 == 1 ~ 'healthy', # activity status
                                      HRPactIv3 == 2 ~ 'healthy',
                                      HRPactIv3 == 3 ~ 'healthy',
                                      HRPactIv3 == 4 ~ 'healthy',
                                      HRPactIv3 == 5 ~ 'healthy',
                                      HRPactIv3 == 6 ~ 'long-term sick',
                                      HRPactIv3 == 7 ~ 'healthy',
                                      HRPactIv3 == 8 ~ 'healthy')) %>% 
   filter(!is.na(Economic_Status)) %>% # remove other activity
   group_by(BMI_cat,Economic_Status) %>% 
   summarise(num = sum(wt_int)) %>% 
   unique() %>% 
   ungroup() %>% 
   group_by(BMI_cat) %>% 
   mutate(p = num / sum(num)) %>% 
   filter(Economic_Status == 'long-term sick')

 
 # economic activity status by BMI
 hse_2017 %>% 
    filter(Age35g > 7 & # working age
              Age35g <17,
           BMIok == 1) %>% # 18+ with valid BMI
    mutate(BMI_cat = case_when( BMI>18.5 & BMI<25 ~ 'healthy',
                                BMI>30 ~ 'obese'),
           Economic_Status = case_when(HRPactiv2 == 1 ~ 'healthy', # activity status
                                       HRPactiv2 == 2 ~ 'healthy',
                                       HRPactiv2 == 3 ~ 'healthy',
                                       HRPactiv2 == 4 ~ 'healthy',
                                       HRPactiv2 == 5 ~ 'healthy',
                                       HRPactiv2 == 6 ~ 'healthy',
                                       HRPactiv2 == 7 ~ 'long-term sick',
                                       HRPactiv2 == 8 ~ 'healthy',
                                       HRPactiv2 == 9 ~ 'healthy')) %>% 
    filter(!is.na(Economic_Status)) %>% # remove other activity
    group_by(BMI_cat,Economic_Status) %>% 
    summarise(num = sum(wt_int)) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(BMI_cat) %>% 
    mutate(p = num / sum(num)) %>% 
    filter(Economic_Status == 'long-term sick')
 
 
 # economic activity status by BMI
 hse_2016 %>% 
   filter(Age35g > 7 & # working age
            Age35g <17,
          BMIok == 1) %>% # 18+ with valid BMI
   mutate(BMI_cat = case_when( BMI>18.5 & BMI<25 ~ 'healthy',
                               BMI>30 ~ 'obese'),
          Economic_Status = case_when(HRPactiv3 == 1 ~ 'healthy', # activity status
                                      HRPactiv3 == 2 ~ 'healthy',
                                      HRPactiv3 == 3 ~ 'healthy',
                                      HRPactiv3 == 4 ~ 'healthy',
                                      HRPactiv3 == 5 ~ 'healthy',
                                      HRPactiv3 == 6 ~ 'long-term sick',
                                      HRPactiv3 == 7 ~ 'healthy',
                                      HRPactiv3 == 8 ~ 'healthy')) %>% 
   filter(!is.na(Economic_Status)) %>% # remove other activity
   group_by(BMI_cat,Economic_Status) %>% 
   summarise(num = sum(wt_int)) %>% 
   unique() %>% 
   ungroup() %>% 
   group_by(BMI_cat) %>% 
   mutate(p = num / sum(num)) %>% 
   filter(Economic_Status == 'long-term sick')
 
 