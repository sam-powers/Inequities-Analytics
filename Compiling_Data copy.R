library(foreign)
library(dlookr) 
library(survey)
library(srvyr)
library(DataExplorer)
library(tableone)
library(tidyverse)


setwd("/Volumes/GoogleDrive/My Drive/Fulbright [Its Happening]/Inequities & Analytics/Data")
EICV_2017_households <- read.dta("./EICV5/Data_Files/EICV5/cs_S0_S5_Household.dta")
EICV_2017_agriculture <- read.dta("./EICV5/Data_Files/EICV5/cs_S7B1_land_Agriculture.dta")
EICV_2017_persons_load <- read.dta("./EICV5/Data_Files/EICV5/cs_S1_S2_S3_S4_S6A_S6E_Person.dta")
EICV_2017_Poverty_Panel <- read.dta("./EICV5/Data_Files/EICV5/EICV5_Poverty_file.dta")
EICV_2017_ubedehe <- read.dta("./EICV5/Data_Files/EICV5/cs_S9C_Vup_ubudehe_and_Rssp_schemes.dta")
EICV_2017_expenditure1 <- read.dta("./EICV5/Data_Files/EICV5/cs_S8A1_expenditure.dta")
EICV_2017_services <- read.dta("./EICV5/Data_Files/EICV5/cs_S5F_Access_to_services.dta")


nrow(EICV_2017_households)  # 14580
nrow(EICV_2017_agriculture) # 14579
nrow(EICV_2017_ubedehe)     # 14572
nrow(EICV_2017_Poverty_Panel) # 14580
nrow(EICV_2017_persons_load)# 64314
nrow(EICV_2017_persons_load %>%
  group_by(hhid) %>%
  summarize(count = n()))   # 14580
nrow(EICV_2017_expenditure1)# 1006020
nrow(EICV_2017_expenditure1 %>%
  filter(s8a1q1 == "12.5.3.1.01")) # 14580
nrow(EICV_2017_services) # 218700
nrow(EICV_2017_services %>%
       filter(s5fq0 == "Health centre")) # 14580


interview_Dates_2017<-
  EICV_2017_households %>%
  select(hhid, s0q18y, s0q18m)
  

EICV_2017_household_level_load <-
  EICV_2017_households %>%
  inner_join(EICV_2017_agriculture) %>%
  inner_join(EICV_2017_ubedehe) %>%
  inner_join(EICV_2017_Poverty_Panel, by = c("hhid", "clust",  "district", "region", "weight", "pop_wt", "quintile"))
 

EICV_2017_household_level_load_cleanup  <-
  EICV_2017_household_level_load %>%
   rename(
    Household_Own_Land = s7b1q1,
    VUP_Benefit_2012 = s9cq4e,
    VUP_Benefit_2013 = s9cq4f,
    VUP_Benefit_2014 = s9cq4g,
    VUP_Benefit_2015 = s9cq4h,
    VUP_Benefit_2016 = s9cq4i,
    VUP_Benefit_2017 = s9cq4j,
    Ever_VUP = s9cq2,
    ubudehe =  s0qb,
    Main_Problem = s5eq1,
    Main_Problem_1 = s5eq21,
    Main_Problem_2 = s5eq22,
    Response_To_Problem_1 = s5eq3a,
    Response_To_Problem_2 = s5eq3b,
    distance_to_drinking_water = s5cq4,
    type_of_toilet = s5cq20,
    main_source_lighting = s5cq16,
    amount_paid_electricity = s5cq17,
    drinking_water = s5cq1
    
  ) %>%
  mutate(
    drinking_water_1 = as.factor(ifelse(
      s5cq5 == "Yes", as.character(s5cq3), as.character(s5cq7)
    )),
    VUP_Benefit_2017 = fct_explicit_na(VUP_Benefit_2017, na_level = "NA"),
    Ever_VUP = fct_explicit_na(Ever_VUP, na_level = "NA")
    
    )


EICV_2017_persons <-
  EICV_2017_persons_load %>%
#  inner_join(interview_Dates_2017) %>%
  
  mutate(
    Age_2016 = ifelse(s0q18y == 2017,  s1q3y - 1, s1q3y),
    have_health_insurance = ifelse(s3q2 == "None", 0, 1) ,
    have_mutual_insurance = ifelse(s3q2 == "Mutual insurance", 1, 0) ,               
    gotcare_if_ill =  ifelse(s3q3 == "Yes", ifelse(
                                                s3q4 == "Yes", 1, 0),
                             NA),
    able_to_read = ifelse(s1q3y >14, ifelse(s4bq4 == "Yes", 1, 0), NA),
    
    primary_completed = 
      ifelse(s1q3y < 13, NA,
      ifelse(s4aq2 %in% 
                                 c("Pre-primary", "Not complete P1", "Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5"), 0, 1
                               )
      ),
    
    ever_been_school = ifelse(s4aq1 == "Yes", 1, 0),
    children_in_school_year = ifelse((Age_2016 < 13 & Age_2016 > 6),
                            ifelse(s4aq7 == "Yes", 1, 0),
                                 NA),
    
    children_in_school_week = ifelse(
                                  (Age_2016 < 13 & Age_2016 > 6),
                                     ifelse(s4aq13 == "Yes",
                                        ifelse(s4aq14 == 7,
                                            ifelse(s4aq15 == "Holidys", NA , 1), 0), 0
                                     ),
                                     NA),
  
    children_in_school = ifelse(Age_2016 > 6 & Age_2016 < 13,
                                ifelse(
                                  s4aq6b %in%  c("Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5", "Primary 6,7,8"), 1 , 0)
                                , NA),
    
    children_in_secondary = ifelse(Age_2016 > 12 & Age_2016 < 19,
                                ifelse(
                                  s4aq6b %in%  c("Secondary 1", "Secondary 2", "Secondary 3", "Secondary 4", "Secondary 5", "Secondary 6"), 1 , 0)
                                , NA),
    
    children_in_any_school= ifelse(Age_2016 > 6 & Age_2016 < 19,
                                   ifelse(
                                     s4aq6b %in%  c("Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5", "Primary 6,7,8", "Secondary 1", "Secondary 2", "Secondary 3", "Secondary 4", "Secondary 5", "Secondary 6"), 1 , 0)
                                   , NA)
      ) %>%
  rename(
    reason_for_health_visit = s3q5,
    sex = s1q1
    
  ) 
  

EICV_2017_person_to_household <-
    EICV_2017_persons %>%
    group_by(hhid ) %>%
    summarize(
      have_health_insurance = mean( have_health_insurance, na.rm = TRUE),
      have_mutual_insurance = mean(have_mutual_insurance, na.rm = TRUE),
      gotcare_if_ill = mean(gotcare_if_ill, na.rm = TRUE),
      able_to_read = mean(able_to_read, na.rm = TRUE)
    )

  
EICV_2017_heads_of_households <-
    EICV_2017_persons %>%
       filter(s1q2 == "Household head _HH_") %>%
         transmute(
        hhid = hhid,
         househead_completed_primary = ifelse( 
            s1q2 == "Household head _HH_", ifelse(
            s4aq3 %in% c("No Diploma",NA), 0, 1), 
                NA),

         househead_completed_secondary = ifelse( 
            s1q2 == "Household head _HH_", ifelse(
            s4aq3 %in% c( "No Diploma", "Primary completed", "Post primary certificate", NA), 0, 1),
                 NA),
         
         househead_read = ifelse(
           s4bq4 == "Yes", 1, 0
         )
         
)

  
EICV_2017_Healthcare_Expenditure <-
  EICV_2017_expenditure1 %>% 
    filter(s8a1q1 == "12.5.3.1.01") %>%
      rename(
         healthcare_purchased_this_year = s8a1q2,
         amount_spent_healthcare = s8a1q3,
         where_purchased_healthcare = s8a1q4
         
       )

EICV_2017_Access_Health_Center <-
EICV_2017_services %>%
  filter(s5fq0 == "Health centre") %>%
  transmute(
    hhid = hhid,
    Often_Use_HC = s5fq1,
    Reason_Little_Used_HC = s5fq2,
    Time_Hours_HC = s5fq3h,
    Time_Mins_HC = s5fq3m,
    Distance_HC = s5fq4,
    Satisfaction_HC = s5fq6,
    Quality_Trend_HC = s5fq7
  )

  
EICV_2017_household_level <-
  EICV_2017_household_level_load_cleanup %>%
  inner_join(EICV_2017_person_to_household) %>%
  inner_join(EICV_2017_heads_of_households) %>%
  inner_join(EICV_2017_Healthcare_Expenditure) %>%
  inner_join(EICV_2017_Access_Health_Center) %>%
  mutate(province = province.x,
         ur = ur.x
         )
nrow(EICV_2017_household_level)
nrow(EICV_2017_persons)



#### Earlier Data ####
EICV_2014_households <- read.spss("./EICV5/Data_Files/EICV4/cs_S0_S5_Household.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_agriculture <- read.spss("./EICV5/Data_Files/EICV4/cs_s7b1_land_agriculture.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_persons_load <- read.spss("./EICV5/Data_Files/EICV4/cs_s1_s2_s3_s4_s6a_s6e_s6f_person.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_Poverty_Panel <- read.spss("./EICV5/Data_Files/EICV4/EICV4-Poverty-file.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_ubedehe <- read.spss("./EICV5/Data_Files/EICV4/cs_s9c_vup.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_expenditure1 <- read.spss("./EICV5/Data_Files/EICV4/cs_s8a1_expenditure.sav", to.data.frame = TRUE, use.value.labels = TRUE)
EICV_2014_services <- read.spss("./EICV5/Data_Files/EICV4/cs_s5e_access to services.sav", to.data.frame = TRUE, use.value.labels = TRUE)



nrow(EICV_2014_households)      # 14419
nrow(EICV_2014_agriculture)     # 14419
nrow(EICV_2014_ubedehe)         # 14419
nrow(EICV_2014_Poverty_Panel)   # 14419
nrow(EICV_2014_persons_load)    # 66081
nrow(EICV_2014_persons_load %>%
       group_by(hhid) %>%
       summarize(count = n()))  # 14419
nrow(EICV_2014_expenditure1)    # 994911
nrow(EICV_2014_expenditure1 %>%
       filter(s8a1q0 == "Health insurance (Mutuelle,RAMA,MMI, etc.)")) # 14419
nrow(EICV_2014_services) # 216285
nrow(EICV_2014_services %>%
       filter(s5eq0 == "Health centre")) # 14419

# Okay, so unfortunately, there are, infact, slight shifts between the two surveys. This is very sad and frustrating. 
# But the numbers line up which is encouraging!

EICV_2014_household_level_load <-
  EICV_2014_households %>%
  inner_join(EICV_2014_agriculture) %>%
  inner_join(EICV_2014_ubedehe) %>%
  inner_join(EICV_2014_Poverty_Panel, by = c("hhid", "clust"))

nrow(EICV_2014_household_level_load) # 14419

EICV_2014_household_level_load_cleanup  <-
  EICV_2014_household_level_load %>%
  rename(
    
    Household_Own_Land = s7b1q1,
    ubudehe =  s0qb,
    distance_to_water = s5cq2,
    type_of_toilet = s5cq20,
    main_source_lighting = s5cq16,
    amount_paid_electricity = s5cq17,
    drinking_water = s5cq1
    

  ) %>%
  
  mutate(
    drinking_water_1 = as.factor(ifelse(
      s5cq5 == "Yes", as.character(s5cq3), as.character(s5cq7)
    ))
  ) %>%

  select("hhid", 
         "province", 
         "district.x", 
         "ur2012", 
         "ur2_2012", 
         "region.x", 
         "weight", 
         "clust", 
         "surveyh", 
         "quintile.x", 
         "poverty", 
         "Consumption", 
         "hhtype", 
         "s0q19m", 
         "s0q19y",
         Household_Own_Land,
         ubudehe,
         drinking_water,
         distance_to_water,
         type_of_toilet,
         main_source_lighting,
         amount_paid_electricity,
         ) %>%
  rename(district = district.x,
         region = region.x,
         quintile = quintile.x
  )


EICV_2014_persons <- 
  EICV_2014_persons_load %>%
  mutate(
    Age = as.numeric(as.character(s1q3y)),
    Age_2013 = ifelse(s0q19y == 14,  Age - 1, Age),
    have_health_insurance = ifelse(s3q3 %in% c("None",NA), 0, 1) ,
    have_mutual_insurance = ifelse(s3q3 == "Mutual insurance", 1, 0) ,               
    gotcare_if_ill =  ifelse(s3q4 == "Yes", ifelse(
      s3q6 == "Yes", 1, 0),
      NA),
    able_to_read = ifelse(Age >14, ifelse(s4bq3 == "Yes", 1, 0), NA),
    
    
    primary_completed = 
      ifelse(Age < 15, NA,
             ifelse(s4aq2 %in% 
                      c("Pre-primary", "Not complete P1", "Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5"), 0, 1
             )
      ),
    
    ever_been_school = ifelse(s4aq1 == "Yes", 1, 0),
    
    children_in_school_year = ifelse((Age < 13 & Age > 6),
                                ifelse(s4aq7 == "Yes", 1, 0),
                                NA),
    
    children_in_school_week = ifelse( (Age < 13 & Age > 6),
                                ifelse(s4aq14 == 7,
                                  ifelse(s4aq13 == "Yes",
                                        ifelse(s4aq15 == "Holidys", NA , 1), 
                                                    0), 
                                                  0),
                                                NA),
    
    children_in_school = ifelse(Age_2013 > 6 & Age_2013 < 13,
                                ifelse(
                                  s4aq6b %in%  c("Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5", "Primary 6,7,8"), 1 , 0)
                                , NA ),
    
    children_in_secondary = ifelse(Age_2013 > 12 & Age_2013 < 19,
                                ifelse(
                                  s4aq6b %in%  c("Secondary 1", "Secondary 2", "Secondary 3", "Secondary 4", "Secondary 5", "Secondary 6"), 1 , 0)
                                , NA )
  ) %>%
  rename(
    reason_for_health_visit = s3q7,
    sex = s1q1
    
  ) 



EICV_2014_person_to_household <-
  EICV_2014_persons %>%
  group_by(hhid ) %>%
  summarize(
    have_health_insurance = mean( have_health_insurance, na.rm = TRUE),
    have_mutual_insurance = mean(have_mutual_insurance, na.rm = TRUE),
    gotcare_if_ill = mean(gotcare_if_ill, na.rm = TRUE),
    able_to_read = mean(able_to_read, na.rm = TRUE)
  )




EICV_2014_heads_of_households <-
  EICV_2014_persons %>%
  filter(s1q2 == "Household head (HH)") %>%
  transmute(
    hhid = hhid,
    
    househead_completed_primary = ifelse( 
        s4aq3 %in% c("No Diploma",NA), 0, 1), 

    househead_completed_secondary = ifelse( 
        s4aq3 %in% c( "No Diploma", "Primary completed", "Post primary certificate", NA), 0, 1),

    househead_read = ifelse(
      s4bq3 == "Yes", 1, 0
    )
    
  ) 

EICV_2014_Healthcare_Expenditure <-
  EICV_2014_expenditure1 %>% 
  filter(s8a1q0 == "Health insurance (Mutuelle,RAMA,MMI, etc.)") %>%
  transmute(
    hhid = hhid,
    healthcare_purchased_this_year = s8a1q2,
    amount_spent_healthcare = s8a1q3,
    where_purchased_healthcare = s8a1q4
    
  )

EICV_2014_Access_Health_Center <-
  EICV_2014_services %>%
  filter(s5eq0 == "Health centre") %>%
  transmute(
    hhid = hhid,
    Often_Use_HC = s5eq1,
    Reason_Little_Used_HC = s5eq2,
    Time_Hours_HC = s5eq3h,
    Time_Mins_HC = s5eq3m,
    Distance_HC = s5eq4,
    Satisfaction_HC = s5eq6,
    Quality_Trend_HC = s5eq7
  )




EICV_2014_household_level <-
  EICV_2014_household_level_load_cleanup %>%
  inner_join(EICV_2014_person_to_household) %>%
  inner_join(EICV_2014_heads_of_households , by = c("hhid")) %>%
  inner_join(EICV_2014_Healthcare_Expenditure, by = c("hhid"))  %>%
  inner_join(EICV_2014_Access_Health_Center)
  
nrow(EICV_2014_household_level)
nrow(EICV_2014_persons)






