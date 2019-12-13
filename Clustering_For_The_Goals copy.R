library(cluster)    

Household_Variables <-
  EICV_2017_household_level %>%
 
  mutate(
    ubudehe = ifelse(ubudehe == "Category 1", 1, 0),
    Support_2017 = ifelse(VUP_Benefit_2017 == "NA", 0, 1),
    Support_2014 = ifelse(is.na(VUP_Benefit_2014) == TRUE, 0, 1),
   ## own_land = ifelse(Household_Own_Land == "Yes", 1, 0),
  ##  healthcare_purchased = ifelse(healthcare_purchased_this_year == "Yes", 1, 0),
    improved_water_source = ifelse(drinking_water %in% 
                                     c("Public stand/pipe", "Public standpipe", "Borehole", "Tube Well /Borehole", "Protected well", "Protected spring", "Rain water", "Other _specify_", "Piped into dwelling", "Piped into yard", "Piped to Yard/Plot", "Other"), 
                                   1, 
                                   ifelse(drinking_water == "<NA>", NA, 0) ),
    improved_sanitation = ifelse(type_of_toilet %in% c("Flush toilet", "Pit latrine with solid slab"), 1, 0),
    electric_lighting = ifelse(main_source_lighting %in% c("Electricity from EUCL", "Other electricity distributors	"), 1, 0)
    #      Main_Problem_Food = ifelse(Main_Problem_1 == "Unusually high prices for food" | Main_Problem_2 == "Unusually high prices for food", 1, 0)
  ) 


Population_To_Household <-
  EICV_2017_persons %>%
  mutate(
    women_in_school = ifelse(sex == "Female", children_in_any_school, NA),
    men_in_school = ifelse(sex == "Male", children_in_any_school, NA),

    is_child = ifelse(Age_2016 < 18, 1, 0)
    
  ) %>%
  group_by(hhid) %>%
  summarize(
#    able_to_read  = mean(able_to_read, na.rm = TRUE),
    
 #   have_health_insurance  = mean(have_health_insurance),

    #    have_mutual_insurance_PC  = survey_mean(have_mutual_insurance),
    #    have_mutual_insurance_NW  = unweighted(sum(have_mutual_insurance)),
    
#    gotcare_if_ill  = mean(gotcare_if_ill, na.rm = TRUE),
#    gotcare_if_ill_NW  = unweighted(sum(gotcare_if_ill, na.rm = TRUE)),
    
#    primary_completed_PC = survey_mean(primary_completed, na.rm = TRUE),
#    primary_completed_NW  = unweighted(sum(primary_completed, na.rm = TRUE)),
    
    #    children_in_school_year_PC = survey_mean(children_in_school_year, na.rm = TRUE),
    #    children_in_school_year_NW  = unweighted(sum(children_in_school_year, na.rm = TRUE)),
    
    #    children_in_school_week_PC = survey_mean(children_in_school_week, na.rm = TRUE),
    #    children_in_school_week_NW  = unweighted(sum(children_in_school_week, na.rm = TRUE)),
    
    primary_attendance = mean(children_in_school, na.rm = TRUE),
#    primary_attendance_NW  = unweighted(sum(children_in_school, na.rm = TRUE)),
    
    secondary_attendance = mean(children_in_secondary, na.rm = TRUE),
#    secondary_attendance_NW  = unweighted(sum(children_in_secondary, na.rm = TRUE)),
    
    women_school  = mean(women_in_school, na.rm = TRUE) ,
#    women_primary_NW  = unweighted(sum(women_in_primary, na.rm = TRUE) ),
    
    men_school  = mean(men_in_school, na.rm = TRUE),
#    men_primary_NW  = unweighted(sum(men_in_primary, na.rm = TRUE) ),
    
#    women_secondary  = mean(women_in_secondary, na.rm = TRUE) ,
#    women_secondary_NW  = unweighted(sum(women_in_secondary, na.rm = TRUE) ),
    
#    men_secondary  = mean(men_in_secondary, na.rm = TRUE),
#    men_secondary_NW  = unweighted(sum(men_in_secondary, na.rm = TRUE) )
    
    
    #      Average_Housemembers_Insured_PC = survey_mean(have_health_insurance), # This is more of a population-level metric 
    #      Average_Housemembers_Insured_NW = unweighted(n())
  number_children = sum(is_child),

size_household = n()
    
  ) %>%

  ungroup()




household_level_descriptors <-
Household_Variables %>% inner_join(Population_To_Household, by = "hhid")






kmeans_data <- 
household_level_descriptors %>% 
  mutate(poverty = ifelse(poverty == "Non Poor", 1, 0)) %>%
  select(hhid,
         district,
         size_household, 
          weight,
         number_children,
         men_school, women_school,
         Main_Problem,
         Main_Problem_1,
         Main_Problem_2,
         poverty, 
         have_health_insurance, gotcare_if_ill, 
         primary_attendance, secondary_attendance, able_to_read, 
         improved_sanitation, improved_water_source,
         electric_lighting
         ) %>%
  mutate(womens_empowerment = ifelse( women_school <  men_school , 0, 1),
         Main_Problem = ifelse(Main_Problem == "Yes", 1, 0),
         education = rowMeans(select(.,primary_attendance, secondary_attendance, able_to_read), na.rm = TRUE),
         healthcare = rowMeans(select(., have_health_insurance, gotcare_if_ill), na.rm = TRUE),
         water_sanitation = rowMeans(select(.,improved_sanitation, improved_water_source), na.rm = TRUE)
           ) 


SDG_Score <-
kmeans_data %>%
  mutate(
    SDGs = rowMeans( select(., poverty, healthcare, education, womens_empowerment, water_sanitation, electric_lighting), na.rm = TRUE),
    Quintiles = as.factor(ntile(SDGs, 5))
  )
  

SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  group_by( Quintiles) %>%
  summarize(percent = survey_mean(),
            family_size = survey_mean(size_household),
            children = survey_mean(number_children),
            problem = survey_mean(Main_Problem)
  ) %>%
  mutate(
    percent = round(percent *100,2)
  ) %>%
#  filter(Quintiles == 1) %>%
#  arrange(-percent) %>%
  View()


SDG_Score %>%
as_survey(
  weight = weight
  
) %>%
  group_by( district, Quintiles) %>%
  summarize(percent = survey_mean(),
            family_size = survey_mean(size_household),
            children = survey_mean(number_children),
            problem = survey_mean(Main_Problem)
  ) %>%
  mutate(
    percent = round(percent *100,2)
  ) %>%
  #  filter(Quintiles == 1) %>%
  #  arrange(-percent) %>%
  View()



SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  mutate(Main_Problem_1 = fct_explicit_na(Main_Problem_1), 
         Main_Problem_2 = fct_explicit_na(Main_Problem_2),
         Problem = ifelse( Main_Problem_1 == "Drought/irregular rains, prolonged dry spell" | Main_Problem_1 == "Drought/irregular rains, prolonged dry spell", "Drought", "Other"  )) %>%
  group_by( Quintiles, Problem) %>%
  summarize(percent = survey_mean(),
  ) %>%
  mutate(
    percent = round(percent *100,2)
  ) %>%
  #  filter(Quintiles == 1) %>%
  #  arrange(-percent) %>%
  View()


SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  mutate(Main_Problem_1 = fct_explicit_na(Main_Problem_1), 
         Main_Problem_2 = fct_explicit_na(Main_Problem_2),
         Problem = ifelse( Main_Problem_1 == "NA" | Main_Problem_1 == "NA", "None", "Issue"  )) %>%
  group_by( Quintiles, Problem) %>%
  summarize(percent = survey_mean(),
  ) %>%
  mutate(
    percent = round(percent *100,2)
  ) %>%
  #  filter(Quintiles == 1) %>%
  #  arrange(-percent) %>%
  View()


# Percent in quintiles one through three 
SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  group_by( district, Quintiles) %>%
  summarize(percent = survey_mean(),
            family_size = survey_mean(size_household),
            children = survey_mean(number_children),
            problem = survey_mean(Main_Problem)
  ) %>%
  mutate(
    percent = round(percent *100,2)
  ) %>%
  filter(Quintiles %in% c(1, 2, 3) ) %>%
  group_by(district) %>%
  summarise(percent = sum(percent)) %>%
  arrange(-percent) %>%
  View()
  

# What attributes do the clusters share?
SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  group_by( Quintiles) %>%
  summarize(primary_attendance = survey_mean(primary_attendance, na.rm = TRUE),
            secondary_attendance = survey_mean(secondary_attendance, na.rm = TRUE),
            able_to_read = survey_mean(able_to_read, na.rm = TRUE),
            healthcare = survey_mean(have_health_insurance, na.rm = TRUE),
            gotcare_if_ill = survey_mean(gotcare_if_ill, na.rm = TRUE),
            improved_sanitation = survey_mean(improved_sanitation, na.rm = TRUE),
            improved_water_source = survey_mean(improved_water_source, na.rm = TRUE),
            womens_empowerment = survey_mean(womens_empowerment, na.rm = TRUE),
            electric_lighting = survey_mean(electric_lighting, na.rm = TRUE),
            poverty = survey_mean(poverty, na.rm = TRUE)
  ) %>%
  View()


##################
# Data to output #
setwd("/Volumes/GoogleDrive/My Drive/Fulbright [Its Happening]/Inequities & Analytics/Data")

SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  group_by( district, Quintiles) %>%
  summarize(percent = survey_mean(),
            family_size = survey_mean(size_household),
            children = survey_mean(number_children),
            problem = survey_mean(Main_Problem)
  ) %>%
  mutate(
    percent = round(percent *100,2)
  )  %>%
write.csv(. , file = "quintiles_by_district.csv")




SDG_Score %>%
  as_survey(
    weight = weight
    
  ) %>%
  group_by( district) %>%
  summarize(education = survey_mean(education, na.rm = TRUE),
            healthcare = survey_mean(healthcare, na.rm = TRUE),
            water_sanitation = survey_mean(water_sanitation, na.rm = TRUE),
            womens_empowerment = survey_mean(womens_empowerment, na.rm = TRUE),
            electric_lighting = survey_mean(electric_lighting, na.rm = TRUE),
            poverty = survey_mean(poverty, na.rm = TRUE)
  ) %>%

  write.csv(. , file = "SDGs_by_district.csv")


