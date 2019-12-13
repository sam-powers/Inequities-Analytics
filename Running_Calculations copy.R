library(srvyr)
library(ggrepel)
library("Hmisc")
library(survey)
library(tidyverse)


###############################
EICV_2017_household_level %>% head()

# There are poor people. 
EICV_2017_household_level %>%
  as_survey(
    
    id = hhid,
    cluster = clust,
    strata = ur,
    weight = weight
    
  ) %>%
  group_by(
    
    poverty
    
  ) %>%
  summarize(
    
    proportion = survey_mean()
  )

#### The Western province has the greateds proportion per its population. 
EICV_2017_household_level %>%
  as_survey(
    
    id = hhid,
    cluster = clust,
    strata = ur,
    weight = weight
    
  ) %>%
  group_by(
    
    province, poverty
    
  ) %>%
  summarize(
    
    proportion = survey_mean()
  ) %>%
  ungroup() %>%
  mutate(percent = round(proportion*100, 2)) %>%
  select(-proportion_se, -proportion ) %>%
  spread(province, percent)


#### Nyamasheke (36%) and Nyaruguru (24%) have the greatest issue. 
severe_district_level <-
  EICV_2017_household_level %>%
  as_survey(
    
    id = hhid,
    cluster = clust,
    strata = ur,
    weight = weight
    
  ) %>%
  group_by(
    
    district, poverty
    
  ) %>%
  summarize(
    
    proportion = survey_mean()
  ) %>%
  ungroup() %>%
  mutate(percent = round(proportion*100, 2)) %>%
  select(-proportion_se, -proportion ) %>%
  spread(poverty, percent) %>%
  mutate(Poor = `Severely poor` + ` Moderately poor`) %>%
  arrange(desc(Poor)) %>%
  select(district, Poor, everything())

severe_district_level %>% View()
# Nyamasheke is the most severe

# Got care if ill #
EICV_2017_persons %>%
  as_survey(
    
    #id = hhid,
    cluster = clust,
    strata = ur,
    weight = weight
    
  ) %>%
  group_by(poverty) %>%
  summarize(
    proportion = survey_mean(gotcare_if_ill, na.rm = TRUE)
    
  )

# Okay, so what do I need to do? I need to make movement charts for each SDG describing just the extremely poor broken down by district.  
# This means I need to calculate disagregated percentages for each of the SDG indicators 






#####################################
# Enough practicing, lets calculate #
#####################################

# Poverty
nrow(EICV_2014_household_level)
nrow(EICV_2017_household_level)
nrow(EICV_2014_persons)
nrow(EICV_2017_persons)

poverty_changes <-
# 2017 Population Poverty
EICV_2017_persons %>%
  as_survey(
    weight = weight
  ) %>%
  mutate(
    SeverePoverty = ifelse(poverty == "Severely poor", 1, 0)
  ) %>%
  summarize(
    Value  = survey_mean(SeverePoverty, na.rm = TRUE),
    UnweightedN = unweighted(sum(SeverePoverty))
  ) %>%
  transmute(
    Indicator = "Severe Poverty",
    Year = 2017,
    Value = round(Value*100,1),
    Level = "Population",
    UnweightedN = UnweightedN
  ) %>%
  
  bind_rows(

# 2014 Population 
  EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  mutate(
    SeverePoverty = ifelse(poverty == "1", 1, 0)
  ) %>%
  summarize(
    Value  = survey_mean(SeverePoverty, na.rm = TRUE),
    UnweightedN = unweighted(sum(SeverePoverty))
  ) %>%
  transmute(
    Indicator = "Severe Poverty",
    Year = 2014,
    Value = round(Value*100,1),
    Level = "Population",
    UnweightedN = UnweightedN

) ) %>%
  
  bind_rows(
 # 2017 Household Poverty    
    EICV_2017_household_level %>%
      as_survey(
        weight = weight
      ) %>%
      mutate(
        SeverePoverty = ifelse(poverty == "Severely poor", 1, 0)
      ) %>%
      summarize(
        Value  = survey_mean(SeverePoverty, na.rm = TRUE),
        UnweightedN = unweighted(sum(SeverePoverty))
      ) %>%
      transmute(
        Indicator = "Severe Poverty",
        Year = 2017,
        Value = round(Value*100,1),
        Level = "Household",
        UnweightedN = UnweightedN
      )
  
  ) %>%
  
  bind_rows(
    # 2014 Household 
    
    EICV_2014_household_level %>%
      as_survey(
        weight = weight
      ) %>%
      mutate(
        SeverePoverty = ifelse(poverty == "1", 1, 0)
      ) %>%
      summarize(
        Value  = survey_mean(SeverePoverty),
        UnweightedN = unweighted(sum(SeverePoverty))
      ) %>%
      transmute(
        Indicator = "Severe Poverty",
        Year = 2014,
        Value = round(Value*100,1),
        Level = "Household",
        UnweightedN = UnweightedN
      )
  ) %>%
  mutate(
    Value = paste0(Value, "% (", UnweightedN, ")")
    
  ) %>%
  select( -UnweightedN) %>%
  spread(Year, Value )
  
poverty_changes  

##########################

# District-Level Households 2017
  District_SDGs_Households_17 <-
  EICV_2017_household_level %>%
    as_survey(
      weight = weight
    ) %>%
    filter(
      poverty %in% c("Severely poor", "Non Poor")
    ) %>%
    mutate(
      ubudehe = ifelse(ubudehe == "Category 1", 1, 0),
      Support_2017 = ifelse(VUP_Benefit_2017 == "NA", 0, 1),
      Support_2014 = ifelse(is.na(VUP_Benefit_2014) == TRUE, 0, 1),
      own_land = ifelse(Household_Own_Land == "Yes", 1, 0),
      healthcare_purchased = ifelse(healthcare_purchased_this_year == "Yes", 1, 0),
      improved_water_source = ifelse(drinking_water %in% 
                                       c("Public stand/pipe", "Public standpipe", "Borehole", "Tube Well /Borehole", "Protected well", "Protected spring", "Rain water", "Other _specify_", "Piped into dwelling", "Piped into yard", "Piped to Yard/Plot", "Other"), 
                                     1, 
                                     ifelse(drinking_water == "<NA>", NA, 0) ),
      improved_sanitation = ifelse(type_of_toilet %in% c("Flush toilet", "Pit latrine with solid slab"), 1, 0),
      electric_lighting = ifelse(main_source_lighting %in% c("Electricity from EUCL", "Other electricity distributors	"), 1, 0)
#      Main_Problem_Food = ifelse(Main_Problem_1 == "Unusually high prices for food" | Main_Problem_2 == "Unusually high prices for food", 1, 0)
    
) %>%
  group_by(district, poverty) %>%
    summarize(
      First_Ubudehe_PC  = survey_mean(ubudehe,  na.rm = TRUE),
      First_Ubudehe_NW = unweighted(sum(ubudehe,  na.rm = TRUE)),
     
      Support_2017_PC  = survey_mean(Support_2017,  na.rm = TRUE),
      Support_2017_NW  = unweighted(sum(Support_2017,  na.rm = TRUE)),
      
      Support_2014_PC  = survey_mean(Support_2014,  na.rm = TRUE),
      Support_2014_NW  = unweighted(sum(Support_2014,  na.rm = TRUE)),  
      
      own_land_PC  = survey_mean(own_land,  na.rm = TRUE),
      own_land_NW  = unweighted(sum(own_land,  na.rm = TRUE)),
      
#      healthcare_purchased_PC  = survey_mean(healthcare_purchased,  na.rm = TRUE),
#      healthcare_purchased_NW  = unweighted(sum(healthcare_purchased,  na.rm = TRUE)),
      
      improved_water_source_PC  = survey_mean(improved_water_source, na.rm = TRUE),
      improved_water_source_NW  = unweighted(sum(improved_water_source,  na.rm = TRUE)),
      
      improved_sanitation_PC  = survey_mean(improved_sanitation,  na.rm = TRUE),
      improved_sanitation_NW  = unweighted(sum(improved_sanitation,  na.rm = TRUE)),
      
      electric_lighting_PC  = survey_mean(electric_lighting,  na.rm = TRUE),
      electric_lighting_NW  = unweighted(sum(electric_lighting,  na.rm = TRUE))
      
      
      
#      Main_Problem_Food_PC  = survey_mean(Main_Problem_Food, na.rm = TRUE),
#      Main_Problem_Food_NW  = unweighted(sum(Main_Problem_Food, na.rm = TRUE))
      
      
#      Average_Housemembers_Insured_PC = survey_mean(have_health_insurance), # This is more of a population-level metric 
#      Average_Housemembers_Insured_NW = unweighted(n())
      
      
          ) %>%
  mutate(
    Year = "2017",
    Level = "Household"
    
  ) %>%
  ungroup()




SDG_House_17 <-
District_SDGs_Households_17 %>%
  gather(Indicator, Value, -c(district,poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange(district, poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
) %>% spread(`Value Class`, Value) %>%
  transmute( 
    District = district,
    Year = ifelse(Domain == "Support_2014", "2014", "2017"),
    Level = Level,
    Poverty = poverty,
    Domain = ifelse(Domain %in% c("Support_2014", "Support_2017"), "Support", Domain),
    Display = round(`_PC`*100, 2),
    Low = round((`_PC` - 1.96* `_se`)*100,2),
    High = round((`_PC` + 1.96* `_se` )*100,2),
    `Unweighted N` = `_NW`
    ) 
  

# SDG_House_17 %>% View()

##############
# District-Level = Populations 2017
District_SDGs_Populations_17 <-
  EICV_2017_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c("Severely poor", "Non Poor")
  ) %>%
  mutate(
    women_in_primary = ifelse(sex == "Female", children_in_school, NA),
    men_in_primary = ifelse(sex == "Male", children_in_school, NA),
    
    women_in_secondary = ifelse(sex == "Female", children_in_secondary, NA),
    men_in_secondary = ifelse(sex == "Male", children_in_secondary, NA)

  ) %>%
  group_by(district, poverty) %>%
  summarize(
    able_to_read_PC  = survey_mean(able_to_read, na.rm = TRUE),
    able_to_read_NW = unweighted(sum(able_to_read, na.rm = TRUE)),
    
    have_health_insurance_PC  = survey_mean(have_health_insurance),
    have_health_insurance_NW  = unweighted(sum(have_health_insurance)),
    
#    have_mutual_insurance_PC  = survey_mean(have_mutual_insurance),
#    have_mutual_insurance_NW  = unweighted(sum(have_mutual_insurance)),
    
    gotcare_if_ill_PC  = survey_mean(gotcare_if_ill, na.rm = TRUE),
    gotcare_if_ill_NW  = unweighted(sum(gotcare_if_ill, na.rm = TRUE)),
    
    primary_completed_PC = survey_mean(primary_completed, na.rm = TRUE),
    primary_completed_NW  = unweighted(sum(primary_completed, na.rm = TRUE)),
    
#    children_in_school_year_PC = survey_mean(children_in_school_year, na.rm = TRUE),
#    children_in_school_year_NW  = unweighted(sum(children_in_school_year, na.rm = TRUE)),
    
#    children_in_school_week_PC = survey_mean(children_in_school_week, na.rm = TRUE),
#    children_in_school_week_NW  = unweighted(sum(children_in_school_week, na.rm = TRUE)),
    
    primary_attendance_PC = survey_mean(children_in_school, na.rm = TRUE),
    primary_attendance_NW  = unweighted(sum(children_in_school, na.rm = TRUE)),
    
    secondary_attendance_PC = survey_mean(children_in_secondary, na.rm = TRUE),
    secondary_attendance_NW  = unweighted(sum(children_in_secondary, na.rm = TRUE)),

    women_primary_PC  = survey_mean(women_in_primary, na.rm = TRUE) ,
    women_primary_NW  = unweighted(sum(women_in_primary, na.rm = TRUE) ),

    men_primary_PC  = survey_mean(men_in_primary, na.rm = TRUE),
    men_primary_NW  = unweighted(sum(men_in_primary, na.rm = TRUE) ),

    women_secondary_PC  = survey_mean(women_in_secondary, na.rm = TRUE) ,
    women_secondary_NW  = unweighted(sum(women_in_secondary, na.rm = TRUE) ),

    men_secondary_PC  = survey_mean(men_in_secondary, na.rm = TRUE),
    men_secondary_NW  = unweighted(sum(men_in_secondary, na.rm = TRUE) )

    
    #      Average_Housemembers_Insured_PC = survey_mean(have_health_insurance), # This is more of a population-level metric 
    #      Average_Housemembers_Insured_NW = unweighted(n())
    
    
  ) %>%
  mutate(
    Year = "2017",
    Level = "Population",
    sex_ratio_secondary_PC = women_secondary_PC/men_secondary_PC,
    sex_ratio_primary_PC = women_primary_PC/men_primary_PC
    

  ) %>%
  ungroup()




SDGS_Pop_17 <-
  
District_SDGs_Populations_17 %>%
  gather(Indicator, Value, -c(district,poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange(district, poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% 
  spread(`Value Class`, Value) %>%
  transmute( 
    District = district,
    Year = Year,
    Level = Level,
    Poverty = poverty, 
    Domain = Domain,
    Display = round(`_PC`*100, 2),
    Low = round((`_PC` - 1.96* `_se`)*100,2),
    High = round((`_PC` + 1.96* `_se` )*100,2),
    `Unweighted N` = `_NW`
  )
# SDGS_Pop_17 %>% View()

######################################

# District-Level Households 2014 
District_SDGs_Households_14 <-
  EICV_2014_household_level %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1,3)
  ) %>%
  mutate(
    ubudehe = ifelse(ubudehe == "Category 1", 1, 0),
    own_land = ifelse(Household_Own_Land == "Yes", 1, 0),
    healthcare_purchased = ifelse(healthcare_purchased_this_year == "Yes", 1, 0),
    improved_water_source = ifelse(drinking_water %in% 
                                     c("Public stand/pipe", "Public standpipe", "Borehole", "Tube Well /Borehole", "Protected well", "Protected spring", "Rain water", "Other _specify_", "Piped into dwelling", "Piped into yard", "Piped to Yard/Plot", "Other"), 
                                   1,   ifelse(drinking_water == "<NA>", NA, 0) ),

    improved_sanitation = ifelse(type_of_toilet %in% c("Flush toilet", "Pit latrine with solid slab"), 1, 0),
    electric_lighting = ifelse(main_source_lighting %in% c("Electricity from EWSA", "Other electricity distributors"), 1, 0)
    
    
    
    
  ) %>%
  group_by(district, poverty) %>%
  summarize(
    First_Ubudehe_PC  = survey_mean(ubudehe),
    First_Ubudehe_NW = unweighted(sum(ubudehe)),

    own_land_PC  = survey_mean(own_land),
    own_land_NW  = unweighted(sum(own_land)),
    
#    healthcare_purchased_PC  = survey_mean(healthcare_purchased),
#    healthcare_purchased_NW  = unweighted(sum(healthcare_purchased)),
    
    improved_water_source_PC  = survey_mean(improved_water_source),
    improved_water_source_NW  = unweighted(sum(improved_water_source)),
    
    improved_sanitation_PC  = survey_mean(improved_sanitation),
    improved_sanitation_NW  = unweighted(sum(improved_sanitation)),
    
    electric_lighting_PC  = survey_mean(electric_lighting),
    electric_lighting_NW  = unweighted(sum(electric_lighting))
    
    #      Average_Housemembers_Insured_PC = survey_mean(have_health_insurance), # This is more of a population-level metric 
    #      Average_Housemembers_Insured_NW = unweighted(n())
    
    
  ) %>%
  mutate(
    Year = "2014",
    Level = "Household"
    
  ) %>%
  ungroup()

SDG_House_14 <-
  District_SDGs_Households_14 %>%
  gather(Indicator, Value, -c(district,poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange(district, poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% spread(`Value Class`, Value) %>%
  transmute( 
    District = district,
    Year = Year,
    Level = Level,
    Poverty = ifelse(poverty == 1, "Severely poor", "Non Poor"),
    Domain = Domain,
    Display = round(`_PC`*100, 2),
    Low = round((`_PC` - 1.96* `_se`)*100,2),
    High = round((`_PC` + 1.96* `_se` )*100,2),
    `Unweighted N` = `_NW`
  )

# SDG_House_14 %>% View()


  



##############

# District-Level  Populations 2014
District_SDGs_Populations_14 <-
  EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1, 3)
  ) %>%
  group_by(district, poverty) %>%
  
  mutate(
    women_in_primary = ifelse(sex == "Female", children_in_school, NA),
    men_in_primary = ifelse(sex == "Male", children_in_school, NA),
    
    women_in_secondary = ifelse(sex == "Female", children_in_secondary, NA),
    men_in_secondary = ifelse(sex == "Male", children_in_secondary, NA)
    
  ) %>%
  
  summarize(
    able_to_read_PC  = survey_mean(able_to_read, na.rm = TRUE),
    able_to_read_NW = unweighted(sum(able_to_read, na.rm = TRUE)),
    
    have_health_insurance_PC  = survey_mean(have_health_insurance),
    have_health_insurance_NW  = unweighted(sum(have_health_insurance)),
    
#    have_mutual_insurance_PC  = survey_mean(have_mutual_insurance, na.rm = TRUE),
#    have_mutual_insurance_NW  = unweighted(sum(have_mutual_insurance, na.rm = TRUE)),
    
    gotcare_if_ill_PC  = survey_mean(gotcare_if_ill, na.rm = TRUE),
    gotcare_if_ill_NW  = unweighted(sum(gotcare_if_ill, na.rm = TRUE)), 
    
    primary_completed_PC = survey_mean(primary_completed, na.rm = TRUE),
    primary_completed_NW  = unweighted(sum(primary_completed, na.rm = TRUE)),
    
 #   children_in_school_year_PC = survey_mean(children_in_school_year, na.rm = TRUE),
 #   children_in_school_year_NW  = unweighted(sum(children_in_school_year, na.rm = TRUE)),
    
 #   children_in_school_week_PC = survey_mean(children_in_school_week, na.rm = TRUE),
 #   children_in_school_week_NW  = unweighted(sum(children_in_school_week, na.rm = TRUE)),
    
    primary_attendance_PC = survey_mean(children_in_school, na.rm = TRUE),
    primary_attendance_NW  = unweighted(sum(children_in_school, na.rm = TRUE)),
    
    secondary_attendance_PC = survey_mean(children_in_secondary, na.rm = TRUE),
    secondary_attendance_NW  = unweighted(sum(children_in_secondary, na.rm = TRUE)),


    women_primary_PC  = survey_mean(women_in_primary, na.rm = TRUE) ,
    women_primary_NW  = unweighted(sum(women_in_primary, na.rm = TRUE) ),

    men_primary_PC  = survey_mean(men_in_primary, na.rm = TRUE),
    men_primary_NW  = unweighted(sum(men_in_primary, na.rm = TRUE) ),

    women_secondary_PC  = survey_mean(women_in_secondary, na.rm = TRUE) ,
    women_secondary_NW  = unweighted(sum(women_in_secondary, na.rm = TRUE) ),

    men_secondary_PC  = survey_mean(men_in_secondary, na.rm = TRUE),
    men_secondary_NW  = unweighted(sum(men_in_secondary, na.rm = TRUE) )
    
    #      Average_Housemembers_Insured_PC = survey_mean(have_health_insurance), # This is more of a population-level metric 
    #      Average_Housemembers_Insured_NW = unweighted(n())
    
    
  ) %>%
  mutate(
    Year = "2014",
    Level = "Population",
    sex_ratio_secondary_PC = women_secondary_PC/men_secondary_PC,
    sex_ratio_primary_PC = women_primary_PC/men_primary_PC
    
  ) %>%
  ungroup()

svydata <-
EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1, 3)
  ) 

  svyttest(~able_to_read, ~poverty, svydata)


SDGS_Pop_14 <-
  District_SDGs_Populations_14 %>%
  gather(Indicator, Value, -c(district,poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange(district, poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% 
  spread(`Value Class`, Value) %>%
  transmute( 
    District = district,
    Year = Year,
    Level = Level,
    Poverty = ifelse(poverty == 1, "Severely poor", "Non Poor"),
    Domain = Domain,
    Display = round(`_PC`*100, 2),
    Low = round((`_PC` - 1.96* `_se`)*100,2),
    High = round((`_PC` + 1.96* `_se` )*100,2),
    `Unweighted N` = `_NW`
  )
# SDGS_Pop_14 %>% View()





######################################
SDG_Evaluation <-
SDGS_Pop_14 %>%
  bind_rows(
    SDGS_Pop_17,
    SDG_House_14,
    SDG_House_17
  )  

# SDG_Evaluation %>% filter(District == "Nyarugenge") %>%  View()

sdg_eval_graph <-
SDG_Evaluation %>%
  group_by(District, Level, Year, Domain) %>% 
  arrange(District, Year, Level, Domain) %>%
  mutate(Difference_From_Poor = Display - lag(Display)) %>% 
  ungroup() %>%
  filter(Poverty == "Non Poor")

disparity_identification <-
SDG_Evaluation %>%
  group_by(District, Level, Year, Domain) %>%
  arrange(District, Year, Level, Domain) %>%
  mutate(Difference_From_Poor = Display - lag(Display)) %>%
  ungroup() %>%
#  filter(Poverty == "Non Poor") %>%
  group_by(District, Level, Domain) %>%
  arrange(Poverty, District, Level, Domain) %>%
  mutate(Difference_From_Year = Difference_From_Poor - lag(Difference_From_Poor)) %>%
  ungroup() %>%
  arrange(District, Domain, Year, desc(Poverty))

disparity_identification %>% View()


disparity_identification %>%
  filter(Poverty == "Non Poor", Year == 2017) %>%
  mutate(disparity_increased = ifelse(Difference_From_Year>= 0, 1, 0),
         Increases = ifelse(Difference_From_Year < 0, NA, Difference_From_Year)
        )  %>%
  group_by(Domain) %>%
  summarize(Percent_Increased = round(mean(disparity_increased)*100, 2),
            min_increase = min(Increases, na.rm = TRUE),
            median_increase = median(Increases, na.rm = TRUE),
            max_increase = max(Increases, na.rm = TRUE)
            ) %>%
  View()



############
# Looking at some things #
disparity_identification %>% 
  filter( Domain %in% c("men_primary", "men_secondary")) %>%
  View()

Disparities_2014_2017 <-
disparity_identification %>%
  filter(Poverty == "Non Poor", Year == 2017, !Domain  %in% c("sex_ratio_primary", "sex_ratio_secondary")) %>%
  select(District, Domain, Difference_From_Year) %>%
  spread(Domain, Difference_From_Year)
  
Disparities_2014_2017 %>% View()





