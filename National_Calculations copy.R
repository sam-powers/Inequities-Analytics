
# National-Level Households 2017
National_SDGs_Households_17 <-
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
  group_by( poverty) %>%
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




SDG_House_17_National <-
  National_SDGs_Households_17 %>%
  gather(Indicator, Value, -c(poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange( poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% spread(`Value Class`, Value) %>%
  transmute( 
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
# National-Level = Populations 2017
National_SDGs_Populations_17 <-
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
  group_by( poverty) %>%
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




SDGS_Pop_17_National <-
  
  National_SDGs_Populations_17 %>%
  gather(Indicator, Value, -c(poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange( poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% 
  spread(`Value Class`, Value) %>%
  transmute( 
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

# National-Level Households 2014 
National_SDGs_Households_14 <-
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
  group_by( poverty) %>%
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

SDG_House_14_National <-
  National_SDGs_Households_14 %>%
  gather(Indicator, Value, -c(poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange( poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% spread(`Value Class`, Value) %>%
  transmute( 
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

# National-Level  Populations 2014
National_SDGs_Populations_14 <-
  EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1, 3)
  ) %>%
  group_by(poverty) %>%
  
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




SDGS_Pop_14_National <-
  National_SDGs_Populations_14 %>%
  gather(Indicator, Value, -c(poverty, Year, Level) ) %>%
  separate(Indicator, c("Domain", "Value Class"), -3  ) %>%
  arrange( poverty, Domain) %>%
  mutate(
    Domain = ifelse(`Value Class` == "_se", lag(Domain, 1), Domain)
  ) %>% 
  spread(`Value Class`, Value) %>%
  transmute( 
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
SDG_Evaluation_National <-
  SDGS_Pop_14_National %>%
  bind_rows(
    SDGS_Pop_17_National,
    SDG_House_14_National,
    SDG_House_17_National
  )  



SDG_Evaluation_National %>%
  arrange(Domain, Year, desc(Poverty)) %>%
  group_by(Domain, Year) %>%
  mutate(Difference_From_Poor = Display - lag(Display), Ratio_Of_Poor = Display/lag(Display) ) %>%
  ungroup() %>%
  select(Year, Domain, Difference_From_Poor) %>%
  filter(is.na(Difference_From_Poor) == FALSE, !Domain %in% c("sex_ration_primary", "sex_ratio_secondary", "own_land", "sex_ratio_primary")) %>%
   spread(Year, Difference_From_Poor) %>%
  mutate(Disparity_Change_Per_Year = (`2017` - `2014`)/3,
         Disparity_Resolution_Years = 
           ifelse(`2017` > 0 & Disparity_Change_Per_Year > 0, "Never", 
                  ifelse(`2017` > 0 & Disparity_Change_Per_Year < 0, round(`2017`/Disparity_Change_Per_Year*-1,2), NA
                          
                  )
           )
  )


SDG_Evaluation_National %>%
  arrange(Domain, Year, desc(Poverty)) %>%
  group_by(Domain, Year) %>%
  mutate(Difference_From_Poor = Display - lag(Display), Ratio_Of_Poor = Display/lag(Display) ) %>%
  ungroup() %>%
  select(Year, Domain, Ratio_Of_Poor) %>%
  filter(is.na(Ratio_Of_Poor) == FALSE, !Domain %in% c("sex_ration_primary", "sex_ratio_secondary", "own_land", "sex_ratio_primary")) %>%
  spread(Year, Ratio_Of_Poor) %>%
  mutate(Disparity_Change_Per_Year = (`2017` - `2014`)/3
  ) %>%
  mutate(
    Years = round((`2017` - 1)/Disparity_Change_Per_Year *-1, 2)
  )
  
  
SDG_Evaluation_National_Table <-
SDG_Evaluation_National %>%
  arrange(Domain, desc(Poverty), Year ) %>%
  group_by(Domain, Poverty) %>%
  mutate(Difference_From_Year = Display - lag(Display), Ratio_Of_Year = Display/lag(Display) ) %>%
  mutate(yearly_increase = Difference_From_Year/3) %>%
  filter(Year == 2017) %>%
  ungroup() %>%
  group_by(Domain, Year) %>%
  mutate( relative_rate = yearly_increase - lag(yearly_increase),
          current_difference = Display - lag(Display),
          time_to_meeting = round(current_difference/relative_rate,2)*-1 ,
          time_to_100 = round((100 - Display)/ yearly_increase,2),
    meet_pre_100 = 
           ifelse(relative_rate > 0, "No", 
                  ifelse(time_to_meeting < time_to_100, "Yes", "No")
                  )
         ) %>%
  ungroup() %>%
select(Domain, Poverty, Display, Difference_From_Year, yearly_increase, relative_rate, time_to_meeting, time_to_100, meet_pre_100) %>%
  group_by(Domain) %>%
  mutate(time_to_100_2 = lag(time_to_100),
         time_to_100_3 = ifelse(time_to_100 > time_to_100_2, time_to_100, time_to_100_2) ) %>% 
  mutate(time_to_resolve = ifelse(time_to_meeting <0, time_to_100_3, ifelse(time_to_meeting < abs(time_to_100_3), time_to_meeting, time_to_100_3)
                                  )
         ) %>%
  select(Domain, Poverty, Display, relative_rate, time_to_meeting, time_to_resolve)  %>%
  mutate(`Severely Poor` = lag(Display), relative_rate = relative_rate*-1) %>%
  filter(is.na(relative_rate) == FALSE) %>%
  mutate(`Time if Reductions Are Constant` = ifelse(time_to_meeting < 0, "Never", paste0(time_to_meeting)),
         `Time if Growth Rates Are Constant` = ifelse(time_to_resolve < 0, "Never", paste0(time_to_resolve))
         ) %>%

select(Domain, `Severely Poor`, 
         `Non Poor` = Display, 
         `Yearly Disparity Reduction` = relative_rate, 
         `Time if Reductions Are Constant`, 
         `Time if Growth Rates Are Constant`)

SDG_Evaluation_National %>% View()
SDG_Evaluation_National_Table %>% View()

  
  
