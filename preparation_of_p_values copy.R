# 2017 household level p-values
household_2017_pval_data <-
EICV_2017_household_level %>%
 as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c("Severely poor", "Non Poor")
  ) %>%
  mutate(
    poverty = factor(poverty),
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
  )

###
indicator_names <- c("ubudehe", "Support_2017", 'Support_2014', "improved_water_source", "improved_sanitation", "electric_lighting", "own_land")

district_names <- levels(EICV_2017_household_level$district)

output <-  matrix(ncol = length(indicator_names), nrow = length(district_names))

j <- 1

for (indicator_name in indicator_names) {
  i <- 1
for (district_name in district_names) {
  
  mod <- paste0("~ ", indicator_name, " + poverty")
  
  
  output[i, j] = round(as.numeric(svychisq(eval(parse(text = mod)), design = household_2017_pval_data %>%
                                     filter(district == district_name), 
                                   na.rm = TRUE)$p.value[[1]]),4)
  i <- i + 1
  }
  j<-j+1
}

colnames(output ) <- indicator_names

household_pvals_2017 <- output %>% as.data.frame() %>% mutate(district = district_names, year = 2017)

#####################
### 2017 Population-Level Data

population_2017_pval_data <-
  EICV_2017_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c("Severely poor", "Non Poor")
  ) %>%
  mutate(
    poverty = factor(poverty),
    women_in_primary = ifelse(sex == "Female", children_in_school, NA),
    men_in_primary = ifelse(sex == "Male", children_in_school, NA),
    
    women_in_secondary = ifelse(sex == "Female", children_in_secondary, NA),
    men_in_secondary = ifelse(sex == "Male", children_in_secondary, NA)
    
  )

###
indicator_names <- c("able_to_read", "have_health_insurance", 'gotcare_if_ill', "primary_completed", "children_in_school", "children_in_secondary")

district_names <- levels(EICV_2017_persons$district)

output_2 <-  matrix(ncol = length(indicator_names), nrow = length(district_names))

j <- 1

for (indicator_name in indicator_names) {
  i <- 1
  for (district_name in district_names) {
    
    mod <- paste0("~ ", indicator_name, " + poverty")
    
    
    output_2[i, j] = round(as.numeric(svychisq(eval(parse(text = mod)), design = population_2017_pval_data %>%
                                               filter(district == district_name), 
                                             na.rm = TRUE)$p.value[[1]]),4)
    i <- i + 1
  }
  j<-j+1
}

colnames(output_2 ) <- indicator_names

persons_pvals_2017 <- output_2 %>% as.data.frame() %>% mutate(district = district_names, year = 2017)



##################################

# 2014 household level p-values
household_2014_pval_data <-
  EICV_2014_household_level %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1,3)
  ) %>%
  mutate(
    poverty = as.factor(ifelse(poverty == 1, "Severely poor", "Non Poor")),
    ubudehe = ifelse(ubudehe == "Category 1", 1, 0),
    own_land = ifelse(Household_Own_Land == "Yes", 1, 0),
    healthcare_purchased = ifelse(healthcare_purchased_this_year == "Yes", 1, 0),
    improved_water_source = ifelse(drinking_water %in% 
                                     c("Public stand/pipe", "Public standpipe", "Borehole", "Tube Well /Borehole", "Protected well", "Protected spring", "Rain water", "Other _specify_", "Piped into dwelling", "Piped into yard", "Piped to Yard/Plot", "Other"), 
                                   1,   ifelse(drinking_water == "<NA>", NA, 0) ),
    
    improved_sanitation = ifelse(type_of_toilet %in% c("Flush toilet", "Pit latrine with solid slab"), 1, 0),
    electric_lighting = ifelse(main_source_lighting %in% c("Electricity from EWSA", "Other electricity distributors"), 1, 0)
    
  )

###
indicator_names <- c("ubudehe", "improved_water_source", "improved_sanitation", "electric_lighting", "own_land")

district_names <- levels(EICV_2014_household_level$district)

output_3 <-  matrix(ncol = length(indicator_names), nrow = length(district_names))

j <- 1

for (indicator_name in indicator_names) {
  i <- 1
  for (district_name in district_names) {
    
    mod <- paste0("~ ", indicator_name, " + poverty")
    
    output_3[i, j] = tryCatch(round(as.numeric(svychisq(eval(parse(text = mod)), design = household_2014_pval_data %>%
                                               filter(district == district_name), 
                                             na.rm = TRUE)$p.value[[1]]),4), error = function(e){NA})
    i <- i + 1
  }
  j<-j+1
}

colnames(output_3 ) <- indicator_names

household_pvals_2014 <- output_3 %>% as.data.frame() %>% mutate(district = district_names, year = 2014)




#####################

population_2014_pval_data <-
  EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  filter(
    poverty %in% c(1, 3)
  ) %>%
  mutate(
    poverty = factor(poverty),
    women_in_primary = ifelse(sex == "Female", children_in_school, NA),
    men_in_primary = ifelse(sex == "Male", children_in_school, NA),
    
    women_in_secondary = ifelse(sex == "Female", children_in_secondary, NA),
    men_in_secondary = ifelse(sex == "Male", children_in_secondary, NA)
    
  )

###
indicator_names <- c("able_to_read", "have_health_insurance", 'gotcare_if_ill', "primary_completed", "children_in_school", "children_in_secondary")

district_names <- levels(EICV_2014_persons$district)

output_4 <-  matrix(ncol = length(indicator_names), nrow = length(district_names))

j <- 1

for (indicator_name in indicator_names) {
  i <- 1
  for (district_name in district_names) {
    
    mod <- paste0("~ ", indicator_name, " + poverty")
    
    
    output_4[i, j] = tryCatch(round(as.numeric(svychisq(eval(parse(text = mod)), design = population_2014_pval_data %>%
                                                 filter(district == district_name), 
                                               na.rm = TRUE)$p.value[[1]]),4), error = function(e) {NA})
    i <- i + 1
  }
  j<-j+1
}

colnames(output_4) <- indicator_names

persons_pvals_2014 <- output_4 %>% as.data.frame() %>% mutate(district = district_names, year = 2014)


p_values_2014 <- 
persons_pvals_2014 %>% 
  rename(District = district,
         primary_attendance = children_in_school,
         secondary_attendance = children_in_secondary,
         Year = year
            ) %>%
  gather(Domain, p_value, -c(Year, District)) %>%
  bind_rows(
household_pvals_2014 %>% 
  rename(District = district,
         First_Ubudehe = ubudehe,
         Year = year) %>%
  gather(Domain, p_value, -c(Year, District))
)  %>%
  bind_rows (
  household_pvals_2017 %>% 
    select(Support = Support_2014, District = district) %>%
    gather(Domain, p_value, -District) %>%
    mutate(Year = 2014)

  ) %>%
  mutate(Year = as.character(Year))



p_values_2017 <- 
  persons_pvals_2017 %>% 
  rename(District = district,
         primary_attendance = children_in_school,
         secondary_attendance = children_in_secondary,
         Year = year
  ) %>%
  gather(Domain, p_value, -c(Year, District)) %>%
  bind_rows(
    household_pvals_2017 %>% 
      rename(District = district,
             First_Ubudehe = ubudehe,
             Year = year,
             Support = Support_2017) %>%
      select(-Support_2014) %>%
      gather(Domain, p_value, -c(Year, District))
  ) %>%
  mutate(Year = as.character(Year))









