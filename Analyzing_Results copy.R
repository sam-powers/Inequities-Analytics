library(ggrepel)
library("Hmisc")
library(corrplot)
library(tidyverse)


# Percent of villages with increase in disparity in specific domain

disparity_increases <-
disparity_identification %>%
  filter(Poverty == "Non Poor", Year == 2017) %>%
  mutate(disparity_increased = ifelse(Difference_From_Year>= 0, 1, 0),
         Increases = ifelse(Difference_From_Year < 0, NA, Difference_From_Year)
  ) %>%
  group_by(Domain) %>%
  summarize(Percent_Increased = round(mean(disparity_increased, na.rm = TRUE)*100, 2),
            min_increase = min(Increases, na.rm = TRUE),
            median_increase = median(Increases, na.rm = TRUE),
            max_increase = max(Increases, na.rm = TRUE)
  )



#########
# What disparities exist at the beginning?
disparities_2014 <-
disparity_identification %>%
filter(Year == 2014, Poverty == "Non Poor") %>%
  left_join(p_values_2014) %>%
  select(District, Domain, Difference_From_Poor, Display, p_value) %>%
  group_by(Domain) %>%
  mutate(Indicator = ifelse(Difference_From_Poor > 0, 1, 0), 
         Ratio = Display/(Display - Difference_From_Poor),
         p_indicator = ifelse(p_value <.05, 1, 0),
         significant_disparity_indicator = Indicator*p_indicator ) %>%
  summarize(
    Percent_Significant_Disparity = mean(significant_disparity_indicator, na.rm = TRUE), 
    Percent_Disparity = mean(Indicator, na.rm = TRUE), 
            Med_Difference = median(Difference_From_Poor), 
            min_difference = min(Difference_From_Poor), 
            max_difference = max(Difference_From_Poor),
            Med_Ratio = median(Ratio))

View(disparities_2014)

# What disparities exist at the End?

disparities_2017 <-
  disparity_identification %>%
  filter(Year == 2017, Poverty == "Non Poor") %>%
  left_join(p_values_2017) %>%
  select(District, Domain, Difference_From_Poor, Display, p_value) %>%
  group_by(Domain) %>%
  mutate(Indicator = ifelse(Difference_From_Poor > 0, 1, 0), 
         Ratio = Display/(Display - Difference_From_Poor),
         p_indicator = ifelse(p_value <.05, 1, 0),
         significant_disparity_indicator = Indicator*p_indicator ) %>%
  summarize(
    Percent_Significant_Disparity = mean(significant_disparity_indicator, na.rm = TRUE), 
    Percent_Disparity = mean(Indicator, na.rm = TRUE), 
    Med_Difference = median(Difference_From_Poor), 
    min_difference = min(Difference_From_Poor), 
    max_difference = max(Difference_From_Poor),
    Med_Ratio = median(Ratio))

View(disparities_2017)


disparities_2017

# Changing number of disparities. 


# Restructuring Disparities into a reasonable framework
Disparities_2014_2017 <-
  disparity_identification %>%
  filter(Poverty == "Non Poor", Year == 2017, !Domain  %in% c("sex_ratio_primary", "sex_ratio_secondary")) %>%
  select(District, Domain, Difference_From_Year) %>%
  spread(Domain, Difference_From_Year)

Disparities_2014_2017 %>% View()


disparity_identification %>%
  filter(Domain == "improved_water_source") %>%
  View()
         
         
         
#########
mydata.rcorr = rcorr(as.matrix(Disparities_2014_2017[,-1]))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

mydata.cor = cor(Disparities_2014_2017[,-1], method = c("spearman"))
corrplot(mydata.cor)

palette = colorRampPalette(c("red", "white", "blue")) (100)
heatmap(x = mydata.cor, col = palette, symm = TRUE)


################################
# SDG 1: Poverty


poverty_changes_district <-
  # 2017 Population Poverty
  EICV_2017_persons %>%
  as_survey(
    weight = weight
  ) %>%
  mutate(
    SeverePoverty = ifelse(poverty == "Severely poor", 1, 0)
  ) %>%
  group_by(district) %>%
  
  summarize(
    Value  = survey_mean(SeverePoverty),
    UnweightedN = unweighted(sum(SeverePoverty))
  ) %>%
  transmute(
    Indicator = "Severe Poverty",
    District = district, 
    Year = 2017,
    Value = round(Value*100,1),
    Level = "Population",
    UnweightedN = UnweightedN
  ) %>%
  
  bind_rows(

EICV_2014_persons %>%
  as_survey(
    weight = weight
  ) %>%
  mutate(
    SeverePoverty = ifelse(poverty == "1", 1, 0)
  ) %>%
  group_by(district) %>%
  summarize(
    Value  = survey_mean(SeverePoverty),
    UnweightedN = unweighted(sum(SeverePoverty))
  ) %>%
  transmute(
    Indicator = "Severe Poverty",
    District = district, 
    Year = 2014,
    Value = round(Value*100,1),
    Level = "Population",
    UnweightedN = UnweightedN

    ) 
)%>%
  
  bind_rows(
    # 2017 Household Poverty    
    EICV_2017_household_level %>%
      as_survey(
        weight = weight
      ) %>%
      mutate(
        SeverePoverty = ifelse(poverty == "Severely poor", 1, 0)
      ) %>%
      group_by(district) %>%
      summarize(
        Value  = survey_mean(SeverePoverty),
        UnweightedN = unweighted(sum(SeverePoverty))
      ) %>%
      transmute(
        Indicator = "Severe Poverty",
        District = district, 
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
      group_by(district) %>%
      summarize(
        Value  = survey_mean(SeverePoverty),
        UnweightedN = unweighted(sum(SeverePoverty))
      ) %>%
      transmute(
        Indicator = "Severe Poverty",
        District = district, 
        Year = 2014,
        Value = round(Value*100,1),
        Level = "Household",
        UnweightedN = UnweightedN
      )
  ) %>%
  # mutate(
  #   Value = paste0(Value, "% (", UnweightedN, ")")
  #   
  # ) %>%
  select( -UnweightedN) %>%
  spread(Year, Value )

sdg1<-
poverty_changes_district %>%
  filter(Level == "Population") %>%
  mutate(
    Change = `2017` - `2014`
  ) %>%
  mutate(Increase = ifelse(Change >= 0, 1, 0),
         Percent_Increase = round(Change/`2014`*100,1) 
                                            ) %>%
  select(District, Level, `2014`, `2017`, Change, Percent_Increase, Increase) %>%
  mutate(
    Increase_Vals = ifelse(Increase == 1, Change, NA), 
    Increase_Pct = ifelse(Increase == 1, Percent_Increase, NA),
    Decrease_Vals = ifelse(Increase == 0, Change, NA), 
    Decrease_Pct = ifelse(Increase == 0, Percent_Increase, NA)
       )

sdg1 %>% select(District, Change)

sdg1 %>%
  summarize(
            Increase = mean(Increase), 
            Median_Increase = median(Increase_Vals, na.rm = TRUE),
            Median_Pct_Increase = median(Increase_Pct, na.rm = TRUE),
            Median_Decrease = median(Decrease_Vals, na.rm = TRUE),
            Median_Pct_Decrease = median(Decrease_Pct, na.rm = TRUE)
            )




sdg1 %>% select(District, Change)

######################################
# Which districts are we discussing? #

ranking_districts <-
disparity_identification %>% 
  filter(Poverty == "Non Poor", Year == 2017, !Domain %in% c(
    "men_primary",
    "men_secondary",
    "women_primary",
    "women_secondary",
    "sex_ratio_secondary",
    "Support",
    "First_Ubudehe",
    "own_land"
  )
    
         ) %>%
  select(District, Domain, Difference_From_Year) %>%
  mutate(Disparity_Increase = ifelse(Difference_From_Year > 0, 1, 0)) %>%
  group_by(District) %>%
  mutate(Total_Increases = sum(Disparity_Increase, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-Disparity_Increase) %>%
  spread(Domain, Difference_From_Year) %>%
  arrange(-Total_Increases) %>% 
  inner_join(
    sdg1 %>% select(District, `2017`)
  )
  
  
# Estimated years to end the disparity may be a really really solid metric to utilize in this situation. # 

ranking_districts %>%
  View()
  


















############################
  ##########################
    ########################
      ######################
    ########################
  ##########################
############################
  
# Graphing #
  


theme_opts2 <- list(theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          plot.margin = unit(
                            c(t = .1, r = .5,b = .1, l = .1), "in"
                          ),
                          panel.background = element_blank(),
                          plot.background = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line(),
                          #    axis.text.x = element_blank(),
                          #   axis.text.y = element_blank(),
                          #   axis.ticks = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_text(vjust = 1, size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0) ),
                          plot.title = element_text(size=18, hjust = .5 ))
)




p <- ggplot(sdg_eval_graph , 
            aes(x = as.numeric(Year), 
                y = Difference_From_Poor,  
                group = Domain
                #  color = highlight, 
                #  alpha = opaque 
            )) +
  
  geom_line() +
  
  geom_text_repel(data = sdg_eval_graph %>%
                    group_by(District, Domain) %>%
                    arrange(desc(Year)) %>%
                    slice(1) ,
                  
                  aes(x = as.numeric(Year) +.1, label =  Domain), 
                  color = "Black", alpha = 1, hjust = 0, size = 3,
                  #  nudge_x = .5,
                  segment.size = .2,
                  segment.color = '#cccccc',
                  #  nudge_y = .05,
                  force = 2,
                  xlim = c(2017, 2020)
                  
                  
  ) +
  
  facet_wrap(.~District , scales = "free_y") +
  # scale_color_manual(values=c("grey", "#800000", "blue"), guide = "none") +
  # scale_alpha_continuous(range= c( .35,1 ), guide="none") +
  coord_cartesian(clip = 'off') +
  theme(panel.spacing = unit(3, "lines")) +
  
  theme_opts2 +
  labs(y = "Percent Difference Between Non-Poor and Severely Poor", x = "Year") +
  ylim(-30, 100) 



ggsave("Data_Files/percent_differences.jpg", p, width=20, height=20, dpi=200) 

sdg_eval_graph

################


