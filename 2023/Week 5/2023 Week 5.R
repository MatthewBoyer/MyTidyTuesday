#########################################################################################################

# Tidy Tuesday - 2023 Week 5: Cats on the Move

#########################################################################################################

# Import tidyverse and tidytuesday
library(tidyverse)
library(tidytuesdayR)

#==========================================================================================
# Import cats data
#==========================================================================================

tuesdata <- tidytuesdayR::tt_load(2023, week = 5)

cats <- tuesdata$cats_uk
cats_ref <- tuesdata$cats_uk_reference

#==========================================================================================
# Experiment with data
#==========================================================================================

# Calculate each cat's average ground speed
cat_spd <- cats %>% 
  group_by(tag_id) %>% 
  summarise(mean_spd = mean(ground_speed, na.rm = T))

# Merge sex data with average speed data
cat_spd <- cat_spd %>% 
  merge(cats_ref %>% select(tag_id,
                            animal_sex,
                            hrs_indoors,
                            age_years),
        by = "tag_id")

# Create tiers based on hours spent indoors
cat_spd <- cat_spd %>% 
  mutate(
    
    out_rank = ifelse(hrs_indoors >= 10 & hrs_indoors <= 13,
                      "Average (10-13)",
                      ifelse(hrs_indoors > 13,
                             "Above Average (13+)",
                             ifelse(hrs_indoors < 10,
                                    "Below Average (< 10)",
                                    ""))),
    out_rank = as.factor(out_rank)
    
  )

# Remove "Tag" from cat names
cat_spd$tag_id <- str_replace(cat_spd$tag_id, "-Tag", "")

# Create column to store tooltip data
cat_spd <- cat_spd %>% 
  mutate(
    
    tooltip_text = paste0(tag_id, "\n",
                          round(mean_spd), "m/sec", "\n",
                          toupper(animal_sex), "\n",
                          "Daily Time Indoors:", hrs_indoors, "hr", "\n",
                          "Age:", age_years)
  )

#===========================================================================================
# Create experimental data visualizations
#===========================================================================================

# Load plotting packages
library(plotly)
library(ggiraph)
library(patchwork)


# Create plot of mean speed vs cat sex
cat_graph <- 
  ggplot(data = cat_spd,
         aes(x = age_years,
             y = mean_spd,
             data_id = tag_id)) +
  
  geom_point(data = cat_spd,
             aes(colour = factor(animal_sex),
                 shape = out_rank,
                 tooltip = tooltip_text,)) +
  
  geom_col_interactive(color = "black",
                       fill = "#0072B2",
                       size = 0.5)

# Display interactive graph
girafe(ggobj = cat_graph)
