###############################################################################-

# Code to simulate capture and recruitment data for nifflers, from the magical 
# wizarding world of Harry Potter. Nifflers live until about 10 years old in the
# wild, and reach sexual maturity at 2 years of age. We know that niffler's 
# recruitment rate is directly tied to thier confidence in acquiring treasure, 
# which we beleive is tied to bigger body size. 

# In these data sets, we captured and GPS-tagged male and female nifflers. We 
# weighed them and then tracked them over time to determine the amount of 
# treasure they collected each week, and for females, the number of offspring 
# produced. 

# We are going to be simulating 3 datasets. 
#     1. Capture data - containing date of capture, age, sex, weight (g), and ID
#     2. Treasure data - containing ID, date, and treasure haul (g)
#     3. Recruitment data - containing ID, year, and number of offspring

# NOTE - these data are obviously made up to tickle and delight, and to teach:
#  - reading in data
#  - cleaning and joining tables
#  - creating derived variables
#  - summarizing and visualizing
#  - fitting simple models in an ecological context

# Code created by: Allison C Keever
# Last updated on: 12/5/2025

###############################################################################-

# Prep workspace---------------------------------------------------------------
# Load packages
library(tidyverse)

# Set global parameters for the study
n_nifflers <- 240
study_years <- 2015:2024
weeks_per_yr <- 26
sites <- c("Gringotts", "Hogsmeade", "Forbidden Forest", "Diagon Alley")



# Simulate data-------------------------------------------------------
# Simulate clean capture data
capture <- data.frame(
  sex = sample(c("F", "M"), n_nifflers, replace = TRUE),
  age = rpois(n_nifflers, 4), 
  capture_year = sample(study_years, n_nifflers, replace = TRUE),
  site = sample(sites, n_nifflers, replace = TRUE, 
                prob = c(0.35, 0.25, 0.25, 0.15))) %>%
  mutate(
    capture_date = make_date(capture_year, 1, 1) + 
      sample(1:365, n(), replace = TRUE),
    weight_g = round(400 + 50 * age + if_else(sex == "M", 80, 0) +
      rnorm(n(), mean = 0, sd = 60))) %>%
  arrange(capture_date) %>%
  mutate(id = sprintf("%s%03d", sex, 1:n()))
  

# Simulate clean treasure data
# Random year effect: 
year_eff <- rnorm(length(study_years), mean = 0, sd = 0.15)
names(year_eff) <- study_years
# Data
treasure <- capture %>%
  mutate(
    capture_year = as.integer(capture_year),
    max_follow_years = pmax(
      1, 
      pmin((10 + sample(0:2, n_nifflers, replace = TRUE)) - age,
           max(study_years) - capture_year + 1)),
    years_followed = map_int(max_follow_years, ~ sample(1:.x, 1)),
    year = map2(capture_year, years_followed, ~ seq(.x, .x + .y - 1))) %>%
  select(id, sex, age_at_capture = age, weight_g, site, year) %>%
  unnest(year) %>%
  group_by(id) %>%
  mutate(
    years_since_capture = year - min(year),
    age_current = age_at_capture + years_since_capture) %>%
  ungroup() %>%
  mutate(week = list(1:26)) %>%
  unnest(week) %>%
  mutate(
    date = make_date(year, 1, 1) + weeks(week - 1),
    # simple relationship: heavier + male + prime-age → more treasure
    weight_centered = (weight_g - mean(weight_g, na.rm = TRUE)) / 100,
    sex_eff = if_else(sex == "M", 0.2, 0),
    age_eff = 0.05 * (age_current - 4),
    site_eff = case_when(
      site == "Gringotts" ~ 0.40,          # best treasure sources
      site == "Diagon Alley" ~ 0.25,       # lots of coins dropped
      site == "Hogsmeade" ~ 0.10,          # shops, pubs
      site == "Forbidden Forest" ~ -0.15,  # scarce human treasure
      TRUE ~ 0),
    year_eff = year_eff[as.character(year)],
    log_mu = 5 + 0.7 * weight_centered + sex_eff + age_eff + site_eff + year_eff,
    treasure_g = rlnorm(n(), meanlog = log_mu, sdlog = 0.35)) %>%
  select(id, date, treasure_g, site) %>%
  arrange(date, site)


# Simulate clean recruitment data
# Get mean treasure per year
treasure_yearly <- treasure %>%
  mutate(year = year(date)) %>%
  group_by(id, year) %>%
  summarise(mean_treasure = mean(treasure_g),
            n_weeks = n(),
            .groups = "drop")

# Join with capture
recruitment <- treasure_yearly %>%
  inner_join(capture %>%
               select(id, sex, age_at_capture = age, capture_year),
             by = "id") %>%
  mutate(age_current = age_at_capture + (year - capture_year)) %>%
  filter(sex == "F", age_current >= 2) %>%
  # Link recruitment to condition (mean_treasure)
  mutate(
    treasure_sc = (mean_treasure - mean(mean_treasure)) / sd(mean_treasure),
    lambda = exp(0.5 + 0.4 * treasure_sc),
    offspring = rpois(n(), lambda)) %>%
  select(id, year, offspring) %>%
  arrange(year)


# Add messiness to data-------------------------------------------------------
# Missing values to age and weight in capture data
capture$age[sample(seq_len(n_nifflers), 
                   size = ceiling(0.05 * n_nifflers))] <- NA
capture$weight_g[sample(seq_len(n_nifflers), 
                        size = ceiling(0.05 * n_nifflers))] <- NA

# Messy site names in capture data 
gring_idx <- which(capture$site == "Gringotts")

if(length(gring_idx) >= 5){
  capture$site[sample(gring_idx, 2)] <- "gringotts"       
  capture$site[sample(gring_idx, 1)] <- "Gringotts "      
  capture$site[sample(gring_idx, 1)] <- "Gringotts Bank"  
}

# Capture year stored incorrectly (as character)
capture$capture_year <- as.character(capture$capture_year)

# Weight outliers (just a few)
good_weights <- which(!is.na(capture$weight_g))
# Too big
capture$weight_g[sample(good_weights, 2)] <- capture$weight_g[sample(
  good_weights, 2)] * 3
# Too small
capture$weight_g[sample(good_weights, 2)] <- sample(50:100, 2)

# Duplicate IDs (data entry mistakes)
capture$id[sample(2:n_nifflers, 2)] <- capture$id[1]

# Some missing weekly treasure values
treasure$treasure_g[sample(seq_len(nrow(treasure)), 
                           size = ceiling(0.05 * nrow(treasure)))] <- NA

# A few extreme outliers (huge treasure hauls)
t_good <- which(!is.na(treasure$treasure_g))
t_out <- sample(t_good, 3)
treasure$treasure_g[t_out] <- treasure$treasure_g[t_out] * 8

# Slightly inconsistent site names (to match capture-type issues)
ff_idx <- which(treasure$site == "Forbidden Forest")
if (length(ff_idx) >= 3) {
  treasure$site[sample(ff_idx, 2)] <- "ForbiddenForest"
}

# Some missing offspring counts (missed breeding records)
recruitment$offspring[
  sample(seq_len(nrow(recruitment)),
         size = ceiling(0.05 * nrow(recruitment)))] <- NA_integer_

# A couple of suspiciously high offspring counts
r_good <- which(!is.na(recruitment$offspring))
if (length(r_good) >= 3) {
  r_out <- sample(r_good, 2)
  recruitment$offspring[r_out] <- recruitment$offspring[r_out] * 3
}

# Year stored as character instead of numeric
recruitment$year <- as.character(recruitment$year)

# Clean up and save files------------------------------------------------------
rm(list = setdiff(ls(), c("capture", "treasure", "recruitment")))

# Write to files
write.csv(capture, "Data/nifflers_capture.csv", row.names = FALSE)
write.csv(treasure, "Data/nifflers_treasure.csv", row.names = FALSE)
write.csv(recruitment, "Data/nifflers_recruitment.csv", row.names = FALSE)

rm(list = ls())



