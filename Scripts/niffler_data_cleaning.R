###############################################################################-

# Clean and prepare niffler capture, treasure, and recruitment data

#   This script:
#     - Reads raw CSVs
#     - Cleans types and messy text fields
#     - Creates cleaned tables:
#         capture, treasure, recruitment
#     - Creates joined/derived objects:
#         cap_treasure, treasure_yearly, recruitment_summary

# Code created by: Allison C Keever
# Last updated on: 12/5/2025

###############################################################################-

# Load packages
library(tidyverse)

# Read in raw data
capture_raw <- read_csv("Data/nifflers_capture.csv")
treasure_raw <- read_csv("Data/nifflers_treasure.csv")
recruitment_raw <- read_csv("Data/nifflers_recruitment.csv")

# Capture data clean up ------------------------------------------------------
# Look at data structure
glimpse(capture_raw)

# The data types look mostly reasonable. I want to check ranges and missingness.
summary(capture_raw)

# I see that age ranges from 0 to 10. In reality, age 0 at capture might be odd,
# but for now I’ll leave it and just note it as something to think about.
# There are also a few NAs in age and weight_g.

# Check sex levels
unique(capture_raw$sex)
# No issues with sex: either "M" or "F", consistent.

# Check site values
unique(capture_raw$site)
# I see some issues with Gringotts (different spellings / spacing) and possibly
# other sites. I’ll want to standardize these so joins and summaries behave.

# Now I’ll create a cleaned capture data set. I’ll keep the raw version untouched.
capture <- capture_raw %>%
  mutate(
    # Make sure capture_year is numeric (if it came in as character)
    capture_year = as.integer(capture_year),
    
    # Clean up site names: trim whitespace, then standardize variants
    site = str_trim(site),
    site = str_replace_all(site, "Gringotts Bank", "Gringotts"),
    site = str_replace_all(site, "gringotts", "Gringotts"),
    
    # Make sex a factor
    sex = as.factor(sex)
  )

# Quick check of cleaned capture data
summary(capture)
unique(capture$site)

# Check for duplicated IDs (these can cause trouble in joins)
any(duplicated(capture$id))
which(duplicated(capture$id))

# Identify duplicated ID values
dup_ids <- capture$id[duplicated(capture$id)]
dup_ids

# View the duplicated rows
capture %>% 
  filter(id %in% dup_ids)

# These appear to be accidental duplicate entries.
# In real data, we'd try to figure out:
#   - Was the animal entered twice in error?
#   - Was the ID mistyped somewhere?
#   - Does one of these belong to a different ID entirely?
#
# Sometimes you can use other tables to help figure out which
# version is "real." For example, we can look in the treasure
# or recruitment data to see which rows actually show up there.

# Anti-join example (see which capture rows have NO matching treasure data)
anti_join(recruitment, capture, by = "id") 

# It seems F032 and F087 ids are not in the capture sheet. F032 shows up in the 
# recruitment sheet in 2017 which means that is likely the female capture at 
# age 2 in 2016 and F087 is the other. I will replace them. Note: In real field 
# data, you would double-check original datasheets. Here we are making an 
# informed correction for demonstration purposes. 
capture <- capture %>%
  mutate(id = case_when(
    sex == "F" & capture_year == 2016 & id == "M001" ~ "F032", 
    sex == "F" & capture_year == 2018 & id == "M001" ~ "F087",
    TRUE ~ id))

# Recheck for duplicates
any(duplicated(capture$id))

# Treasure data clean up -----------------------------------------------------
glimpse(treasure_raw)
summary(treasure_raw)

# Check site names here too
unique(treasure_raw$site)

# Check date type
class(treasure_raw$date)

# Clean treasure:
#  - Ensure date is Date
#  - Standardize site names the same way as in capture
#  - Fix crazy high number by dropping
treasure <- treasure_raw %>%
  mutate(
    # Make sure date is a Date object
    date = as_date(date),
    
    # Clean site names
    site = str_trim(site),
    site = str_replace_all(site, "ForbiddenForest", "Forbidden Forest")
  )

summary(treasure)
unique(treasure$site)

# Check missingness in treasure_g
sum(is.na(treasure$treasure_g))

# Recruitment data clean up --------------------------------------------------
glimpse(recruitment_raw)
summary(recruitment_raw)

# Check year type
class(recruitment_raw$year)

# Clean recruitment:
#  - Convert year to integer
recruitment <- recruitment_raw %>%
  mutate(year = as.integer(year))

summary(recruitment)

# Check missing offspring and range
sum(is.na(recruitment$offspring))
range(recruitment$offspring, na.rm = TRUE)

# 42 is obviously an error, nifflers can't have that many. That was a mistype
# that we will have to drop. Find all values greater than 10, as those are 
# starting to be unrealistic
recruitment %>% filter(offspring > 10)

# 12 is likely just a big litter, but 42 needs to be dropped. 
recruitment <- recruitment %>% 
  filter(offspring <= 12)
range(recruitment$offspring, na.rm = TRUE)

# Join capture + treasure ----------------------------------------------------
# I want a dataset that has treasure plus individual info (sex, weight, etc.).

# Before joining, it’s useful to know how many rows in each:
nrow(capture)
nrow(treasure)

# Join on id
cap_treasure <- treasure %>%
  left_join(
    capture %>% select(id, sex, age, weight_g, site, capture_year),
    by = "id"
  )

glimpse(cap_treasure)

# Check for NAs introduced by the join (e.g., ids in treasure not in capture)
summary(cap_treasure)

# Create yearly treasure summaries -------------------------------------------
# I want mean and total treasure per id-year (will be useful for recruitment analysis).

treasure_yearly <- treasure %>%
  mutate(year = year(date)) %>%
  group_by(id, year) %>%
  summarise(mean_treasure = mean(treasure_g, na.rm = TRUE),
            total_treasure = sum(treasure_g, na.rm = TRUE),
            n_weeks = n(),
            .groups = "drop")

glimpse(treasure_yearly)
summary(treasure_yearly)

# Join recruitment with yearly treasure and capture data
# Goal: one row per female per year with:
#   - offspring
#   - mean_treasure
#   - total_treasure
#   - age at recruitment (age_current)

recruitment_datum <- recruitment %>%
  # Add in mean treasure per id-year
  left_join(treasure_yearly, by = c("id", "year")) %>%
  # Add capture info
  left_join(capture %>% select(id, sex, age, capture_year), by = "id") %>%
  mutate(
    # Age at time of recruitment
    age_current = age + (year - capture_year)
  )

glimpse(recruitment_datum)

# For now I’ll keep all rows (including those with missing mean_treasure or
# offspring) and handle missingness when I summarize or model the data.

# At this point we have:
#   capture              - cleaned individual-level capture data
#   treasure             - cleaned weekly treasure data
#   recruitment          - cleaned annual recruitment data
#   cap_treasure         - joined capture + treasure
#   treasure_yearly      - mean treasure per id-year
#   recruitment_datum    - recruitment + treasure + capture info

# Clean up workspace
rm(dup_ids, capture_raw, recruitment_raw, treasure_raw)
