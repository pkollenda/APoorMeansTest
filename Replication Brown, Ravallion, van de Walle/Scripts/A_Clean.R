# Download from World Bank Microdata Site, Uganda 2011/2012 LSMS-ISA Wave 3
# 

pacman::p_load(haven, tidyverse, survey)

# Collect Information on Household Head ----
# Household Head can also still be in education which needs to be included below, otherwise to often missing (or now 0).
household_head <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC2.dta")[c("HHID", "PID", "h2q3", "h2q4", "h2q8", "h2q10")] %>% 
  filter(h2q4 == 1) %>% 
  select(-h2q4) %>% 
  # Add information on head gender, age and marital status
  mutate(head_female = h2q3 == 2, head_married = h2q10 %in% c(1, 2), head_divorced = h2q10 == 3, head_widow = h2q10 == 4, head_married_ever = h2q10 != 5, head_married_never = h2q10 == 5) %>%
  select(HHID, PID, head_female, head_age = h2q8, head_married, head_divorced, head_widow, head_married_ever, head_married_never) %>%
  # # Add information on head education
  left_join(read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC4.dta")[c("HHID", "PID", "h4q5", "h4q7", "h4q9")], by = c("HHID", "PID")) %>%
  mutate(highest_educ = case_when(
          h4q5 == 1 ~ 0,
          h4q5 == 2 & h4q7 %in% c(2, 10, 11, 12, 13, 14, 15, 16) ~ 0,
          h4q5 == 2 & h4q7 %in% c(17, 31, 32, 33, 41) ~ 1,
          h4q5 == 3 & h4q9 %in% c(32) ~ 1,
          h4q5 == 2 & h4q7 %in% c(34, 35, 36, 21, 22, 23, 51, 61) ~ 2,
          h4q5 == 3 & h4q9 %in% c(34, 50, 61) ~ 2,
          h4q5 == 2 & h4q7 == 99 ~ 0,
          TRUE ~ NA_real_),
         head_educ_primary = highest_educ >= 1, head_educ_secondary = highest_educ == 2) %>%
  select(-c(highest_educ, h4q5, h4q7, h4q9)) %>%
  # # Add information on head work and occupation
  left_join(read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC8.dta")[c("HHID", "PID", "h8q4", "h8q5", "h8q6", "h8q7", "h8q8", "h8q9", "h8q10", "h8q11", "h8q12", "h8q13", "h8q14", "h8q15")], by = c("HHID", "PID")) %>%
  mutate(head_work_salary = (h8q5 == 1 | h8q11 == 1), head_work_selfemployed = (h8q7 == 1 | h8q9 == 1), head_work_farm = (h8q13 == 1),
         head_work_daily = (h8q15 == 1)) %>%
  mutate(head_work_daily = replace_na(head_work_daily, FALSE)) %>%
  select(-c(PID, h8q4:h8q15))

# Collect Information on Household Composition ----
household_composition <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC2.dta")[c("HHID", "PID", "h2q3", "h2q8")] %>% 
  # Missing are widows, disables(m/f), orphans (m/f).
  group_by(HHID) %>% 
  mutate(gender = factor(if_else(h2q3 == 1, "m", "f")), agegroup = cut(h2q8, c(0, 5, 14, 64, 110), labels = c("5", "14", "64", "65"), include.lowest = T)) %>% 
  mutate(agegroup = fct_cross(gender, agegroup, sep = "_")) %>% 
  select(-c(h2q3, h2q8, gender)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = agegroup, values_from = agegroup, names_prefix = "shares_") %>% 
  mutate(across(starts_with("shares_"), ~!is.na(.x))) %>% select(-shares_NA) %>% 
  group_by(HHID) %>% 
  summarise(hhsize = n(), across(starts_with("shares_"), mean)) %>% 
  ungroup()

# Collect Information on Household ----
household_educ <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC4.dta")[c("HHID", "PID", "h4q5", "h4q7", "h4q9")] %>%
  mutate(highest_educ = case_when(
    PID == "115300270402" ~ 2,
    h4q5 == 1 | (h4q5 == 2 & h4q7 %in% c(2, 10, 11, 12, 13, 14, 15, 16)) ~ 0,
    h4q5 == 2 & h4q7 %in% c(17, 31, 32, 33, 41) ~ 1,
    h4q5 == 2 & h4q7 %in% c(34, 35, 36, 21, 22, 23, 51, 61) ~ 2,
    h4q5 == 3 & h4q9 %in% c(1, 10, 11, 12, 13, 14, 15) ~ 0,
    h4q5 == 3 & h4q9 %in% c(16, 17, 30, 31, 32, 40) ~ 1,
    h4q5 == 3 & h4q9 %in% c(33, 34, 35, 50, 61) ~ 2,
    is.na(h4q5) & h4q9 == 15 ~ 0,
    TRUE ~ NA_real_)) %>% 
  group_by(HHID) %>% 
  summarise(highest_educ = max(highest_educ, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hh_educ_primary = highest_educ >= 1, hh_educ_secondary = highest_educ == 2) %>% 
  select(HHID, hh_educ_primary, hh_educ_secondary)

household_living <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC9A.dta")[c("HHID", "h9q4", "h9q5", "h9q6", "h9q7", "h9q22", "h9q3")] %>% 
  # Members per room, Kitchen room, 
  transmute(water_piped = h9q7 %in% c(1, 2), water_well = h9q7 %in% c(4, 5),
            toilet_flush = h9q22 %in% c(6, 7), toilet_pit = h9q22 %in% c(1, 2, 3, 4, 5),
            roof_natural = h9q4 %in% c(1, 2), roof_rudimentary = h9q4 %in% c(3, 4), roof_finished = h9q4 %in% c(5, 6, 7, 8),
            wall_natural = h9q5 %in% c(1, 2), wall_rudimentary = h9q5 %in% c(3, 4, 5), wall_finished = h9q5 %in% c(6, 7, 8, 24),
            floor_natural = h9q6 %in% c(1, 2), floor_rudimentary = h9q6 %in% c(3), floor_finished = h9q6 %in% c(4, 5, 6, 96),
            rooms = h9q3, HHID)

household_energy <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC10B.dta") %>% 
  mutate(fuel = fct_drop(fct_collapse(factor(h10q8_1), "electric_gas" = c("1", "2"), "coal" = "6", "wood" = c("4", "5")))) %>% 
  group_by(HHID, fuel) %>% 
  summarise(used = any(h10q8_2 == 1)) %>% 
  pivot_wider(names_from = fuel, values_from = used, names_prefix = "fuel_", values_fill = FALSE) %>% 
  select(HHID, fuel_electric_gas, fuel_wood, fuel_coal) %>% ungroup() %>% 
  # Add information on electricity use
  full_join(read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC10A.dta") %>% transmute(HHID, electricity = h10q1 == 1, kitchen_room = h10q12 == 1), by = "HHID") %>% 
  # Add information on stove availability
  full_join(read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC10B.dta") %>% group_by(HHID) %>% summarise(stove_any = any(h10q8_1 %in% c(1, 2, 3, 4, 5, 6, 7) & h10q8_2 == 1)) %>% ungroup(), by = "HHID")

household_assets <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC14.dta")[c("HHID", "h14q2", "h14q3")] %>% 
  pivot_wider(names_from = h14q2, values_from = h14q3) %>% 
  select(HHID, "radio" = `7`, "television" = `6`, "bicycle" = `10`, "motorbike" = `11`, "car" = `12`,
         "mobile_phone" = `16`, "computer" = `17`, "iron" = `5`,
         "generator" = `8`, "own_house" = `1`) %>% 
  mutate(across(-HHID, ~case_when(.x == 1 ~ TRUE, .x == 2 ~ FALSE, is.na(.x) ~ NA)))

# Households and Merge
households <- read_dta("Replication Brown, Ravallion, van de Walle/Data/GSEC1.dta")[c("HHID", "region", "urban", "month", "mult")] %>% 
  full_join(household_assets, by = "HHID") %>% 
  full_join(household_composition, by = "HHID") %>% 
  full_join(household_educ, by = "HHID") %>% 
  full_join(household_energy, by = "HHID") %>% 
  full_join(household_head, by = "HHID") %>% 
  full_join(household_living, by = "HHID") %>% 
  # Add outcome variable
  full_join(read_dta("Replication Brown, Ravallion, van de Walle/Data/UNPS 2011-12 Consumption Aggregate.dta")[c("HHID", "cpexp30", "welfare", "poor")], by = "HHID") %>%
  filter(HHID != "205300170503") %>% 
  # Changes which need combined variables
  mutate(cpp = cpexp30 / hhsize * 12, rooms = if_else(rooms == 0, as.double(hhsize), hhsize / rooms)) %>% 
  # Remove households with missing data for any of the variables
  filter(across(everything(), ~!is.na(.x))) %>% 
  # Create dummies for regressions
  mutate(hhsize_fct = cut(hhsize, c(1, 3, 5, 7, 9, Inf), right = FALSE),
         head_age_fct = cut(head_age, c(14, 24, seq(29, 85, 5), 105), right = FALSE),
         month = factor(month), region = factor(region))

rm(household_assets, household_composition, household_educ, household_head, household_energy, household_living)


# Save as RDS, open in R with readRDS(.)
saveRDS(households, "Replication Brown, Ravallion, van de Walle/Data/prepared/households")
# Save as Stata dta file, open in Stata
write_dta(households, "Replication Brown, Ravallion, van de Walle/Data/prepared/households.dta")

rm(households)


# Calculate survey means to compare with table A1 from A Poor Means Test
# households <- readRDS("Data/prepared/households")
# svy <- svydesign(ids = ~1, weights = ~mult, data = households)
# vars <- formula(paste("~", 
#                       paste("cpp", "cpexp30", "welfare", "poor",
#                             "water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_natural", "floor_rudimentary", "floor_finished", "wall_natural", "wall_rudimentary", "wall_finished", "roof_natural", "roof_rudimentary", "roof_finished", "rooms", "kitchen_room", "fuel_electric_gas", "fuel_coal", "fuel_wood", "electricity",
#                             "radio", "television", "bicycle", "motorbike", "car", "mobile_phone", "computer", "stove_any", "iron", "generator", "own_house", "urban",
#                             "head_age", "head_female", "head_educ_primary", "head_educ_secondary", "hh_educ_primary", "hh_educ_secondary", "head_married_ever", "head_married", "head_divorced", "head_widow", "head_married_never", "hhsize", "head_work_salary", "head_work_selfemployed", "head_work_farm", "head_work_daily",
#                             "shares_f_5", "shares_m_5", "shares_f_14", "shares_m_14", "shares_f_64", "shares_m_64", "shares_f_65", "shares_m_65", 
#                             sep = " + ")))
# means <- svymean(vars, design = svy) %>% round(digits = 3)
# means <- means[which(!str_detect(names(means), "FALSE"))]
# names(means) <- str_remove(names(means), "TRUE")
# means

# Note: means not exact, especially for total consumption (important!), max educ hh primary,
# all head work variables except farm. Missing still share widows, disabled (m/f), orphans (m/f).
# 
# Sample size different from paper, is 2747 but should be 2671 or 2650
# 
# In paper they present summary stats for all these variables, but only use a subset in the basic PMT regression.
# Also, description of variables in regression table hints to different construction (i.e. fuel_electric_gas_kerosine), work (last week vs. 12 months)
# rm(list = ls())
