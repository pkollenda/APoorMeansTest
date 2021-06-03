# Basic PMT

basicpmt <- paste("toilet_flush", "toilet_pit", "floor_finished", "wall_finished", 
                  "roof_finished", "fuel_electric_gas", "fuel_coal", "urban",
                  "head_age_fct", "head_female", "head_educ_primary", "head_educ_secondary", 
                  "head_divorced", "head_widow", "head_work_salary", "head_work_selfemployed",
                  "shares_f_5", "shares_m_5", "shares_f_14", "shares_m_14", "shares_f_65", "shares_m_65",
                  "month", "region",
                  sep = " + ")
basicpmt <- formula(paste("log(cpp) ~", basicpmt))

lm_robust(basicpmt, households)
