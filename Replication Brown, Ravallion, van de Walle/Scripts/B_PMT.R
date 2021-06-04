# Load data and packages
pacman::p_load(tidyverse, estimatr, texreg)
households <- readRDS("Replication Brown, Ravallion, van de Walle/Data/prepared/households")

# Basic PMT
basicpmt <- paste("toilet_flush", "toilet_pit", "floor_finished", "wall_finished", 
                  "roof_finished", "fuel_electric_gas", "fuel_coal", "urban",
                  "head_age_fct", "head_female", "head_educ_primary", "head_educ_secondary", 
                  "head_divorced", "head_widow", "head_work_salary", "head_work_selfemployed",
                  "shares_f_5", "shares_m_5", "shares_f_14", "shares_m_14", "shares_f_65", "shares_m_65",
                  "hhsize_fct", "month", "region",
                  sep = " + ")
basicpmt <- formula(paste("log(cpp) ~", basicpmt))

# Linear regression with robust standard errors (Stata style), not yet clustered at PSU level (where is PSU variable in WB data?)
basicpmt <- lm_robust(basicpmt, households, weights = mult, se_type = "stata", clusters = region)
screenreg(basicpmt, omit.coef = "region|month|head_age_fct|hhsize", 
          digits = 3, stars = c(0.01, 0.05, 0.1), single.row = T,
          custom.note = "Dummies for households size intervals, head age intervals, region and interview month included but omitted.")

# Extended (Big) PMT
bigpmt <- paste("water_piped", "water_well", "toilet_flush", "toilet_pit", 
                "floor_finished", "wall_finished", "roof_finished", "rooms", "kitchen_room", 
                "fuel_electric_gas", "fuel_coal", "electricity", 
                "radio", "television", "bicycle", "motorbike", "car", "mobile_phone", "computer", "stove_any", "iron", "generator", "own_house",
                "urban",
                "head_age_fct", "head_female", "head_educ_primary", "head_educ_secondary", "hh_educ_primary", "hh_educ_secondary",
                "head_divorced", "head_widow", "head_married_never", "head_work_salary", "head_work_selfemployed", "head_work_daily",
                "shares_f_5", "shares_m_5", "shares_f_14", "shares_m_14", "shares_f_65", "shares_m_65",
                "hhsize_fct", "month", "region",
                sep = " + ")
bigpmt <- formula(paste("log(cpp) ~", bigpmt))   

# Linear regression with robust standard errors (Stata style), not yet clustered at PSU level (where is PSU variable in WB data?)
bigpmt <- lm_robust(bigpmt, households, weights = mult, se_type = "stata", clusters = region)
screenreg(bigpmt, omit.coef = "region|month|head_age_fct|hhsize", 
          digits = 3, stars = c(0.01, 0.05, 0.1), single.row = T,
          custom.note = "Dummies for households size intervals, head age intervals, region and interview month included but omitted.")




# Save dataset with actual outcomes and fitted values
bpmt <- tibble(logyhat = basicpmt$fitted.values, yhat = exp(logyhat),
               logy = unlist(model.frame(basicpmt)[basicpmt$outcome]), y = exp(logy),
               w = households$mult / sum(households$mult),
               resid = logy - logyhat)  

# Plot residuals against log real consumption per capita (Figure 2, page 117)
ggplot(bpmt, aes(x = logy, y = resid)) + 
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_vline(xintercept = quantile(bpmt$logyhat, 0.2), color = "red") +
  labs(x = "Actual log consumption", y = "Residuals", title = "Uganda 2011/12 (Basic PMT)") +
  scale_x_continuous(breaks = c(8, 10, 12, 14, 16)) +
  theme_minimal()

# Calculate poverty lines and targeting measures:
#  Poverty lines, use weighteds quantiles
my_quantile <- function(x, w, p){
  oo <- order(x)
  cum.w <- cumsum(w[oo])/sum(w)
  cdf <- approxfun(cum.w, x[oo], yleft = min(x), yright = max(x), ties = min)
  return(cdf(p))
}

z = map_dbl(c(0.2, 0.4), ~my_quantile(bpmt$y, bpmt$w, .x))

# Headcount - H = F(z) = sum(w * I(y_i <= z))
h = map_dbl(z, ~sum(bpmt$w * (bpmt$y <= .x)))

# PMT score is simply the fitted value, since there is no training / testing split (bpmt$yhat)
# Inclusion Error Rate - households identified as poor (yhat<=z) which are not (y>z) as proportion of all identified as poor
ier <- map_dbl(z, ~ sum(bpmt$w * (bpmt$y > .x & bpmt$yhat <= .x)) / sum(bpmt$w * (bpmt$yhat <= .x)))
# Exclusion Error Rate - households identified as non-poor (yhat > z) which are (y<=z) as proportion of real poor
eer <- map_dbl(z, ~ sum(bpmt$w * (bpmt$yhat > .x & bpmt$y <= .x)) / sum(bpmt$w * (bpmt$y <= .x)))
# Targeting Error Rate - 