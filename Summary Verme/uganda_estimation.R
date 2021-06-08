#
# Poverty Targeting a la Verme 2020 and Brown, Ravallion, van de Walle 2016
# Data: Uganda LSMS-ISA 2011/12
# 
# Plan: 
# 1) Replicate Brown et al. basic PMT and extended PMT OLS regression results
# 2) Add train-test split to Brown et al. model and see if targeting accuracy (IER, EER, TER) holds up
# 3) Estimate LASSO and Random Forest Regression and compare with targeting measures from OLS, quantile regression and poverty weighted least squares.
# 4) Create 500 bootstrap samples and re-estimate basic PMT, extended PMT, LASSO and Random Forest model and visualize uncertainty of predictions.
# 
# Author: Philipp Kollenda, 08. June 2021
# 

### Load packages and data ----
pacman::p_load(tidyverse, estimatr, ranger, glmnet, glmnetUtils, haven, furrr)

# Get data from previous cleaning of the LSMS-ISA to replicate Brown et al.
data = readRDS("Replication Brown, Ravallion, van de Walle/Data/prepared/households") %>% 
  # Exclude three outliers (top 0.1 percent) in terms of income, probably a data error?
  filter(cpp < quantile(cpp, 0.999))

### Create helper functions ----
#  For poverty lines, use weighted quantiles
my_quantile <- function(x, w, q){
  oo <- order(x)
  cum.w <- cumsum(w[oo])/sum(w)
  cdf <- approxfun(cum.w, x[oo], yleft = min(x), yright = max(x), ties = min)
  return(cdf(q))
}
# Calculation of targeting measures
my_targetmeasures <- function(y, yhat, z, zhat, w){
  
  # Headcount: poverty rate based on predicted incomes and fixed poverty line (from training data)
  H = sum(w * (yhat <= z))
  # Inclusion Error Rate: of all predicted poor, share which are not truly poor
  IER = sum(w * (y > z & yhat <= z)) / sum(w * (yhat <= z))
  # Exclusion Error Rate: of all true poor, share which are falsely not identified as poor. 
  EER = sum(w * (yhat > z & y <= z)) / sum(w * (y <= z))
  # Targeting Error Rate: if poverty rate is fixed, IER = EER = TER
  TER = sum(w * (yhat > zhat & y <= z)) / sum(w * (y <= z))
  
  return(list(H = H, IER = IER, EER = EER, TER = TER))
}
# Estimation Function:
my_estim <- function(df, formula, z_q, nobeta, model, testsplit = TRUE){
  
  if(testsplit){
    df_test = df %>% filter(test == TRUE)
    w_test = df_test$mult / sum(df_test$mult)
    
    df = df %>% filter(test == FALSE)
  } else {
    df_test = df
    w_test = df$mult / sum(df$mult)
  }

  w = df$mult / sum(df$mult)
  z = my_quantile(df$cpp, w, z_q)
  
  # Regression
  if(model == "ols"){
    reg = lm_robust(formula = formula, data = df, weights = w, se_type = "stata", clusters = region)
    betas = tidy(reg) %>% select(term, estimate, std.error) %>% 
      filter(!str_detect(term, "_fct|month|region|Intercept")) %>% 
      mutate(across(c(estimate, std.error), round, digits = 4),
             term = str_remove(term, "TRUE"))
    
    # Calculate targeting measures
    res = my_targetmeasures(y = df_test$cpp, z = z, w = w_test,
                            yhat = exp(predict(reg, newdata = df_test)),
                            zhat = my_quantile(exp(predict(reg, newdata = df_test)), w_test, z_q))
    
  }
  
  if(model == "forest"){
    # Random Forest does not incorporate survey weights (how?) nor clusters.
    reg = ranger(formula = formula, data = df)
    
    # Calculate targeting measures
    res = my_targetmeasures(y = df_test$cpp, z = z, w = w_test,
                            yhat = exp(predict(reg, data = df_test)$predictions),
                            zhat = my_quantile(exp(predict(reg, data = df_test)$predictions), w_test, z_q))
  }
  
  if(model == "lasso"){
    # LASSO does not incorporate survey weights (how?) nor clusters.
    reg = glmnetUtils::cv.glmnet(formula = formula, data = haven::zap_labels(df), alpha = 1)

    # Calculate targeting measures
    res = my_targetmeasures(y = df_test$cpp, z = z, w = w_test,
                            yhat = exp(predict(reg, newdata = haven::zap_labels(df_test))),
                            zhat = my_quantile(exp(predict(reg, newdata = haven::zap_labels(df_test))), w_test, z_q))
  }
  

  if(nobeta){return(res = res)}
  return(list(betas = betas, res = res))
}

# Basic Model
basicpmt <- paste("toilet_flush", "toilet_pit", "floor_finished", "wall_finished", 
                  "roof_finished", "fuel_electric_gas", "fuel_coal", "urban",
                  "head_age_fct", "head_female", "head_educ_primary", "head_educ_secondary", 
                  "head_divorced", "head_widow", "head_work_salary", "head_work_selfemployed",
                  "shares_f_5", "shares_m_5", "shares_f_14", "shares_m_14", "shares_f_65", "shares_m_65",
                  "hhsize_fct", "month", "region",
                  sep = " + ")
# Extended Model
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

# Huge Model possible by using ".^2" in formula, but takes forever and does not converge (?)

# Cross possible z values with model formulas
cross = cross2(list(z0.2 = 0.2, z0.4 =  0.4), list(basic = basicpmt, big = bigpmt)) %>% 
  setNames(c("z2basic", "z4basic", "z2big", "z4big"))

# Collect Results formula
my_collect <- function(data, cross){
  results = bind_rows(
    # OLS Estimation
    map_dfr(cross, ~my_estim(data, formula(paste("log(cpp) ~", .x[[2]])), .x[[1]], testsplit = TRUE, nobeta = TRUE, model = "ols")) %>% 
      mutate(z = rep(c(0.2, 0.4), 2), covariates = c("Basic", "Basic", "Extended", "Extended"), model = "OLS"),
    # Random Forest Estimation
    forest = map_dfr(cross, ~my_estim(data, formula(paste("log(cpp) ~", .x[[2]])), .x[[1]], testsplit = TRUE, nobeta = TRUE, model = "forest")) %>% 
      mutate(z = rep(c(0.2, 0.4), 2), covariates = c("Basic", "Basic", "Extended", "Extended"), model = "Random Forest"),
    # LASSO Estimation
    lasso = map_dfr(cross, ~my_estim(data, formula(paste("log(cpp) ~", .x[[2]])), .x[[1]], testsplit = TRUE, nobeta = TRUE, model = "lasso")) %>% 
      mutate(z = rep(c(0.2, 0.4), 2), covariates = c("Basic", "Basic", "Extended", "Extended"), model = "LASSO")
  )
  
  results_long = results %>% pivot_longer(names_to = "measure", cols = c(H, IER, EER, TER))
  return(results_long)
}

### Estimation ----
set.seed(1896)
index = sample(seq_len(nrow(data)), size = round(nrow(data)*0.5))
results = my_collect(data %>% mutate(test = row_number() %in% index), cross)

## Bootstrapping
plan(multisession)
nboot = 500

boot_index = map(1:nboot, ~sample(seq_len(nrow(data)), size = round(nrow(data)*0.5)))
boot_results = future_map_dfr(boot_index, ~my_collect(data %>% mutate(test = row_number() %in% .x), cross), .options = furrr_options(seed = 07062019))
boot_ci = boot_results %>% group_by(z, covariates, model, measure) %>% 
  summarise(ci_low = quantile(value, 0.05), ci_high = quantile(value, 0.95), .groups = "drop")

results = inner_join(results, boot_ci, by = c("z", "covariates", "model", "measure"))

### Save Results ----
# Targeting Measure: H, IER, EER, TER
# Poverty: at 0.2 and 0.4 (fixed line for H, IER, EER / fixed rate for TER)
# Covariates: basic, extended, (incl. polynomials+interactions)
# Model: OLS, LASSO, Random Forest
saveRDS(results, "Replication Brown, Ravallion, van de Walle/Data/prepared/results")
saveRDS(results, "Replication Brown, Ravallion, van de Walle/Data/prepared/boot_results")
