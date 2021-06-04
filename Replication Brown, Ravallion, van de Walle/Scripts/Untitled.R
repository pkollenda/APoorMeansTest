mymean <- function(data, var) {
  data <- inner_join(data, households[c("HHID", "mult")], by = "HHID")
  svy <- svydesign(ids = ~1, weights = ~mult, data = data)
  
  means <- svymean(formula(paste("~", var)), design = svy) %>% round(digits = 3)
  print(means)
}
