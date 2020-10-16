read_csv("national.csv")
setwd("/Users/clintrooker/Desktop/PS 811/ps811-exercises/misc")
national <- read.csv("national.csv")
View(national)


#1 

columns_loop <- numeric()

for (row in 1:nrow(national)) {
  columns_loop[row] <- length(national[row, ])
}

columns_loop


#2 
tapply(
  X <- national$christianity_protestant,
  INDEX <- list(national$state), 
  FUN = mean, 
  na.rm = TRUE  
)

#2 using tidyverse
national %>%
  group_by(state) %>%
  summarize(
    mean_nom = mean(christianity_protestant, na.rm = TRUE)
  ) 

#3 
sapply(national, class)

#4
log(national$buddhism_all)

#5
years <- national %>%
  mutate_at(
    .vars = vars(christianity_all),
    .funs = ~ . > 300000     #True if christianity all is > 300000
  )
years2 <- ifelse(years$christianity_all == TRUE, c(years$year), FALSE) 
#if true, the year is printed in a vector. How do I combine these/make them
#cleaner and more straightforward?
View(years2)
View(years)

#6
nested_code <- national %>%
  group_by(state) %>%
  nest() %>%
  print()


model$national

#7
nested_models <- nested_code %>%
  mutate(
    model = map(
      .x = data, 
      .f = ~ lm(dual_religion ~ judaism_percent, data = .x)
    )
  ) %>%
  print()

#8 
nested_coefs <- nested_models %>%
  mutate(coefs = map(model, coefficients)) %>%
  print()

#9
nested_coefs$coefs

#10
nested_coefs %>% pull(model)


