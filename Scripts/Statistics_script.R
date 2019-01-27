# Fluid Water ANOVA

fluidwater_model <- lm(grams ~ visit, data = fluidwater_df)

fluidwater_aov <- anova(fluidwater_model)

# Food Water ANOVA

foodwater_model <- lm(grams ~ factor(visit), data = foodwater_df)

foodwater_aov <- anova(foodwater_model)

## Tukey Significance Testing

# Food Water Post Hoc Analysis
foodwater_posthoc <- TukeyHSD(foodwater_aov, conf.level = 0.95)

foodwater_posthoc_frame <- as.data.frame(foodwater_posthoc$`factor(visit)`)

write.csv(foodwater_posthoc_frame, 'foodwater_posthoc.csv')

# Fluid Water Post Hoc Analysis
fluidwater_posthoc <- TukeyHSD(fluidwater_aov, conf.level = 0.95)

fluidwater_posthoc_frame <- as.data.frame(fluidwater_posthoc$visit)

write.csv(fluidwater_posthoc_frame, 'fluidwater_posthoc.csv')

# Cleaning up Tukey Tables (love that alliteration)

foodwater_sign_visit_df <- foodwater_posthoc_data %>%
  filter(p.adj <= 0.05)

write.csv(foodwater_sign_visit_df, 'foodwater_Tukey_sign.csv')

fluidwater_sign_visit_df <- fluidwater_posthoc_data %>% 
  filter(p.adj <= 0.05)

write.csv(fluidwater_sign_visit_df, 'fluidwater_Tukey_sign.csv')

# Coefficient of Variation tests 
foodwater_cv_df <- foodwater_df %>%
  select(
    SubNum, 
    grams, 
    visit,
    visit_num
  ) %>% 
  na.omit()

foodwater_cv <- with(foodwater_cv_df, asymptotic_test(grams, visit))

foodwater_cv_MSLRT <- with(foodwater_cv_df, mslr_test(
  nr = 1e4,
  grams,
  visit 
))
