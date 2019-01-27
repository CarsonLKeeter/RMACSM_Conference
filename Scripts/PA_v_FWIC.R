# PA Analysis

pa_df <- df %>% 
  select(
    SubNum,
    Group,
    D3_FoodWater_g,
    D4_FoodWater_g,
    D5_FoodWater_g,
    D6_FoodWater_g,
    D7_FoodWater_g,
    D8_FoodWater_g,
    D9_FoodWater_g,
    D10_FoodWater_g, 
    D3_FluidWater_g,
    D4_FluidWater_g,
    D5_FluidWater_g,
    D6_FluidWater_g,
    D7_FluidWater_g,
    D8_FluidWater_g,
    D9_FluidWater_g,
    D10_FluidWater_g,
    D3_TotalWater_g,
    D4_TotalWater_g,
    D5_TotalWater_g,
    D6_TotalWater_g,
    D7_TotalWater_g,
    D8_TotalWater_g,
    D9_TotalWater_g,
    D10_TotalWater_g,
    WK1.iPAQ.Activity.Level
  ) %>%
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120")
  ) %>%
  na.omit()

pa_df$foodwater1_mean <- rowMeans(pa_df[,3:7])

pa_df$foodwater2_mean <- rowMeans(pa_df[,8:10])

pa_df$fluidwater1_mean <- rowMeans(pa_df[,11:15])

pa_df$fluidwater2_mean <- rowMeans(pa_df[,16:18])

pa_df$totalwater1_mean <- rowMeans(pa_df[,19:23])

pa_df$totalwater2_mean <- rowMeans(pa_df[,24:26])

pa_df$FWIC <- (pa_df$foodwater2_mean - pa_df$foodwater1_mean)

pa_df$WK1.iPAQ.Activity.Level[pa_df$WK1.iPAQ.Activity.Level == ""] <- NA

pa_df <- pa_df %>% na.omit()

# Plots 

ipaq_FWIC <- ggplot(
  data = pa_df,
  aes(
    x = WK1.iPAQ.Activity.Level,
    y = FWIC
  )
) +
  geom_boxplot(
    
  ) +
  labs(
    x = "Physical Activity Level",
    y = "Change in Mean Food Water Intake",
    title = "PA Level and FWIC"
  )

# Stats 

pa_vFWIC <- aov(FWIC ~ WK1.iPAQ.Activity.Level, data = pa_df)

summary(pa_vFWIC)
