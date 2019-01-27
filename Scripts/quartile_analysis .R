# Tertile Analysis

tert_df <- df %>% 
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
    D10_TotalWater_g
  ) %>%
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120")
         ) %>%
  na.omit() 

tert_df$foodwater1_mean <- rowMeans(tert_df[,3:7])

tert_df$foodwater2_mean <- rowMeans(tert_df[,8:10])

tert_df$fluidwater1_mean <- rowMeans(tert_df[,11:15])

tert_df$fluidwater2_mean <- rowMeans(tert_df[,16:18])

tert_df$totalwater1_mean <- rowMeans(tert_df[,19:23])

tert_df$totalwater2_mean <- rowMeans(tert_df[,24:26])

tert_df$FWIC <- (tert_df$foodwater2_mean - tert_df$foodwater1_mean)

tert_df <- tert_df %>%
  mutate(quan_fluid = ifelse(
    fluidwater1_mean <= 1951.8616, '1st',
      ifelse(fluidwater1_mean > 1951.8616 & fluidwater1_mean < 2474.4623, '2nd',
        ifelse(fluidwater1_mean >= 2474.4623 & fluidwater1_mean < 3076.4222, '3rd',
          ifelse(fluidwater1_mean >= 3076.4222, '4th', NA)))),
        quan_total = ifelse(
    totalwater1_mean <= 2541.232, '1st',
      ifelse(totalwater1_mean > 2541.232 & totalwater1_mean < 3154.135, '2nd',
        ifelse(totalwater1_mean >= 3154.135 & totalwater1_mean < 3857.757, '3rd',
          ifelse(totalwater1_mean >= 3857.757, '4th', NA )))))
 

fw_tert <- tert_df %>%       # Food Water
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
    quan_total,
    quan_fluid
  )

fw_tert_gathered <- gather(fw_tert, visit, grams, D3_FoodWater_g:D10_FoodWater_g)   

# Plots 

total_water_boxplot <- ggplot(
  data = tert_df,
  aes(
    x = quan_total,
    y = foodwater2_mean
  )
) +
  geom_boxplot(

  ) +
  labs(
    x = "Baseline Total Water Intake Quartile",
    y = "Restriction Food Water Intake",
    title = "Baseline Total Water Intake"
  )

fluid_water_boxplot <- ggplot(
  data = tert_df,
  aes(
    x = quan_fluid,
    y = foodwater2_mean
  )
) +
  geom_boxplot(
    
  ) +
  labs(
    x = "Baseline Fluid Water Intake Quartile",
    y = "Restriction Food Water Intake",
    title = "Baseline Fluid Water Intake"
  )
# Stats 

FWIC_v_totalfluid <- aov(FWIC ~ quan_total, data = tert_df)

summary(FWIC_v_totalfluid)

FWIC_v_fluidwater <- aov(FWIC ~ quan_fluid, data = tert_df)

summary(FWIC_v_fluidwater)

thsd_totalfluid_quart <- TukeyHSD(restriction_v_totalfluid, conf.level = 0.95)

thsd_totalfluid_quart_df <- as.data.frame(thsd_totalfluid_quart$quan_total)

write.csv(x = thsd_totalfluid_quart_df, file = "thsd_totalfluid_quart_df.csv" )

thsd_fluidintake_quart <- TukeyHSD(restriction_v_fluidwater, conf.level = 0.95)

thsd_fluidintake_quart_df <- as.data.frame(thsd_fluidintake_quart$quan_fluid)

write.csv(thsd_fluidintake_quart_df, file = "thsd_fluidintake_quart_df.csv")
