# Obesity analysis 

ob_df <- df %>% 
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
    BMI_Cat_BL,
    BodyFat_Cat_BL,
    WK1.iPAQ.Activity.Level,
    Gender,
    Height_BL,
    Age,
    BMI_BL,
    V1_Bmass_BL,
    TotalP_Fat_BL,
    W_C_Risk_BL,
    WtoHratio_BL
  ) %>%
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120")
  ) %>%
  na.omit()

ob_df$foodwater1_mean <- rowMeans(ob_df[,3:7])

ob_df$foodwater2_mean <- rowMeans(ob_df[,8:10])

ob_df$fluidwater1_mean <- rowMeans(ob_df[,11:15])

ob_df$fluidwater2_mean <- rowMeans(ob_df[,16:18])

ob_df$totalwater1_mean <- rowMeans(ob_df[,19:23])

ob_df$totalwater2_mean <- rowMeans(ob_df[,24:26])

ob_df$FWIC <- (ob_df$foodwater2_mean - ob_df$foodwater1_mean)

ob_df$WK1.iPAQ.Activity.Level[ob_df$WK1.iPAQ.Activity.Level == ""] <- NA


ob_df <- ob_df %>%
  mutate(BMI_new = ifelse(
    BMI_Cat_BL == "Normal", 'Normal',
    ifelse(BMI_Cat_BL == "Obesity I", 'Obese',
           ifelse(BMI_Cat_BL == "Obesity II", 'Obese',
                  ifelse(BMI_Cat_BL == "Obesity III", 'Obese',
                         ifelse(BMI_Cat_BL == "Overweight", 'Overweight',
                                ifelse(BMI_Cat_BL == "Underweight", 'Underweight', NA)))))),
    BF_new = ifelse(
      BodyFat_Cat_BL == "Very Poor: ", 'Very Poor',
      ifelse(BodyFat_Cat_BL == "Very Lean", 'Excellent',
             ifelse(BodyFat_Cat_BL == "Poor", 'Poor',
                    ifelse(BodyFat_Cat_BL == "Good", 'Good',
                           ifelse(BodyFat_Cat_BL == "Fair", 'Fair',
                                  ifelse(BodyFat_Cat_BL == "Excellent", 'Excellent', 
                                         ifelse(BodyFat_Cat_BL == "Very Poor", 'Very Poor', NA))))))),
    BF_quan = ifelse(
      TotalP_Fat_BL <= 21.6, '1st',
      ifelse(TotalP_Fat_BL > 21.6 & TotalP_Fat_BL <= 28.1, '2nd',
             ifelse(TotalP_Fat_BL > 28.1 & TotalP_Fat_BL <= 34.7, '3rd',
                    ifelse(TotalP_Fat_BL > 34.7, '4th', NA))))
      
    )

ob_df$gender_mix <- with(ob_df, paste(Gender, BF_new))



# Dems for abstract 
mean(ob_df$FWIC)

sd(ob_df$FWIC)

total_num <- nrow(ob_df)

dem_ob <- table(ob_df$Gender)

height_mean_ob <- mean(ob_df$Height_BL)

height_sd_ob <- sd(ob_df$Height_BL)

weight_mean_ob <- mean(ob_df$V1_Bmass_BL)

weight_sd_ob <- sd(ob_df$V1_Bmass_BL)

age_mean <- mean(ob_df$Age)

age_sd <- sd(ob_df$Age)

BMI_cat_table <- table(ob_df$BMI_Cat_BL)

BF_cat_table <- table(ob_df$BF_new)

BMI_avg <- mean(ob_df$BMI_BL)

BMI__sd <- sd(ob_df$BMI_BL)

BF_avg <- mean(ob_df$TotalP_Fat_BL)

BF_sd <- sd(ob_df$TotalP_Fat_BL)

ob_df$BMI_new <- factor(ob_df$BMI_new, levels = c("Underweight", "Normal", "Overweight", "Obese"))

ob_df$BF_new  <- factor(ob_df$BF_new, levels = c("Excellent", "Good", "Fair", "Poor", "Very Poor"))

# Plots 

ob_FWIC_plot <- ggplot(
  data = ob_df,
  aes(
    x = BMI_new,
    y = FWIC
  )
) +  
  geom_hline(
    yintercept = 0,
    linetype= 2,
    color = "#333333"
  ) + 
  stat_boxplot(
    geom = "errorbar",
    size = .3
  ) + 
  geom_boxplot(
  
  ) +
  labs(
    x = "BMI Category",
    y = "Change in Mean Food Water Intake (mL)",
    title = "BMI and Change in Food Water Consumption"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "snow"
    )
  )

ob_FWIC_plot

bf_fwic_plot <- ggplot(
  data = subset(
    ob_df,
    !is.na(BF_new)),
  aes(
    x = BF_new,
    y = FWIC
    )
  ) + 
    geom_hline(
      yintercept = 0,
      linetype= 2,
      color = "#333333"
  ) + 
    stat_boxplot(
      geom = "errorbar",
      size = .3
  ) +
  geom_boxplot(
    
  ) +
  labs(
    x = "Body Fat Category",
    y = "Change in Mean Food Water Intake (mL)",
    title = "Body Fat and Change in Food Water Consumption"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "snow"
    )
  )

bf_fwic_plot

##  Stats 

# BMI 
ob_v_fwic <- aov(FWIC ~ BMI_new, data = ob_df)

summary(ob_v_fwic)

ob_v_fwic_tuk <- TukeyHSD(x = ob_v_fwic, conf.level = 0.95)

ob_v_fwic_tuk_df <- as.data.frame(ob_v_fwic_tuk$BMI_new)

write.csv(ob_v_fwic_tuk_df, file = "ob_v_fwic_tuk_df1.csv")

bmi_fit <- lm(FWIC ~ BMI_BL, data = ob_df)

summary(bmi_fit)

# Body Fat 

bf_fwic <- aov(FWIC ~ BF_new, data = ob_df)

summary(bf_fwic)

bf_fwic_tuk <- TukeyHSD(x = bf_fwic, conf.level = 0.95)

bf_fwic_tuk_df <- bf_fwic_tuk$BF_new

write.csv(bf_fwic_tuk_df, file = "bf_fwic_tuk_df.csv")

# More BF% 

bf_quan_fit <- lm(FWIC ~ BF_quan, data = ob_df)

summary(bf_quan_fit)

bf_cat_fit <- lm(FWIC ~ BF_new, data = ob_df)

summary(bf_cat_fit)

bf_gender_fit <- lm(FWIC ~ Gender, data = ob_df)

summary(bf_gender_fit)

bf_raw_fit <- lm(FWIC ~ TotalP_Fat_BL, data = ob_df)

summary(bf_raw_fit)

# HW Ratio 

hip_FWIC_aov <- aov(FWIC ~ W_C_Risk_BL, data = ob_df)

summary(hip_FWIC_aov)

h2w_FWIC_fit <- lm(FWIC ~ WtoHratio_BL, data = ob_df)

summary(h2w_FWIC_fit)

