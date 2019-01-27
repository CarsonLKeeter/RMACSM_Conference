# Script for ACSM poster figures 

# Box Plots: 

ob_FWIC_plot   # plotted in 'Obesity_v_FWIC.R'

bf_fwic_plot   # ""


# Dataframes for Line Plots: 

ob_df_gathered <- df %>% 
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
    BMI_Cat_BL,
    BodyFat_Cat_BL,
    WK1.iPAQ.Activity.Level,
    Gender,
    Height_BL,
    Age,
    BMI_BL,
    V1_Bmass_BL,
    TotalP_Fat_BL
  ) %>%
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120")
  ) %>%
  na.omit() %>%
  gather(visit, grams, D3_FoodWater_g:D10_FoodWater_g) 

ob_df_gathered <- ob_df_gathered %>%
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
                                         ifelse(BodyFat_Cat_BL == "Very Poor", 'Very Poor', NA))))))))

ob_df_gathered$visit <- parse_number(ob_df_gathered$visit)
    
ob_df_gathered <- ob_df_gathered %>%
  na.omit()

ob_df_gathered$BMI_new <- factor(ob_df_gathered$BMI_new, levels = c("Underweight", "Normal", "Overweight", "Obese"))

ob_df_gathered$BF_new <- factor(ob_df_gathered$BF_new, levels = c("Excellent", "Good", "Fair", "Poor", "Very Poor"))


# Line Plots: 

bf_line <- ggplot(
  data = ob_df_gathered,
  aes(
    x = visit,
    y = grams,
    color = BF_new
  )
) + 
  geom_smooth(
    alpha = .1
  ) + 
  geom_vline(
    xintercept = 7,
    linetype = 2,
    color = "grey"
  ) +
  geom_text(
    aes(
      x = 5,
      y = 1050,
      label = "\nBL"
    ),
    color = "black",
    size = 5
  ) +
  geom_text(
    aes(
      x = 9,
      y = 1050,
      label = "\nRestr."
    ),
    color = "black",
    size = 5
  ) + 
  scale_x_discrete(
    limits = c(3:10)
  ) + 
  labs(
    x = "Day",
    y = "Food Water Intake (mL)",
    title = "Body Fat and Food Water Consumption"
  ) + 
  scale_color_discrete(
    name = "Body Fat % Cat."
  ) + 
  theme(
    panel.background = element_rect(
      fill = "snow"
    )
  )

bf_line 


bmi_line <- ggplot(
  data = ob_df_gathered,
  aes(
    x = visit,
    y = grams,
    color = BMI_new
  )
) + 
  geom_smooth(
    alpha = .1
  ) + 
  geom_vline(
    xintercept = 7,
    linetype = 2,
    color = "grey"
  ) +
  geom_text(
    aes(
      x = 5,
      y = 1050,
      label = "\nBL"
    ),
    color = "black",
    size = 5
  ) +
  geom_text(
    aes(
      x = 9,
      y = 1050,
      label = "\nRestr."
    ),
    color = "black",
    size = 5
  ) + 
  scale_x_discrete(
    limits = c(3:10)
  ) + 
  labs(
    x = "Day",
    y = "Food Water Intake (mL)",
    title = "BMI and Food Water Consumption"
  ) + 
  scale_color_discrete(
    name = "BMI Cat."
  ) + 
  theme(
    panel.background = element_rect(
      fill = "snow"
    )
  )

bmi_line

grid.arrange(ob_FWIC_plot, bf_fwic_plot, bmi_line, bf_line)
