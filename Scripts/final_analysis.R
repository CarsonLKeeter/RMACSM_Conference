# "Final" Analysis 

# Analysis 1: 

fw_final <- df %>%        # All days for analysis 
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
    D11_FoodWater_g
  ) %>% 
  na.omit()

fw_mean_intake <- fw_final %>% 
  mutate(week1avg = (D3_FoodWater_g + D4_FoodWater_g + D5_FoodWater_g + D6_FoodWater_g + D7_FoodWater_g)/ 5) %>%
  mutate(week2avg = (D8_FoodWater_g + D9_FoodWater_g + D10_FoodWater_g)/3) %>% 
  mutate(FWIC = week2avg - week1avg)

fw_mean_intake_df <- fw_mean_intake %>%
  select(
    SubNum,
    Group,
    week1avg,
    week2avg
  ) %>% 
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120"))

fw_mean_intake_df_plot <- gather(fw_mean_intake_df, visit, grams, week1avg:week2avg)

FWIC_mean <- mean(fw_mean_intake$FWIC)

# Analysis 1: 

FWIC_v_setvalue_cons_twosided <- t.test(fw_mean_intake$FWIC, alternative = "two.sided", mu = 108.7384) #see analysis 1, mu = mean(dif_fw_a1_week1$greatest_change)

FWIC_v_setvalue_cons_less <- t.test(fw_mean_intake$FWIC, alternative = "less", mu = 108.7384)

FWIC_v_setvalue_cons_greater <- t.test(fw_mean_intake$FWIC, alternative = "greater", mu = 108.7384)

# Analysis 2: 

FWIC_v_setvalue_lib_twosided <- t.test(fw_mean_intake$FWIC, alternative = "two.sided", mu = 27.14374) # see analysis 2, mu = mean(seq_dif_week1$mean_dif)

FWIC_v_setvalue_lib_less <- t.test(fw_mean_intake$FWIC, alternative = "less", mu = 27.14374)

FWIC_v_setvalue_lib_greater <- t.test(fw_mean_intake$FWIC, alternative = "greater", mu = 27.14374)

# Just to be sure (JTBS) analysis: 

cons_lib <- t.test(dif_fw_a1_week1$greatest_change, seq_dif_week1$mean_dif, paired = TRUE)

JTBS_FWIC_greatest_dif <- t.test(fw_mean_intake$FWIC, dif_fw_a1_week1$greatest_change, paired = TRUE)

JTBS_FWIC_seq_dif <- t.test(fw_mean_intake$FWIC, seq_dif_week1$mean_dif, paired = TRUE)

JTBS_bl_res_bygroup <- ggplot(
  data = fw_mean_intake_df_plot,
  aes(
    x = visit, 
    y = grams,
    fill = Group 
  )
) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(
      width = .9
    )
  ) + 
  labs(
    x = NULL,
    y = "Intake (g)",
    title = "Average Food Water Intake"
  ) + 
  scale_x_discrete(
    labels = c("Baseline", "Restriction")
  )
