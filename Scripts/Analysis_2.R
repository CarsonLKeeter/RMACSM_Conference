# Analysis 2 

# Is the change observed differently from the average daily fluctuation? 
# Find: Set value = Average daily difference between consecutive for all subjects
# Compare: change score between baseline and fluid restriction. 

fw_an2 <- df %>%        # All days for analysis 
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

fw_a2_week1 <- fw_an2 %>%      #Week 1 
  select(
    SubNum,
    Group,
    D3_FoodWater_g,
    D4_FoodWater_g,
    D5_FoodWater_g,
    D6_FoodWater_g,
    D7_FoodWater_g
  )

seq_dif_week1 <- fw_a2_week1 %>%                    #Subtracts sequentially and finds mean  
  mutate(d3_d4 = D3_FoodWater_g - D4_FoodWater_g,
         d4_d5 = D4_FoodWater_g - D5_FoodWater_g,
         d5_d6 = D5_FoodWater_g - D6_FoodWater_g,
         d6_d7 = D6_FoodWater_g - D7_FoodWater_g) %>% 
  mutate(mean_dif = (d3_d4 + d4_d5+ d5_d6+ d6_d7)/4)

mean_avg_dif_week1 <- mean(seq_dif_week1$mean_dif)


fw_a2_week2 <- fw_an2 %>%                         # Week 2 
  select(
    SubNum,
    Group,
    D8_FoodWater_g,
    D9_FoodWater_g,
    D10_FoodWater_g
  )

seq_dif_week2 <- fw_a2_week2 %>%                    #Subtracts sequentially and finds mean  
  mutate(d8_d9 = D8_FoodWater_g - D9_FoodWater_g,
         d9_d10 = D9_FoodWater_g - D10_FoodWater_g) %>%
  mutate(mean_dif = (d8_d9 + d9_d10)/2)

mean_avg_dif_week2 <- mean(seq_dif_week2$mean_dif)

# Stats 

seq_dif_ttest <- t.test(seq_dif_week1$mean_dif, seq_dif_week2$mean_dif) # Compare: distributions of both seq dif

seq_v_change_score_week1 <- t.test(seq_dif_week1$mean_dif, dif_fw_a1_week1$greatest_change)

seq_v_change_score_week2 <- t.test(seq_dif_week2$mean_dif, dif_fw_a1_week2$largest_change)
