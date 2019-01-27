# Analysis 1
# Find: Mean change score = mean change restriction (avg 8-10) - mean change baseline (avg 3-7)
#       Set value = mean largest change during baseline
#       Compare change score between baseline and restriction

# Food Water Mean Change Score 

fw_an1 <- df %>%        # All days for analysis 
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

fw_a1_week1 <- fw_an1 %>%      #Week 1 
  select(
    SubNum,
    Group,
    D3_FoodWater_g,
    D4_FoodWater_g,
    D5_FoodWater_g,
    D6_FoodWater_g,
    D7_FoodWater_g
  )
# DONT RERUN without replacing new data  
dif_fw_a1_week1 <- combn(x = colnames(fw_a1_week1)[3:7],             # Subtracts every combination of day
      m = 2, 
      FUN = function(x) fw_a1_week1[[x[1]]] - fw_a1_week1[[x[2]]])

colnames(dif_fw_a1_week1) <- combn(colnames(fw_a1_week1)[3:7], 2, paste, collapse="_")

dif_fw_a1_week1 <- as.data.frame(dif_fw_a1_week1)   

# Wrote dif_fw_a1_week1 as .csv to extract values 
# furthest away from 0 in cmd line

mean_change_fw_week1 <- mean(dif_fw_a1_week1$greatest_change)           # Mean change in week 1


fw_a1_week2 <- fw_an1 %>%      #Week 2 
  select(
    SubNum,
    Group,
    D8_FoodWater_g,
    D9_FoodWater_g,
    D10_FoodWater_g
  )
#DONT RERUN without replacing new data 
dif_fw_a1_week2 <- combn(x = colnames(fw_a1_week2)[3:5],             # Subtracts every combination of day
                         m = 2, 
                         FUN = function(x) fw_a1_week2[[x[1]]] - fw_a1_week2[[x[2]]])

colnames(dif_fw_a1_week2) <- combn(colnames(fw_a1_week2)[3:5], 2, paste, collapse="_")

dif_fw_a1_week2 <- as.data.frame(dif_fw_a1_week2) 

# Wrote dif_fw_a1_week1 as .csv to extract values 
# furthest away from 0 in cmd line

mean_change_fw_week2 <- mean(dif_fw_a1_week2$largest_change)

# Change scores 

fw_change_score <- tibble(dif_fw_a1_week1$greatest_change, dif_fw_a1_week2$largest_change)

fw_change_score$change_score <- (dif_fw_a1_week2$largest_change - dif_fw_a1_week1$greatest_change)

mean_change_score <- mean(fw_change_score$change_score)

names(fw_change_score) <- c("week1", "week2", "change_score")


# Stats 

# Week 1 change vs Week 2 change 

week1_2_change_ttest <- t.test(dif_fw_a1_week1$greatest_change, dif_fw_a1_week2$largest_change)

week1_v_avg_change_score <- t.test(fw_change_score$change_score, fw_change_score$week1)

week2_v_avg_change_score <- t.test(fw_change_score$change_score, fw_change_score$week2)

week1_v_setvalue <- t.test(fw_change_score$week1, alternative = "two.sided", mu = 108.7384)

week2_v_setvalue <- t.test(fw_change_score$week2, alternative = "two.sided", mu = 108.7384)

sv_v_avg_change_sc <- t.test(fw_change_score$change_score, alternative = "two.sided", mu = 108.7384)


