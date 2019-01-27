# Food Water differences 

fw <- df %>% 
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

new_foodwater <- combn(x = colnames(fw)[3:7], 
                       m = 2, 
                       FUN = function(x) fw[[x[1]]] - fw[[x[2]]])

colnames(new_foodwater) <- combn(colnames(fw)[3:7], 2, paste, collapse="_")

new_foodwater <- as.data.frame(new_foodwater)

new_foodwater <- abs(new_foodwater)

weekone_fw_fluc <- new_foodwater_max$fluc

sd_week1_fw_fluc <- sd(weekone_fw_fluc)

fw_week2 <- df %>% 
  select(
    SubNum,
    Group,
    D8_FoodWater_g,
    D9_FoodWater_g,
    D10_FoodWater_g
  ) %>% 
  na.omit() 

fw_week2_fluc <- combn(x = colnames(fw_week2)[3:5], 
                       m = 2, 
                       FUN = function(x) fw[[x[1]]] - fw[[x[2]]])

colnames(fw_week2_fluc) <- combn(colnames(fw_week2)[3:5], 2, paste, collapse="_")

fw_week2_fluc <- as.data.frame(fw_week2_fluc)

fw_week2_fluc$mean <- rowMeans(fw_week2_fluc[,1:3])

sd_fw_week2 <- sd(fw_week2$mean)

# Fluid Water Differences 
flw <- df %>% 
  select(
    SubNum,
    Group,
    D3_FluidWater_g,
    D4_FluidWater_g,
    D5_FluidWater_g,
    D6_FluidWater_g,
    D7_FluidWater_g,
    D8_FluidWater_g,
    D9_FluidWater_g,
    D10_FluidWater_g,
    D11_FluidWater_g
  ) %>% 
  na.omit()

new_fluidwater <- combn(x = colnames(flw)[3:7], 
                       m = 2, 
                       FUN = function(x) flw[[x[1]]] - flw[[x[2]]])

colnames(new_fluidwater) <- combn(colnames(flw)[3:7], 2, paste, collapse="_")

new_fluidwater <- as.data.frame(new_fluidwater)

new_fluidwater <- abs(new_fluidwater)

weekone_flw_fluc <- new_fluidwater_max$fluc

sd_flw_fluc <- sd(weekone_flw_fluc)

flw_week2 <- df %>% 
  select(
    SubNum,
    Group,
    D8_FluidWater_g,
    D9_FluidWater_g,
    D10_FluidWater_g
  ) %>% 
  na.omit()

flw_week2_fluc <- combn(x = colnames(flw_week2)[3:5], 
                       m = 2, 
                       FUN = function(x) flw[[x[1]]] - flw[[x[2]]])

colnames(flw_week2_fluc) <- combn(colnames(flw_week2)[3:5], 2, paste, collapse="_")

flw_week2_fluc <- as.data.frame(flw_week2_fluc)

flw_week2_fluc$mean <- rowMeans(flw_week2_fluc[,1:3])

sd_flw_week2 <- sd(flw_week2$mean)

# Stats 

foodwater_week_ttest <- t.test(weekone_fw_fluc, fw_week2$mean)

fluidwater_week_ttest <- t.test(weekone_flw_fluc, flw_week2$mean) 
