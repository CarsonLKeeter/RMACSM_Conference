# Required packages

library(tidyverse)
library(gridExtra)
library(tibble)
library(knitr)
library(cvequality)
library(ggsignif)
library(ggpubr)

# Food Water 

visit_num_foodwater <- quick_fix$visit_num

foodwater_df <- df %>% 
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

foodwater_df <- gather(foodwater_df, visit, grams, D3_FoodWater_g:D11_FoodWater_g)

foodwater_df$visit_num <- visit_num_foodwater

foodwater_df <- foodwater_df %>% 
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120"))

food_water_plot <- ggplot(
  data = foodwater_df,
  aes(
    x = visit_num,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    method = loess,
    aes(
      x = visit_num,
      y = grams
  ),
  se=TRUE,
  alpha = .2,
  size = .5
) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Water in Food Consumption",
    x = "Visit",
    y = "Grams"
  ) +
  geom_jitter(
    alpha = .5
  )

food_water_plot

# Fluid Water

visit_num_fluid <- fluidwater_num$visit_num

fluidwater_df <- df %>% 
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
  
fluidwater_spread_df <- df %>% 
  select(
    SubNum,
    Group,
    visit_num,
    visit,
    D3_FluidWater_g,
    D4_FluidWater_g,
    D5_FluidWater_g,
    D6_FluidWater_g,
    D7_FluidWater_g,
    D8_FluidWater_g,
    D9_FluidWater_g,
    D10_FluidWater_g,
    D11_FluidWater_g
  )%>% 
  na.omit()

fluidwater_df <- gather(fluidwater_df, visit, grams, D3_FluidWater_g:D11_FluidWater_g)

fluidwater_df$visit_num <- visit_num_fluid

fluidwater_df <- fluidwater_df %>% 
  filter(!SubNum %in% c("13","37","39","41","58","93","119","120"))

fluid_water_plot <- ggplot(
  data = fluidwater_df,
  aes(
    x = visit_num,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    method = "loess",
    se=TRUE,
    alpha = .2,
    formula = y ~ x,
    size = .5
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Fluid Water Consumption",
    x = "Visit",
    y = "Grams"
  ) + 
  geom_jitter(
    alpha = .5
    )

fluid_water_plot

# Fluid Carb

fluidcarbs_df <- df %>%
  select(
    SubNum,
    Group,
    D3_fluid_Carbs_g,
    D4_fluid_Carbs_g,
    D5_fluid_Carbs_g,
    D6_fluid_Carbs_g,
    D7_fluid_Carbs_g,
    D8_fluid_Carbs_g,
    D9_fluid_Carbs_g,
    D10_fluid_Carbs_g,
    D11_fluid_Carbs_g
  ) %>% 
  na.omit()

fluidcarbs_df <- gather(fluidcarbs_df, visit, grams, D3_fluid_Carbs_g:D11_fluid_Carbs_g)  

fluidcarbs_df$visit_num <- visit_num

fluid_carb_plot <- ggplot(
  data = fluidcarbs_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = fluidcarbs_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2,
    size = .5
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Fluid Carbohydrate Consumption",
    x = "Visit",
    y = "Grams"
  )

fluidfat_df <- df %>%
  select(
    SubNum,
    Group,
    D3_fluid_Fat_g,
    D4_fluid_Fat_g,
    D5_fluid_Fat_g,
    D6_fluid_Fat_g,
    D7_fluid_Fat_g,
    D8_fluid_Fat_g,
    D9_fluid_Fat_g,
    D10_fluid_Fat_g,
    D11_fluid_Fat_g
  ) %>%
  na.omit()

fluidfat_df <- gather(fluidfat_df, visit, grams, D3_fluid_Fat_g:D11_fluid_Fat_g)

fluidfat_df$visit_num <- visit_num

fluid_fat_plot <- ggplot(
  data = fluidfat_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = fluidfat_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Fluid Fat Consumption",
    x = "Visit",
    y = "Grams"
  )

fluidpro_df <- df %>%
  select(
    SubNum,
    Group,
    D3_fluid_Protein_g,
    D4_fluid_Protein_g,
    D5_fluid_Protein_g,
    D6_fluid_Protein_g,
    D7_fluid_Protein_g,
    D8_fluid_Protein_g,
    D9_fluid_Protein_g,
    D10_fluid_Protein_g,
    D11_fluid_Protein_g
  ) %>%
  na.omit()

fluidpro_df <- gather(fluidpro_df, visit, grams, D3_fluid_Protein_g:D11_fluid_Protein_g)

fluid_pro_plot <- ggplot(
  data = fluidpro_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = fluidpro_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Fluid Protein Consumption",
    x = "Visit",
    y = "Grams"
  )

foodprotein_df <- df %>% 
  select( 
    SubNum,
    Group,
    D3_PRO_g,
    D4_PRO_g,
    D5_PRO_g,
    D6_PRO_g,
    D7_PRO_g,
    D8_PRO_g,
    D9_PRO_g,
    D10_PRO_g,
    D11_PRO_g
    ) %>%
  na.omit()

foodprotein_df <- gather(foodprotein_df, visit, grams, D3_PRO_g:D11_PRO_g)

foodprotein_df$visit_num <- visit_num

food_pro_plot <- ggplot(
  data = foodprotein_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = foodprotein_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Food Protein Consumption",
    x = "Visit",
    y = "Grams"
  )

foodfat_df <-df %>% 
  select(
    SubNum,
    Group,
    D3_Fat_g,
    D4_Fat_g,
    D5_Fat_g,
    D6_Fat_g,
    D7_Fat_g,
    D8_Fat_g,
    D9_Fat_g,
    D10_Fat_g,
    D11_Fat_g
  ) %>%
  na.omit()

foodfat_df <- gather(foodfat_df, visit, grams, D3_Fat_g:D11_Fat_g)

foodfat_df$visit_num <- visit_num

food_fat_plot <- ggplot(
  data = foodfat_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = foodfat_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Food Fat Consumption",
    x = "Visit",
    y = "Grams"
  )

foodcarbs_df <- df %>%
  select(
    SubNum,
    Group,
    D3_CHO_g,
    D4_CHO_g,
    D5_CHO_g,
    D6_CHO_g,
    D7_CHO_g,
    D8_CHO_g,
    D9_CHO_g,
    D10_CHO_g,
    D11_CHO_g
  ) %>% 
  na.omit()

foodcarbs_df <- gather(foodcarbs_df, visit, grams, D3_CHO_g:D11_CHO_g)

foodcarbs_df$visit_num <- visit_num

food_carb_plot <- ggplot(
  data = foodcarbs_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = foodcarbs_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Food Carbohydrate Consumption",
    x = "Visit",
    y = "Grams"
  )

totalfood_df <- df %>%
  select(
    SubNum,
    Group,
    D3_CHO_g,
    D4_CHO_g,
    D5_CHO_g,
    D6_CHO_g,
    D7_CHO_g,
    D8_CHO_g,
    D9_CHO_g,
    D10_CHO_g,
    D11_CHO_g,
    D2_Fat_g,
    D3_Fat_g,
    D4_Fat_g,
    D5_Fat_g,
    D6_Fat_g,
    D7_Fat_g,
    D8_Fat_g,
    D9_Fat_g,
    D10_Fat_g,
    D11_Fat_g,
    D2_PRO_g,
    D3_PRO_g,
    D4_PRO_g,
    D5_PRO_g,
    D6_PRO_g,
    D7_PRO_g,
    D8_PRO_g,
    D9_PRO_g,
    D10_PRO_g,
    D11_PRO_g
    ) %>%
  mutate(
    totalD2 = D2_CHO_g + D2_Fat_g + D2_PRO_g,
    totalD3 = D3_CHO_g + D3_Fat_g + D3_PRO_g,
    totalD4 = D4_CHO_g + D4_Fat_g + D4_PRO_g,
    totalD5 = D5_CHO_g + D5_Fat_g + D5_PRO_g,
    totalD6 = D6_CHO_g + D6_Fat_g + D6_PRO_g,
    totalD7 = D7_CHO_g + D7_Fat_g + D7_PRO_g,
    totalD8 = D8_CHO_g + D8_Fat_g + D8_PRO_g,
    totalD9 = D9_CHO_g + D9_Fat_g + D9_PRO_g,
    totalD10 = D10_CHO_g + D10_Fat_g + D10_PRO_g,
    totalD11 = D11_CHO_g + D11_Fat_g + D11_PRO_g
    ) %>%
  na.omit()

totalfood_df <- gather(totalfood_df, visit, grams, totalD3:totalD11)

totalfood_df$visit_num <- visit_num

total_food_plot <- ggplot(
  data = totalfood_df,
  aes(
    x = visit_num$`foodwater_df$visit_num`,
    y = grams,
    color = Group
  )
) + 
  geom_smooth(
    data = totalfood_df,
    method = loess,
    aes(
      x = visit_num$`foodwater_df$visit_num`,
      y = grams
    ),
    se=TRUE,
    alpha = .2
  ) +
  scale_x_discrete(
    limits = c(3:11)
  ) +
  labs(
    title = "Total Food Consumption",
    x = "Visit",
    y = "Grams"
  )

all_fluid_plots <- grid.arrange(fluid_carb_plot, fluid_fat_plot, fluid_pro_plot, fluid_water_plot)

food_v_fluid_plots <- grid.arrange(food_water_plot, fluid_water_plot)

all_food_plots <- grid.arrange(food_carb_plot, food_fat_plot, food_pro_plot, total_food_plot)

all_plots <- grid.arrange(food_v_fluid_plots, all_fluid_plots, all_food_plots)

food_v_fluid_df <- tibble(fluidwater_df$grams, foodwater_df$grams)


