
# Dataframes and Plots for "final" analysis 

final_df <- tibble(fw_mean_intake$FWIC, dif_fw_a1_week1$greatest_change, seq_dif_week1$mean_dif)

names(final_df) <- c("mean_FWIC", "greatest_change_week1", "seq_change_week1")

boxplot_df <- gather(final_df, comp, difvalue, mean_FWIC:seq_change_week1)

data_comp <- list(c("mean_FWIC", "greatest_change_week1"),c("mean_FWIC", "seq_change_week1"))
                  
final_boxplot_a1 <- ggplot(
  data = boxplot_df, 
  aes(
    x = comp,
    y = difvalue
  )
) + 
  geom_boxplot(
    
  ) + 
  scale_x_discrete(
    labels = c("Greatest Baseline Change", "FWIC", "Sequential Baseline Change ")
  ) + 
  labs(
    x = NULL,
    y = expression(Delta * " in ml"),
    title = "Changes in Food Water Intake"
  ) 
  

  