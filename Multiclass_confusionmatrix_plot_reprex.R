library(tidyverse)
library(cowplot)
library(caret)

Example_data1 <- data.frame(id = seq(1,100),
                           pred_class = sample(LETTERS[1:10], 100, replace=TRUE)) %>% 
  group_by(id) %>% 
  dplyr::mutate(true_class = ifelse(rbinom(1,1,0.85)==1,as.character(pred_class), 
                                    sample(LETTERS[1:10], 1))) %>% 
  ungroup() %>% 
  dplyr::mutate_if(is.character, as.factor)

Example_ConfMat1 <- confusionMatrix(data      = Example_data1$pred_class, 
                                    reference = Example_data1$true_class)

Example_plot1 <- ggplot(as.data.frame(Example_ConfMat1$table), 
                          aes(x=factor(Reference, levels = rev(levels(Reference))), 
                              y=Prediction, fill=Freq)) + 
  scale_fill_viridis_c(option = "A", begin = 0.15, end = 1, direction = -1) +
  scale_x_discrete(position = "top") + 
  coord_equal() +
  geom_tile(color = "gray80") +
  labs(x = "Ground Truth", y = "Prediction",
       caption = paste("Accuracy:",round(Example_ConfMat1$overall["Accuracy"],3),
                       "Kappa:",round(Example_ConfMat1$overall["Kappa"],3))) +
  geom_text(aes(label = ifelse(Freq > 0, Freq, '-')), color = "gray50") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 0)
  )

## Duplicate plot to show the `plot_grid` results
Example_plot2 <- Example_plot1
Example_plot3 <- Example_plot1
Example_plot4 <- Example_plot1

### example of using cowplot::plot_grid() to organize a bunch of ggplots
cowplot::plot_grid(Example_plot1, Example_plot2, Example_plot3, Example_plot4,
                   labels = c("EX1", "EX2", "EX3", "EX4"), align = "h", nrow = 2)