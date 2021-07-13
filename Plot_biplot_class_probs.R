library(cowplot)
library(ggrepel)

xx <- predict(f1, newdata = as.matrix(step_rf_dat_test_trans[,-c(1:2)])) %>% 
  matrix(.,ncol=9L, byrow = TRUE) %>% 
  data.frame() %>% 
  `colnames<-`(c("xx",levels(step_rf_dat_trans$Quarry_Zone_Group))) %>% 
  data.frame(Sample = step_rf_dat_test$Sample) %>% 
  rownames_to_column('id') %>%
  gather(pred_class, cnt, 2:10) %>% 
  filter(pred_class != "xx")

class_plot <- ggplot(xx, aes(x = pred_class, y = Sample, fill = cnt, label = round(cnt,2))) +
  geom_tile() +
  geom_text(color = "gray50") +
  scale_fill_viridis_c(name="Probability", option = "D") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x="Predicted Quarry Lcoation", y = "Sample Number") +
  theme_bw()

# biplot
NAA_test_preds <- predict(NAA_pca, newdata = NAA_test_data) %>% 
  as.data.frame() %>% 
  dplyr::select(PC1,PC2) %>% 
  dplyr::mutate(Sample = NAA_test_data$Sample)

obs_plot <- ggbiplot(NAA_pca, groups = NAA_train_data$Quarry_Zone_Group, 
         ellipse = TRUE, ellipse.prob = .75, var.axes = FALSE) +
  geom_point(data = NAA_test_preds, aes(x =PC1,y=PC2)) +
  geom_text_repel(data = NAA_test_preds, aes(x =PC1,y=PC2,label = Sample),
                  point.padding = 0.5, force = 3, max.iter = 8000) +
  theme_bw()

var_plt <- ggbiplot(NAA_pca, groups = NAA_train_data$Quarry_Zone_Group, 
         circle = T, alpha = 0.35, varname.size = 4, varname.adjust = 3) +
  scale_x_continuous(limits = c(-1.75,1.75)) +
  scale_y_continuous(limits = c(-1.75,1.75)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

upper_row <- plot_grid(var_plt, obs_plot, labels = c('', ''), align = 'h', rel_heights = c(1,1))
plot_grid(upper_row, class_plot, nrow = 2, 
                   rel_widths = c(1,1), rel_heights = c(1,0.65))
ggsave("test.png", width = 9, height = 6)
