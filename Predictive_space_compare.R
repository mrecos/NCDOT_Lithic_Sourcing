# devtools::install_github("jaredhuling/jcolors")
library(jcolors)

join_XGB_pca <- fit(object = spec_XGB,
                    formula = NAA_form,
                    data = NAA_train_data_pca[,3:5]) ## ONLY comps 1 & 2

join_KNN_pca <- fit(object = spec_KNN,
                    formula = NAA_form,
                    data = NAA_train_data_pca[,3:5]) ## ONLY comps 1 & 2

join_RF_pca <- fit(object = spec_rf,
                    formula = NAA_form,
                    data = NAA_train_data_pca[,3:5]) ## ONLY comps 1 & 2

join_SVM_pca <- fit(object = spec_SVM,
                    formula = NAA_form,
                    data = NAA_train_data_pca[,3:5]) ## ONLY comps 1 & 2

spec_GLM <- multinom_reg(penalty = 1, mixture = 0.1) %>% 
  set_engine("glmnet")
join_GLM_pca <- fit(object = spec_GLM,
                    formula = NAA_form,
                    data = NAA_train_data_pca[,3:5]) ## ONLY comps 1 & 2


range(NAA_train_data_pca$PC1)
range(NAA_train_data_pca$PC2)

sim_pc12 <- data.frame(PC1 = seq(-4,4,0.05),
                       PC2 = seq(-4,4,0.05)) %>% 
  tidyr::expand(PC1, PC2)

#### Mahanalobis distance fit/predict
sim_X_bar <- NAA_train_data_pca[,3:5] %>%
  split(.$Quarry_Zone_Group) %>%
  map(., function(.x) colMeans(.x[,-1]))

sim_I <- NAA_train_data_pca[,3:5] %>%
  split(.$Quarry_Zone_Group) %>%
  map(., function(.x) cov(.x[,-1])) %>%
  map(., function(.x) solve(.x, tol = 1.18394e-23)) ## had to make tol very small to solve

# joint_NAA_data_pca == full joint dataset
sim_y_train = sim_pc12 %>%
  mutate(Sample = seq(1,nrow(sim_pc12),1)) %>% 
  dplyr::select(Sample, everything()) %>% 
  split(.$Sample) %>% 
  map(., function(.x) as.matrix(.x[,-1]))

sim_mah_distance <- map(sim_y_train, function(x) map2(sim_X_bar, sim_I, 
                                                           function(y,z) mah_func(x,y,z))) %>% 
  map(., bind_rows) %>% 
  bind_rows(., .id = "Sample")  

sim_mah_distance2 <- sim_mah_distance %>% 
  gather(.pred_class, value, -Sample) %>% 
  dplyr::group_by(Sample) %>% 
  filter(value == min(value)) %>% 
  dplyr::select(-value) %>%
  ungroup() %>% 
  mutate(Sample = as.numeric(Sample)) %>% 
  left_join(sim_pc12 %>%  mutate(Sample = seq(1,nrow(sim_pc12),1)), ., by = "Sample")
################################################################

sim_pred_XGB <- predict(join_XGB_pca, new_data = sim_pc12) %>% 
  bind_cols(sim_pc12,.)

sim_pred_KNN <- predict(join_KNN_pca, new_data = sim_pc12) %>% 
  bind_cols(sim_pc12,.)

sim_pred_RF <- predict(join_RF_pca, new_data = sim_pc12) %>% 
  bind_cols(sim_pc12,.)

sim_pred_SVM <- predict(join_SVM_pca, new_data = sim_pc12) %>% 
  bind_cols(sim_pc12,.)

sim_pred_GLM <- predict(join_GLM_pca, new_data = sim_pc12) %>% 
  bind_cols(sim_pc12,.)

MAH_space_12 <- ggplot(sim_mah_distance2, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_bw()

XGB_space_12 <- ggplot(sim_pred_XGB, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_bw()

KNN_space_12 <- ggplot(sim_pred_KNN, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_bw()

RF_space_12 <- ggplot(sim_pred_RF, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

SVM_space_12 <- ggplot(sim_pred_SVM, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_bw()

GLM_space_12 <- ggplot(sim_pred_GLM, aes(PC1, PC2, fill = .pred_class)) +
  geom_tile() +
  scale_fill_jcolors(palette = "pal6") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_bw()

legend_b <- get_legend(RF_space_12 + theme(legend.direction = "horizontal",
                                           legend.justification="center" ,
                                           legend.box.just = "bottom",
                                           legend.title=element_blank()))
plot_row <- plot_grid(MAH_space_12 + theme(legend.position = "none"),
                      XGB_space_12 + theme(legend.position = "none"),
                      KNN_space_12 + theme(legend.position = "none"),
                      RF_space_12  + theme(legend.position = "none"),
                      SVM_space_12 + theme(legend.position = "none"),
                      GLM_space_12 + theme(legend.position = "none"),
          labels = c("Mahalanobis","XGB", "KNN","RF","SVM (RBF)","GLM"),
          align = "h", nrow = 2)
plot_grid(plot_row, legend_b, ncol = 1, 
          rel_heights = c(1, 0.2), rel_widths = c(1,2))

# ggsave("Pred_space_compare.png", width = 9, height = 8.25)


