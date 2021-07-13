library(randomForest)
library(caret)
library(tidymodels)

"%ni%" <- Negate("%in%")
fit_model <- function(split, spec) {
  cat(Sys.time(),"\n")
  fit(
    object = spec, 
    formula = NAA_form,
    data = analysis(split) # <- pull out training set
  )
}
compute_pred <- function(split, model) {
  assess <- assessment(split)
  pred_class <- predict(model, new_data = assess)
  bind_cols(assess, pred_class)
}
compute_perf <- function(pred_df) {
  numeric_metrics <- metric_set(accuracy, kap)
  numeric_metrics(
    pred_df, 
    truth = Quarry_Zone_Group, 
    estimate = .pred_class
  )
}
fit_all_specs_all_splits <- function(split_df, spec_df) {
  split_df %>%
    mutate(
      spec_perf = map(
        splits, 
        fit_all_specs_one_split, 
        spec_df = spec_df
      )
    ) %>%
    dplyr::select(splits, id, spec_perf)
}
fit_all_specs_one_split <- function(split, spec_df) {
  spec_df %>%
    mutate(
      accuracy = map_dbl(
        specs, 
        fit_one_spec_one_split, 
        split = split
      )
    )
}
fit_one_spec_one_split <- function(spec, split) {
  cat("Fitting next model...","\n")
  mod <- fit_model(split, spec)
  pred_df <- compute_pred(split, mod)
  perf_df <- compute_perf(pred_df)
  
  # pull out only rmse
  perf_df %>%
    filter(.metric == "accuracy") %>%
    pull(.estimate)
}

step_rf_dat <- joint_NAA_wide %>% 
  dplyr::filter(Quarry_Zone_Group != "UNK") %>% 
  dplyr::select(-Quarry_Zone) %>% 
  mutate(Quarry_Zone_Group = as.factor(Quarry_Zone_Group))
step_rf_dat_test <- joint_NAA_wide %>% 
  dplyr::filter(Sample %in% paste0("FBL0",72:80)) %>% 
  # dplyr::select(-Sample, -Quarry_Zone) %>% 
  mutate(Quarry_Zone_Group = as.factor(Quarry_Zone_Group))
AECOM_rf_dat <- AECOM_NAA_wide %>% 
  dplyr::select(-Sample, -Quarry_Zone) %>% 
  mutate(Quarry_Zone_Group = as.factor(Quarry_Zone_Group))

NAA_form <- formula(Quarry_Zone_Group ~ .)

#### TESTING TO ADD PRINCOMP COMPONENTS TO DATA B/C recipe USES PRCOMP!!!!!!!!!!!!!
step_rf_dat_princomp <- princomp(step_rf_dat[,-c(1:2)])
step_rf_dat_princomp <- step_rf_dat_princomp$scores[,1:5]  ## Number of comps
step_rf_dat <- cbind(step_rf_dat[,1:2],step_rf_dat_princomp)
###################################################################################

data_transform <- recipe(NAA_form, 
                      data = step_rf_dat) %>%
  ## STEP TO IGNORE 'Sample'variable
                      # step_BoxCox(all_numeric()) %>%
                      # step_center(all_numeric()) %>%
                      # step_scale(all_numeric()) %>%
                      # step_pca(all_numeric(),num_comp = 5) %>%
                      prep() 
step_rf_dat_trans      <- bake(data_transform, new_data = step_rf_dat)
AECOM_rf_dat_trans     <- bake(data_transform, new_data = AECOM_rf_dat) %>% dplyr::select(-Sample)
step_rf_dat_test_trans <- bake(data_transform, new_data = step_rf_dat_test)

# single RF model
f1 <- xgboost::xgboost(label=step_rf_dat_trans$Quarry_Zone_Group,
              data=as.matrix(step_rf_dat_trans[,-c(1:2)]), ntree = 3000, nrounds = 100,
              objective = "multi:softprob", num_class = 9L, verbose = FALSE)
f1

AECOM_preds <- predict(f1, newdata = as.matrix(AECOM_rf_dat_trans[,-1])) %>% 
  matrix(.,ncol=9L, byrow = TRUE) %>% 
  data.frame() %>% 
  `colnames<-`(c("xx",levels(step_rf_dat_trans$Quarry_Zone_Group))) %>% 
  data.frame(Sample = AECOM_NAA_wide$Sample) %>% 
  rownames_to_column('id') %>%
  gather(pred_class, cnt, 2:10) %>% 
  dplyr::group_by(id) %>% 
  filter(cnt == max(cnt)) %>% # top_n(cnt, n = 1) also works
  arrange(Sample)
AECOM_preds

Step_preds <- predict(f1, newdata = as.matrix(step_rf_dat_test_trans[,-c(1:2)])) %>% 
  matrix(.,ncol=9L, byrow = TRUE) %>% 
  data.frame() %>% 
  `colnames<-`(c("xx",levels(step_rf_dat_trans$Quarry_Zone_Group))) %>% 
  data.frame(Sample = step_rf_dat_test$Sample) %>% 
  rownames_to_column('id') %>%
  gather(pred_class, cnt, 2:10) %>% 
  dplyr::group_by(id) %>% 
  filter(cnt == max(cnt)) %>% # top_n(cnt, n = 1) also works
  arrange(Sample)
Step_preds

Step_train_preds <- predict(f1, newdata = as.matrix(step_rf_dat_trans[,-c(1:2)])) %>% 
  matrix(.,ncol=9L, byrow = TRUE) %>% 
  data.frame() %>% 
  `colnames<-`(c("xx",levels(step_rf_dat_trans$Quarry_Zone_Group))) %>% 
  data.frame(Sample = step_rf_dat$Sample,
             Truth  = step_rf_dat$Quarry_Zone_Group) %>% 
  rownames_to_column('id') %>%
  gather(pred_class, cnt, 2:10) %>% 
  dplyr::group_by(id) %>% 
  filter(cnt == max(cnt)) %>% # top_n(cnt, n = 1) also works
  arrange(Sample)
Step_train_preds
table(Step_train_preds$pred_class)

# single fit on cv
spec_rf <- boost_tree(trees = 200, mode = "classification", mtry = 7) %>% 
  set_engine("xgboost")

cv_splits <- vfold_cv(
  data = step_rf_dat_trans, 
  v = 5,
  repeats = 1
)

cv_splits <- cv_splits %>%
  mutate(
    models_rf = map(splits, fit_model, spec_rf),
    pred_rf   = map2(splits, models_rf, compute_pred),
    perf_rf   = map(pred_rf, compute_perf),
    accuracy  = map_dbl(perf_rf, ~as.numeric(.x[1,3])),
    kappa     = map_dbl(perf_rf, ~as.numeric(.x[2,3]))
  )
hist(cv_splits$accuracy)
mean(cv_splits$accuracy)

# Hyperparameter sweep
spec_rf_varying <- boost_tree(mtry = varying(), trees = 2000) %>% 
  set_engine("xgboost")

param_grid <- 
  mtry %>% 
  range_set(c(1, 10)) %>%
  grid_regular(levels = 10) %>%
  mutate(
    specs = merge(., spec_rf_varying)
  )

resampled_grid <- fit_all_specs_all_splits(
  split_df = cv_splits, 
  spec_df = param_grid
)

unnested_grid <- 
  resampled_grid %>%
  unnest(spec_perf) %>%
  dplyr::select(-specs)

unnested_grid %>% 
  group_by(mtry) %>% 
  dplyr::summarise(mean_accuracy = mean(accuracy, na.rm=TRUE)) %>% 
  ggplot(., aes(mtry,mean_accuracy)) +
  scale_x_continuous(breaks =1:20) +
  scale_y_continuous(breaks = seq(0,1,0.005)) +
  geom_line() +
  theme_bw()

### Would need to run LOOCV to find out which data points are causing a rift between mtry 7 and 7+
## should try to do this with class probs if possible to get more info, but works for max prob class
get_id_left_out <- function(x,col_name){
  # rediculous factor workaround
  as.character(assessment(x)[,col_name][1,1,drop=TRUE])
}

loocv_splits <- loo_cv(data = step_rf_dat_trans) %>%
  mutate(
    id         = map_chr(splits, get_id_left_out, "Sample"),
    Quarry     = map_chr(splits, get_id_left_out, "Quarry_Zone_Group"),
    models_rf  = map(splits, fit_model, spec_rf),
    pred_rf    = map2(splits, models_rf, compute_pred),
    perf_rf    = map(pred_rf, compute_perf),
    accuracy   = map_dbl(perf_rf, ~ as.numeric(.x[1,3])),
    kappa      = map_dbl(perf_rf, ~ as.numeric(.x[2,3])),
    pred_class = map_chr(pred_rf, ~ as.character(.x[1,".pred_class"][1,1,drop=TRUE]))
  )
table(loocv_splits$pred_class)

loocv_results <- loocv_splits %>% 
  dplyr::select("Sample" = id, "Quarry_Zone_Group" = Quarry, pred_class) %>% 
  mutate(correct = as.numeric(Quarry_Zone_Group == pred_class),
         Quarry_Zone_Group = as.factor(Quarry_Zone_Group),
         pred_class = as.factor(pred_class))
loocv_cm <- confusionMatrix(data = loocv_results$pred_class, reference = loocv_results$Quarry_Zone_Group)

ggplot(as.data.frame(loocv_cm$table), 
       aes(x=factor(Reference, levels = rev(levels(Reference))), y=Prediction, fill=Freq)) + 
  # scale_x_discrete(labels = c("Positive","Negative")) +
  # scale_y_discrete(labels = c("Negative","Positive")) +
  # labs(x = "True Class", y = "Predicted Class") +
  scale_fill_viridis_c(option = "A", begin = 0.15, end = 1, direction = -1) +
  scale_x_discrete(position = "top") + 
  coord_equal() +
  geom_tile(color = "gray80") +
  labs(x = "Ground Truth", y = "Prediction") +
  geom_text(aes(label = ifelse(Freq > 0, Freq, '-')), color = "gray50") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 0)
  )


