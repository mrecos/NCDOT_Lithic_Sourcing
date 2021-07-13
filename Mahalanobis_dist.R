library(Rtsne)
library(factoextra)
library(viridisLite)
library(ggbiplot)
library(rgr)

### NAA
joint_NAA_wide <- joint_NAA %>% 
  filter(Source != "AECOM") %>%  ####### JUST STEP SAMPLES FOR NOW
  spread(Element, Value) %>% 
  dplyr::select(-Source) %>% 
  na.omit()

AECOM_NAA_wide <- joint_NAA %>% 
  filter(Source == "AECOM") %>%  ####### JUST STEP SAMPLES FOR NOW
  spread(Element, Value) %>% 
  dplyr::select(-Source) %>% 
  na.omit()

NAA_train_data <- joint_NAA_wide %>% 
  dplyr::filter(Quarry_Zone_Group != "UNK") 
NAA_test_data <- joint_NAA_wide %>% 
  dplyr::filter(Quarry_Zone_Group == "UNK") 

NAA_mat_logged      <- NAA_train_data[,-c(1:3)] # logged only

## prcomp
NAA_pca <- prcomp(NAA_train_data[,-c(1:3)] , scale = FALSE)
## princomp (legacy method, but closer results to paper)
NAA_pca2 <- princomp(NAA_train_data[,-c(1:3)] )
fviz_eig(NAA_pca2)

# get the dim of the component that contains at least 90% of the variance
comp_90pcnt <- get_eigenvalue(NAA_pca2) %>% 
  rownames_to_column(var = "dim") %>% 
  dplyr::mutate(dim = as.numeric(str_replace(dim,"Dim.",""))) %>% 
  dplyr::filter(cumulative.variance.percent >= 90) %>% 
  dplyr::filter(dim == min(dim)) %>% 
  pull(dim)

# get the loadings for the `comp_90pcnt` components and bind that sample/quarry/label data
NAA_train_data_pca <- cbind(NAA_train_data[,1:3],as.data.frame(NAA_pca2$scores[,1:comp_90pcnt]))

####### !! WOW - quite a big difference on class distance between 5 or components above!! ###  !!

##### distance on train examples THAT ARE USED TO MAKE GROUP MEANS - NOT Cross-Validated
## Step et al. pg79 - use LOOCV aka "jackknifing"
## would be ok for testing TEST and AECOM class membership
## substitute `NAA_train_data_pca` for distance on PCA comps, or `NAA_train_data` for log raw data
## Results vary a good bit depending on PCA scores vs. log concentraion data (but loo is unstable with log data)
## so this is like doing my ML class assignment on logged concentration and not PCA score...
# these are the X_bar for each group in Step
X_bar <- NAA_train_data_pca %>% 
  dplyr::select(-Sample, -Quarry_Zone) %>% 
  split(.$Quarry_Zone_Group) %>% 
  map(., function(.x) colMeans(.x[,-1])) 

# I from step pg78. inverted cov matricies of each quarry group
I <- NAA_train_data_pca %>% 
  dplyr::select(-Sample, -Quarry_Zone) %>% 
  split(.$Quarry_Zone_Group) %>% 
  map(., function(.x) cov(.x[,-1])) %>% 
  map(., function(.x) solve(.x, tol = 1.18394e-23)) ## had to make tol very small to solve

# y from step, point to measure distance from for each group centorid (X_bar)
y_train = NAA_train_data_pca %>% 
  dplyr::select(-Quarry_Zone, -Quarry_Zone_Group) %>% 
  split(.$Sample) %>% 
  map(., function(.x) as.matrix(.x[,-1]))

mah_func <- function(yi, X_barx, Ix){
  mahalanobis(yi, X_barx, cov = Ix, inverted = TRUE)
}

# Mahalanobis distances
# nested map over all samples to all quarries, then bind back to tibble
mah_distance <- map(y_train, function(x) map2(X_bar, I, function(y,z) mah_func(x,y,z))) %>% 
  map(., bind_rows) %>% 
  bind_rows(., .id = "Sample") 

mah_distance_min <- mah_distance %>% 
  gather(Group_min, value, -Sample) %>% 
  dplyr::group_by(Sample) %>% 
  filter(value == min(value)) %>% 
  dplyr::select(-value) %>%
  left_join(mah_distance, ., by = "Sample")

table(mah_distance_min$Group_min)

######## LOOCV to generate class probabilites on TRAIN data
## for this, X_bar and I need to be calcualted for each n-1 observations.
loo_Xbar_I <- function(train_data){
  # group centroid
  X_bar <- train_data %>% 
    dplyr::select(-Sample, -Quarry_Zone) %>% 
    split(.$Quarry_Zone_Group) %>% 
    map(., function(.x) colMeans(.x[,-1])) 
  # inverse of group cov matrix
  I <- train_data %>%
    dplyr::select(-Sample, -Quarry_Zone) %>%
    split(.$Quarry_Zone_Group) %>%
    map(., function(.x) cov(.x[,-1])) %>%
    map(., function(.x) solve(.x, tol = 1.18394e-23)) ## had to make tol very small to solve

    return(list(X_bar = X_bar, I = I))
}

loo_mah_func <- function(yi,name,dat){
  dat <- dplyr::filter(dat, Sample != name)
  loo_dat <- loo_Xbar_I(dat)
  loo_man <- map2(loo_dat$X_bar, loo_dat$I, function(x,z) mah_func(yi,x,z))
}

loo_mah_distance <- map2(y_train, names(y_train), 
                     function(.y,.name) loo_mah_func(.y, .name, NAA_train_data_pca)) %>% 
  map(., bind_rows) %>% 
  bind_rows(., .id = "Sample")

loo_mah_distance_min <- loo_mah_distance %>% 
  gather(Group_min, value, -Sample) %>% 
  dplyr::group_by(Sample) %>% 
  filter(value == min(value)) %>% 
  dplyr::select(-value) %>%
  left_join(loo_mah_distance, ., by = "Sample") %>% 
  left_join(., dplyr::select(NAA_train_data_pca, Sample, Quarry_Zone_Group), by = "Sample")

table(loo_mah_distance_min$Group_min)
table(NAA_train_data_pca$Quarry_Zone_Group)

mah_results <- loo_mah_distance_min %>% 
  dplyr::select(Sample, Quarry_Zone_Group, "pred_class" = Group_min) %>% 
  mutate(correct = as.numeric(Quarry_Zone_Group == pred_class),
         Quarry_Zone_Group = as.factor(Quarry_Zone_Group),
         pred_class = as.factor(pred_class))
mean(mah_results$correct)

loo_moh_cm <- confusionMatrix(data = mah_results$pred_class, reference = mah_results$Quarry_Zone_Group)

ggplot(as.data.frame(loo_moh_cm$table), 
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

###
xx <- cbind(table(NAA_train_data_pca$Quarry_Zone_Group), 
      table(loo_mah_distance_min$Group_min),
      table(loocv_splits$pred_class)) %>% 
  data.frame() %>% 
  rownames_to_column(var = "Group") %>% 
  dplyr::select(Group, "Actual" = X1, "Mahalanobis" = X2, "Boosted Trees" = X3)


