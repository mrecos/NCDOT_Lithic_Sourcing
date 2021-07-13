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

## various ways to look at data
NAA_mat_logged      <- NAA_train_data[,-c(1:3)] # logged only
NAA_mat_centered    <- as.matrix(t(apply(NAA_train_data[,-c(1:3)], 1, scale, scale = FALSE)))

## prcomp
NAA_pca <- prcomp(NAA_mat_logged, scale = FALSE)
# fviz_eig(NAA_pca)

## princomp (legacy method, but closer results to paper)

NAA_pca2 <- princomp(NAA_mat_logged)
NAA_pred <- predict(NAA_pca2, newdata = AECOM_NAA_wide[,-c(1:3)])

groups <- as.factor(NAA_train_data$Quarry_Zone_Group)
fviz_pca_ind(NAA_pca2,
             axes = c(1,2),
             col.ind = groups,
             palette = rainbow(14),
             addEllipses = TRUE,
             ellipse.type = "norm",
             ellipse.level=0.90,
             legend.title = "Region",
             repel = TRUE
)

fviz_pca_var(NAA_pca,
             axes = c(1,2),
             # habillag = groups,
             # palette = rainbow(14),
             addEllipses = FALSE,
             # ellipse.type = "confidence",
             legend.title = "Region",
             repel = TRUE
)

## distance (calculates on log of concetraction, not PCA score [step pg78])
## so this is like doing my ML class assignment on logged concentration and not PCA score...

# these are the X_bar for each group in Step
X_bar <- NAA_train_data %>% 
  dplyr::select(-Sample, -Quarry_Zone) %>% 
  split(.$Quarry_Zone_Group) %>% 
  map(., function(.x) colMeans(.x[,-1])) 

# y from step, point to measure distance from for each group centorid (X_bar)
y_train = NAA_train_data %>% 
  dplyr::select(-Quarry_Zone, -Quarry_Zone_Group) %>% 
  split(.$Sample) %>% 
  map(., function(.x) as.matrix(.x[,-1]))

# I from step pg78. inverted cov matricies of each quarry group
I <- NAA_train_data %>% 
  dplyr::select(-Sample, -Quarry_Zone) %>% 
  split(.$Quarry_Zone_Group) %>% 
  map(., function(.x) cov(.x[,-1])) %>% 
  map(., function(.x) solve(.x, tol = 1.18394e-23)) ## had to make tol very small to solve

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


## testing my approach to distance vs. base::mahalnobis function Same answer!!!
xx1 <- (y_train[[1]] - X_bar$Chatham.1) %*% I$Chatham.1 %*% (y_train[[1]] - X_bar$Chatham.1)[1,]
xx1m <- mahalanobis(y_train[[1]] , X_bar$Chatham.1, cov = I$Chatham.1, inverted = TRUE)



#### tsne

## Curating the database for analysis with both t-SNE and PCA
Labels<-joint_NAA_wide$Quarry_Zone_Group 
joint_NAA_wide$Quarry_Zone_Group <-as.factor(joint_NAA_wide$Quarry_Zone_Group )
## for plotting
colors = rainbow(length(unique(joint_NAA_wide$Quarry_Zone_Group )))
names(colors) = unique(joint_NAA_wide$Quarry_Zone_Group )

## Executing the algorithm on curated data
tsne <- Rtsne(joint_NAA_wide[,-1], dims = 2, pca = TRUE, perplexity=20, 
              verbose=TRUE, max_iter = 2500)

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=joint_NAA_wide$Quarry_Zone_Group , col=colors[joint_NAA_wide$Quarry_Zone_Group ])


