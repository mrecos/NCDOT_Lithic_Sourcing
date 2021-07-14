### Quick script to load new 2021 dataset and predict with model fit to 2019 data
### Requires `NAA_model_compare.Rmd` to be run first to `join_XGB_nonpca` object and others


new_dat <- read.csv("./DATA/FB_Rhyolites_20210712.csv") %>% 
  rename("Sample" = ANID,
         "Quarry_Zone" = Local.Subregion) %>% 
  mutate(Quarry_Zone_Group = case_when(      # Step et al. pg102
    Quarry_Zone %in% c("Uwharries Eastern","Uwharries Southeastern","Uwharries Southeast",
                       "Uwharries Southern","Uwharries Western") ~ "Uwharries.1",
    Quarry_Zone == "Uwharries Asheboro" ~ "Uwharries.2",
    Quarry_Zone %in% c("Chatham Pittsboro","Chatham Siler City",
                       "Chatham County") ~ "Chatham.1",
    Quarry_Zone == "Chatham Silk Hope" ~ "Chatham.2",
    Quarry_Zone == "Cumberland County" ~ "Cumberland",
    Quarry_Zone == "Orange County" ~ "Orange",
    Quarry_Zone == "Person County" ~ "Person",
    Quarry_Zone == "Durham County" ~ "Durham",
    Quarry_Zone == "Fort Bragg" ~ "Fort Bragg",
    Quarry_Zone == "Cape Fear" ~ "Cape Fear"
  ))

joint_NAA <- new_dat %>% 
  dplyr::select(., common_elemets) %>% 
  mutate(Source = "NEWDATA") %>% 
  # rbind(., Step_NAA[common_elemets] %>% 
  #         mutate(Source = "Steponaitis")) %>% 
  dplyr::select(-As, -Cr, -Ni, -Sr, -U, -V) %>%  # Step et al. pg79
  mutate_if(is.numeric, log, base = 10) %>% # Step et al. pg77
  mutate_if(is.numeric,replace_Inf_0) %>%   # Step et al. pg77
  # filter(Sample != "FBL039") %>%            # Step et al. pg79
  gather(Element, Value, -Sample, -Source) %>% 
  left_join(., dplyr::select(new_dat, Sample, Quarry_Zone), by = "Sample") %>% 
  mutate(Quarry_Zone = if_else(Source == "NEWDATA","NEWDATA",Quarry_Zone)) %>% 
  mutate(Quarry_Zone = if_else(is.na(Quarry_Zone),"UNK",Quarry_Zone)) 

joint_NAA_wide <- joint_NAA %>% 
  spread(Element, Value) %>% 
  dplyr::select(-Source) %>% 
  na.omit()

join_XGB_pred_nonpca <- predict(join_XGB_nonpca, new_data = joint_NAA_wide) %>% 
  bind_cols(joint_NAA_wide,.) %>% 
  left_join(., select(new_dat, Sample, Quarry_Zone_Group), by = "Sample") 

join_XGB_pred_nonpca_cf <- join_XGB_pred_nonpca %>% 
  filter(Quarry_Zone_Group %ni% c("Cape Fear", "Fort Bragg")) %>% 
  mutate(Quarry_Zone_Group = factor(Quarry_Zone_Group, levels = levels(.$.pred_class)))


loocv_cm_XGB_nonpca <- confusionMatrix(data = join_XGB_pred_nonpca_cf$.pred_class, 
                                       reference = join_XGB_pred_nonpca_cf$Quarry_Zone_Group)

ggplot(as.data.frame(as.data.frame(loocv_cm_XGB_nonpca$table)), 
       aes(x=factor(Reference, levels = rev(levels(Reference))), y=Prediction, fill=Freq)) + 
  scale_fill_viridis_c(option = "D", begin = 0.15, end = 1, direction = -1) +
  scale_x_discrete(position = "top") + 
  coord_equal() +
  geom_tile(color = "gray80") +
  labs(x = "Ground Truth", y = "Prediction",
       caption = paste("Accuracy:",round(loocv_cm_XGB_nonpca$overall["Accuracy"],3),
                       "Kappa:",round(loocv_cm_XGB_nonpca$overall["Kappa"],3))) +
  geom_text(aes(label = ifelse(Freq > 0, Freq, '-')), color = "gray50") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 0)
  )


# table of test sample group assignment
AECOM_XGB_pred_nonpca <- join_XGB_pred_nonpca %>% 
  filter(grepl("Fort Bragg", Quarry_Zone_Group)) %>% 
  select(Sample, Quarry_Zone_Group, .pred_class)

kable(AECOM_XGB_pred_nonpca)
