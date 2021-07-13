center_this <- function(x) as.vector(scale(x, center = TRUE, scale = FALSE))

joint_NAA <- dplyr::select(AECOM_NAA, common_elemets) %>% 
  mutate(Source = "AECOM") %>% 
  rbind(., Step_NAA[common_elemets] %>% 
          mutate(Source = "Steponaitis")) %>% 
  dplyr::select(-As, -Cr, -Ni, -Sr, -U, -V) %>%  # Step et al. pg79
  mutate_if(is.numeric, log, base = 10) %>%      # Step et al. pg77
  mutate_if(is.numeric,replace_Inf_0) %>%        # Step et al. pg77
  filter(Sample != "FBL039") %>%                 # Step et al. pg79
  gather(Element, Value, -Sample, -Source) %>% 
  left_join(., dplyr::select(Step_prov_dat, Sample, Quarry_Zone), by = "Sample") %>% 
  mutate(Quarry_Zone = if_else(Source == "AECOM","AECOM",Quarry_Zone)) %>% 
  mutate(Quarry_Zone = if_else(is.na(Quarry_Zone),"UNK",Quarry_Zone)) %>% 
  mutate(Quarry_Zone_Group = case_when(      # Step et al. pg102
    Quarry_Zone %in% c("Uwharries Eastern","Uwharries Southeastern",
                       "Uwharries Southern","Uwharries Western") ~ "Uwharries.1",
    Quarry_Zone == "Uwharries Asheboro" ~ "Uwharries.2",
    Quarry_Zone %in% c("Chatham Pittsboro","Chatham Siler City") ~ "Chatham.1",
    Quarry_Zone == "Chatham Silk Hope" ~ "Chatham.2",
    Quarry_Zone == "Cumberland County" ~ "Cumberland",
    Quarry_Zone == "Orange County" ~ "Orange",
    Quarry_Zone == "Person County" ~ "Person",
    Quarry_Zone == "Durham County" ~ "Durham",
    Quarry_Zone == "UNK" ~ "UNK",
    Quarry_Zone == "AECOM" ~ "AECOM"
  ))

Step_NAA_wide <- joint_NAA %>% 
  filter(Source != "AECOM") %>% 
  spread(Element, Value) %>% 
  dplyr::select(-Source) %>% 
  na.omit()

pca1  <- princomp(Step_NAA_wide[,-c(1:3)], cor = TRUE)
pca2  <- princomp(Step_NAA_wide[,-c(1:3)], cor = FALSE)
pca9  <- prcomp(Step_NAA_wide[,-c(1:3)], retx = TRUE, center = TRUE, scale. = FALSE)
pca10 <- prcomp(Step_NAA_wide[,-c(1:3)], retx = TRUE, center = TRUE, scale. = TRUE)
pca11 <- prcomp(Step_NAA_wide[,-c(1:3)], retx = TRUE, center = FALSE, scale. = TRUE) #
pca12 <- prcomp(Step_NAA_wide[,-c(1:3)], retx = TRUE, center = FALSE, scale. = FALSE) 

pca13 <- prcomp(t(Step_NAA_wide[,-c(1:3)]), retx = TRUE, center = TRUE, scale. = FALSE)
pca14 <- prcomp(t(Step_NAA_wide[,-c(1:3)]), retx = TRUE, center = TRUE, scale. = TRUE)
pca15 <- prcomp(t(Step_NAA_wide[,-c(1:3)]), retx = TRUE, center = FALSE, scale. = TRUE) #
pca16 <- prcomp(t(Step_NAA_wide[,-c(1:3)]), retx = TRUE, center = FALSE, scale. = FALSE) 

## pca2, pca9, or cv1 (pca 9 and cv1 are the same)
cv1 <- cov.wt(as.matrix(Step_NAA_wide[,-c(1:3)]))
summary(princomp(Step_NAA_wide[,-c(1:3)], cor = FALSE, covmat = cv1))

eigen(cov(as.matrix(Step_NAA_wide[,-c(1:3)]),method="pearson"))
