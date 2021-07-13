library(tidyverse)
library(readxl)

data_loc <- "./DATA"

replace_Inf_0 <- function(x){
  if_else(is.infinite(x),0,x)
}

### Steponaitis et.al 2006 Data
Step_NAA_dat1 <- read_xls(file.path(data_loc, "SteponaitisEtAl2006Data", "TableD01_8_21_clean.xls"))
Step_NAA_dat2 <- read_xls(file.path(data_loc, "SteponaitisEtAl2006Data", "TableD02_8_21_clean.xls"))
Step_XRF_major_dat <- read_xls(file.path(data_loc, 
                                         "SteponaitisEtAl2006Data", "TableE01_8_20_clean.xls"))
Step_XRF_trace_dat <- read_xls(file.path(data_loc, 
                                         "SteponaitisEtAl2006Data", "TableE02_8_20_clean.xls"))
Step_prov_dat <- read_xls(file.path(data_loc, 
                                         "SteponaitisEtAl2006Data", "TableA02_9_09_05_clean.xls"))
### AECOM DATA
AECOM_XRF <- read_xlsx(file.path(data_loc, "NAA and XRF data.xlsx"), sheet = "XRF_data") %>%
  dplyr::select(sort(names(.)))
AECOM_NAA <- read_xlsx(file.path(data_loc, "NAA and XRF data.xlsx"), sheet = "NAA_data") %>% 
  dplyr::select(sort(names(.)))

Step_NAA <- full_join(Step_NAA_dat1, Step_NAA_dat2, by = 'Sample') %>% 
  dplyr::select(sort(names(.)))
Step_XRF <- full_join(Step_XRF_major_dat, Step_XRF_trace_dat, by = 'Sample')  %>% 
  dplyr::select(sort(names(.)))

### joined data comparison NAA
common_elemets <- colnames(AECOM_NAA)[colnames(AECOM_NAA) %in% colnames(Step_NAA)]

joint_NAA <- dplyr::select(AECOM_NAA, common_elemets) %>% 
  mutate(Source = "AECOM") %>% 
  rbind(., Step_NAA[common_elemets] %>% 
          mutate(Source = "Steponaitis")) %>% 
  dplyr::select(-As, -Cr, -Ni, -Sr, -U, -V) %>%  # Step et al. pg79
  mutate_if(is.numeric, log, base = 10) %>% # Step et al. pg77
  mutate_if(is.numeric,replace_Inf_0) %>%   # Step et al. pg77
  filter(Sample != "FBL039") %>%            # Step et al. pg79
  gather(Element, Value, -Sample, -Source) %>% 
  left_join(., dplyr::select(Step_prov_dat, Sample, Quarry_Zone)) %>% 
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

# test to make sure we have the right elements as step et al.
step_elements <- c("La","Lu","Nd","Sm","Yb","Ce","Co",
                   "Cs","Eu","Fe","Hf","Rb","Sb","Sc",
                   "Ta","Tb","Th","Zn","Zr","Al","Ba",
                   "Ca","Dy","K","Mn","Na","Ti")
all.equal(sort(unique(joint_NAA$Element)), sort(step_elements))

NAA_plot_order <- joint_NAA %>% 
  group_by(Element) %>% 
  dplyr::summarise(mean = mean(Value, na.rm = TRUE)) %>% 
  arrange(desc(mean))

ggplot(joint_NAA) +
  aes(x = Element, y = Value, color = Source) +
  geom_boxplot(width = 0.75, shape = 1) +
  scale_x_discrete(limits=unique(NAA_plot_order$Element)) +
  theme_bw()

### joined data comparison XRF
common_elemets <- colnames(AECOM_XRF)[colnames(AECOM_XRF) %in% colnames(Step_XRF)]
joint_XRF <- dplyr::select(AECOM_XRF, common_elemets) %>% 
  mutate(Source = "AECOM") %>% 
  rbind(., Step_XRF[common_elemets] %>% 
          mutate(Source = "Steponaitis")) %>% 
  gather(Element, Value, -Sample, -Source) %>% 
  left_join(., dplyr::select(Step_prov_dat, Sample, Quarry_Zone)) %>% 
  mutate(Quarry_Zone = if_else(is.na(Quarry_Zone),"UNK",Quarry_Zone),
         Quarry_Zone = if_else(Source == "AECOM","AECOM",Quarry_Zone))

joint_XRF_means <- joint_XRF %>% 
  group_by(Source, Element) %>% 
  dplyr::summarise(Mean = mean(Value, na.rm = TRUE),
            STD   = sd(Value, na.rm = TRUE)) %>% 
  nest(Mean, STD, .key = "value_col") %>% 
  spread(key = Source, value = value_col) %>% 
  unnest()

ggplot(joint_XRF, aes(x = Element, y = Value)) +
  geom_point() +
  facet_grid(~Source) +
  theme_bw()
  
  
  