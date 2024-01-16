df_long <- df %>%
  mutate(age_hcm = primary_diagnosis_age, # renaming variable
         age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         age_af = age_arrhythmia_a_fib, # renaming af var
         age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         age_hf = pmin(age_lvsd, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm),
         age_ablation = if_else(event_ablation==1, t2_ablation,NA)) %>%
  dplyr::select(pid,primary_diagnosis_age,
                age_htn,
                age_obstruction,
                age_srt, 
                age_af , 
                #age_cardiac_arrest,
                age_arrhythmia_nsvt, 
                age_ablation,
                age_vt,
                #age_syncope,  
                age_icd,
                age_lvsd, age_nyha_hf,
                #age_hf,
                age_stroke, 
                age_htxvad, age_death,
                sarc_status,
  ) %>%
  pivot_longer(cols = starts_with("age_"),
               names_to = "outcome",
               values_to = "age") %>%
  drop_na() %>%
  mutate(age = case_when(age==0~primary_diagnosis_age,
                         T~age)) # remove rows with missing age values

# group the dataset by patient ID and outcome, and calculate the rank of age within each group
df_long <- df_long %>%
  group_by(pid) %>%
  mutate(rank = rank(age, ties.method = "first")) %>%
  ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>%
  group_by(pid) %>% arrange(pid,rank) %>%
  mutate(previous_event = ifelse(rank == 1, "share", lag(outcome))) %>%
  ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age)


#Diagnostic triplets
triplets <-
  df_long %>% 
  filter(sarc_status %in% c("SARC(+)", "SARC(-)")) %>%
  select(!previous_event) %>% 
  pivot_wider(names_from = rank, values_from = c(outcome, age)) %>%
  mutate(vuf1 = paste(outcome_1,outcome_2,outcome_3,age_1,age_2,age_3, sep = ";"),
         vuf2 = paste(outcome_2,outcome_3,outcome_4,age_2,age_3,age_4, sep = ";"),
         vuf3 = paste(outcome_3,outcome_4,outcome_5,age_3,age_4,age_5, sep = ";"),
         vuf4 = paste(outcome_4,outcome_5,outcome_6,age_4,age_5,age_6, sep = ";"),
         vuf5 = paste(outcome_5,outcome_6,outcome_7,age_5,age_6,age_7, sep = ";"),
         vuf6 = paste(outcome_6,outcome_7,outcome_8,age_6,age_7,age_8, sep = ";"),
         vuf7 = paste(outcome_7,outcome_8,outcome_9,age_7,age_8,age_9, sep = ";"),
         vuf8 = paste(outcome_8,outcome_9,outcome_10,age_8,age_9,age_10, sep = ";"),
         #         vuf9 = paste(outcome_9,outcome_10,outcome_11,age_9,age_10,age_11, sep = ";")
         #vuf10 = paste(outcome_10,outcome_11,age_11)
  ) %>% #select(contains("vuf"))
  pivot_longer(cols = contains("vuf")) %>% select(pid,sarc_status,value) %>% 
  separate(col = value, into = c("source", "to", "destination", "age1", "age2", "age3"),sep = ";") %>% 
  mutate(across(c(age1, age2, age3), ~as.numeric(.x)),
         triplet = paste(source,to,destination),
         time_lag1 = age2-age1,
         time_lag2 = age3-age2) %>% drop_na() %>%
  filter(time_lag1<5) %>% 
  filter(time_lag2<5) %>% 
  group_by(triplet, sarc_status) %>% 
  summarise(n= n(), age1 = mean(age1), age2 = mean(age2), age3 = mean(age3), .groups = "drop") %>% 
  separate(triplet, into = c("source","to", "destination"), sep = " ") %>%
  mutate(triplet = paste(source,to,destination)) %>% 
  group_by(triplet) %>% mutate(triplet_frequency =sum(n)) %>% ungroup() %>% 
  arrange(desc(n)) %>% 
  group_by(sarc_status) %>% mutate(rank = row_number()) %>% ungroup() %>%
  add_count(triplet) %>%
  mutate(triplet = fct_reorder(triplet, desc(triplet_frequency), .fun = min)) %>% 
  arrange(age1) %>% 
  pivot_longer(cols = c(age1,age2,age3)) %>% 
  group_by(rank, sarc_status) %>% 
  mutate(end = lead(value),
         start = lag(value),
         end = max(end,na.rm = T),
         start = min(start,na.rm = T)) %>% 
  mutate(name = case_when(name=="age1"~source,
                          name=="age2"~to,
                          T~destination)) 


triplets %>% 
  filter(sarc_status %in% c("SARC(+)", "SARC(-)")) %>%
  group_by(triplet) %>% filter(row_number()==1) %>% arrange(desc(triplet_frequency)) %>% 
  ggplot(aes(x= triplet_frequency))+
  geom_histogram(binwidth = 1)

triplets %>% 
  filter(sarc_status %in% c("SARC(+)", "SARC(-)")) %>%
  filter(rank<11) %>%  
  group_by(triplet) %>% mutate(min_start = min(value)) %>% ungroup() %>% 
  mutate(age_rank = dense_rank(desc(min_start)),
         name = case_when(name=="arrhythmia_nsvt"~"NSVT",
                          name=="obstruction"~"LV obstruction",
                          name=="srt"~"SRT",
                          name=="htn"~"Hypertension",
                          name=="icd"~"ICD",
                          name=="nyha_hf"~"NYHA III-IV",
                          name=="af"~"Atrial Fibrillation",
                          #name=="nyha_hf"~"HF symptoms",
                          T~name),
         name = fct_infreq(name),
  ) %>% 
  ggplot(aes(y=age_rank, x= value, color = name, yend =age_rank, xend =end))+
  geom_text(aes(label = paste(n), x=end+.5), color = "black", size = 3, hjust = 0)+
  geom_segment(color= "black")+
  scale_y_discrete(breaks = seq(0,25,1))+
  scale_x_continuous(breaks = seq(0,70,2))+
  #scale_color_manual(name=NULL ,values = c(ggsci::pal_jama()(7)))+
  
  labs(x= "Mean age at occurence",
       y= "")+
  scale_size(range = c(2,5), name = NULL, breaks = NULL)+
  scale_shape_manual(values = c(16,18))+
  #scale_color_scico_d(palette = "vanimo")+
  #geom_text(aes(x=10, label = source))+
  geom_point(aes(size = 1/rank, shape = sarc_status),  show.legend = T)+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text.x = ggtext::element_markdown(family = "Roboto", color = "black", size = 10),
        axis.text.y =element_blank(),# element_markdown(family = "Roboto", color = "black"),
        axis.title = ggtext::element_markdown(family = "Roboto", color = "black"),
        panel.grid.major = element_line(color = "gray89"),
        axis.ticks.y = element_blank(),
        strip.text = element_text(family = "Roboto", color = "white", face = "bold", size = 12),
        strip.background = element_rect(fill = "#be1e32"),
        legend.position = #c(.6,.2),
          "bottom",
        legend.direction = #"vertical",
          "horizontal",
        legend.key.size = unit(1, "lines"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray99", #color = "black", linetype = 1
        ),
        legend.text = element_text(family = "Roboto", color = "black", size = 10)
  )+
  guides(shape = guide_legend(override.aes = list(size = 4)),
         color = guide_legend(override.aes = list(size = 4)))+
  coord_cartesian(xlim = c(36,66), ylim = c(0.4,16.6), expand = F)
facet_wrap(~sarc_status, scales = "free_x")
ggsave(filename = "Triplets.tiff", compression = "lzw", height = 16, width = 18, units = "cm", dpi =1200)


###########
# This section includes a relative risk of triplet-events
#######
triplets %>% ungroup() %>% 
  # filter(rank<=10) %>% 
  select(sarc_status,triplet, n) %>% 
  mutate(sarc_status = if_else(sarc_status=="SARC(+)","sarc","non")) %>%
  group_by(triplet, sarc_status) %>% filter(row_number()==1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sarc_status, values_from = n) %>% 
  arrange(desc(sarc)) %>% #drop_na() %>% #filter(non>1 & sarc>1) %>% 
  mutate(RR = (non/2739)/(sarc/2715)) %>% 
  filter(sarc>=14 | non>=20) %>% 
  select(triplet,RR) %>% left_join(triplets) %>% filter(rank<11) %>% 
  group_by(triplet) %>% mutate(min_start = min(value)) %>% ungroup() %>% 
  mutate(age_rank = dense_rank(desc(min_start)),
         name = case_when(name=="arrhythmia_nsvt"~"NSVT",
                          name=="obstruction"~"LV obstruction",
                          name=="srt"~"SRT",
                          name=="htn"~"Hypertension",
                          name=="icd"~"ICD",
                          name=="nyha_hf"~"NYHA III-IV",
                          name=="af"~"Atrial Fibrillation",
                          #name=="nyha_hf"~"HF symptoms",
                          T~name),
         name = fct_infreq(name),
  ) %>% group_by(triplet) %>% mutate(rr= if_else(row_number()==1, round(1/RR,2), NA_real_)) %>% ungroup() %>% 
  left_join(trip_rr_ci) %>% 
  group_by(triplet) %>% mutate(across(c(.est,.lower,.upper), ~if_else(row_number()==1, round(1/.x,2), NA_real_)),
                               rr_ci = paste(.est," (CI ", .upper, "-", .lower,")", sep = ""),
                               rr_ci = if_else(str_detect(rr_ci, "NA"), NA_character_,rr_ci)) %>% ungroup() %>% 
  
  ggplot(aes(y=age_rank, x= value, color = name, yend =age_rank, xend =end))+
  geom_text(aes(label = paste(n), x=end+.5), color = "black", size = 3, hjust = 0)+
  geom_segment(color= "black")+
  scale_y_discrete(breaks = seq(0,25,1))+
  scale_x_continuous(breaks = seq(0,66,2))+
  scale_color_manual(name=NULL ,values = c(ggsci::pal_jama()(7)))+
  
  labs(x= "Mean age at occurence",
       y= "")+
  scale_size(range = c(2,5), name = NULL, breaks = NULL)+
  scale_shape_manual(values = c(16,18))+
  #scale_color_scico_d(palette = "vanimo")+
  #geom_text(aes(x=10, label = source))+
  geom_point(aes(size = 1/rank, shape = sarc_status),  show.legend = T)+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text.x = ggtext::element_markdown(family = "Roboto", color = "black", size = 10),
        axis.text.y =element_blank(),# element_markdown(family = "Roboto", color = "black"),
        axis.title = ggtext::element_markdown(family = "Roboto", color = "black"),
        panel.grid.major = element_line(color = "gray89"),
        axis.ticks.y = element_blank(),
        strip.text = element_text(family = "Roboto", color = "white", face = "bold", size = 12),
        strip.background = element_rect(fill = "#be1e32"),
        legend.position = #c(.6,.2),
          "bottom",
        legend.direction = #"vertical",
          "horizontal",
        legend.key.size = unit(1, "lines"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray99", #color = "black", linetype = 1
        ),
        legend.text = element_text(family = "Roboto", color = "black", size = 10)
  )+
  guides(shape = guide_legend(override.aes = list(size = 4)),
         color = guide_legend(override.aes = list(size = 4)))+
  coord_cartesian(xlim = c(36,71), ylim = c(0.4,18.6), expand = F)+
  geom_text(aes(x=68.5, label = rr), color = "black", family = "Helvetica")+
  annotate("text", x= 68.5, y=17.4, label = "Relative risk\nin sarc",
           family = "Helvetica", fontface = "bold", vjust = 0.5, size = 3.5)



#######


triplets %>% group_by(sarc_status, triplet) %>% filter(row_number()==1) %>% ungroup() %>% 
  mutate(obstruction = str_detect(triplet, "obstruc")) %>% 
  group_by(obstruction, sarc_status) %>% summarise(n= n(), sum = sum(triplet_frequency))
