df_long1 <- 
  dfposneg %>% 
  mutate(age_hcm = primary_diagnosis_age, # renaming variable
         age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         age_af = age_arrhythmia_a_fib, # renaming af var
         age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm)) %>% 
  dplyr::select(pid,primary_diagnosis_age,first_encounter_age,
                age_hcm, age_htn,age_obstruction,age_srt,
                age_af , age_vt, age_syncope,  age_icd,
                #age_hf,
                age_lvsd, age_nyha_hf, 
                age_stroke,age_htxvad, age_death,
                sarc_status,
  ) %>% 
  pivot_longer(cols = starts_with("age_"), 
               names_to = "outcome",
               values_to = "age") %>%
  mutate(outcome = case_when(str_detect(outcome, "lvef")~"age_lvef50",
                             str_detect(outcome, "age_vad")~"age_transplant",
                             T~outcome)) %>% 
  drop_na() %>% 
  mutate(age = case_when(age==0~primary_diagnosis_age,
                         T~age),
         age1 = case_when(age==0~first_encounter_age,
                          T~age)) %>% 
  filter(outcome != "age_hcm")
# remove rows with missing age values

# group the dataset by patient ID and outcome, and calculate the rank of age within each group
df_long1 <- df_long1 %>% 
  group_by(pid) %>% 
  mutate(rank = rank(age, ties.method = "first")) %>%
  ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>% 
  group_by(pid) %>% arrange(pid,rank) %>% 
  mutate(previous_event = ifelse(rank == 1, "share", lag(outcome)),
         time_to = if_else(age<=primary_diagnosis_age, 0, age-primary_diagnosis_age),
         #time_to = if_else(age1<=first_encounter_age, 0, age1-first_encounter_age)
         ) %>% 
  ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age,time_to) %>% 
  mutate()

df_long1 %>% 
  mutate(outcome = fct_relevel(outcome, "htn", "obstruction", "syncope",  "icd","af", "nyha_hf", 
                               "srt", "stroke", "vt", "lvsd", "htxvad"),
         feature = case_when(outcome =="htn"~"Hypertension",
                             outcome =="obstruction"~"LV obstruction",
                             outcome =="syncope"~"Syncope",
                             outcome =="icd"~"ICD",
                             outcome =="af"~"Atrial fibrillation",
                             outcome =="nyha_hf"~"NYHA III-IV",
                             outcome =="srt"~"Septal reduction therapy",
                             outcome =="stroke"~"Stroke",
                             outcome =="vt"~"Ventricular tachycardia",
                             outcome =="lvsd"~"LV systolic dysfunction",
                             outcome =="htxvad"~"Cardiac transplantation",
                             outcome =="death"~"Death",
                             T~outcome
         ),
         feature = fct_reorder(feature, as.numeric(outcome)),
         #time_to = if_else(time_to>=20,19.9999,time_to)
  ) %>% 
  
  filter(abs(time_to)<20) %>% 
  filter(feature %in% c("Atrial fibrillation", "NYHA III-IV", "Ventricular tachycardia", 
                        "LV systolic dysfunction", "Cardiac transplantation", "Death")) %>% 
  filter(time_to>0) %>% 
  ggplot()+
  #geom_density(aes(x=time_to, fill = sarc_status), alpha = .3)+
  geom_histogram(aes(x=time_to, fill = sarc_status), alpha = .9, position = position_dodge(), 
                 binwidth = 2)+
  #geom_density(aes(x=time_to, fill = sarc_status), alpha = .9, position = position_dodge(), binwidth = 3)+
  facet_wrap(~feature, scales = "free_y", nrow = 3
  )+
  scale_fill_scico_d()+
  labs(x= "Years from diagnosis of HCM",
       y= "Number of patients")+
  scale_x_continuous(breaks = seq(0,20,4))+
#  scale_y_log10()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        panel.grid.major.x = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black", hjust = 1),
        plot.title = element_markdown(family = "Roboto", color = "black"),
        strip.text = element_markdown(family = "Roboto", color = "black", face= "bold"),
        strip.background = element_rect(fill = #"#7AA6DCFF"
                                          #scico(1, palette="cork", begin = .9) 
                                          "gray90"
        ),
        legend.position = "bottom",#c(0.875,.16),
        legend.title = element_blank(), legend.text = element_markdown(family = "Roboto", color = "black")
  )
ggsave(filename = "Figure 3.pdf", device = cairo_pdf, height = 18, width = 24, units = "cm", dpi =2900)
