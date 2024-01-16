hf %>%
  mutate(time1 = t2_obstruction-echo_age0,
         sarc = fct_rev(factor(sarc_status))
         )%>%
  filter(time1>0) %>%
  coxph(Surv(time1, event_obstruction)~sarc+primary_diagnosis_age+sex+obese+is_proband+htn, data = .)%>% 
  broom::tidy(exponentiate=T, conf.int = T)




hf %>%
  mutate(time1 = t2_af-first_encounter_age,
         sarc = factor(sarc_status)
  )%>%
  filter(time1>0) %>%
  coxph(Surv(time1, event_af)~sarc+first_encounter_age+sex+obese+htn, data = .)%>% 
  broom::tidy(exponentiate=T, conf.int = T)


hf$event_hcm_death
hf %>% 
  mutate(time1 = t2_death-first_encounter_age ,
         ) %>% 
  filter(time1>0) %>% 
  coxph(Surv(time1, event_hcm_death)~sarc_status, data = .)%>% 
  broom::tidy(exponentiate = T, conf.int=T) 

hf %>% 
  mutate(time1 = t2_death-first_encounter_age ,
  ) %>% 
  filter(time1>0) %>% 
  coxph(Surv(first_encounter_age,t2_death, event_hcm_death)~sarc_status, data = .)%>% 
  broom::tidy(exponentiate = T, conf.int=T) 

hf %>% 
  mutate(time1 = t2_death-first_encounter_age ,
  ) %>% 
  filter(time1>0) %>% 
  coxph(Surv(first_encounter_age,t2_death, event_death)~sarc_status, data = .)%>% 
  broom::tidy(exponentiate = T, conf.int=T) 

hf %>% tabyl(sarc_status, event_hcm_death) %>% fisher.test()
