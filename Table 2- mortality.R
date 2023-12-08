dfposneg %>%  #filter(fu_lvsd_cmp>=0) %>% 
  select(#event_lvef50, 
    #primary_diagnosis_age,
    event_death, 
    death_cause,sarc_status,#sex
  ) %>% 
  mutate(#event_lvef50 = if_else(event_lvef50==1, "LVSD", "No LVSD"),
    death_cause = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                            str_detect(death_cause, "SCD")~"Sudden cardiac death",
                            str_detect(death_cause, "Myoca|Cardio|Strok|CAD")~"Other cardiovascular death",
                            str_detect(death_cause, "Fail|HCM")~"Heart failure",
                            T~death_cause),
    death_cause = fct_infreq(death_cause)) %>% 
  gtsummary::tbl_summary(by = sarc_status,
                         label = list(#event_srt~"Septal reduction therapy",
                                      #event_vad~"Left ventricular assist device",
                                      #event_transplant~"Cardiac transplantation",
                                      event_death~"All-cause mortality",
                                      death_cause~"Causes of death"
                                      #             primary_diagnosis_age~"Age at HCM diagnosis"
                         )
  ) %>% 
  gtsummary::bold_labels() %>% gtsummary::add_p() %>% 
  as_gt() %>% 
  gtExtras::gt_theme_nytimes() %>% 
  tab_header("Table 2: All-cause and cause-specific mortality in sarcomeric and non-sarcomeric hypertrophic cardiomyopathy")  
   gtsave("vuffi.rtf")#%>%
  gtsave("mortality.doc")
  
  
  gtExtras::gt_highlight_rows(
    # Row to highlight
    rows = 2, 
    # Background color
    fill = "white",
    # Bold for target column only
    bold_target_only = F,
    # Select target column
    target_col = characteristic
  )


death_df <-
dfposneg %>%  #filter(fu_lvsd_cmp>=0) %>% 
  select(#event_lvef50, 
    #primary_diagnosis_age,
    event_death, pid,
    death_cause,sarc_status,#sex
  ) %>% 
  mutate(#event_lvef50 = if_else(event_lvef50==1, "LVSD", "No LVSD"),
    death_cause = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                            str_detect(death_cause, "SCD")~"Sudden cardiac death",
                            str_detect(death_cause, "Myoca|Cardio|Strok|CAD")~"Other cardiovascular death",
                            str_detect(death_cause, "Fail|HCM")~"Heart failure",
                            T~death_cause)) %>% 
  filter(event_death==1) %>% pivot_wider(names_from = death_cause, values_from = event_death) %>% 
  mutate(across(3:8, ~if_else(is.na(.x),0,.x)))
  death_df
library(epiR);library(epitools)
 rbind( 
  death_df %>% 
  rr_func(sarc_status, 3, "Non-cardiovascular death"),
  
  death_df %>% 
    rr_func(sarc_status, 4, colnames(death_df)[4]),
  
  death_df %>% 
    rr_func(sarc_status, 5, colnames(death_df)[5]),
  
  death_df %>% 
    rr_func(sarc_status, 6, colnames(death_df)[6]),
  
  death_df %>% 
    rr_func(sarc_status, 7, colnames(death_df)[7]),
  
  death_df %>% 
    rr_func(sarc_status, 8, colnames(death_df)[8])) %>% drop_na()
 
 dfposneg %>% surv_fit(Surv(t2_death-first_encounter_age, event_death)~sarc_status, data=.) %>% 
   gtsummary::tbl_survfit( times = c(1,5,10,15,20))
 
 
 dfposneg %>%  #filter(fu_lvsd_cmp>=0) %>% 
   select(#event_lvef50, 
     #primary_diagnosis_age,
     event_death, pid,
     death_cause,sarc_status,#sex
   ) %>% 
   mutate(#event_lvef50 = if_else(event_lvef50==1, "LVSD", "No LVSD"),
     death_causes = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                              str_detect(death_cause, "SCD")~"Sudden cardiac death",
                              str_detect(death_cause, "Myoca|Cardio|Strok|CAD")~"Other cardiovascular death",
                              str_detect(death_cause, "Fail|HCM")~"Heart failure",
                              T~death_cause),
     event_hcm_death = if_else(death_causes %in% c("Sudden cardiac death","Heart failure"),1,0)) %>% 
   
   janitor::tabyl(sarc_status, event_hcm_death) %>% 
   fisher.test()
 
 