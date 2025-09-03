library(tidyverse); library(gt); library(gtsummary); library(flextable)
 library(stringr)
 # Creating a massive table containing 6 groups of patients in the study
 dfposneg<- df %>%  filter(sarc_status %in% c("SARC(+)","SARC(-)"#,"SARC(U)"
 ))
 dfposneg %>% #filter(primary_diagnosis_age<18) %>% 
   select(sex, primary_diagnosis_age, f_hx_scd_esc,#echo_septal_morphologyx,
          first_encounter_age, is_proband,
          race, 
          sarc_status,sysbp,diabp,bmi,obese,bsa,nyha,lge,cmri_lge_percent0, cmri_lvef0,cmri_lv_mass0,cmri_max_lvt0,
          
          lvsd,severe_lvsd, htn,af,obstruction,
          event_icd, SCD_aSCD,
          nyha_hf, stroke, syncope,
          echo_max_lvt,echo_lvef, echo_la,echo_lvidd,echo_lvidd_bsa,echo_lvids,echo_lvids_bsa,echo_lvot_gradient,
          echo_esc_risk_score0,esc_risk) %>%
   mutate(#race = case_when(race %in% c("Black", "White", "Asian", "Other or Not Reported")~race,
          #                 T~"Other or Not Reported"),
     child_diag = if_else(primary_diagnosis_age<=18, 'Yes', 'No', NA),
     
          race = case_when(race %in% c("Black", "White", "Asian", "Other or Not Reported",
                                       'Native Hawaiian or Other Pacific Islander',
                                       'More than One',
                                       'American Indian or Alaska Native'
                                       )~race,
                           T~"Other or Not Reported"),
          
          race = fct_infreq(race)) %>% 
   gtsummary::tbl_summary(by = sarc_status,
                          value = list(),
                          statistic = list(primary_diagnosis_age~"{median} [{p25}, {p75}]",
                                           echo_max_lvt~"{median} [{p25}, {p75}]",
                                           echo_lvef~"{mean} ± {sd}",
                                           cmri_lvef0~"{mean} ± {sd}",
                                           echo_la~"{mean} ± {sd}"
                          ),
                          digits = list(primary_diagnosis_age~c(1,1),
                                        echo_max_lvt~c(1,1),
                                        echo_lvef~c(1,1),
                                        cmri_lvef0~c(1,1),
                                        echo_la~c(1,1)),
                          label = list(primary_diagnosis_age~"Age at HCM diagnosis",
                                       first_encounter_age~"Age at inclusion in SHaRe",
                                       is_proband~"Proband",
                                       SCD_aSCD~"History of cardiac arrest",
                                       race~"Race")) %>% gtsummary::add_p() %>% 
   gtsummary::bold_labels() #%>% #add_overall() 
 as_gt()  #%>% gtsave("vuf.rtf")
 
 
 #################
 dfposneg %>% filter(is_pediatric==T) %>% 
   select(sex, primary_diagnosis_age, f_hx_scd_esc,#echo_septal_morphologyx,
          first_encounter_age, is_proband, race, 
          sarc_status,sysbp,diabp,bmi,obese,bsa,nyha,lge,cmri_lge_percent0, cmri_lvef0,cmri_lv_mass0,cmri_max_lvt0,
          
          lvsd,severe_lvsd, htn,af,obstruction,
          event_icd, SCD_aSCD,
          nyha_hf, stroke, syncope,
          echo_max_lvt,echo_lvef, echo_la,echo_lvidd,echo_lvidd_bsa,echo_lvids,echo_lvids_bsa,echo_lvot_gradient,
          echo_esc_risk_score0,esc_risk) %>%
   mutate(race = case_when(race %in% c("Black", "White", "Asian", "Other or Not Reported")~race,
                           T~"Other or Not Reported"),
          race = fct_infreq(race)) %>% 
   gtsummary::tbl_summary(by = sarc_status,
                          value = list(),
                          statistic = list(primary_diagnosis_age~"{median} ({p25} to {p75})",
                                           echo_max_lvt~"{median} ({p25} to {p75})",
                                           echo_lvef~"{mean} ± {sd}",
                                           cmri_lvef0~"{mean} ± {sd}",
                                           echo_la~"{mean} ± {sd}"
                          ),
                          digits = list(primary_diagnosis_age~c(1,1),
                                        echo_max_lvt~c(1,1),
                                        echo_lvef~c(1,1),
                                        cmri_lvef0~c(1,1),
                                        echo_la~c(1,1)),
                          label = list(primary_diagnosis_age~"Age at HCM diagnosis",
                                       first_encounter_age~"Age at inclusion in SHaRe",
                                       is_proband~"Proband",
                                       SCD_aSCD~"History of cardiac arrest",
                                       race~"Race")) %>% gtsummary::add_p() %>% 
   gtsummary::bold_labels() #%>% #add_overall() 
 as_gt()  #%>% gtsave("vuf.rtf")
 
 
 
 #####
 
 dfposneg %>%  #filter(fu_lvsd_cmp>=0) %>% 
   select(#event_lvef50, 
     #primary_diagnosis_age,
     event_srt,event_vad,
     event_transplant, event_death, 
     death_cause,sarc_status,sex
   ) %>% 
   mutate(#event_lvef50 = if_else(event_lvef50==1, "LVSD", "No LVSD"),
     death_cause = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                             str_detect(death_cause, "SCD")~"Sudden cardiac death",
                             str_detect(death_cause, "Myoca|Cardio|Strok|CAD")~"Other cardiovascular death",
                             str_detect(death_cause, "Fail|HCM")~"Heart failure",
                             T~death_cause),
     death_cause = fct_infreq(death_cause)) %>% 
   gtsummary::tbl_summary(by = sex,
                          label = list(event_srt~"Septal reduction therapy",
                                       event_vad~"Left ventricular assist device",
                                       event_transplant~"Cardiac transplantation",
                                       event_death~"All-cause mortality",
                                       death_cause~"Causes of death"
                                       #             primary_diagnosis_age~"Age at HCM diagnosis"
                          )
   ) %>% 
   gtsummary::bold_labels() %>% gtsummary::add_p() #%>% gtsummary::as_gt() %>% gtsave("death.png")
 
 coxph(Surv(primary_diagnosis_age,t2_death, event_death)~sarc_status,data = dfposneg)
 
 coxph(Surv(t2_death-first_encounter_age, event_death)~sarc_status,data = dfposneg)
 
 
 ##################
 
 #################
 dfposneg %>% filter(first_encounter_age<18) %>% 
   select(sex, primary_diagnosis_age, f_hx_scd_esc,#echo_septal_morphologyx,
          first_encounter_age, is_proband, race, 
          sarc_status,sysbp,diabp,bmi,obese,bsa,nyha,lge,cmri_lge_percent0, cmri_lvef0,cmri_lv_mass0,cmri_max_lvt0,
          
          lvsd,severe_lvsd, htn,af,obstruction,
          event_icd, SCD_aSCD,
          nyha_hf, stroke, syncope,
          echo_max_lvt,echo_lvef, echo_la,echo_lvidd,echo_lvidd_bsa,echo_lvids,echo_lvids_bsa,echo_lvot_gradient,
          echo_esc_risk_score0,esc_risk) %>%
   mutate(race = case_when(race %in% c("Black", "White", "Asian", "Other or Not Reported")~race,
                           T~"Other or Not Reported"),
          race = fct_infreq(race)) %>% 
   gtsummary::tbl_summary(by = sarc_status,
                          value = list(),
                          statistic = list(primary_diagnosis_age~"{median} ({p25} to {p75})",
                                           echo_max_lvt~"{median} ({p25} to {p75})",
                                           echo_lvef~"{mean} ± {sd}",
                                           cmri_lvef0~"{mean} ± {sd}",
                                           echo_la~"{mean} ± {sd}"
                          ),
                          digits = list(primary_diagnosis_age~c(1,1),
                                        echo_max_lvt~c(1,1),
                                        echo_lvef~c(1,1),
                                        cmri_lvef0~c(1,1),
                                        echo_la~c(1,1)),
                          label = list(primary_diagnosis_age~"Age at HCM diagnosis",
                                       first_encounter_age~"Age at inclusion in SHaRe",
                                       is_proband~"Proband",
                                       SCD_aSCD~"History of cardiac arrest",
                                       race~"Race")) %>% gtsummary::add_p() %>% 
   gtsummary::bold_labels() #%>% #add_overall() 
 as_gt()  #%>% gtsave("vuf.rtf")
 
 
 
 #####
 
 dfposneg %>%  #filter(fu_lvsd_cmp>=0) %>% 
   select(#event_lvef50, 
     #primary_diagnosis_age,
     event_srt,event_vad,
     event_transplant, event_death, 
     death_cause,sarc_status,sex
   ) %>% 
   mutate(#event_lvef50 = if_else(event_lvef50==1, "LVSD", "No LVSD"),
     death_cause = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                             str_detect(death_cause, "SCD")~"Sudden cardiac death",
                             str_detect(death_cause, "Myoca|Cardio|Strok|CAD")~"Other cardiovascular death",
                             str_detect(death_cause, "Fail|HCM")~"Heart failure",
                             T~death_cause),
     death_cause = fct_infreq(death_cause)) %>% 
   gtsummary::tbl_summary(by = sex,
                          label = list(event_srt~"Septal reduction therapy",
                                       event_vad~"Left ventricular assist device",
                                       event_transplant~"Cardiac transplantation",
                                       event_death~"All-cause mortality",
                                       death_cause~"Causes of death"
                                       #             primary_diagnosis_age~"Age at HCM diagnosis"
                          )
   ) %>% 
   gtsummary::bold_labels() %>% gtsummary::add_p() #%>% gtsummary::as_gt() %>% gtsave("death.png")
 