hr_df<-
  rbind(
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htn, event_htn, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_obstruction, event_obstruction, sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_vt, event_vt, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_stroke, event_stroke, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_death, event_death, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htxvad, event_htxvad, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_lvsd, event_lvsd, 'age',t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_obstruction, event_obstruction, sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_af, event_af, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_nyha_hf, event_nyha_hf, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_lvsd, event_lvsd, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_vt, event_vt, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_stroke, event_stroke, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_death, event_death, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_htxvad, event_htxvad, 'age',t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_htn, event_htn, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_af, event_af,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_lvsd, event_lvsd, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_vt, event_vt, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_stroke, event_stroke, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_death, event_death, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_obstruction, event_obstruction, t2_htxvad, event_htxvad, 'age',t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htn, event_htn, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_obstruction, event_obstruction, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_lvsd, event_lvsd,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad, 'age',t2_srt = t2_srt, event_srt = event_srt),
    
    
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_htn, event_htn, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_obstruction, event_obstruction, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_af, event_af, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_vt, event_vt, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_stroke, event_stroke, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_strata(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_death, event_death,sarc_status, 'age',t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad, 'age',t2_srt = t2_srt, event_srt = event_srt)
    
##    chris_hr_time(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htx_cv_death, event_htx_cv_death, 'age',t2_srt = t2_srt, event_srt = event_srt),
#  chris_hr_time(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_htx_cv_death, event_htx_cv_death, 'age',t2_srt = t2_srt, event_srt = event_srt)
    
  )

hr_inter_df<-
  rbind(
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htn, event_htn, sarc_status , 'age', F,t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_obstruction, event_obstruction, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_vt, event_vt, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_stroke, event_stroke, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_death, event_death, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htxvad, event_htxvad, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_lvsd, event_lvsd, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_obstruction, event_obstruction, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_af, event_af, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_lvsd, event_lvsd, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_vt, event_vt, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_stroke, event_stroke, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_death, event_death, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_htxvad, event_htxvad, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_htn, event_htn, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_lvsd, event_lvsd, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_vt, event_vt, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_stroke, event_stroke, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_death, event_death, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_htxvad, event_htxvad, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htn, event_htn, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_obstruction, event_obstruction, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_lvsd, event_lvsd, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    
    
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htn, event_htn, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_obstruction, event_obstruction, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_af, event_af, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_stroke, event_stroke, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt),
    chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad, sarc_status , 'age', F, t2_srt = t2_srt, event_srt = event_srt)
  )
fig3_df<-
  hr_inter_df %>% 
  filter(str_detect(term, ":")) %>% 
  #  filter(term != 'Composite VA')
  #filter(p.value<.05) %>% 
  select(term, interaction_estimate=estimate, int.low = conf.low, int.high = conf.high, outcome, p_interaction =p.value) %>% 
  mutate(term = str_replace(term, ':SARC',""),
         term = str_replace(term, '(\\+)',""),
         term = str_replace(term, '[(]',""),
         term = str_replace(term, '[)]',"")) %>% 
  right_join(hr_df) %>%
  filter(term!= 'sex') %>% 
  filter(!str_detect(term,'Male')) %>% 
  filter(!str_detect(term,'2')) %>% 
  filter(!str_detect(term,'srt')) %>% 
  mutate(exposure = factor(term),
         .est = estimate,
         .lower = conf.low,
         .upper = conf.high,
         p= p.value) %>% 
  select(exposure, outcome, .est, .lower, .upper, p, statistic, interaction_estimate,
         int.low, int.high, p_interaction) %>% 
  mutate(p = p*37,
         p_di = if_else(p<.05,1,0),
         vuffi = if_else(p<0.05, round(.est,2), NA),
         vuffi = case_when(vuffi>10~round(vuffi,0),
                           vuffi>4~round(vuffi,1),
                           vuffi>1~vuffi,
                           T~NA),
         vuffi_lo = case_when(.lower>10~round(.lower,0),
                              .lower>4~round(.lower,1),
                              .lower>1~round(.lower,2),
                              T~NA),
         vuffi_hi = case_when(.upper>10~round(.upper,0),
                              .upper>4~round(.upper,1),
                              .upper>1~round(.upper,2),
                              T~NA),
         vuffi_label = if_else(p<0.05 & vuffi>1, 
                               #                         glue::glue("{vuffi} \n ({round(.lower,2)}-{round(.upper,2)})"), 
                               glue::glue("{vuffi} \n ({vuffi_lo}-{vuffi_hi})"), 
                               NA_character_),
         
         vuffi_label_int = if_else(!is.na(interaction_estimate), 
                                   #                         glue::glue("{vuffi} \n ({round(.lower,2)}-{round(.upper,2)})"), 
                                   glue::glue("{round({interaction_estimate},2)} \n ({round({int.low},2)}-{round({int.high},2)})"), 
                                   NA_character_),
         vuffi_label_e = if_else(p<0.05 & vuffi>1, 
                                 #                         glue::glue("{vuffi} \n ({round(.lower,2)}-{round(.upper,2)})"), 
                                 glue::glue("{vuffi}"), 
                                 NA_character_),
         exposure = factor(exposure, levels = c("obesity","htn","obstruction","af",
                                                "nyha_hf","lvsd","vt"),
                           labels = c("Obesity","Hypertension","Obstruction","Atrial fibrillation",
                                      "NYHA III-IV","LVSD","Composite VA")),
         outcome = factor(outcome, levels = c("htn","obstruction","af",
                                              "nyha_hf","lvsd","vt", "stroke", "htxvad", "death"),
                          labels = c("Hypertension","Obstruction","Atrial <br> fibrillation",
                                     "NYHA III-IV","LVSD","Composite VA",
                                     "Stroke", "Cardiac <br> transplantation", "Death")),
         
  ) %>% 
  mutate(inter = case_when(interaction_estimate>1~3,
                           interaction_estimate<1~1,
                           T~2),
         inter = factor(inter, levels = c(1,2,3), labels = c("Non-sarcomeric", "Equal", "Sarcomeric"))) %>% 
  filter(!is.na(vuffi)| (exposure== "Hypertension" &outcome== "Stroke")| (exposure== "LVSD" &outcome== "Atrial <br> fibrillation")) %>%
  filter(!(outcome%in% "Hypertension"), !(exposure %in% c('NYHA III-IV'))) %>% 
  mutate(size = if_else(!is.na(vuffi),1,0),
         p_rank = rank(-p)^4)

fig3_df %>% 
  filter(outcome!='Obstruction') %>% 
  ggplot(aes(x=outcome, y=exposure, 
             #fill = inter,
             fill = -log10(p), 
             label = vuffi_label))+
  geom_point(aes(size = size, alpha = size), shape = 21, show.legend = T, color = "white")+
  scale_size(range = c(15,23), guide = "none")+
  scale_alpha(range = c(0,1), guide = "none")+
  #geom_tile(show.legend = F, color = "white", size = 2)+
  geom_text(vjust=.2 ,family = "Roboto", size =3.3, color = "black", fontface = "bold")+
  #ggsci::scale_fill_jama()+
  labs(x= "Outcome", y = "Exposure")+
  #scale_fill_manual(values = c("#7580a2", "#c8c8c8", "#F9CCF9"))+
  #viridis::scale_fill_viridis(option = "H")+
  ggsci::scale_fill_material()+
  theme(panel.background = element_rect(color = "black", fill = "white"),
        axis.title.x = ggtext::element_markdown(color = "black", family = "Roboto",size = 12),
        axis.title.y = ggtext::element_markdown(color = "black", family = "Roboto",size = 12),
        axis.text.x = ggtext::element_markdown(color = "black", family = "Roboto", size = 10),
        axis.text.y = ggtext::element_markdown(color = "black", family = "Roboto", size = 10),
        strip.background = element_rect(fill = "gray"),
        strip.text = ggtext::element_markdown(color = "black", family = "Roboto",
                                              face = "bold"),
        legend.position = "bottom",
        # panel.border = element_rect(color = "black")
  )

ggsave(filename = 'Circ - Figure 5.pdf', device = cairo_pdf , height = 15, width = 20, units = "cm", dpi =1000)
