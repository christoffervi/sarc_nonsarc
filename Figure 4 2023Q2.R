rbind(
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_obstruction, event_obstruction,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
  
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_obstruction, event_obstruction, sarc_status, 'age', F),
  
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
  
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_lvsd, event_lvsd,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death,sarc_status, 'age', F),
  
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_af, event_af,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_lvsd, event_lvsd,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_htxvad, event_htxvad,sarc_status, 'age', F),
  chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_death, event_death,sarc_status, 'age', F)
) %>% 
  separate(term, c('term', 'sarc'), ":") %>% 
  filter(!is.na(sarc)) %>% 
  
  mutate(exposure = factor(term),
         .est = estimate,
         .lower = conf.low,
         .upper = conf.high,
         p= p.value) %>% 
  select(term,exposure, outcome, .est, .lower, .upper, p, statistic, sarc) %>% 
  right_join(
    
    rbind(
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_obstruction, event_obstruction, sarc_status, 'age', F),
      
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_lvsd, event_lvsd,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death,sarc_status, 'age', F),
      
      # chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      # chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_af, event_af,sarc_status, 'age', F),
      # chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_death, event_death,sarc_status, 'age', F),
      # chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      # 
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death,sarc_status, 'age', F),
      
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_af, event_af,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_lvsd, event_lvsd,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_death, event_death,sarc_status, 'age', F)
    ) %>% 
      filter(str_detect(term, ":")) %>% 
      separate(term, c("term", "vuf")) %>%
      mutate(term = if_else(str_detect(term, 'nyha'), 'nyha_hf', term)) %>% 
      select(term, outcome, effect = estimate, t_stat = statistic, p_dif = p.value)) %>% 
  filter(p_dif<.05) %>%
  
  mutate(#p = p*56,
    p_di = if_else(p<.05,1,0),
    vuffi = if_else(p<1.05, round(.est,2), NA),
    vuffi = case_when(vuffi>10~round(vuffi,0),
                      vuffi>4~round(vuffi,1),
                      vuffi>0~vuffi,
                      T~NA),
    vuffi_lo = case_when(.lower>10~round(.lower,0),
                         .lower>4~round(.lower,1),
                         .lower>0~round(.lower,2),
                         T~NA),
    vuffi_hi = case_when(.upper>10~round(.upper,0),
                         .upper>4~round(.upper,1),
                         .upper>0~round(.upper,2),
                         T~NA),
    vuffi_label = #if_else(p<0.05 & vuffi>1, 
      #                         glue::glue("{vuffi} \n ({round(.lower,2)}-{round(.upper,2)})"), 
      glue::glue("{vuffi} \n ({vuffi_lo}-{vuffi_hi})"), 
    #       NA_character_),
    
    
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
  filter(#!is.na(vuffi)
    #             (exposure== "Hypertension" &outcome== "Stroke")
    #  (exposure== "LVSD" &outcome== "Atrial <br> fibrillation")
  ) %>%
  filter(outcome!= "Hypertension") %>% 
  #  mutate(size = if_else(!is.na(vuffi),1,0),
  #        p_rank = rank(-p)^4) %>% 
  ggplot(aes(x=outcome, y=exposure, 
             
             label = vuffi_label))+
  geom_point(aes(#fill = t_stat^2
    fill = -log10(p)
  ), shape = 21, show.legend = F, color = "white",size=23)+
  scale_size(range = c(15,23), guide = "none")+
  #scale_alpha(range = c(0,1), guide = "none")+
  #geom_tile(show.legend = F, color = "white", size = 2)+
  geom_text(vjust=.2 ,family = "Roboto", size =3.3, color = "black", fontface = "bold")+
  #ggsci::scale_fill_jama()+
  labs(x= "Outcome", y = "Exposure",
       caption = )+
  scale_fill_gradient2(low = '#7580a2', mid = 'white', high = '#F9CCF9', midpoint = 0)+
  #scale_fill_manual(values = c("#7580a2", "#c8c8c8", "#F9CCF9"))+
  #  ggsci::scale_fill_gsea()+
  ggsci::scale_fill_material()+
  theme(panel.background = element_rect(color = "black", fill = "white"),
        axis.title = ggtext::element_markdown(color = "black", family = "Roboto"),
        axis.text.x = ggtext::element_markdown(color = "black", family = "Roboto", size = 7),
        axis.text.y = ggtext::element_markdown(color = "black", family = "Roboto", size = 7),
        strip.background = element_rect(fill = "gray"),
        strip.text = ggtext::element_markdown(color = "black", family = "Roboto",
                                              face = "bold"),
        legend.position = "bottom",
        # panel.border = element_rect(color = "black")
  )+
  facet_wrap(~sarc)
ggsave(filename = "Figure 4 2023Q2.pdf", device = cairo_pdf, height = 13, width = 26, units = "cm", dpi =2900)

