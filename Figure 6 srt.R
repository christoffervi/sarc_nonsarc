chris_colors_1 <- c('#48554C', '#8D4D5C','#57B4A9',  '#80A0E3', '#EAE133','#734A7A', '#FA6F38','#A9BCDB','#666A6F','#302A20','#28534B')

fig4_df<-
  rbind(
    #Obesity
    #  chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_obesity, event_obesity, t2_obstruction, event_obstruction,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af,sarc_status, 'age', F),
    #    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_death, event_death,sarc_status, 'age', F),
    
    #Hypertension
    #    chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_htn, event_htn, t2_obstruction, event_obstruction,sarc_status, 'age', F),
    #    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_af, event_af,sarc_status, 'age', F),
    
    # LV Obstruction
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_vt, event_vt,sarc_status, 'age', F),
    
    #AF
    #  chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_af, event_af, t2_obstruction, event_obstruction,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_af, event_af, t2_lvsd, event_lvsd,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death,sarc_status, 'age', F),
    
    # LVSD
    #  chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_obstruction, event_obstruction,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt,sarc_status, 'age', F),
    #    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_stroke, event_stroke,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad,sarc_status, 'age', F),
    chris_hr_time_interaction2(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death,sarc_status, 'age', F)
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
      #Obesity
      chris_hr_time_interaction(dfposneg, pid, echo_age0, t2_obesity, event_obesity, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af,sarc_status, 'age', F),
      #  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_death, event_death,sarc_status, 'age', F),
      
      #Hypertension
      chris_hr_time_interaction(dfposneg, pid, echo_age0, t2_htn, event_htn, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      #  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_af, event_af,sarc_status, 'age', F),
      
      # LV Obstruction
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_vt, event_vt,sarc_status, 'age', F),
      
      #AF
      #  chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_af, event_af, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_af, event_af,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, echo_age0, t2_af, event_af, t2_lvsd, event_lvsd,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death,sarc_status, 'age', F),
      
      # LVSD
      #  chris_hr_time_interaction2(dfposneg, pid, echo_age0, t2_lvsd, event_lvsd, t2_obstruction, event_obstruction,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt,sarc_status, 'age', F),
      #      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_stroke, event_stroke,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad,sarc_status, 'age', F),
      chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death,sarc_status, 'age', F)
    ) %>%
      filter(str_detect(term, ":")) %>%
      separate(term, c("term", "vuf")) %>%
      mutate(term = if_else(str_detect(term, 'nyha'), 'nyha_hf', term)) %>%
      select(term, outcome, effect = estimate, effect_lo = conf.low, effect_hi = conf.high, t_stat = statistic, p_dif = p.value)) %>%
  #filter(p_dif<.05) %>%
  
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
  filter(!is.na(.est)) %>%
  mutate(sarc = factor(sarc, levels = c('SARC(-)', 'SARC(+)'), labels = c('Non-sarcomeric HCM', 'Sarcomeric HCM'))) %>%
  #  mutate(size = if_else(!is.na(vuffi),1,0),
  #        p_rank = rank(-p)^4) %>%
  
  arrange(desc(exposure),desc(outcome)) %>%
  group_by(sarc) %>%
  mutate(r= row_number()) %>% #select(exposure, outcome, r)
  #  ungroup() %>%
  mutate(r = case_when(r<7~r,
                       r<13~r+1,
                       r<20~r+2,
                       r>=20~r+3,
                       T~r)) %>%
  group_by(exposure) %>%
  mutate(rmax = max(r)+1) %>%
  ungroup() %>%
  mutate(p_difs = case_when(p_dif >0.1 ~glue::glue('{round({p_dif},2)}'),
                            p_dif >0.05 ~glue::glue('{round({p_dif},3)}'),
                            p_dif >0.001 ~glue::glue('**{round({p_dif},3)}**'),
                            T~'**<0.001**'),
         p_dif = case_when(p_dif >0.1 ~glue::glue('{round({p_dif},2)}'),
                           p_dif >0.05 ~glue::glue('{round({p_dif},3)}'),
                           p_dif >0.001 ~glue::glue('{round({p_dif},3)}'),
                           T~'<0.001'))


fig4_df %>% 
  mutate(exposure_outcome = glue::glue('**{exposure}** to *{outcome}*'),
         exposure_outcome = fct_reorder2(exposure_outcome, outcome, exposure)) %>% 
  filter(str_detect(p_dif,'.0')) %>% 
  ggplot(aes(y=.est, x=exposure_outcome, ymin = .lower, ymax = .upper, group = sarc ,
             
             label = vuffi_label))+
  labs(title = 'Interactions between genetic status and **exposures** on *outcomes*',
       x='**Exposures and outcomes**',
       y = '**Hazard ratio** <br> Combined effect of genetic status and exposure')+
  annotate('text', x = 8.5, y = c(8 , 24), label = c('Effect ratio', 'P for interaction'),
           fontface = 'bold', family = 'Roboto')+
  
  #geom_text(aes(y=0.01, x = rmax, label = exposure), fontface = 'bold', hjust = 0)+
  geom_errorbar(width =.1,aes(color = sarc),
                position = posi)+
  geom_point(position = posi, aes(fill = sarc), shape = 21, size = 4)+
  geom_text(aes(y= 8, label = round(effect,2)), family = 'Roboto')+
  geom_richtext(aes(y= 24, label = p_dif), family = 'Roboto',
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_segment(aes(y = 1, yend = 1, x= 0.6, xend = 8.6), color = 'black', linetype = 2)+
  scale_y_log10(breaks = c(.2,.5,1,2,4))+
  scale_x_discrete()+
  coord_flip(ylim =c (0.5,32), clip = 'off')+
  col_chris+fill_chris+
  #  scale_color_scico_d()+
  #  scale_fill_scico_d()+
  #  scale_color_manual(values = chris_colors_3[2:3])+
  #  scale_fill_manual(values = chris_colors_3[2:3])+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(colour = 'black'),
        plot.title = element_textbox(family = "Roboto", color = "black",
                                     padding = margin(5.5, 5.5, 5.5, 5.5),
                                     margin = margin(0, 0, 10, 0),
                                     hjust = 0,
                                     fill = "white"),
        axis.text.x = element_markdown(family = "Roboto", color = "black", size =11),
        axis.text.y = element_markdown(family = "Roboto", color = "black", size =11),
        axis.title.x = element_markdown(family = "Roboto", color = "black", size =11, hjust = .2),
        axis.title.y = element_markdown(family = "Roboto", color = "black", size =14, hjust = 1),
        legend.position = 'bottom', #c(.8,.0),
        legend.title = element_blank(),
        legend.key = element_blank(),
  )
ggsave(filename = 'Figure6_2025_03_06.pdf', device = cairo_pdf , height = 6.3, width = 8, units = "in", dpi =1000)
ggsave(filename = 'Figure6_2025_03_06.tiff', compression = 'lzw', height = 6.3, width = 8, units = "in", dpi =2000)
