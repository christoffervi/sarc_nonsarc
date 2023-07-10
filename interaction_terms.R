

fig4_df<-
rbind(
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obesity, event_obesity, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_htn, event_htn, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_obstruction, event_obstruction, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_af, event_af, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_nyha_hf, event_nyha_hf, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_vt, event_vt, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_lvsd, event_lvsd, t2_htxvad, event_htxvad, sarc_status , 'age', F),
  
  
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_htn, event_htn, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_obstruction, event_obstruction, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_nyha_hf, event_nyha_hf, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_lvsd, event_lvsd, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_af, event_af, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_stroke, event_stroke, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_death, event_death, sarc_status , 'age', F),
  chris_hr_time_interaction(dfposneg, pid, first_encounter_age, t2_vt, event_vt, t2_htxvad, event_htxvad, sarc_status , 'age', F)
) %>% 
  
  mutate(exposure = factor(term),
         .est = estimate,
         .lower = conf.low,
         .upper = conf.high,
         p= p.value) %>% 
  select(exposure, outcome, .est, .lower, .upper, p, statistic)
  

fig4_df %>% mutate(interaction = case_when(p<.05~.est, T~NA),
                   interaction = case_when(str_detect(exposure,":")~interaction, T~NA),
                   interaction = lead(interaction)) %>% 
  filter(!str_detect(exposure, ":"))

  ######
  fig4_df %>% 
  mutate(interaction = case_when(p<.05~.est, T~NA),
             interaction = case_when(str_detect(exposure,":")~interaction, T~NA),
             interaction = lead(interaction)) %>% 
  filter(!str_detect(exposure, ":")) %>% 
  mutate(p = p,
         p_di = if_else(p<.05,1,0),
         vuffi = if_else(p<0.05, round(.est,2), NA),
         vuffi = case_when(vuffi>10~round(vuffi,0),
                           vuffi>2~round(vuffi,1),
                           vuffi>1~vuffi,
                           T~NA),
         vuffi_lo = case_when(.lower>10~round(.lower,0),
                              .lower>2~round(.lower,1),
                              .lower>1~round(.lower,2),
                              T~NA),
         vuffi_hi = case_when(.upper>10~round(.upper,0),
                              .upper>2~round(.upper,1),
                              .upper>1~round(.upper,2),
                              T~NA),
         vuffi_label = if_else(p<0.05 & vuffi>1, 
                               #                         glue::glue("{vuffi} \n ({round(.lower,2)}-{round(.upper,2)})"), 
                               glue::glue("{vuffi} \n ({vuffi_lo}-{vuffi_hi})"), 
                               NA_character_),
         exposure = factor(exposure, levels = c("obesity","htn","obstruction","af",
                                                "nyha_hf","lvsd","vt",
                                                "obesity:SARC(+)","htn:SARC(+)","obstruction:SARC(+)","af:SARC(+)",
                                                "nyha_hf:SARC(+)","lvsd:SARC(+)","vt:SARC(+)"),
                           labels = c("Obesity","Hypertension","Obstruction","Atrial fibrillation",
                                      "NYHA III-IV","LVSD","Composite VT",
                                      "Obesity - Sarcomeric HCM","Hypertension - Sarcomeric HCM","Obstruction - Sarcomeric HCM",
                                      "Atrial fibrillation - Sarcomeric HCM",
                                      "NYHA III-IV - Sarcomeric HCM","LVSD - Sarcomeric HCM","Composite VT - Sarcomeric HCM")),
         outcome = factor(outcome, levels = c("htn","obstruction","af",
                                              "nyha_hf","lvsd","vt", "stroke", "htxvad", "death"),
                          labels = c("Hypertension","Obstruction","Atrial <br> fibrillation",
                                     "NYHA III-IV","LVSD","Composite VT",
                                     "Stroke", "Cardiac <br> transplantation", "Death")))-> fig4_df2
fig4_df2
#x<-
  fig4_df2%>% 
  # filter(!is.na(vuffi)| (exposure== "LVSD" &outcome== "Composite VT")| (exposure== "LVSD" &outcome== "Atrial <br> fibrillation")) %>%
  mutate(size = if_else(!is.na(vuffi),1,0),
         inter = if_else(interaction>1, "3", "1", "2")) %>% 
  ggplot(aes(x=outcome, y=exposure, fill = inter, label = vuffi_label))+
  geom_point(aes(size = size, alpha = size), shape = 21, show.legend = T, color = "white")+
  scale_size(range = c(0,22), guide = "none")+
  scale_alpha(range = c(0,1), guide = "none")+
  #geom_tile(show.legend = F, color = "white", size = 2)+
  geom_text(family = "Roboto", size =3, color = "black", fontface = "bold")+
  #ggsci::scale_fill_gsea()+
  #ggsci::scale_color_gsea()+
#  ggsci::scale_color_material()+
#  ggsci::scale_fill_material(breaks = c(0,10,20,30,40))+
  labs(x= "Outcome", y = "Exposure")+
  scale_fill_scico_d(palette = "berlin")+
  #viridis::scale_fill_viridis(option = "H")+
  theme(panel.background = element_rect(color = "black", fill = "white"),
        axis.title = ggtext::element_markdown(color = "black", family = "Roboto"),
        axis.text.x = ggtext::element_markdown(color = "black", family = "Roboto", size = 7),
        axis.text.y = ggtext::element_markdown(color = "black", family = "Roboto", size = 7),
        strip.background = element_rect(fill = "gray"),
        strip.text = ggtext::element_markdown(color = "black", family = "Roboto",
                                              face = "bold"),
        legend.position = "bottom",
        # panel.border = element_rect(color = "black")
  )
x
ggsave(filename = "cox_fig_unadjusted_timevar.pdf", device = cairo_pdf, height = 18, width = 36, units = "cm", dpi =2900)
# dfposneg<-
#   dfposneg %>% mutate(af_nyha = if_else(t2_af<t2_nyha_hf, event_nyha_hf,1,0),
#                       af_nyha_age = glue::glue("{t2_af, event_af};{t2_nyha_hf, event_nyha_hf}"),
#                       af_lvsd = if_else(t2_af, event_af<t2_lvsd, event_lvsd,1,0),
#                       af_lvsd_age = glue::glue("{t2_af, event_af};{t2_lvsd, event_lvsd}"),
#                       af_stroke = if_else(t2_af, event_af<t2_stroke, event_stroke,1,0),
#                       af_stroke_age = glue::glue("{t2_af, event_af};{t2_stroke, event_stroke}"),
#                       af_htxvad = if_else(t2_af, event_af<t2_htxvad, event_htxvad,1,0),
#                       af_htxvad_age = glue::glue("{t2_af, event_af};{t2_htxvad, event_htxvad}"),
#                       af_death = if_else(t2_af, event_af<t2_death, event_death,1,0),
#                       af_death_age = glue::glue("{t2_af, event_af};{t2_death, event_death}"),
#                       
#                       af_nyha_age = glue::glue("{t2_af, event_af};{t2_nyha_hf, event_nyha_hf}"),
#                       af_lvsd_age = glue::glue("{t2_af, event_af};{t2_lvsd, event_lvsd}"),
#                       af_stroke_age = glue::glue("{t2_af, event_af};{t2_stroke, event_stroke}"),
#                       af_htxvad_age = glue::glue("{t2_af, event_af};{t2_htxvad, event_htxvad}"),
#                       af_death_age = glue::glue("{t2_af, event_af};{t2_death, event_death}"),
#                       
#                       
#                       lvsd_htxvad = glue::glue("{t2_lvsd, event_lvsd};{t2_htxvad, event_htxvad}"),
#                       lvsd_death = glue::glue("{t2_lvsd, event_lvsd};{t2_death, event_death}"),
#                       
#                       vt_lvsd = glue::glue("{t2_vt, event_vt};{t2_lvsd, event_lvsd}"),
#                       vt_htxvad = glue::glue("{t2_vt, event_vt};{t2_htxvad, event_htxvad}"),
#                       vt_death = glue::glue("{t2_vt, event_vt};{t2_death, event_death}"),
#                       
#                       nyha_htxvad = glue::glue("{t2_nyha_hf, event_nyha_hf};{t2_htxvad, event_htxvad}"),
#                       nyha_death = glue::glue("{t2_nyha_hf, event_nyha_hf};{t2_death, event_death}"),
#                       
#                       
#                       obstruction_nyha = glue::glue("{t2_obstruction, event_obstruction};{t2_nyha_hf, event_nyha_hf}"),
#                       
#   )
# 
# fig4_df %>% arrange(p) %>% filter(.est>1) %>% filter(p*162<.05) %>% filter(duplicated(.)==F)
# 
# dfposneg %>% #pivot_longer(c(af_nyha, af_lvsd, af_stroke, af_htxvad, af_death), names_to = "events", values_to = "yes") %>%  
#   #filter(!is.na(yes)) %>%
#   pivot_longer(c(af_nyha_age, af_lvsd_age, af_stroke_age, af_htxvad_age, af_death_age,lvsd_htxvad,lvsd_death,
#                  nyha_htxvad,nyha_death,
#                  obstruction_nyha,
#                  vt_lvsd,
#                  vt_htxvad,vt_death)) %>% 
#   separate(value, into = c("age1", "age2"), sep = ";") %>% 
#   select(name, age1, age2,, sarc_status) %>% 
#   mutate(across(2:3, ~as.numeric(.x))) %>% 
#   drop_na() %>% 
#   filter(age1!=0) %>% 
#   filter(age1<age2) %>%
#   
#   filter(age2-age1<5) %>%
#   group_by(name,, sarc_status) %>% 
#   summarise(n=n(), age1 = mean(age1), age2 = mean(age2)) %>% 
#   ggplot(aes(x = age1, y = name, fill = , sarc_status))+
#   geom_point(size = 3, shape = 21)+
#   geom_point(aes(x = age2),size = 3, shape = 21)+
#   geom_text(aes(x= age1 - .5,label = n))+
#   facet_wrap(~, sarc_status)
# ggsave(filename = "Figure 4_3.tiff", compression = "lzw", height = 16, width = 32, units = "cm", dpi =1300)
# 
