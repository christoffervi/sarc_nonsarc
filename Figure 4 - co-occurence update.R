bf_cor <- 1#6*9-6

rbind(
  calculate_rr_age(dfneg, age_obesity, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_htxvad,bf_cor, T),
  calculate_rr_age(dfneg, age_obesity, age_lvsd,bf_cor, T),
  
  calculate_rr_age(dfneg, age_htn, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_lvsd,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_htn, age_htxvad,bf_cor, T),
  
  calculate_rr_age(dfneg, age_obstruction, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_lvsd,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_obstruction, age_htxvad,bf_cor, T),
  
  calculate_rr_age(dfneg, age_af, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_lvsd,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_af, age_htxvad,bf_cor, T),
  
  calculate_rr_age(dfneg, age_nyha_hf, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_lvsd,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_nyha_hf, age_htxvad,bf_cor, T),
  
  calculate_rr_age(dfneg, age_lvsd, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_vt,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_lvsd, age_htxvad,bf_cor, T),
  
  
  calculate_rr_age(dfneg, age_vt, age_htn,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_obstruction,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_lvsd,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_af,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_stroke,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_death,bf_cor, T),
  calculate_rr_age(dfneg, age_vt, age_htxvad,bf_cor, T)) %>% 
  mutate(term = "SARC(-)") %>% 
  
  rbind(rbind(
    calculate_rr_age(dfpos, age_obesity, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_htxvad,bf_cor, T),
    calculate_rr_age(dfpos, age_obesity, age_lvsd,bf_cor, T),
    
    calculate_rr_age(dfpos, age_htn, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_lvsd,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_htn, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfpos, age_obstruction, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_lvsd,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_obstruction, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfpos, age_af, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_lvsd,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_af, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfpos, age_nyha_hf, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_lvsd,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_nyha_hf, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfpos, age_lvsd, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_vt,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_lvsd, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfpos, age_vt, age_htn,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_obstruction,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_lvsd,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_af,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_stroke,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_death,bf_cor, T),
    calculate_rr_age(dfpos, age_vt, age_htxvad,bf_cor, T)
  ) %>% 
    rbind(
  calculate_rr_age(dfpos, age_htn, age_obstruction,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_af,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_lvsd,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_vt,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_htn, age_htxvad,bf_cor, T),

  calculate_rr_age(dfpos, age_obstruction, age_htn,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_af,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_lvsd,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_vt,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_obstruction, age_htxvad,bf_cor, T),

  calculate_rr_age(dfpos, age_af, age_htn,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_obstruction,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_lvsd,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_vt,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_af, age_htxvad,bf_cor, T),

  calculate_rr_age(dfpos, age_nyha_hf, age_htn,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_obstruction,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_af,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_lvsd,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_vt,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_nyha_hf, age_htxvad,bf_cor, T),

  calculate_rr_age(dfpos, age_lvsd, age_htn,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_obstruction,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_af,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_vt,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_lvsd, age_htxvad,bf_cor, T),

  calculate_rr_age(dfpos, age_vt, age_htn,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_obstruction,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_nyha_hf,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_lvsd,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_af,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_stroke,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_death,bf_cor, T),
  calculate_rr_age(dfpos, age_vt, age_htxvad,bf_cor, T)

) %>% mutate(term = "SARC(+)")) %>% 
  filter(!str_detect(exposure, "(-)")) %>% 
  mutate(p = if_else(p>1,1,p),
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
         exposure = str_replace(exposure, "age_",""),
         outcome = str_replace(outcome, "age_",""),
          exposure = factor(exposure, levels = c("obesity (+)","htn (+)","obstruction (+)","af (+)",
                                                 "nyha_hf (+)","lvsd (+)","vt (+)"),
                            labels = c("Obesity","Hypertension","Obstruction","Atrial fibrillation",
                                       "NYHA III-IV","LVSD","Composite VA")),
          outcome = factor(outcome, levels = c("htn","obstruction","af",
                                               "nyha_hf","lvsd","vt", "stroke", "htxvad", "death"),
                           labels = c("Hypertension","Obstruction","Atrial <br> fibrillation",
                                      "NYHA III-IV","LVSD","Composite VA",
                                      "Stroke", "Cardiac <br> transplantation", "Death")),
term = factor(term, levels = c("SARC(+)", "SARC(-)"), 
                               labels = c("Sarcomeric HCM", "Non-Sarcomeric HCM"))
  )-> fig4_df
fig4_df
x<-
  fig4_df%>% 
  filter(!is.na(vuffi)| (exposure== "LVSD" &outcome== "Composite VA")| (exposure== "LVSD" &outcome== "Atrial <br> fibrillation")) %>%
  mutate(size = if_else(!is.na(vuffi),1,0)) %>% 
  ggplot(aes(x=outcome, y=exposure, fill = -log10(p), label = vuffi_label))+
  geom_point(aes(size = size, alpha = size), shape = 21, show.legend = T, color = "white")+
  scale_size(range = c(0,22), guide = "none")+
  scale_alpha(range = c(0,1), guide = "none")+
  #geom_tile(show.legend = F, color = "white", size = 2)+
  geom_text(family = "Roboto", size =3, color = "black", fontface = "bold")+
  #ggsci::scale_fill_gsea()+
  #ggsci::scale_color_gsea()+
  ggsci::scale_color_material()+
  ggsci::scale_fill_material(breaks = c(0,10,15,20,25))+
  labs(x= "Outcome", y = "Exposure")+
  #scale_fill_scico()+
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
  )+
  facet_wrap(~term)
x
ggsave(filename = "supp 3 -rr.pdf", device = cairo_pdf, height = 16, width = 32, units = "cm", dpi =3300)

dfposneg<-
dfposneg %>% mutate(af_nyha = if_else(age_af<age_nyha_hf,1,0),
                 af_nyha_age = glue::glue("{age_af};{age_nyha_hf}"),
                 af_lvsd = if_else(age_af<age_lvsd,1,0),
                 af_lvsd_age = glue::glue("{age_af};{age_lvsd}"),
                 af_stroke = if_else(age_af<age_stroke,1,0),
                 af_stroke_age = glue::glue("{age_af};{age_stroke}"),
                 af_htxvad = if_else(age_af<age_htxvad,1,0),
                 af_htxvad_age = glue::glue("{age_af};{age_htxvad}"),
                 af_death = if_else(age_af<age_death,1,0),
                 af_death_age = glue::glue("{age_af};{age_death}"),
                 
                 af_nyha_age = glue::glue("{age_af};{age_nyha_hf}"),
                 af_lvsd_age = glue::glue("{age_af};{age_lvsd}"),
                 af_stroke_age = glue::glue("{age_af};{age_stroke}"),
                 af_htxvad_age = glue::glue("{age_af};{age_htxvad}"),
                 af_death_age = glue::glue("{age_af};{age_death}"),
                 
                 
                 lvsd_htxvad = glue::glue("{age_lvsd};{age_htxvad}"),
                 lvsd_death = glue::glue("{age_lvsd};{age_death}"),
                 
                 vt_lvsd = glue::glue("{age_vt};{age_lvsd}"),
                 vt_htxvad = glue::glue("{age_vt};{age_htxvad}"),
                 vt_death = glue::glue("{age_vt};{age_death}"),
                 
                 nyha_htxvad = glue::glue("{age_nyha_hf};{age_htxvad}"),
                 nyha_death = glue::glue("{age_nyha_hf};{age_death}"),
                 
                 
                 obstruction_nyha = glue::glue("{age_obstruction};{age_nyha_hf}"),
                 
                 )

fig4_df %>% arrange(p) %>% filter(.est>1) %>% filter(p<.05) %>% filter(duplicated(.)==F)

dfposneg %>% #pivot_longer(c(af_nyha, af_lvsd, af_stroke, af_htxvad, af_death), names_to = "events", values_to = "yes") %>%  
  #filter(!is.na(yes)) %>%
  pivot_longer(c(af_nyha_age, af_lvsd_age, af_stroke_age, af_htxvad_age, af_death_age,lvsd_htxvad,lvsd_death,
                 nyha_htxvad,nyha_death,
                 obstruction_nyha,
                 vt_lvsd,
                 vt_htxvad,vt_death)) %>% 
  separate(value, into = c("age1", "age2"), sep = ";") %>% 
  select(name, age1, age2,sarc_status) %>% 
  mutate(across(2:3, ~as.numeric(.x))) %>% 
  drop_na() %>% 
  filter(age1!=0) %>% 
  filter(age1<age2) %>%
  
  filter(age2-age1<5) %>%
  group_by(name,sarc_status) %>% 
  summarise(n=n(), age1 = mean(age1), age2 = mean(age2)) %>% 
  ggplot(aes(x = age1, y = name, fill = sarc_status))+
  geom_point(size = 3, shape = 21)+
  geom_point(aes(x = age2),size = 3, shape = 21)+
  geom_text(aes(x= age1 - .5,label = n))+
  facet_wrap(~sarc_status)
ggsave(filename = "Figure 4_3.tiff", compression = "lzw", height = 16, width = 32, units = "cm", dpi =1300)

