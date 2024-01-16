dfposneg %>% filter(event_obstruction==1 &t2_obstruction-first_encounter_age>0) %>% 
  ggplot()+
  geom_density(aes(x= t2_obstruction, fill = sarc_status))
geom_histogram(aes(x= t2_obstruction, fill = sarc_status))


dfposneg %>% filter(event_af==1 &t2_af-first_encounter_age>0) %>% 
  ggplot()+
  geom_density(aes(y= t2_af, fill = sarc_status), alpha = .5)+
  stat_summary(aes(y= first_encounter_age, x=.01, group = sarc_status), geom = "pointrange", fun = "mean",
               position = position_dodge(width = .02) )+
  coord_flip()


dfposneg %>% group_by(sarc_status) %>% 
  summarise(q = quantile(first_encounter_age,na.rm=T, c(.25,.5,.75)))

dfposneg %>% filter(event_af==1 &t2_af-first_encounter_age>0) %>% 
  ggplot()+
  geom_density(aes(y= t2_af, fill = sarc_status), alpha = .5)+
  geom_hline(aes(yintercept = 43.1), linetype = 2)+
  geom_hline(aes(yintercept = 56.2), linetype = 2)+
  stat_summary(aes(y= first_encounter_age, x=.01, group = sarc_status), geom = "pointrange", fun = "mean",
               position = position_dodge(width = .02) )+
  coord_flip()



dfposneg %>% filter(event_af==1 &t2_af-first_encounter_age>0) %>% 
  ggplot()+
  geom_histogram(aes(y= t2_af, fill = sarc_status), alpha = .5, binwidth = 2)+
  geom_hline(aes(yintercept = 43.1), linetype = 2)+
  geom_hline(aes(yintercept = 56.2), linetype = 2)+
  coord_flip()







fig_df<-
  df_long %>%
  filter(sarc_status %in% c('SARC(+)', 'SARC(-)')) %>% 
  mutate(outcome = fct_relevel(outcome, 'hcm', "htn", "obstruction", "syncope",  "icd","af", "nyha_hf", 
                               "srt", "stroke", "vt", "lvsd", "htxvad"),
         feature = case_when(outcome =="htn"~"Hypertension",
                             outcome =="obstruction"~"LV obstruction",
                             outcome =="syncope"~"Syncope",
                             outcome =="icd"~"ICD",
                             outcome =="af"~"Atrial fibrillation",
                             outcome =="nyha_hf"~"NYHA III-IV",
                             outcome =="srt"~"Septal reduction therapy",
                             outcome =="stroke"~"Stroke",
                             outcome =="vt"~"Composite ventricular arrhythmia",
                             outcome =="lvsd"~"LV systolic dysfunction",
                             outcome =="htxvad"~"Cardiac transplantation",
                             outcome =="death"~"All-cause mortality",
                             outcome =='hcm'~'HCM diagnosis',
                             T~outcome
         ),
         feature = fct_reorder(feature, as.numeric(outcome)),
         #time_to = if_else(time_to>=20,19.9999,time_to)
  ) %>% 
  
  # filter(abs(age)<20) %>% 
  filter(feature %in% c('HCM diagnosis',
                        "Atrial fibrillation", "NYHA III-IV", "Composite ventricular arrhythmia", 
                        "LV systolic dysfunction", "Cardiac transplantation", "All-cause mortality"))

fig_df %>% 
  filter(age>0) %>% 
  #filter(time_to>0) %>% 
  ggplot()+
  geom_histogram(aes(x=age, fill = sarc_status), alpha = .9, position = position_dodge(), 
                 binwidth = 5)+
  facet_wrap(~feature, scales = "free_y", nrow = 3
  )+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  labs(x= "Age in Years",
       y= "Number of patients")+
  scale_x_continuous(breaks = seq(0,100,10))+
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
ggsave(filename = "Figure 3- age alternative2.pdf", device = cairo_pdf, height = 18, width = 24, units = "cm", dpi =2900)



fig_df %>% 
  mutate(feature1 = fct_relevel(feature, 'HCM diagnosis', "Composite ventricular arrhythmia", "Cardiac transplantation",
                                "LV systolic dysfunction", "Atrial fibrillation","NYHA III-IV")) %>% 
  filter(age>0) %>% 
  ggplot()+
  ggridges::geom_density_ridges2(aes(x=age, y=feature1,fill = sarc_status),
                                 size = .25,
                                 alpha = .5,
                                 panel_scaling = F,
                                 rel_min_height = .06,
                                 
                                 scale = 1)+
  stat_summary(aes(x=age, y=feature1,color = sarc_status, group = sarc_status),
               fun = 'median', show.legend = F)+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  labs(x= "Age in Years",
       y= "Distribution of Age at Cardiovascular Events")+
  scale_x_continuous(breaks = seq(0,100,10))+
  coord_cartesian(xlim = c(0,100), ylim = c(.8,8.2), expand = F, clip = 'off')+
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
ggsave(filename = "Figure 3- age alternative3.pdf", device = cairo_pdf, 
       height = 16, width = 16, units = "cm", dpi =2900)

skew_df<-  
  df_long1 %>% 
  filter(age>0) %>% 
  filter(outcome %in% c("af", "nyha_hf", "vt", 
                        "lvsd", "htxvad", "death")) %>%
  mutate(sarc_status = if_else(sarc_status=='SARC(+)', "positive", "negative")) %>% 
  group_by(sarc_status, outcome) %>% 
  summarise(kurtosis = moments::kurtosis(age)-3, 
            skewness = moments::skewness(age),
            .groups = "drop",) %>% 
  pivot_wider(names_from = sarc_status, values_from = c(kurtosis, skewness)) %>% 
  mutate(diff= kurtosis_negative-kurtosis_positive)

skew_df
ske

fig_df %>% 
  filter(age>0) %>% 
  filter(feature!= 'HCM diagnosis') %>% 
  ggplot()+
  ggridges::geom_density_ridges2(aes(x=age, y=1,fill = sarc_status), alpha = .7, rel_min_height = .05,
  )+
  facet_wrap(~feature, #scales = "free_y", 
             nrow = 3
  )+
  geom_point(aes(x=37.5, y=1, color = scico(1, palette = 'batlow', direction = -1)),
             show.legend = F
  )+
  geom_point(aes(x=53.2, y=1, color = scico(1, palette = 'batlow')),
             show.legend = F
  )+
  coord_cartesian(xlim = c(0,100))+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  labs(x= "Age in Years",
       y= "Density of Patients")+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = 0)+
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

ggsave(filename = "Figure 2 2023Q2.pdf", device = cairo_pdf, height = 12, width = 18, units = "cm", dpi =2900)
