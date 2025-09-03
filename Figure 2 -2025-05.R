col_chris <- scale_color_manual(values = c("#001959", '#CC79A7'))
fill_chris <- scale_fill_manual(values = c("#001959", '#CC79A7'))

####Supplementary figure 2 part one ###########################
####
#Atrial Fibrillation
spl <- as_tibble(survSplit(Surv(first_encounter_age, t2_af,
                                event = event_af)~sarc_status, data = dfposneg %>% filter(t2_af>first_encounter_age),
                           cut = c(30,45,55,65), 
                           episode = "agegroup")) %>%
  #Calculate time for each age group and event type
  mutate(time = t2_af-first_encounter_age) %>% 
  group_by(agegroup, sarc_status) %>% 
  summarise(nyear = sum(time), ncas = sum(event_af)) %>% 
  ungroup() 
obs<-matrix(spl$ncas, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

tar<-matrix(spl$nyear, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

std<- matrix(data = c(100,100,100,100,100), nrow = 1, byrow = TRUE, 
             dimnames = list("", c("1",'2','3','4','5')))



#Create a data frame with observed and expected events, calculate the complement of the age-standardized rate (for plotting purposes), and bind to the original data
dat.df <- spl %>% select(ncas,nyear) %>% 
  as.matrix() %>% 
  epiR::epi.conf(ctype = "inc.rate", method = "byar") %>%  
  bind_cols(spl) %>% 
  bind_rows(epiR::epi.directadj(obs,tar,std)$adj.strata %>% 
              tibble() %>% 
              select(sarc_status = strata,
                     est,lower,upper, nyear = tar, ncas=obs) %>% 
              mutate(agegroup = 6)
  ) %>% 
  mutate(sur = 1-est)

p1<-
  dat.df %>% filter(!is.na(sarc_status)) %>% 
  mutate(age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", "&gt;65", "**ASI**")),
         across(1:3, ~.x*1000)) %>% 
  ggplot(aes(x= age, y= est, group = sarc_status, ymin = lower, ymax= upper, fill = sarc_status))+
  scale_x_discrete()+
  annotate("rect", xmin = 5.5,xmax=6.5,ymin=-100,
           ymax=trunc(max(dat.df$upper)*1000)+2,
           fill = "#e6e1e1FF", alpha = .5)+
  geom_errorbar(position = position_dodge(width = .3), width = .1)+
  geom_point(position = position_dodge(width = .3), shape = c(21,21,21,21,21,21,21,21,21,21,22,22), size = 4, 
             show.legend = F)+
  #scale_color_manual(values = chris_colors_3[2:3])+
  #scale_fill_manual(values = chris_colors_3[2:3])+
  fill_chris+
  #scale_fill_scico_d(palette = "batlow")+
  scale_y_continuous(breaks = seq(0,200,10))+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(-0,trunc(max(dat.df$upper)*1000)+2), expand = F, clip = "off")+
  annotate("text", x= c(5.17,4.83), y = c(dat.df$est[9]*1000,dat.df$est[10]*1000),
           label = c("Sarc+","Sarc-"), hjust = c(0,1), vjust= .5, color = "darkslategrey")+
  labs(#title ="Age-specific incidence of obstruction",
    y= "Age-specific incidence per 1,000 person-years",
    x = "Age groups",
  )+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text.x = element_markdown(family = "Roboto", 
                                       color = "black", size = 10),
        axis.title.x = element_text(family = "Roboto", size = 10, hjust = .5, face = "bold"),
        axis.title.y = element_text(family = "Roboto", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(family = "Roboto", size = 10))



smr_all<-
  dat.df %>% 
  select(sarc_status,agegroup, nyear, ncas) %>% 
  mutate(group = if_else(sarc_status=="SARC(+)", "group1", "group2")) %>% 
  select(!sarc_status) %>% 
  pivot_wider(names_from = group, values_from = c(nyear,ncas)) 

smr<- 
  smr_all %>% 
  filter(agegroup<6) %>% 
  mutate(group1_rate = ncas_group1/nyear_group1,
         group2_rate = ncas_group2/nyear_group2,
         group1_expected = group2_rate*nyear_group1) %>% 
  group_by(agegroup) %>% 
  mutate(group1_smr = epiR::epi.smr(ncas_group1,group1_expected)$est,
         group1_smr_low = epiR::epi.smr(ncas_group1,group1_expected)$lower,
         group1_smr_upp = epiR::epi.smr(ncas_group1,group1_expected)$upper,
         group1_smr_p = epiR::epi.smr(ncas_group1,group1_expected)$p.value) %>% 
  ungroup() 

smr_asi<-
  smr %>%
  summarise(agegroup = 6,
            nyear_group1 = smr_all$nyear_group1[6], 
            nyear_group2 = smr_all$nyear_group2[6],
            ncas_group1 = smr_all$ncas_group1[6],
            ncas_group2 = smr_all$ncas_group2[6],
            group1_rate = ncas_group1/nyear_group1,
            group2_rate = ncas_group2/nyear_group2,
            group1_expected = sum(group1_expected)) %>% 
  mutate(group1_smr = epiR::epi.smr(ncas_group1,group1_expected)$est,
         group1_smr_low = epiR::epi.smr(ncas_group1,group1_expected)$lower,
         group1_smr_upp = epiR::epi.smr(ncas_group1,group1_expected)$upper,
         group1_smr_p = epiR::epi.smr(ncas_group1,group1_expected)$p.value)

dat.df.smr<-
  smr %>% select(agegroup,group1_smr, group1_smr_low, group1_smr_upp,group1_smr_p) %>% 
  bind_rows(smr_asi %>% select(agegroup,group1_smr, group1_smr_low, group1_smr_upp,group1_smr_p))


p2<-
  dat.df %>% filter(!is.na(sarc_status) & agegroup<7) %>% 
  mutate(#age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", ">65")),
    across(1:3, ~.x*1000)) %>% 
  ggplot(aes(x= agegroup, y= sarc_status, group = sarc_status, fill = sarc_status))+
  
  geom_text(aes(label = round(nyear,0)), size = 3.5, 
            show.legend = F,family = "Roboto")+
  col_chris+
  #scale_color_scico_d(palette = "batlow")+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(0,2.4), expand = F, clip = "off")+
  geom_segment(aes(x= .7,xend=.8,y=sarc_status, yend = sarc_status, color = sarc_status), show.legend = F, linewidth=2)+
  annotate("text", x= c(.6), y = c(2.6),family = "Roboto", 
           fontface = "bold",
           label = c("Years at risk"), hjust = 0, vjust= 0)+
  theme_void(base_family = "Roboto")

p3<-
  dat.df.smr  %>%
  mutate(p = case_when(group1_smr_p<0.0001~ "p <0.0001",
                       T~paste("p =",sep = "", round(group1_smr_p,3)))) %>% 
  ggplot(aes(x= agegroup, y= 1, label=paste(round(group1_smr,2),
                                            "\n (",
                                            round(group1_smr_low,2),
                                            #" to ",
                                            "-",
                                            round(group1_smr_upp,2),
                                            ")\n",
                                            p,
                                            sep = "")))+
  geom_text(family = "Roboto", size =3.5)+ theme_void()+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(.5,1.5), expand = F, clip = "off")+
  annotate("text", x= c(.6), y = c(1.6),family = "Roboto", fontface = "bold",
           label = c("Standardized incidence ratio (CI)"), hjust = 0, vjust= 0)
p4<-
  hf %>% 
  mutate(time1 = t2_af-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_af)~sarc_status, data = .) %>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x)) %>% 
  filter(time<10.1) %>% 
  ggplot(aes(x=time, y= estimate, ymin = conf.low, ymax= conf.high, color = strata))+
  geom_line( show.legend=F)+
  geom_ribbon(aes(fill = strata, alpha = .2),show.legend=F)+
  annotate("text", x= 7.8, y = c(0.12, 0.23), label = c("Sarc+", "Sarc-"),
           color = c( '#CC79A7', scico(1, palette = "batlow", direction = 1)),
           # scico(2, palette = "batlow", direction = -1), 
           angle= 25, size = 6,
           family = "Roboto")+
  
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,.25),
                  expand = F, clip = 'off')+
  labs(x = "Years from first SHaRe visit",
       y = "Cumulative incidence")+
  annotate('text',x=5,y=.27,label = "Atrial Fibrillation", family ='Roboto', fontface = 'bold',
           size = 4.5, hjust = .5)+
  # scale_color_manual(values = chris_colors_3[2:3])+
  #  scale_fill_manual(values = chris_colors_3[2:3])+
  # scale_fill_scico_d()+
  # scale_color_scico_d()+
  col_chris+fill_chris+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_text(family = "Roboto", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Roboto", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(family = "Roboto", size = 10),
        axis.text.y = element_text(family = "Roboto", size = 10))+
  annotate("text", x= .5,y=.125, hjust =0, family = "Roboto",
           label = glue::glue("log rank {surv_pvalue(hf %>% 
                mutate(time1 = t2_af-first_encounter_age ) %>% 
                filter(time1>0) %>% 
                surv_fit(Surv(time1, event_af)~sarc_status, data = .))[,4]}"))
p5<-
  hf %>% 
  mutate(time1 = t2_af-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_af)~sarc_status, data = .)%>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x),
         time2 = trunc(time),
         strata = str_replace(strata, "sarc_status=", "")) %>% 
  group_by(strata, time2) %>% 
  mutate(r= row_number()) %>% filter(r==1, time2<10.4) %>% 
  summarise(risk = n.risk, .groups = "drop") %>%  #pivot_wider(names_from = strata, values_from = risk) 
  filter(time2 %in% c(0,2,4,6,8,10)) %>% 
  ggplot(aes(x=time2, y= strata, label = risk))+
  geom_text(size= 3.5, family = "Roboto")+
  theme_void()+
  #scale_color_manual(values = chris_colors_3[2:3])+
  #scale_fill_manual(values = chris_colors_3[2:3])+
  #scale_color_scico_d()+
  col_chris+
  geom_segment(aes(x= -.3,xend=-.5,y=strata, yend = strata, color = strata), show.legend = F, linewidth=2)+
  annotate("text", x= c(0), y = c(2.6),family = "Roboto", fontface = "bold",
           label = c("Numbers at risk"), hjust = 0, vjust= 0)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,2.4),
                  expand = F, clip = "off")

#### Supplementary figure 2 part two ###########################
# COMPOSITE VT

spl <- as_tibble(survSplit(Surv(first_encounter_age, t2_vt,
                                event = event_vt)~sarc_status, data = dfposneg %>% filter(t2_vt>first_encounter_age),
                           cut = c(30,45,55,65), 
                           episode = "agegroup")) %>%
  #Calculate time for each age group and event type
  mutate(time = t2_vt-first_encounter_age) %>% 
  group_by(agegroup, sarc_status) %>% 
  summarise(nyear = sum(time), ncas = sum(event_vt)) %>% 
  ungroup() 
obs<-matrix(spl$ncas, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

tar<-matrix(spl$nyear, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

std<- matrix(data = c(100,100,100,100,100), nrow = 1, byrow = TRUE, 
             dimnames = list("", c("1",'2','3','4','5')))

#Create a data frame with observed and expected events, calculate the complement of the age-standardized rate (for plotting purposes), and bind to the original data
dat.df <- spl %>% select(ncas,nyear) %>% 
  as.matrix() %>% 
  epiR::epi.conf(ctype = "inc.rate", method = "byar") %>%  
  bind_cols(spl) %>% 
  bind_rows(epiR::epi.directadj(obs,tar,std)$adj.strata %>% 
              tibble() %>% 
              select(sarc_status = strata,
                     est,lower,upper, nyear = tar, ncas=obs) %>% 
              mutate(agegroup = 6)
  ) %>% 
  mutate(sur = 1-est)

q1<-
  dat.df %>% filter(!is.na(sarc_status)) %>% 
  mutate(age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", "&gt;65", "**ASI**")),
         across(1:3, ~.x*1000)) %>% 
  ggplot(aes(x= age, y= est, group = sarc_status, ymin = lower, ymax= upper, fill = sarc_status))+
  annotate("rect", xmin = 5.5,xmax=6.5,ymin=-100,
           ymax=trunc(max(dat.df$upper)*1000)+.1,
           fill = "#e6e1e1FF", alpha = .5)+
  scale_x_discrete()+
  geom_errorbar(position = position_dodge(width = .3), width = .1)+
  geom_point(position = position_dodge(width = .3), shape = c(21,21,21,21,21,21,21,21,21,21,22,22), size = 4, 
             show.legend = F)+
  scale_color_manual(values = chris_colors_3[2:3])+
  #  scale_fill_manual(values = chris_colors_3[2:3])+
  fill_chris+
#  scale_fill_scico_d(palette = "batlow")+
  scale_y_continuous(n.breaks = 6)+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(-0,trunc(max(dat.df$upper)*1000)+.1), expand = F, clip = "off")+
  annotate("text", x= c(5.17,4.83), y = c(dat.df$est[9]*1000,dat.df$est[10]*1000),
           label = c("Sarc+","Sarc-"), hjust = c(0,1), vjust= .5, color= "darkslategrey")+
  labs(#title ="Age-specific incidence of obstruction",
    y= "Age-specific incidence per 1,000 person-years",
    x = "Age groups",
  )+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text.x = element_markdown(family = "Roboto", 
                                       color = "black", size = 10),
        axis.title.x = element_text(family = "Roboto", size = 10, hjust = .5, face = "bold"),
        axis.title.y = element_text(family = "Roboto", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(family = "Roboto", size = 10))



smr_all<-
  dat.df %>% 
  select(sarc_status,agegroup, nyear, ncas) %>% 
  mutate(group = if_else(sarc_status=="SARC(+)", "group1", "group2")) %>% 
  select(!sarc_status) %>% 
  pivot_wider(names_from = group, values_from = c(nyear,ncas)) 

smr<- 
  smr_all %>% 
  filter(agegroup<6) %>% 
  mutate(group1_rate = ncas_group1/nyear_group1,
         group2_rate = ncas_group2/nyear_group2,
         group1_expected = group2_rate*nyear_group1) %>% 
  group_by(agegroup) %>% 
  mutate(group1_smr = epiR::epi.smr(ncas_group1,group1_expected)$est,
         group1_smr_low = epiR::epi.smr(ncas_group1,group1_expected)$lower,
         group1_smr_upp = epiR::epi.smr(ncas_group1,group1_expected)$upper,
         group1_smr_p = epiR::epi.smr(ncas_group1,group1_expected)$p.value) %>% 
  ungroup() 

smr_asi<-
  smr %>%
  summarise(agegroup = 6,
            nyear_group1 = smr_all$nyear_group1[6], 
            nyear_group2 = smr_all$nyear_group2[6],
            ncas_group1 = smr_all$ncas_group1[6],
            ncas_group2 = smr_all$ncas_group2[6],
            group1_rate = ncas_group1/nyear_group1,
            group2_rate = ncas_group2/nyear_group2,
            group1_expected = sum(group1_expected)) %>% 
  mutate(group1_smr = epiR::epi.smr(ncas_group1,group1_expected)$est,
         group1_smr_low = epiR::epi.smr(ncas_group1,group1_expected)$lower,
         group1_smr_upp = epiR::epi.smr(ncas_group1,group1_expected)$upper,
         group1_smr_p = epiR::epi.smr(ncas_group1,group1_expected)$p.value)

dat.df.smr<-
  smr %>% select(agegroup,group1_smr, group1_smr_low, group1_smr_upp,group1_smr_p) %>% 
  bind_rows(smr_asi %>% select(agegroup,group1_smr, group1_smr_low, group1_smr_upp,group1_smr_p))

q2<-
  dat.df %>% filter(!is.na(sarc_status) & agegroup<7) %>% 
  mutate(#age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", ">65")),
    across(1:3, ~.x*1000)) %>% 
  ggplot(aes(x= agegroup, y= sarc_status, group = sarc_status, fill = sarc_status))+
  geom_text(aes(label = round(nyear,0)), size = 3.5, 
            show.legend = F,family = "Roboto")+
  col_chris+
#  scale_color_scico_d(palette = "batlow")+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(0,2.4), expand = F, clip = "off")+
  geom_segment(aes(x= .7,xend=.8,y=sarc_status, yend = sarc_status, color = sarc_status), show.legend = F, linewidth=2)+
  annotate("text", x= c(.6), y = c(2.6),family = "Roboto", fontface = "bold",
           label = c("Years at risk"), hjust = 0, vjust= 0)+
  theme_void(base_family = "Roboto")

q3<-
  dat.df.smr  %>%
  mutate(p = case_when(group1_smr_p<0.001~ "p <0.001",
                       T~paste("p =",sep = "", round(group1_smr_p,3)))) %>% 
  ggplot(aes(x= agegroup, y= 1, label=paste(round(group1_smr,2),
                                            "\n (",
                                            round(group1_smr_low,2),
                                            "-",
                                            
                                            round(group1_smr_upp,2),
                                            ")\n",
                                            p,
                                            sep = "")))+
  geom_text(family = "Roboto", size = 3.5)+ theme_void()+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(.5,1.5), expand = F, clip = "off")+
  annotate("text", x= c(.6), y = c(1.6),family = "Roboto", fontface = "bold",
           label = c("Standardized incidence ratio (CI)"), hjust = 0, vjust= 0)
q4<-
  hf %>% 
  mutate(time1 = t2_vt-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_vt)~sarc_status, data = .) %>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x)) %>% 
  filter(time<10.1) %>% 
  ggplot(aes(x=time, y= estimate, ymin = conf.low, ymax= conf.high, color = strata))+
  geom_line( show.legend=F)+
  geom_ribbon(aes(fill = strata, alpha = .2),show.legend=F)+
  annotate("text", x= 7.8, y = c(0.074, 0.020), label = c("Sarc+", "Sarc-"),
           color = c( '#CC79A7', scico(1, palette = "batlow", direction = 1)),
                      # scico(2, palette = "batlow", direction = -1), 
           angle= 25, size = 6,
           family = "Roboto")+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,hf %>% 
                            mutate(time1 = t2_vt-first_encounter_age ) %>% 
                            filter(time1>0) %>% 
                            surv_fit(Surv(time1, event_vt)~sarc_status, data = .) %>% 
                            broom::tidy() %>% filter(time<=10) %>%
                            select(conf.low) %>% summarise(high = 1-min(conf.low)) %>% pull),
                  expand = F, clip = 'off')+
  labs(x = "Years from first SHaRe visit",
       y = "Cumulative incidence")+
  annotate('text',x=5,y=.085,label = "Composite Ventricular Arrhythmias", family ='Roboto', fontface = 'bold',
           size = 4.5, hjust = .5)+
  # scale_color_manual(values = chris_colors_3[2:3])+
  #  scale_fill_manual(values = chris_colors_3[2:3])+
  # scale_fill_scico_d()+
  # scale_color_scico_d()+
  col_chris+fill_chris+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_text(family = "Roboto", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Roboto", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(family = "Roboto", size = 10),
        axis.text.y = element_text(family = "Roboto", size = 10))+
  annotate("text", x= .5,y=.03, hjust =0, family = "Roboto",
           label = glue::glue("log rank {surv_pvalue(hf %>% 
                mutate(time1 = t2_vt-first_encounter_age ) %>% 
                filter(time1>0) %>% 
                surv_fit(Surv(time1, event_vt)~sarc_status, data = .))[,4]}"))
q5<-
  hf %>% 
  mutate(time1 = t2_vt-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_vt)~sarc_status, data = .)%>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x),
         time2 = trunc(time),
         strata = str_replace(strata, "sarc_status=", "")) %>% 
  group_by(strata, time2) %>% 
  mutate(r= row_number()) %>% filter(r==1, time2<11) %>% 
  summarise(risk = n.risk, .groups = "drop") %>%  #pivot_wider(names_from = strata, values_from = risk) 
  filter(time2 %in% c(0,2,4,6,8,10)) %>% 
  ggplot(aes(x=time2, y= strata, label = risk))+
  geom_text(size = 3.5, family = "Roboto")+
  theme_void()+
  col_chris+
  #scale_color_scico_d()+
  geom_segment(aes(x= -.3,xend=-.5,y=strata, yend = strata, color = strata), show.legend = F, linewidth=2)+
  annotate("text", x= c(0), y = c(2.6),family = "Roboto", fontface = "bold",
           label = c("Numbers at risk"), hjust = 0, vjust= 0)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,2.4),
                  expand = F, clip = "off")

# COMBINGING THE PLOTS


layout <- "
AAABBB
AAABBB
AAABBB
CCCDDD
EEEFFF
EEEFFF
EEEFFF
GGGHHH
IIIJJJ"



p4+q4+p5+q5+p1+q1+p2+q2+p3+q3+plot_layout(design = layout)+plot_annotation(tag_levels = list(c("A","C","","","B","D")))
ggsave(filename = "Circ - Figure 2.pdf", device = cairo_pdf, height = 22, width = 30, units = "cm", dpi =2900)
ggsave(filename = "Circ - Figure 2.tiff", compression = 'lzw', height = 22, width = 30, units = "cm", dpi =1600)
