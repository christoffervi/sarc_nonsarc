hf<- hf %>% mutate(event_hf_death = if_else(death_cause=="Heart Failure",1,0),
                   death_causes = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                                            str_detect(death_cause, "SCD")~"Sudden cardiac death",
                                            str_detect(death_cause, "Myoca|Cardio|CAD")~"Other cardiovascular death",
                                            str_detect(death_cause, "Fail|HCM")~"Heart failure",
                                            T~death_cause),
                   event_hcm_death = if_else(death_causes %in% c("Sudden cardiac death","Heart failure", "Stroke"),1,0))
dfposneg<- dfposneg %>% mutate(event_hf_death = if_else(death_cause=="Heart Failure",1,0),
                               death_causes = case_when(str_detect(death_cause, "Non-Card|Infect|Unkn|Proce|Acci|GI")~"Non-cardiovascular death",
                                                       str_detect(death_cause, "SCD")~"Sudden cardiac death",
                                                       str_detect(death_cause, "Myoca|Cardio|CAD")~"Other cardiovascular death",
                                                       str_detect(death_cause, "Fail|HCM")~"Heart failure",
                                                       T~death_cause),
                               event_hcm_death = if_else(death_causes %in% c("Sudden cardiac death","Heart failure", "Stroke"),1,0)
                               )

spl <- as_tibble(survSplit(Surv(first_encounter_age, t2_death,
                                event = event_hcm_death)~sarc_status, data = dfposneg %>% filter(t2_death>first_encounter_age),
                           cut = c(30,45,55,65), 
                           episode = "agegroup")) %>%
  #Calculate time for each age group and event type
  mutate(time = t2_death-first_encounter_age) %>% 
  group_by(agegroup, sarc_status) %>% 
  summarise(nyear = sum(time), ncas = sum(event_hcm_death)) %>% 
  ungroup() 
obs<-matrix(spl$ncas, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

tar<-matrix(spl$nyear, nrow =2, dimnames = list(c("SARC(+)", "SARC(-)"), c("1",'2','3','4','5')))

std<- matrix(data = c(100,100,100,100,100), nrow = 1, byrow = TRUE, 
             dimnames = list("", c("1",'2','3','4','5')))

epiR::epi.directadj(obs,tar,std)

epiR::epi.directadj(obs,tar,std)$adj.strata %>% 
  tibble() %>% 
  select(sarc_status = strata,
         est,lower,upper, nyear = tar, ncas=obs)


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

dat.df
p1<-
  dat.df %>% filter(!is.na(sarc_status)) %>% 
  mutate(age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", "&gt;65", "**ASI**")),
         across(1:3, ~.x*1000)) %>% 
  
  #mutate(agegroup = factor(agegroup, labels = c("<12","12-19", "20-29","30-39", ">40"))) %>% 
  ggplot(aes(x= age, y= est, group = sarc_status, ymin = lower, ymax= upper, fill = sarc_status))+
  geom_errorbar(position = position_dodge(width = .3), width = .1)+
  #geom_line(position = position_dodge(width = .3))+
  geom_point(position = position_dodge(width = .3), shape = c(21,21,21,21,21,21,21,21,21,21,22,22), size = 4, 
             show.legend = F)+
  scale_fill_scico_d(palette = "batlow")+
  scale_y_continuous(breaks = seq(0,200,2.5))+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(0,23), expand = F, clip = "off")+
 # scale_y_log10(breaks = c(2.5,5,10,20,40,80,.16))+
#  coord_cartesian(xlim = c(.6,6.4), ylim = c(0.4,25), expand = F, clip = "off")+
  annotate("text", x= c(5.13,4.87), y = c(17.233379,5.093083),
           label = c("Sarc+","Sarc-"), hjust = c(0,1), vjust= .5)+
  labs(#title ="Age-specific incidence of obstruction",
    y= "Age-specific incidence per 1,000 person-years",
    x = "Age groups",
  )+
  #  annotate("text", x=6,y=35, label = "SIR \n1.34 (1.21-1.48) \np <0.001", family = "Roboto")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        #axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.text.x = element_markdown(family = "Helvetica", 
                                       color = "black", size = 10),
        axis.title.x = element_text(family = "Helvetica", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Helvetica", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        #axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))



###########


dat.df %>% 
  select(sarc_status,agegroup, nyear, ncas) %>% 
  mutate(group = if_else(sarc_status=="SARC(+)", "group1", "group2")) %>% 
  select(!sarc_status) %>% 
  pivot_wider(names_from = group, values_from = c(nyear,ncas)) %>% 
  filter(agegroup<6) %>% 
  mutate(group1_rate = ncas_group1/nyear_group1,
         group2_rate = ncas_group2/nyear_group2,
         group1_expected = group2_rate*nyear_group1) %>% 
  group_by(agegroup) %>% 
  mutate(group1_smr = epiR::epi.smr(ncas_group1,group1_expected)$est,
         group1_smr_low = epiR::epi.smr(ncas_group1,group1_expected)$lower,
         group1_smr_upp = epiR::epi.smr(ncas_group1,group1_expected)$upper,
         group1_smr_p = epiR::epi.smr(ncas_group1,group1_expected)$p.value) %>% 
  ungroup() %>% 
  summarise(obs = sum(ncas_group1), exp = sum(group1_expected))
epiR::epi.smr(391,292)


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
smr

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
  
  #  geom_text(aes(label = paste(round(nyear,0), "\n(",ncas,")",sep = "")), size = 3, 
  #             show.legend = F,family = "Roboto")+
  geom_text(aes(label = round(nyear,0)), size = 2.5, 
            show.legend = F,family = "Roboto")+
  scale_color_scico_d(palette = "batlow")+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(0,2.4), expand = F, clip = "off")+
  geom_segment(aes(x= .7,xend=.8,y=sarc_status, yend = sarc_status, color = sarc_status), show.legend = F, linewidth=2)+
  annotate("text", x= c(.6), y = c(2.4),family = "Roboto", fontface = "bold",
           label = c("Years at risk"), hjust = 0, vjust= 0, size =3)+
  theme_void(base_family = "Roboto")

p3<-
  dat.df.smr  %>%
  mutate(p = case_when(group1_smr_p<0.001~ "p <0.001",
                       T~paste("p =",sep = "", round(group1_smr_p,3)))) %>% 
  ggplot(aes(x= agegroup, y= 1, label=paste(round(group1_smr,2),
                                            "\n (CI: ",
                                            round(group1_smr_low,2),
                                            "-",
                                            
                                            round(group1_smr_upp,2),
                                            ")\n",
                                            p,
                                            sep = "")))+
  geom_text(family = "Roboto", size =2.5)+ theme_void()+
  coord_cartesian(xlim = c(.6,6.4), ylim = c(.5,1.5), expand = F, clip = "off")+
  annotate("text", x= c(.6), y = c(1.4),family = "Roboto", fontface = "bold",
           label = c("SIR"), hjust = 0, vjust= 0, size = 3)


hf %>% 
  mutate(time1 = t2_death-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  coxph(Surv(time1, event_hcm_death)~sarc_status, data = .)%>% broom::tidy(exponentiate = T, conf.int=T) ->cox_death

hf %>% 
  mutate(time1 = t2_death-first_encounter_age,
         event_hcm_death_cmp = case_when(event_hcm_death==1~1,
                                         event_death==1~2,
                                         T~0)) %>% 
  filter(time1>0) %>% 
  rms::npsurv(Surv(time1, factor(event_hcm_death_cmp))~sarc_status, data = .) %>% 
  broom::tidy() %>%filter(state %in% c("1")) %>% 
  ggplot(aes(x=time, y=estimate, color = strata, fill = strata))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = .5)+
  geom_line(aes(x=time, y=estimate), alpha = 1,
            #color = scico(1,palette = "lapaz",,begin = .4),
  )+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,.053),
                  expand = F)+
  labs(x = "Years from first SHaRe visit",
       y = "Cumulative incidence of HCM-related death")+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_text(family = "Helvetica", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Helvetica", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))+
  annotate("text", x= .5,y=.025, hjust =0, family = "Helvetica",
           label = glue::glue("log rank {surv_pvalue(hf %>% 
                mutate(time1 = t2_death-first_encounter_age ) %>% 
                filter(time1>0) %>% 
                surv_fit(Surv(time1, event_hcm_death)~sarc_status, data = .))[,4]}"))

coxph(Surv(time1, event_hcm_death)~sarc_status, data = .)%>% broom::tidy(exponentiate = T, conf.int=T)


p4<-
  hf %>% 
  mutate(time1 = t2_death-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_hcm_death)~sarc_status, data = .) %>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x)) %>% 
  ggplot(aes(x=time, y= estimate, ymin = conf.low, ymax= conf.high, color = strata))+
  geom_line( show.legend=F)+
  geom_ribbon(aes(fill = strata, alpha = .2),show.legend=F)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,.053),
                  expand = F)+
  labs(x = "Years from first SHaRe visit",
       y = "Cumulative incidence of HCM-related death")+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_text(family = "Helvetica", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Helvetica", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))+
  annotate("text", x= .5,y=.025, hjust =0, family = "Helvetica",
           label = glue::glue("log rank {surv_pvalue(hf %>% 
                mutate(time1 = t2_death-first_encounter_age ) %>% 
                filter(time1>0) %>% 
                surv_fit(Surv(time1, event_hcm_death)~sarc_status, data = .))[,4]}"))
p5<-
  hf %>% 
  mutate(time1 = t2_death-first_encounter_age ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_hcm_death)~sarc_status, data = .)%>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x),
         time2 = trunc(time),
         strata = str_replace(strata, "sarc_status=", "")) %>% 
  group_by(strata, time2) %>% 
  mutate(r= row_number()) %>% filter(r==1, time2<11) %>% 
  summarise(risk = n.risk, .groups = "drop") %>%  #pivot_wider(names_from = strata, values_from = risk) 
  #filter(time2 %in% c(0,2,4,6,8,10)) %>% 
  ggplot(aes(x=time2, y= strata, label = risk))+
  geom_text(size= 2.5, family = "Roboto")+
  theme_void()+
  scale_color_scico_d()+
  geom_segment(aes(x= -.3,xend=-.5,y=strata, yend = strata, color = strata), show.legend = F, linewidth=2)+
  annotate("text", x= c(0), y = c(2.4),family = "Roboto", fontface = "bold",
           label = c("Numbers at risk"), hjust = 0, vjust= 0,
           size =3)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,2.4),
                  expand = F, clip = "off")

p4/p5/p1/p2/p3+plot_layout(heights = c(3,1,3,.75,.75))+plot_annotation(tag_levels = list(c("A","","B","","")))
ggsave(filename = "hcm_death_age.tiff", compression = "lzw", height = 26, width = 20, units = "cm", dpi =900)

p4+p1+p5+p2+p3+plot_layout(design = layout)+plot_annotation(tag_levels = list(c("A","B","","","")))

ggsave(filename = "hcm_age.tiff", compression = "lzw", height = 12, width = 24, units = "cm", dpi =900)


p1/p2/p3+plot_layout(heights = c(3,1,1))  

p1/p2/p3/p4+plot_layout(heights = c(3,1,1,3))  
