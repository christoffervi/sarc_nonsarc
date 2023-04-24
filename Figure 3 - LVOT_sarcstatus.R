library(epiR)
##
spl <- as_tibble(survSplit(Surv(echo_age0, t2_obstruction,
                                event = event_obstruction)~sarc_status, data = dfposneg %>% filter(t2_obstruction>echo_age0),
                           cut = c(30,45,55,65), 
                           episode = "agegroup")) %>%
  #Calculate time for each age group and event type
  mutate(time = t2_obstruction-echo_age0) %>% 
  group_by(agegroup, sarc_status) %>% 
  summarise(nyear = sum(time), ncas = sum(event_obstruction)) %>% 
  ungroup() 
obs<-matrix(spl$ncas, nrow =2, dimnames = list(c("sarc+", "sarc-"), c("1",'2','3','4','5')))

tar<-matrix(spl$nyear, nrow =2, dimnames = list(c("sarc+", "sarc-"), c("1",'2','3','4','5')))

std<- matrix(data = c(100,100,100,100,100), nrow = 1, byrow = TRUE, 
             dimnames = list("", c("1",'2','3','4','5')))

epiR::epi.directadj(obs,tar,std)
#Create a data frame with observed and expected events, calculate the complement of the age-standardized rate (for plotting purposes), and bind to the original data
dat.df <- spl %>% select(ncas,nyear) %>% 
  as.matrix() %>% 
  epiR::epi.conf(ctype = "inc.rate", method = "byar") %>%  
  bind_cols(spl) %>% 
  mutate(sur = 1-est)
dat.df

epiR::epi.directadj()

#### In this final part we make the plot based on the data above
p1<-
  dat.df %>% filter(!is.na(sarc_status)) %>% 
  mutate(age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", ">65")),
         across(1:3, ~.x*1000)) %>% 
  
  #mutate(agegroup = factor(agegroup, labels = c("<12","12-19", "20-29","30-39", ">40"))) %>% 
  ggplot(aes(x= age, y= est, group = sarc_status, ymin = lower, ymax= upper, fill = sarc_status))+
  geom_errorbar(position = position_dodge(width = .3), width = .1)+
  geom_line(position = position_dodge(width = .3))+
  geom_point(position = position_dodge(width = .3), shape = 21, size = 4, 
             show.legend = F)+
  scale_fill_scico_d(palette = "batlow")+
  scale_y_continuous(breaks = seq(0,200,10))+
  #scale_y_log10(breaks = c(.01,.02,.04,.08,.16))+
  coord_cartesian(xlim = c(.6,5.4), ylim = c(-0,85), expand = F, clip = "off")+
  annotate("text", x= c(5.2,5), y = c(21,59),
           label = c("Sarc+","Sarc-"), hjust = 0, vjust= 0)+
  labs(#title ="Age-specific incidence of obstruction",
    y= "Age-specific incidence per 1,000 person-years",
    x = "Age groups",
  )+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray79", linetype = 3),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_text(family = "Helvetica", size = 10, hjust = 1),
        axis.title.y = element_text(family = "Helvetica", size = 10, hjust = 1),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(family = "Helvetica", size = 10),
        axis.text.y = element_text(family = "Helvetica", size = 10))
p1
p2<-
  dat.df %>% filter(!is.na(sarc_status)) %>% 
  mutate(age = factor(agegroup, labels = c("<30", "31-45", "46-55", "56-65", ">65")),
         across(1:3, ~.x*1000)) %>% 
  ggplot(aes(x= age, y= sarc_status, group = sarc_status, fill = sarc_status))+
  
  #  geom_text(aes(label = paste(round(nyear,0), "\n(",ncas,")",sep = "")), size = 3, 
  #             show.legend = F,family = "Roboto")+
  geom_text(aes(label = round(nyear,0)), size = 3.5, 
            show.legend = F,family = "Roboto")+
  scale_color_scico_d(palette = "batlow")+
  coord_cartesian(xlim = c(.6,5.4), ylim = c(0,2.4), expand = F, clip = "off")+
  geom_segment(aes(x= .7,xend=.8,y=sarc_status, yend = sarc_status, color = sarc_status), show.legend = F, linewidth=2)+
  annotate("text", x= c(.6), y = c(2.4),family = "Roboto", fontface = "bold",
           label = c("Years at risk"), hjust = 0, vjust= 0)+
  theme_void(base_family = "Roboto")

plot1<- p1/p2  +plot_layout(heights = c(1,.45))
plot1
p3<-
  hf %>% 
  mutate(time1 = t2_obstruction-echo_age0 ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_obstruction)~sarc_status, data = .) %>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x)) %>% 
  ggplot(aes(x=time, y= estimate, ymin = conf.low, ymax= conf.high, color = strata))+
  geom_line( show.legend=F)+
  geom_ribbon(aes(fill = strata, alpha = .2),show.legend=F)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,.5),
                  expand = F)+
  labs(x = "Years from first SHaRe visit",
       y = "Cumulative incidence of obstruction")+
  scale_fill_scico_d()+
  scale_color_scico_d()+
  scale_y_continuous(breaks = seq(0,1,.1))+
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
        axis.text.y = element_text(family = "Helvetica", size = 10))
p4<-
  hf %>% 
  mutate(time1 = t2_obstruction-echo_age0 ) %>% 
  filter(time1>0) %>% 
  surv_fit(Surv(time1, event_obstruction)~sarc_status, data = .)%>% broom::tidy() %>% 
  mutate(across(.cols = c(estimate, conf.low, conf.high), ~1-.x),
         time2 = trunc(time),
         strata = str_replace(strata, "sarc_status=", "")) %>% 
  group_by(strata, time2) %>% 
  mutate(r= row_number()) %>% filter(r==1, time2<16) %>% 
  summarise(risk = n.risk, .groups = "drop") %>%  #pivot_wider(names_from = strata, values_from = risk) 
  #filter(time2 %in% c(0,2,4,6,8,10)) %>% 
  ggplot(aes(x=time2, y= strata, label = risk))+
  geom_text(size= 3.5, family = "Roboto")+
  theme_void()+
  scale_color_scico_d()+
  geom_segment(aes(x= -.3,xend=-.5,y=strata, yend = strata, color = strata), show.legend = F, linewidth=2)+
  annotate("text", x= c(0), y = c(2.4),family = "Roboto", fontface = "bold",
           label = c("Numbers at risk"), hjust = 0, vjust= 0)+
  coord_cartesian(xlim = c(0,10), 
                  ylim= c(0,2.4),
                  expand = F, clip = "off")

p3/p4
p3/p4/p1/p2+plot_layout(heights = c(1,.45,1,.45))+plot_annotation(tag_levels = list(c("A","","B","")))
ggsave(filename = "obstruction_age-inc.tiff", compression = "lzw", height = 24, width = 18, units = "cm", dpi =900)






######



dfposneg %>%
  filter(t2_obstruction>echo_age0) %>% 
  mutate(nyha = if_else(nyha=="4","3", nyha),
         primary_diagnosis_age = primary_diagnosis_age/10,
         echo_max_lvt = echo_max_lvt/10) %>% 
  coxph(Surv(t2_obstruction-echo_age0, event_obstruction)~sex+
          sarc_status+is_proband+htn+obese+ primary_diagnosis_age,data =.) %>% 
  broom::tidy(conf.int=T, exponentiate =T) %>%
  mutate(xmin = min(conf.low),
         xmax = max(conf.high),
         term = case_when(str_detect(term, "SAR")~"Sarc positive",
                          str_detect(term, "max_lvt")~"Max LV wall \n thickness (per 10mm)",
                          str_detect(term, "primar")~"Age at HCM \n (per 10 years)",
                          str_detect(term, "htn")~"Hypertension",
                          str_detect(term, "obese")~"Obese",
                          str_detect(term, "af")~"Atrial fibrillation",
                          str_detect(term, "is_proband")~"Proband",
                          str_detect(term, "Male")~"Male",
                          
                          T~term),
         term = fct_reorder(term, desc(estimate))) %>% 
  ggplot(aes(y=term, x= estimate, xmin = conf.low, xmax = conf.high ))+ 
  geom_segment(aes(x=xmin-.01, xend=xmax+.2, y =term, yend=term ), linetype = 3, color = "gray70")+
  geom_segment(aes(x = 1, xend=1, y=1, yend = 6.4), linetype =2, color = scico(1, palette = "berlin", direction = -1))+
  geom_errorbar(width =.1)+
  geom_point(size = 5, shape = 21, color = "white", fill = scico(1, palette = "berlin", begin = .1))+
  scale_fill_scico_d()+
  scale_x_continuous( trans = "log2", 
                      breaks = scales::trans_breaks("log2", function(x) round(2^x,1))(c(.5, 2.8)))+
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_markdown(family = "Roboto", color = "black"),
        axis.text = element_markdown(family = "Roboto", color = "black", size = 12),
        axis.title = element_markdown(family = "Roboto", color = "black")
  )+
  geom_text(aes(x = 4, label = paste(round(estimate,2), "\n (",round(conf.low,2),"-", round(conf.high,2),")", sep = "" )),
            size = 4)+
  annotate("text", x=4, y=6.6, label = "HR (95%CI)", fontface = "bold", family = "Roboto", size = 4.2)+
  labs(x= "Hazard ratio",
       y="",
       title = "Time to LV obstruction")+
  coord_cartesian(xlim = c(.44,5), expand = F, clip = "off")
