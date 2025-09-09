library(epitools)

#Function for plotting the relative risk
or_func <- function(data, exposure, event,terms,var1 =sex,var2 = first_encounter_age){
  data <- {{data}} %>% select(exposure = {{exposure}}, event = {{event}}, var1 = {{var1}}, var2 = {{var2}})
  glm(event~exposure+var1+var2, data = data, family = 'binomial') %>%broom::tidy(exponentiate = T, conf.int = T)  %>%
    filter(term!='(Intercept)') %>% 
    mutate(hey = term,
      term=terms) %>% 
    filter(row_number()==1)
}

#Function for extracting data used for the plotting
or_p <- function(data, exposure, event,term) {
  data <- {{data}} %>% select(exposure = {{exposure}}, event = {{event}})
  
  riskratio(data$exposure,data$event)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
    mutate(term=term)
}

# Data-set for plotting
or_df<-
  rbind(or_func(dfposneg, sarc_status, event_death, "All-cause mortality"),
        or_func(dfposneg, sarc_status, event_hcm_death, "HCM-related mortality"),
        or_func(dfposneg, sarc_status, obese, "Obesity"),
        # rr_func(dfposneg, sarc_status, event_ablation, "Ablation"),
        or_func(dfposneg, sarc_status, event_arrhythmia_a_fib, "Atrial Fibrillation"),
        or_func(dfposneg, sarc_status, event_arrhythmia_nsvt, "NSVT"),
        or_func(dfposneg, sarc_status, event_cardiac_arrest, "Cardiac Arrest"),
        or_func(dfposneg, sarc_status, event_htn, "Hypertension"),
        or_func(dfposneg, sarc_status, event_icd, "ICD implantation"),
        or_func(dfposneg, sarc_status, event_lvsd, "LVSD"),
        or_func(dfposneg, sarc_status, event_nyha_hf, "NYHA III-IV"),
        or_func(dfposneg, sarc_status, event_obstruction, "Obstruction"),
        or_func(dfposneg, sarc_status, event_srt, "SRT"),
        or_func(dfposneg, sarc_status, event_composite_v_arrhythmia, "Composite VA"),
        or_func(dfposneg, sarc_status, event_stroke, "Stroke"),
        or_func(dfposneg, sarc_status, event_htxvad, "Cardiac transplantation")  
  ) %>%
  left_join( 
    bind_rows(rr_p(dfposneg, sarc_status, event_death, "All-cause mortality"),
              rr_p(dfposneg, sarc_status, event_hcm_death, "HCM-related mortality"),
              rr_p(dfposneg, sarc_status, obese, "Obesity"),
              # rr_p(dfposneg, sarc_status, event_ablation, "Ablation"),
              rr_p(dfposneg, sarc_status, event_arrhythmia_a_fib, "Atrial Fibrillation"),
              rr_p(dfposneg, sarc_status, event_arrhythmia_nsvt, "NSVT"),
              rr_p(dfposneg, sarc_status, event_cardiac_arrest, "Cardiac Arrest"),
              rr_p(dfposneg, sarc_status, event_htn, "Hypertension"),
              rr_p(dfposneg, sarc_status, event_icd, "ICD implantation"),
              rr_p(dfposneg, sarc_status, event_lvsd, "LVSD"),
              rr_p(dfposneg, sarc_status, event_nyha_hf, "NYHA III-IV"),
              rr_p(dfposneg, sarc_status, event_obstruction, "Obstruction"),
              rr_p(dfposneg, sarc_status, event_srt, "SRT"),
              rr_p(dfposneg, sarc_status, event_composite_v_arrhythmia, "Composite VA"),
              rr_p(dfposneg, sarc_status, event_stroke, "Stroke"),
              rr_p(dfposneg, sarc_status, event_htxvad, "Cardiac transplantation")  
    )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                  nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                  sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                  nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%")))
or_df %>% 
  mutate(term_cat = case_when( term %in% c("All-cause mortality","HCM-related mortality")~'Mortality',
                               term %in% c("Atrial Fibrillation","NSVT", 'Cardiac Arrest', 'ICD implantation',
                                           'Composite VA')~'Arrhythmias',
                               term %in% c("Obesity","Hypertension")~'Cardiovascular<br> comorbidities',
                               term %in% c("LVSD","Cardiac transplantation", 'NYHA III-IV')~'Heart Failure',
                               term %in% c("Stroke")~'Stroke',
                               term %in% c("Obstruction", 'SRT')~'Cardiac<br> remodeling',
                               T~'VUF'
  ),
  term_cat = factor(term_cat, levels = c('Cardiovascular<br> comorbidities','Cardiac<br> remodeling','Heart Failure','Arrhythmias','Stroke','Mortality'))) %>% 
  mutate(.est = estimate,
         .lower = conf.low,
         .upper = conf.high
  ) %>% 
  #  filter(term !="Cardiac transplantation") %>% 
  mutate(term = fct_reorder(term, desc(.est)),
         term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "NYHA III-IV", "nyha_hf", "Stroke", "Composite HF", 
                            "All-cause mortality", "HCM-related mortality" , 
                            "Atrial Fibrillation",
                            "NSVT", "Cardiac Arrest", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD", "Composite VA"), 
         term = fct_rev(term),
         term = fct_reorder(term, desc(term_cat))) %>% 
  ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = 
               term_cat
             #log(.est^(1/3))
  ))+
  
  # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=8.2),fill = "gray89", alpha = .2)+
  # geom_rect(aes(xmin=1,xmax = .5,ymin=11.8,ymax=15.2),fill = "gray89", alpha = .2)+
  geom_segment(aes(x=.37, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
  geom_vline(aes(xintercept =1), color = chris_colors_1[10], linetype =2)+
  geom_errorbar(width = .1)+
  geom_point(size = 4, shape= 21)+
  labs(x = "Adjusted Odds Ratio", y="")+
  scale_fill_manual(values = chris_colors_1)+
  scale_y_discrete()+
  scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
  geom_text(aes(x= 11, y = term, label = sarc_prevalence), size = 3.5, vjust = 0.3)+
  geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3.5, vjust = 0.3)+
  geom_text(aes(x= 18, y = term, label = format(round(.est, 2), nsmall = 2)), size = 3.5, vjust = 0.3)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black", hjust = .25),
        legend.position = 
          #c(.51,.51),
          'bottom',
        
        legend.title = element_blank(),
        legend.key = element_blank(),
        # legend
        legend.text = element_markdown(size = 6),
        legend.direction = 'horizontal'
  )+
  guides(fill = guide_legend(nrow = 1))+
  geom_segment(aes(x= c(.9), xend = c(.6), y= 15.6, yend=15.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x= c(1.1), xend = c(1.7), y= 15.6, yend=15.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  annotate("text", x= c(6,11,18), y= 16, label = c(glue::glue("Prevalence \n non-sarcomeric\n HCM n= {nrow(dfneg)}"), 
                                                   glue::glue("Prevalence \n sarcomeric\n HCM n= {nrow(dfpos)}"),
                                                   'Odds \n Ratio'
  ), family= "Roboto", size = 3, fontface = "bold", vjust = .33)+
  annotate("text", x= c(.9,1.1), y= 16, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 3, fontface = "bold", hjust = c(1,0), vjust =0)+
  coord_cartesian(xlim = c(.37,24), ylim=c(.6,17), expand = F, clip = "off")
ggsave(filename = "Circ - Figure 1 - adjusted.tiff", compression = "lzw", height = 16, width = 20, units = "cm", dpi =2200)    
ggsave(filename = "Circ - Figure 1.pdf", device = cairo_pdf, height = 16, width = 20, units = "cm", dpi =2200)    
