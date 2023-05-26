library(epitools)

#Function for plotting the relative risk
rr_func <- function(data, exposure, event,term){
  data <- {{data}} %>% select(exposure = {{exposure}}, event = {{event}})
  riskratio(data$exposure,data$event)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    as_tibble(.,rownames = "rowname") %>% mutate(term=term)
}

#Function for extracting data used for the plotting
rr_p <- function(data, exposure, event,term) {
  data <- {{data}} %>% select(exposure = {{exposure}}, event = {{event}})
  
  riskratio(data$exposure,data$event)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
    mutate(term=term)
}

# Data-set for plotting
rr_df<-
   rbind(rr_func(dfposneg, sarc_status, event_death, "Death"),
         rr_func(dfposneg, sarc_status, obese, "Obesity"),
         rr_func(dfposneg, sarc_status, event_ablation, "Ablation"),
         rr_func(dfposneg, sarc_status, event_arrhythmia_a_fib, "Atrial Fibrillation"),
         rr_func(dfposneg, sarc_status, event_arrhythmia_nsvt, "NSVT"),
         rr_func(dfposneg, sarc_status, event_cardiac_arrest, "Cardiac Arrest"),
         rr_func(dfposneg, sarc_status, event_htn, "Hypertension"),
         rr_func(dfposneg, sarc_status, event_icd, "ICD implantation"),
         rr_func(dfposneg, sarc_status, event_lvsd, "LVSD"),
         rr_func(dfposneg, sarc_status, event_nyha_hf, "NYHA III-IV"),
         rr_func(dfposneg, sarc_status, event_obstruction, "Obstruction"),
         rr_func(dfposneg, sarc_status, event_srt, "SRT"),
         rr_func(dfposneg, sarc_status, event_composite_v_arrhythmia, "Composite VT"),
         rr_func(dfposneg, sarc_status, event_stroke, "Stroke"),
         rr_func(dfposneg, sarc_status, event_htxvad, "Cardiac transplantation")  
         ) %>%
     left_join( 
       bind_rows(rr_p(dfposneg, sarc_status, event_death, "Death"),
                 rr_p(dfposneg, sarc_status, obese, "Obesity"),
                 rr_p(dfposneg, sarc_status, event_ablation, "Ablation"),
                 rr_p(dfposneg, sarc_status, event_arrhythmia_a_fib, "Atrial Fibrillation"),
                 rr_p(dfposneg, sarc_status, event_arrhythmia_nsvt, "NSVT"),
                 rr_p(dfposneg, sarc_status, event_cardiac_arrest, "Cardiac Arrest"),
                 rr_p(dfposneg, sarc_status, event_htn, "Hypertension"),
                 rr_p(dfposneg, sarc_status, event_icd, "ICD implantation"),
                 rr_p(dfposneg, sarc_status, event_lvsd, "LVSD"),
                 rr_p(dfposneg, sarc_status, event_nyha_hf, "NYHA III-IV"),
                 rr_p(dfposneg, sarc_status, event_obstruction, "Obstruction"),
                 rr_p(dfposneg, sarc_status, event_srt, "SRT"),
                 rr_p(dfposneg, sarc_status, event_composite_v_arrhythmia, "Composite VT"),
                 rr_p(dfposneg, sarc_status, event_stroke, "Stroke"),
                 rr_p(dfposneg, sarc_status, event_htxvad, "Cardiac transplantation")  
       )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                   nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                   sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                   nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%")))
     
rr_df %>% mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
) %>% 
#  filter(term !="Cardiac transplantation") %>% 
  mutate(term = fct_reorder(term, desc(.est)),
         term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "NYHA III-IV", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                            "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD", "Composite VT"), 
term = fct_rev(term)) %>% 
  ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
  
 # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=8.2),fill = "gray89", alpha = .2)+
 # geom_rect(aes(xmin=1,xmax = .5,ymin=11.8,ymax=15.2),fill = "gray89", alpha = .2)+
  geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
  geom_vline(aes(xintercept =1), color = "red", linetype =2)+
  geom_errorbar(width = .1)+
  geom_point(size = 4, shape= 21)+
  labs(x = "relative risk ratio", y="")+
  scale_fill_scico(palette = "berlin")+
  scale_y_discrete()+
  scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
  geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3.5, vjust = 0.3)+
  geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3.5, vjust = 0.3)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        legend.position = "none"
  )+
  geom_segment(aes(x= c(.9), xend = c(.6), y= 15.6, yend=15.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x= c(1.1), xend = c(1.7), y= 15.6, yend=15.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  annotate("text", x= c(6,10), y= 16, label = c(glue::glue("Prevalence \n non-sarcomeric\n HCM n= {nrow(dfneg)}"), 
                                                glue::glue("Prevalence \n sarcomeric\n HCM n= {nrow(dfpos)}") 
                                                ), family= "Roboto", size = 3, fontface = "bold", vjust = .33)+
  annotate("text", x= c(.9,1.1), y= 16, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 3, fontface = "bold", hjust = c(1,0), vjust =0)+
  coord_cartesian(xlim = c(.4,12), ylim=c(.6,17), expand = F, clip = "off")
ggsave(filename = "RR2.tiff", compression = "lzw", height = 16, width = 20, units = "cm", dpi =1900)    

ggsave(filename = "RR2.tiff", compression = "lzw", height = 16*.75, width = 24*.75, units = "cm", dpi =2200)    
