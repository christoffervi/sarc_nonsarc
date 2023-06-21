vo2_df<-
  full_join(df %>% pivot_longer(contains("stress_mvo2")&!stress_mvo2x,
                    names_to = "exam", values_to = "stress_mvo2") %>% 
  mutate(r=row_number()) %>% select(pid,sex,exam,stress_mvo2,r),

df  %>%  pivot_longer(contains("stress_age")&!stress_agex,
                      names_to = "stress_age",
                      values_to = "age")%>% 
  mutate(r=row_number())) %>% 
  bind_cols(
df  %>%  pivot_longer(contains("stress_ve_vco2_slope")&!stress_ve_vco2_slopex,
                      names_to = "stress_ve_vco2_slope",
                      values_to = "ve_vco2_slope")%>% select(ve_vco2_slope),
df  %>%  pivot_longer(contains("stress_rer")&!stress_re_rx,
                      names_to = "stress_rer",
                      values_to = "rer")%>% select(rer)
) %>% 
  filter(!is.na(age) &!is.na(stress_mvo2))

vo2_df %>% mutate(age0 = abs(age-clinic_visit_age0),
                  age1 = abs(age-clinic_visit_age1),
                  age2 = abs(age-clinic_visit_age2),
                  age3 = abs(age-clinic_visit_age3),
                  age4 = abs(age-clinic_visit_age4),
                  age5 = abs(age-clinic_visit_age5),
                  age6 = abs(age-clinic_visit_age6),
                  age7 = abs(age-clinic_visit_age7),
                  age8 = abs(age-clinic_visit_age8),
                  age9 = abs(age-clinic_visit_age9),
                  decho_age0 = abs(age-echo_age0),
                  decho_age1 = abs(age-echo_age1),
                  decho_age2 = abs(age-echo_age2),
                  decho_age3 = abs(age-echo_age3),
                  decho_age4 = abs(age-echo_age4),
                  decho_age5 = abs(age-echo_age5),
                  decho_age6 = abs(age-echo_age6),
                  decho_age7 = abs(age-echo_age7),
                  decho_age8 = abs(age-echo_age8),
                  decho_age9 = abs(age-echo_age9),
                  dcmri_age0 = abs(age-cmri_age0),
                  dcmri_age1 = abs(age-cmri_age1),
                  dcmri_age2 = abs(age-cmri_age2),
                  dcmri_age3 = abs(age-cmri_age3),
                  dcmri_age4 = abs(age-cmri_age4),
                  r= row_number()) -> vo2_df


#vo2_df$max_col <- names(vo2_df%>% select(age0:age9))[max.col(replace(vo2_df %>% select(age0:age9), is.na(vo2_df%>% select(age0:age9)),0),ties.method = "first")]
#vo2_df$max_col
get_lowest_col <- function(row) {
  # Find the index of the non-NA values in the row
  non_na_idxs <- which(!is.na(row))
  if (length(non_na_idxs) == 0) {
    # All values in the row are NA, return an empty string
    ""
  } else {
    # Find the index of the minimum value(s)
    min_idxs <- which(row[non_na_idxs] == min(row[non_na_idxs]))
    # Return the name(s) of the column(s) with the minimum value(s)
    paste(names(row)[non_na_idxs][min_idxs], collapse = ",")
  }
}

# Use apply() with MARGIN=1 to apply the function to each row of the dataset
vo2_df$lowest_col <- apply(vo2_df %>% select(age0:age9), 1, get_lowest_col)
vo2_df$lowest_echo <- apply(vo2_df %>% select(decho_age0:decho_age9), 1, get_lowest_col)
vo2_df$lowest_cmri <- apply(vo2_df %>% select(dcmri_age0:dcmri_age4), 1, get_lowest_col)
vo2_df<-
  vo2_df %>% unnest(cols = c(lowest_echo, lowest_col, lowest_cmri))

vo2_df<-
 vo2_df %>% 
    mutate(bmi = case_when(lowest_col=="age0"~clinic_visit_bmi0,
                         lowest_col=="age1"~clinic_visit_bmi1,
                         lowest_col=="age2"~clinic_visit_bmi2,
                         lowest_col=="age3"~clinic_visit_bmi3,
                         lowest_col=="age4"~clinic_visit_bmi4,
                         lowest_col=="age5"~clinic_visit_bmi5,
                         lowest_col=="age6"~clinic_visit_bmi6,
                         lowest_col=="age7"~clinic_visit_bmi7,
                         lowest_col=="age8"~clinic_visit_bmi8,
                         lowest_col=="age9"~clinic_visit_bmi9,
                         T~NA_real_),
         diabp = case_when(lowest_col=="age0"~clinic_visit_b_pd0,
                         lowest_col=="age1"~clinic_visit_b_pd1,
                         lowest_col=="age2"~clinic_visit_b_pd2,
                         lowest_col=="age3"~clinic_visit_b_pd3,
                         lowest_col=="age4"~clinic_visit_b_pd4,
                         lowest_col=="age5"~clinic_visit_b_pd5,
                         lowest_col=="age6"~clinic_visit_b_pd6,
                         lowest_col=="age7"~clinic_visit_b_pd7,
                         lowest_col=="age8"~clinic_visit_b_pd8,
                         lowest_col=="age9"~clinic_visit_b_pd9,
                         T~NA_real_),
         sysbp = case_when(lowest_col=="age0"~clinic_visit_b_ps0,
                         lowest_col=="age1"~clinic_visit_b_ps1,
                         lowest_col=="age2"~clinic_visit_b_ps2,
                         lowest_col=="age3"~clinic_visit_b_ps3,
                         lowest_col=="age4"~clinic_visit_b_ps4,
                         lowest_col=="age5"~clinic_visit_b_ps5,
                         lowest_col=="age6"~clinic_visit_b_ps6,
                         lowest_col=="age7"~clinic_visit_b_ps7,
                         lowest_col=="age8"~clinic_visit_b_ps8,
                         lowest_col=="age9"~clinic_visit_b_ps9,
                         T~NA_real_),
         max_lvt = case_when(str_detect(lowest_echo, "age0")~echo_max_lvt0,
                             str_detect(lowest_echo, "age1")~echo_max_lvt1,
                             str_detect(lowest_echo, "age2")~echo_max_lvt2,
                             str_detect(lowest_echo, "age3")~echo_max_lvt3,
                             str_detect(lowest_echo, "age4")~echo_max_lvt4,
                             str_detect(lowest_echo, "age5")~echo_max_lvt5,
                             str_detect(lowest_echo, "age6")~echo_max_lvt6,
                             str_detect(lowest_echo, "age7")~echo_max_lvt7,
                             str_detect(lowest_echo, "age8")~echo_max_lvt8,
                             lowest_echo=="decho_age9"~echo_max_lvt9,
                             T~NA_real_),
         lvef =case_when(str_detect(lowest_echo, "age0")~echo_lvef0,
                         str_detect(lowest_echo, "age1")~echo_lvef1,
                         str_detect(lowest_echo, "age2")~echo_lvef2,
                         str_detect(lowest_echo, "age3")~echo_lvef3,
                         str_detect(lowest_echo, "age4")~echo_lvef4,
                         str_detect(lowest_echo, "age5")~echo_lvef5,
                         str_detect(lowest_echo, "age6")~echo_lvef6,
                         str_detect(lowest_echo, "age7")~echo_lvef7,
                         str_detect(lowest_echo, "age8")~echo_lvef8,
                         lowest_echo=="decho_age9"~echo_lvef9,
                         T~NA_real_),
         esc_scd_score = case_when(str_detect(lowest_echo, "age0")~echo_esc_risk_score0,
                                   str_detect(lowest_echo, "age1")~echo_esc_risk_score1,
                                   str_detect(lowest_echo, "age2")~echo_esc_risk_score2,
                                   str_detect(lowest_echo, "age3")~echo_esc_risk_score3,
                                   str_detect(lowest_echo, "age4")~echo_esc_risk_score4,
                                   str_detect(lowest_echo, "age5")~echo_esc_risk_score5,
                                   str_detect(lowest_echo, "age6")~echo_esc_risk_score6,
                                   str_detect(lowest_echo, "age7")~echo_esc_risk_score7,
                                   str_detect(lowest_echo, "age8")~echo_esc_risk_score8,
                                   lowest_echo=="decho_age9"~echo_esc_risk_score9,
                                   T~NA_real_),
         la = case_when(str_detect(lowest_echo, "age0")~echo_la0,
                                   str_detect(lowest_echo, "age1")~echo_la1,
                                   str_detect(lowest_echo, "age2")~echo_la2,
                                   str_detect(lowest_echo, "age3")~echo_la3,
                                   str_detect(lowest_echo, "age4")~echo_la4,
                                   str_detect(lowest_echo, "age5")~echo_la5,
                                   str_detect(lowest_echo, "age6")~echo_la6,
                                   str_detect(lowest_echo, "age7")~echo_la7,
                                   str_detect(lowest_echo, "age8")~echo_la8,
                                   lowest_echo=="decho_age9"~echo_la9,
                                   T~NA_real_),
         lvidd = case_when(str_detect(lowest_echo, "age0")~echo_lvi_dd0,
                        str_detect(lowest_echo, "age1")~echo_lvi_dd1,
                        str_detect(lowest_echo, "age2")~echo_lvi_dd2,
                        str_detect(lowest_echo, "age3")~echo_lvi_dd3,
                        str_detect(lowest_echo, "age4")~echo_lvi_dd4,
                        str_detect(lowest_echo, "age5")~echo_lvi_dd5,
                        str_detect(lowest_echo, "age6")~echo_lvi_dd6,
                        str_detect(lowest_echo, "age7")~echo_lvi_dd7,
                        str_detect(lowest_echo, "age8")~echo_lvi_dd8,
                        lowest_echo=="decho_age9"~echo_lvi_dd9,
                        T~NA_real_),
         lvids = case_when(str_detect(lowest_echo, "age0")~echo_lvi_ds0,
                           str_detect(lowest_echo, "age1")~echo_lvi_ds1,
                           str_detect(lowest_echo, "age2")~echo_lvi_ds2,
                           str_detect(lowest_echo, "age3")~echo_lvi_ds3,
                           str_detect(lowest_echo, "age4")~echo_lvi_ds4,
                           str_detect(lowest_echo, "age5")~echo_lvi_ds5,
                           str_detect(lowest_echo, "age6")~echo_lvi_ds6,
                           str_detect(lowest_echo, "age7")~echo_lvi_ds7,
                           str_detect(lowest_echo, "age8")~echo_lvi_ds8,
                           lowest_echo=="decho_age9"~echo_lvi_ds9,
                           T~NA_real_),
         lvot = case_when(str_detect(lowest_echo, "age0")~echo_lvot_gradient0,
                           str_detect(lowest_echo, "age1")~echo_lvot_gradient1,
                           str_detect(lowest_echo, "age2")~echo_lvot_gradient2,
                           str_detect(lowest_echo, "age3")~echo_lvot_gradient3,
                           str_detect(lowest_echo, "age4")~echo_lvot_gradient4,
                           str_detect(lowest_echo, "age5")~echo_lvot_gradient5,
                           str_detect(lowest_echo, "age6")~echo_lvot_gradient6,
                           str_detect(lowest_echo, "age7")~echo_lvot_gradient7,
                           str_detect(lowest_echo, "age8")~echo_lvot_gradient8,
                           lowest_echo=="decho_age9"~echo_lvot_gradient9,
                           T~NA_real_),
          cmr_lvef = case_when(str_detect(lowest_cmri,"age0")~cmri_lvef0,
                               str_detect(lowest_cmri,"age1")~cmri_lvef1,
                               str_detect(lowest_cmri,"age2")~cmri_lvef2,
                               str_detect(lowest_cmri,"age3")~cmri_lvef3,
                             #  str_detect(lowest_cmri,"age4")~cmri_lvef4,
                               T~NA_real_),
         cmr_max_lvt = case_when(str_detect(lowest_cmri,"ge0")~cmri_max_lvt0,
                                 str_detect(lowest_cmri,"age1")~cmri_max_lvt1,
                              lowest_cmri=="dcmri_age2"~cmri_max_lvt2,
                              lowest_cmri=="dcmri_age3"~cmri_max_lvt3,
                              #lowest_cmri=="dcmri_age4"~cmri_max_lvt4,
                              T~NA_real_),
         cmr_lv_mass = case_when(str_detect(lowest_cmri,"dcmri_age0")~cmri_lv_mass0,
                                 str_detect(lowest_cmri,"dcmri_age1")~cmri_lv_mass1,
                              lowest_cmri=="dcmri_age2"~cmri_lv_mass2,
                              lowest_cmri=="dcmri_age3"~cmri_lv_mass3,
                            #  lowest_cmri=="dcmri_age4"~cmri_lv_mass4,
                              T~NA_real_),
         cmr_lge = case_when(str_detect(lowest_cmri,"dcmri_age0")~cmri_lge0,
                             str_detect(lowest_cmri,"dcmri_age1")~cmri_lge1,
                              lowest_cmri=="dcmri_age2"~cmri_lge2,
                              lowest_cmri=="dcmri_age3"~cmri_lge3,
                             # lowest_cmri=="dcmri_age4"~cmri_lge4,
                              T~NA_character_),
         cmr_lge_percent = case_when(str_detect(lowest_cmri,"dcmri_age0")~cmri_lge_percent0,
                                     str_detect(lowest_cmri,"dcmri_age1")~cmri_lge_percent1,
                              lowest_cmri=="dcmri_age2"~cmri_lge_percent2,
                              #lowest_cmri=="dcmri_age3"~cmri_lge_percent3,
                              #lowest_cmri=="dcmri_age4"~cmri_lge_percent4,
                              T~NA_real_)) 

vo2<-
vo2_df %>% 
  
  mutate(af_c = case_when(event_arrhythmia_a_fib==1 & t2_arrhythmia_a_fib<=age~1,
                          T~0),
         htx_c = case_when(event_transplant==1 & t2_transplant<=age~1,
                           event_vad==1 & t2_vad<=age~1,
                          T~0),
         myectomy = case_when(event_myectomy==1 & t2_myectomy<=age~1,
                              T~0),
         htn = case_when(event_htn==1 & t2_htn<=age~1,
                         T~0),
         stroke = case_when(event_stroke==1 & t2_stroke<=age~1,
                         T~0),
  ) %>% 
  select(pid, primary_diagnosis,primary_diagnosis_age, 
         af_c,htx_c,
         myectomy,htn,stroke,
          sex, age, genes_plp, sarc_status,age, stress_mvo2,rer,ve_vco2_slope,bmi, sysbp,diabp,
          max_lvt, la, lvef, lvot, lvidd, lvids,esc_scd_score,
          cmr_lvef,cmr_lge,cmr_lge_percent, cmr_max_lvt, cmr_lv_mass
  )


vo2 %>% filter(primary_diagnosis=="HCM") %>%  
  mutate(sarc_status = fct_relevel(sarc_status, "SARC(+)"),
         cmr_lge = factor(cmr_lge)) %>% 
  select(!c(pid,genes_plp,#sarc_status,
            esc_scd_score,diabp,
            rer, primary_diagnosis, lvot,cmr_lge)&
         !contains("event")&!contains("t2"))  %>% 
  lm(stress_mvo2~., data = .) %>% 
  broom::tidy() %>% arrange(p.value) %>% print(n=Inf)
broom::glance() %>% arrange(p.value)

vo2 %>% 
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  lm(stress_mvo2~age+sarc_status+sex+bmi+af_c+rer, data = .) %>% 
  gtsummary::tbl_regression()

library(Hmisc)
vo2 %>% 
  filter(primary_diagnosis=="HCM" & htx_c!=1 &rer>1) %>%  
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  mutate(d = age-primary_diagnosis_age,
         d = if_else(primary_diagnosis_age<18,1,0,0),
         
         lvef = if_else(lvef<50, 1,0,0)
         ) %>% 
  lm(stress_mvo2~age+sex+bmi+la+d+max_lvt+lvef+
       #af_c+myectomy+htn+stroke+
       sarc_status, data=.) %>% 
  #broom::tidy() %>% arrange(desc(abs(statistic)))
 # gtsummary::tbl_regression()
 # summary()
  broom::augment(interval = "prediction") %>% 
  ggplot(aes(x=.fitted, y =stress_mvo2, ymin = .lower, ymax= .upper))+
  geom_point(aes(), alpha =.5)+
  geom_smooth(method = "lm")+
  #geom_smooth()+  
  geom_ribbon(alpha = .2)
ggsave("vo2_predict.tiff", compression = "lzw", units = "cm", height = 16, width = 24)

vo2 %>% filter(primary_diagnosis=="HCM") %>%  
  mutate(d = age-primary_diagnosis_age) %>% 
  lm(stress_mvo2~age+sex+bmi+la+d+max_lvt+lvef+af_c, data=.) %>% 
  #summary()
  broom::augment(interval = "prediction") %>% filter(abs(.resid)>15) %>% 
  ggplot(aes(x = sex,  y=lvef))+
  geom_sina()


vo2 %>% filter(primary_diagnosis=="HCM" & htx_c!=1) %>%  
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  mutate(d = age-primary_diagnosis_age,
         d = if_else(primary_diagnosis_age<18,1,0,0),
         
         lvef = if_else(lvef<50, 1,0,0),
         age10 = age/10,
         bmi5 = bmi/5,
         la10 = la/10,
         max_lvt5 = max_lvt/5
  ) %>% 
  lm(stress_mvo2~age10+sex+bmi5+la10+d+max_lvt5+lvef+sarc_status+
       af_c+myectomy+htn+stroke, data=.) %>%
broom::tidy(conf.int=T)  %>% 
  filter(term!="(Intercept)") %>% 
  mutate(term = fct_reorder(term, desc(p.value))) %>% 
  ggplot(aes(x= term, y = estimate, ymin = conf.low, ymax= conf.high))+
  geom_errorbar(width =.1)+
  geom_point(size = 3)+
  geom_text(aes( y= 8.5, label= paste(round(estimate,1), "\n (",round(conf.low, 1), " to ", round(conf.high,1),")", sep = "")))+
  geom_hline(aes(yintercept = 0), linetype =2, color = "red")+
  coord_flip(ylim = c(-6.5,9))+
  scale_y_continuous(breaks = seq(-7,7.1))+
  theme(panel.background = element_rect(fill = "white"),
  axis.text = element_markdown(family = "Roboto", color = "black"),
  axis.title = element_markdown(family = "Roboto", color = "black"),
  panel.grid.major.y = element_line(color = "gray79", linetype = 3)
  )
ggsave("vo2_linear_model.tiff", compression = "lzw", width = 28, height = 16, units = "cm")






full_join(
vo2 %>% 
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  lm(stress_mvo2~rcs(age,5)+sex+rcs(bmi,5)+af_c+rcs(rer,3), data = .) %>%
  broom::augment() %>% 
  mutate(pred_vo2 = stress_mvo2/.fitted*100) %>% 
  select(.rownames, stress_mvo2,.fitted, pred_vo2),




vo2 %>% 
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  lm(stress_mvo2~rcs(age,5)+sex+rcs(bmi,5)+af_c+rcs(rer,3)+sarc_status, data = .) %>%
  broom::augment() %>% 
  mutate(pred_vo2 = stress_mvo2/.fitted*100) %>% 
  select(.rownames, stress_mvo2,sarc_status)) %>% 
  t.test(pred_vo2~sarc_status, data =.) %>% broom::tidy()


vo2 %>% 
  filter(sarc_status%in% c("SARC(-)", "SARC(+)")) %>%  
  lm(stress_mvo2~rcs(age,5)+sex+rcs(bmi,5)+af_c+rcs(rer,3), data = .) %>%
  broom::glance() #%>% 
  mutate(pred_vo2 = stress_mvo2/.fitted*100)  
  select(.rownames, stress_mvo2,.fitted, pred_vo2) %>% 
  broom::glance()

  
  
  qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) }
  MESS::wallyplot(lm(stress_mvo2~rcs(age,5)+sex+rcs(bmi,5)+af_c+rcs(rer,3)+sarc_status, data = vo2), FUN=qqnorm.wally, main="")
  