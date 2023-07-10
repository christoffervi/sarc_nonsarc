#loading dataset
d<-
  readr::read_csv("~/Dropbox/BWH SHaRe/SHaRe Dataset for Analysis/For DCC/2022 Nov/SHaRe_DCC_2022_11_16.csv") %>% 
  #Making the dataset more pretty (all lowercase, spaces denoted by "_", etc.)
  janitor::clean_names() %>% rename_with(~gsub("dcc_", "", .))

# Only HCM patients
df<- d %>% filter(primary_diagnosis =="HCM") %>% 
  #Computing events present at baseline and or diagnosis
  mutate(htn = case_when(age_htn<=first_encounter_age~1,T~0),
         af = case_when(age_arrhythmia_a_fib<=first_encounter_age~1,T~0),
         SCD_aSCD = case_when(age_cardiac_arrest<=first_encounter_age~1,T~0),
         nyha_hf = case_when(age_nyha_hf<=first_encounter_age~1,T~0),
         stroke = case_when(age_stroke<=first_encounter_age~1,T~0),
         obstruction = case_when(age_obstruction<=first_encounter_age~1,T~0),
         lvsd = case_when(age_lvef50<=first_encounter_age~1,
                          age_lvef50<=echo_age0~1,T~0),
         severe_lvsd = case_when(age_lvef35<=first_encounter_age~1,
                                 age_lvef35<=echo_age0~1,T~0),
         syncope = case_when(age_syncope<=first_encounter_age~1,T~0),
         t2_htxvad = pmin(t2_vad, t2_transplant, na.rm = T),event_htxvad = pmax(event_vad, event_transplant, na.rm = T), age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         t2_af = t2_arrhythmia_a_fib, event_af = event_arrhythmia_a_fib, age_af = age_arrhythmia_a_fib, # renaming af var
         t2_lvsd = pmin(t2_lvef50, t2_lvef35, na.rm = T),event_lvsd = pmax(event_lvef50, event_lvef35, na.rm = T), age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         t2_vt = pmin(t2_composite_v_arrhythmia, t2_cardiac_arrest, na.rm = T), event_vt = pmax(event_composite_v_arrhythmia, event_cardiac_arrest, na.rm = T),age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         t2_hf = pmin(t2_lvsd,t2_htxvad, t2_composite_hf, t2_nyha_hf, t2_nyha_hf_systolic, na.rm = T), event_hf = pmax(event_lvsd,event_htxvad, event_composite_hf, event_nyha_hf, event_nyha_hf_systolic,na.rm=T), age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_ablation = case_when(event_ablation==1~t2_ablation,T~NA_real_),
         
         
         #LGE
         lge = case_when(cmri_lge0=="Yes"~"yes",
                         cmri_lge0=="No"~"no",
                         cmri_lge0=="no"~"no",
                         T~NA_character_
         ),
         #Computing baseline echo measures
         echo_la = case_when(!is.na(echo_la0)~echo_la0,                                !is.na(echo_la1)~echo_la1,                                !is.na(echo_la2)~echo_la2,                                !is.na(echo_la3)~echo_la3,
                             !is.na(echo_la4)~echo_la4,                                !is.na(echo_la5)~echo_la5,                                !is.na(echo_la6)~echo_la6,                                !is.na(echo_l_ax)~echo_l_ax
         ),
         echo_lvidd = case_when(!is.na(echo_lvi_dd0)~echo_lvi_dd0,                                !is.na(echo_lvi_dd1)~echo_lvi_dd1,                                !is.na(echo_lvi_dd2)~echo_lvi_dd2,                                !is.na(echo_lvi_dd3)~echo_lvi_dd3,
                                !is.na(echo_lvi_dd4)~echo_lvi_dd4,                                !is.na(echo_lvi_dd5)~echo_lvi_dd5,                                !is.na(echo_lvi_dd6)~echo_lvi_dd6,                                !is.na(echo_lvi_ddx)~echo_lvi_ddx
         ),
         echo_lvids = case_when(!is.na(echo_lvi_ds0)~echo_lvi_ds0,                                !is.na(echo_lvi_ds1)~echo_lvi_ds1,                                !is.na(echo_lvi_ds2)~echo_lvi_ds2,                                !is.na(echo_lvi_ds3)~echo_lvi_ds3,
                                !is.na(echo_lvi_ds4)~echo_lvi_ds4,                                !is.na(echo_lvi_ds5)~echo_lvi_ds5,                                !is.na(echo_lvi_ds6)~echo_lvi_ds6,                                !is.na(echo_lvi_dsx)~echo_lvi_dsx
         ),
         echo_max_lvt = case_when(!is.na(echo_max_lvt0)~echo_max_lvt0,                                  !is.na(echo_max_lvt1)~echo_max_lvt1,                                  !is.na(echo_max_lvt2)~echo_max_lvt2,                                  !is.na(echo_max_lvt3)~echo_max_lvt3,
                                  !is.na(echo_max_lvt4)~echo_max_lvt4,                                  !is.na(echo_max_lvt5)~echo_max_lvt5,                                  !is.na(echo_max_lvt6)~echo_max_lvt6,                                  !is.na(echo_max_lv_tx)~echo_max_lv_tx
         ),
         echo_lvot_gradient = case_when(!is.na(echo_lvot_gradient0)~echo_lvot_gradient0,!is.na(echo_lvot_gradient1)~echo_lvot_gradient1,!is.na(echo_lvot_gradient2)~echo_lvot_gradient2, !is.na(echo_lvot_gradient3)~echo_lvot_gradient3,
                                        !is.na(echo_lvot_gradient4)~echo_lvot_gradient4,!is.na(echo_lvot_gradient5)~echo_lvot_gradient5,!is.na(echo_lvot_gradient6)~echo_lvot_gradient6,!is.na(echo_lvot_gradientx)~echo_lvot_gradientx),
         echo_lvef = case_when(!is.na(echo_lvef0)~echo_lvef0,                                  !is.na(echo_lvef1)~echo_lvef1,                                  !is.na(echo_lvef2)~echo_lvef2,                                  !is.na(echo_lvef3)~echo_lvef3,
                               !is.na(echo_lvef4)~echo_lvef4,                                  !is.na(echo_lvef5)~echo_lvef5,                                  !is.na(echo_lvef6)~echo_lvef6,                                  !is.na(echo_lvef_zx)~echo_lvef_zx
         ),
         
         # Baseline findings at clinic visits
         sysbp = case_when(!is.na(clinic_visit_b_ps0)~clinic_visit_b_ps0,                           !is.na(clinic_visit_b_ps1)~clinic_visit_b_ps1,                           !is.na(clinic_visit_b_ps2)~clinic_visit_b_ps2,
                           !is.na(clinic_visit_b_ps3)~clinic_visit_b_ps3,                           !is.na(clinic_visit_b_ps4)~clinic_visit_b_ps4,                           !is.na(clinic_visit_b_ps5)~clinic_visit_b_ps5,                           !is.na(clinic_visit_b_ps6)~clinic_visit_b_ps6,
                           !is.na(clinic_visit_b_ps7)~clinic_visit_b_ps7,                           !is.na(clinic_visit_b_ps8)~clinic_visit_b_ps8,                           !is.na(clinic_visit_b_ps9)~clinic_visit_b_ps9,                           !is.na(clinic_visit_b_psx)~clinic_visit_b_psx
         ),
         diabp = case_when(!is.na(clinic_visit_b_pd0)~clinic_visit_b_pd0,                           !is.na(clinic_visit_b_pd1)~clinic_visit_b_pd1,                           !is.na(clinic_visit_b_pd2)~clinic_visit_b_pd2,                           !is.na(clinic_visit_b_pd3)~clinic_visit_b_pd3,
                           !is.na(clinic_visit_b_pd4)~clinic_visit_b_pd4,                           !is.na(clinic_visit_b_pd5)~clinic_visit_b_pd5,                           !is.na(clinic_visit_b_pd6)~clinic_visit_b_pd6,                           !is.na(clinic_visit_b_pd7)~clinic_visit_b_pd7,
                           !is.na(clinic_visit_b_pd8)~clinic_visit_b_pd8,                           !is.na(clinic_visit_b_pd9)~clinic_visit_b_pd9,                           !is.na(clinic_visit_b_pdx)~clinic_visit_b_pdx         ), 
         bmi = case_when(!is.na(clinic_visit_bmi0)~clinic_visit_bmi0,                         !is.na(clinic_visit_bmi1)~clinic_visit_bmi1,                         !is.na(clinic_visit_bmi2)~clinic_visit_bmi2,                         !is.na(clinic_visit_bmi3)~clinic_visit_bmi3,
                         !is.na(clinic_visit_bmi4)~clinic_visit_bmi4,                         !is.na(clinic_visit_bmi5)~clinic_visit_bmi5,                         !is.na(clinic_visit_bmi6)~clinic_visit_bmi6,                         !is.na(clinic_visit_bmi7)~clinic_visit_bmi7,
                         !is.na(clinic_visit_bmi8)~clinic_visit_bmi8,                         !is.na(clinic_visit_bmi9)~clinic_visit_bmi9,                         !is.na(clinic_visit_bm_ix)~clinic_visit_bm_ix
         ), 
         #Obesity if baseline BMI above 30
         obese = if_else(bmi>=30, 1, 0),
         event_obesity = case_when(obese==1~1,
                                   clinic_visit_bmi0>=30~1,
                                   clinic_visit_bmi1>=30~1,
                                   clinic_visit_bmi2>=30~1,
                                   clinic_visit_bmi3>=30~1,
                                   clinic_visit_bmi4>=30~1,
                                   clinic_visit_bmi5>=30~1,
                                   clinic_visit_bmi6>=30~1,
                                   clinic_visit_bmi7>=30~1,
                                   clinic_visit_bmi8>=30~1,
                                   clinic_visit_bmi9>=30~1,
                                   T~0
         ),
         age_obesity = case_when(obese==1~first_encounter_age,
                                 clinic_visit_bmi0>=30~clinic_visit_age0,
                                 clinic_visit_bmi1>=30~clinic_visit_age1,
                                 clinic_visit_bmi2>=30~clinic_visit_age2,
                                 clinic_visit_bmi3>=30~clinic_visit_age3,
                                 clinic_visit_bmi4>=30~clinic_visit_age4,
                                 clinic_visit_bmi5>=30~clinic_visit_age5,
                                 clinic_visit_bmi6>=30~clinic_visit_age6,
                                 clinic_visit_bmi7>=30~clinic_visit_age7,
                                 clinic_visit_bmi8>=30~clinic_visit_age8,
                                 clinic_visit_bmi9>=30~clinic_visit_age9,
                                 T~NA_real_
         ),
         t2_obesity = if_else(is.na(age_obesity), last_encounter_age,age_obesity),
         bsa = case_when(!is.na(clinic_visit_bsa0)~clinic_visit_bsa0,                         !is.na(clinic_visit_bsa1)~clinic_visit_bsa1,                         !is.na(clinic_visit_bsa2)~clinic_visit_bsa2,                         !is.na(clinic_visit_bsa3)~clinic_visit_bsa3,
                         !is.na(clinic_visit_bsa4)~clinic_visit_bsa4,                         !is.na(clinic_visit_bsa5)~clinic_visit_bsa5,                         !is.na(clinic_visit_bsa6)~clinic_visit_bsa6,                         !is.na(clinic_visit_bsa7)~clinic_visit_bsa7,
                         !is.na(clinic_visit_bsa8)~clinic_visit_bsa8,                         !is.na(clinic_visit_bsa9)~clinic_visit_bsa9,                         !is.na(clinic_visit_bs_ax)~clinic_visit_bs_ax
         ), echo_lvidd_bsa = echo_lvidd/bsa,echo_lvids_bsa = echo_lvids/bsa,
         nyha = case_when(!is.na(clinic_visit_nyha0)~clinic_visit_nyha0,                          !is.na(clinic_visit_nyha1)~clinic_visit_nyha1,                          !is.na(clinic_visit_nyha2)~clinic_visit_nyha2,                          !is.na(clinic_visit_nyha3)~clinic_visit_nyha3,
                          !is.na(clinic_visit_nyha4)~clinic_visit_nyha4,                          !is.na(clinic_visit_nyha5)~clinic_visit_nyha5,                          !is.na(clinic_visit_nyha6)~clinic_visit_nyha6,                          !is.na(clinic_visit_nyha7)~clinic_visit_nyha7,
                          !is.na(clinic_visit_nyha8)~clinic_visit_nyha8,                          !is.na(clinic_visit_nyha9)~clinic_visit_nyha9,                          !is.na(clinic_visit_nyh_ax)~clinic_visit_nyh_ax
         ), nyha = factor(round(nyha,0)),
         f_hx_scd = if_else(is.na(f_hx_scd_esc),"no", "yes"),
         esc_risk = case_when(echo_esc_risk_score0>=6~"High",
                              echo_esc_risk_score0>=4~"Moderate",
                              echo_esc_risk_score0<=4~"Low",
                              T~NA_character_))

#####
#overall timings
df<-
  df %>% mutate(htn_hcm = if_else(age_htn<primary_diagnosis_age,1,0,0),
                hcm_htn = if_else(age_htn>primary_diagnosis_age,1,0,0),
                htn_af = if_else(age_htn<age_arrhythmia_a_fib,1,0,0),
                af_htn = if_else(age_htn>age_arrhythmia_a_fib,1,0,0),
                htn_srt = if_else(age_htn<age_srt,1,0,0),
                srt_htn = if_else(age_htn>age_srt,1,0,0),
                htn_vt = if_else(age_htn<age_vt,1,0,0),
                vt_htn = if_else(age_htn>age_vt,1,0,0),
                htn_icd = if_else(age_htn<age_icd,1,0,0),
                icd_htn = if_else(age_htn>age_icd,1,0,0),
                htn_stroke = if_else(age_htn<age_stroke,1,0,0),
                stroke_htn = if_else(age_htn>age_stroke,1,0,0),
                htn_hf = if_else(age_htn<age_composite_hf,1,0,0),
                hf_htn = if_else(age_htn>age_composite_hf,1,0,0),
                htn_obstruction = if_else(age_htn<age_obstruction,1,0,0),
                obstruction_htn = if_else(age_htn>age_obstruction,1,0,0),
                htn_syncope = if_else(age_htn<age_syncope,1,0,0),
                syncope_htn = if_else(age_htn>age_syncope,1,0,0),
                htn_htxvad = if_else(age_htn<age_htxvad,1,0,0),
                htxvad_htn = if_else(age_htn>age_htxvad,1,0,0),
                #####
                obstruction_hcm = if_else(age_obstruction<primary_diagnosis_age,1,0,0),
                hcm_obstruction = if_else(age_obstruction>primary_diagnosis_age,1,0,0),
                obstruction_af = if_else(age_obstruction<age_arrhythmia_a_fib,1,0,0),
                af_obstruction = if_else(age_obstruction>age_arrhythmia_a_fib,1,0,0),
                obstruction_srt = if_else(age_obstruction<age_srt,1,0,0),
                srt_obstruction = if_else(age_obstruction>age_srt,1,0,0),
                obstruction_vt = if_else(age_obstruction<age_vt,1,0,0),
                vt_obstruction = if_else(age_obstruction>age_vt,1,0,0),
                obstruction_icd = if_else(age_obstruction<age_icd,1,0,0),
                icd_obstruction = if_else(age_obstruction>age_icd,1,0,0),
                obstruction_stroke = if_else(age_obstruction<age_stroke,1,0,0),
                stroke_obstruction = if_else(age_obstruction>age_stroke,1,0,0),
                obstruction_hf = if_else(age_obstruction<age_composite_hf,1,0,0),
                hf_obstruction = if_else(age_obstruction>age_composite_hf,1,0,0),
                obstruction_syncope = if_else(age_obstruction<age_syncope,1,0,0),
                syncope_obstruction = if_else(age_obstruction>age_syncope,1,0,0),
                obstruction_htxvad = if_else(age_obstruction<age_htxvad,1,0,0),
                htxvad_obstruction = if_else(age_obstruction>age_htxvad,1,0,0),
                ######
                af_hcm = if_else(age_af<primary_diagnosis_age,1,0,0),
                hcm_af = if_else(age_af>primary_diagnosis_age,1,0,0),
                af_srt = if_else(age_af<age_srt,1,0,0),
                srt_af = if_else(age_af>age_srt,1,0,0),
                af_vt = if_else(age_af<age_vt,1,0,0),
                vt_af = if_else(age_af>age_vt,1,0,0),
                af_icd = if_else(age_af<age_icd,1,0,0),
                icd_af = if_else(age_af>age_icd,1,0,0),
                af_stroke = if_else(age_af<age_stroke,1,0,0),
                stroke_af = if_else(age_af>age_stroke,1,0,0),
                af_hf = if_else(age_af<age_composite_hf,1,0,0),
                hf_af = if_else(age_af>age_composite_hf,1,0,0),
                af_syncope = if_else(age_af<age_syncope,1,0,0),
                syncope_af = if_else(age_af>age_syncope,1,0,0),
                af_htxvad = if_else(age_af<age_htxvad,1,0,0),
                htxvad_af = if_else(age_af>age_htxvad,1,0,0),
                #######
                srt_hcm = if_else(age_srt<primary_diagnosis_age,1,0,0),
                hcm_srt = if_else(age_srt>primary_diagnosis_age,1,0,0),
                srt_vt = if_else(age_srt<age_vt,1,0,0),
                vt_srt = if_else(age_srt>age_vt,1,0,0),
                srt_icd = if_else(age_srt<age_icd,1,0,0),
                icd_srt = if_else(age_srt>age_icd,1,0,0),
                srt_stroke = if_else(age_srt<age_stroke,1,0,0),
                stroke_srt = if_else(age_srt>age_stroke,1,0,0),
                srt_hf = if_else(age_srt<age_composite_hf,1,0,0),
                hf_srt = if_else(age_srt>age_composite_hf,1,0,0),
                srt_syncope = if_else(age_srt<age_syncope,1,0,0),
                syncope_srt = if_else(age_srt>age_syncope,1,0,0),
                srt_htxvad = if_else(age_srt<age_htxvad,1,0,0),
                htxvad_srt = if_else(age_srt>age_htxvad,1,0,0),
                ################
                vt_hcm = if_else(age_vt<primary_diagnosis_age,1,0,0),
                hcm_vt = if_else(age_vt>primary_diagnosis_age,1,0,0),
                vt_icd = if_else(age_vt<age_icd,1,0,0),
                icd_vt = if_else(age_vt>age_icd,1,0,0),
                vt_stroke = if_else(age_vt<age_stroke,1,0,0),
                stroke_vt = if_else(age_vt>age_stroke,1,0,0),
                vt_hf = if_else(age_vt<age_composite_hf,1,0,0),
                hf_vt = if_else(age_vt>age_composite_hf,1,0,0),
                vt_syncope = if_else(age_vt<age_syncope,1,0,0),
                syncope_vt = if_else(age_vt>age_syncope,1,0,0),
                vt_htxvad = if_else(age_vt<age_htxvad,1,0,0),
                htxvad_vt = if_else(age_vt>age_htxvad,1,0,0),
                ###############
                icd_hcm = if_else(age_icd<primary_diagnosis_age,1,0,0),
                hcm_icd = if_else(age_icd>primary_diagnosis_age,1,0,0),
                icd_stroke = if_else(age_icd<age_stroke,1,0,0),
                stroke_icd = if_else(age_icd>age_stroke,1,0,0),
                icd_hf = if_else(age_icd<age_composite_hf,1,0,0),
                hf_icd = if_else(age_icd>age_composite_hf,1,0,0),
                icd_syncope = if_else(age_icd<age_syncope,1,0,0),
                syncope_icd = if_else(age_icd>age_syncope,1,0,0),
                icd_htxvad = if_else(age_icd<age_htxvad,1,0,0),
                htxvad_icd = if_else(age_icd>age_htxvad,1,0,0),
                #########
                stroke_hcm = if_else(age_stroke<primary_diagnosis_age,1,0,0),
                hcm_stroke = if_else(age_stroke>primary_diagnosis_age,1,0,0),
                stroke_hf = if_else(age_stroke<age_composite_hf,1,0,0),
                hf_stroke = if_else(age_stroke>age_composite_hf,1,0,0),
                stroke_syncope = if_else(age_stroke<age_syncope,1,0,0),
                syncope_stroke = if_else(age_stroke>age_syncope,1,0,0),
                stroke_htxvad = if_else(age_stroke<age_htxvad,1,0,0),
                htxvad_stroke = if_else(age_stroke>age_htxvad,1,0,0),
                #####
                hf_hcm = if_else(age_composite_hf<primary_diagnosis_age,1,0,0),
                hcm_hf = if_else(age_composite_hf>primary_diagnosis_age,1,0,0),
                hf_syncope = if_else(age_composite_hf<age_syncope,1,0,0),
                syncope_hf = if_else(age_composite_hf>age_syncope,1,0,0),
                hf_htxvad = if_else(age_composite_hf<age_htxvad,1,0,0),
                htxvad_hf = if_else(age_composite_hf>age_htxvad,1,0,0),
                #####
                syncope_hcm = if_else(age_syncope<primary_diagnosis_age,1,0,0),
                hcm_syncope = if_else(age_syncope>primary_diagnosis_age,1,0,0),
                syncope_htxvad = if_else(age_syncope<age_htxvad,1,0,0),
                htxvad_syncope = if_else(age_syncope>age_htxvad,1,0,0),
                #####
                htxvad_hcm = if_else(age_htxvad<primary_diagnosis_age,1,0,0),
                hcm_htxvad = if_else(age_htxvad>primary_diagnosis_age,1,0,0),
                
  )

##########


dfpos <- df %>% filter(sarc_status=="SARC(+)")
dfneg<- df %>% filter(sarc_status=="SARC(-)")
dftested <- df %>% filter(sarc_status %in% c("SARC(+)",
                                             "SARC(U)",
                                             "SARC(-)",
                                             "HCM(genocopy)"))
dfposneg <- df %>% filter(sarc_status %in% c("SARC(-)","SARC(+)"))

# create a long dataset with relevant event-variables to rank the timing of events 
df_long <- dfposneg %>%
  mutate(age_hcm = primary_diagnosis_age, # renaming variable
         age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         age_af = age_arrhythmia_a_fib, # renaming af var
         age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm),
         age_ablation = case_when(event_ablation==1~t2_ablation,T~NA_real_),
  ) %>%
  dplyr::select(pid,primary_diagnosis_age,
                age_hcm, 
                age_htn,age_obstruction,age_srt,
                age_af , age_vt, age_syncope,  age_icd,
                age_hf,#age_lvsd, age_nyha_hf,
                age_ablation,
                age_stroke,age_htxvad, age_death,
                sarc_status,
  ) %>%
  pivot_longer(cols = starts_with("age_"),
               names_to = "outcome",
               values_to = "age") %>%
  mutate(outcome = case_when(str_detect(outcome, "lvef")~"age_lvef50",
                             str_detect(outcome, "age_vad")~"age_transplant",
                             T~outcome)) %>%
  drop_na() %>%
  mutate(age = case_when(age==0~primary_diagnosis_age,
                         T~age)) # remove rows with missing age values

# group the dataset by patient ID and outcome, and calculate the rank of age within each group
df_long <- df_long %>%
  group_by(pid) %>%
  mutate(rank = rank(age, ties.method = "first")) %>%
  ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>%
  group_by(pid) %>% arrange(pid,rank) %>%
  mutate(previous_event = ifelse(rank == 1, "share", lag(outcome))) %>%
  ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age)

edges_df <- df_long %>% group_by(previous_event,rank, outcome) %>% 
  summarise(n=n()) %>% ungroup() %>% arrange(rank,desc(n)) %>% 
  mutate(from = paste(rank, previous_event, sep = "_"),
         to = paste(rank+1, outcome, sep = "_")
  )
edges_df
nodes_df <- edges_df %>%  dplyr::select(from,to) %>% 
  pivot_longer(cols = everything(), values_to = "label") %>% 
  dplyr::select(label) %>% unique() %>% 
  mutate(source = row_number()-1,
         name = str_remove(label, "[1234567890]"),
         name = str_remove(name, "[0123456789]"),
         name = str_remove(name, "_"))

edges_df <- edges_df %>% 
  left_join(nodes_df, by = c("from" = "label")) %>% 
  left_join(rename(nodes_df, "target"="source"), by = c("to" = "label"))

my_col <- tibble(name = nodes_df$name %>% unique(),
                 col = scico(13, palette= "batlow"))
my_color <-  'd3.scaleOrdinal() .domain(["share","hcm","htn","syncope","af","obstruction", "vt","icd","stroke","hf",
"srt","htxvad","death"]) 
.range(["#999999" ,"#001959", "#13455F", "#225B60", "#3E6C54", "#5D7843", "#808133", "#A98B2E", "#D49347","#F49E71", "#FDAC9F", "#FCBBCB", "#F9CCF9"])'




#########

hf<-
  dfposneg %>% 
  mutate(time1 = t2_composite_hf-clinic_visit_age0,
         race = case_when(race %in% c("Black", "Asian", "White")~race,
                          T~"Other or not reported"),
         t2_htxvad = pmin(t2_vad, t2_transplant, na.rm = T),event_htxvad = pmax(event_vad, event_transplant, na.rm = T), #merging vad and HTX
         t2_af = t2_arrhythmia_a_fib, event_af = event_arrhythmia_a_fib, # renaming af var
         t2_lvsd = pmin(t2_lvef50, t2_lvef35, na.rm = T),event_lvsd = pmax(event_lvef50, event_lvef35, na.rm = T),
         t2_vt = pmin(t2_composite_v_arrhythmia, t2_cardiac_arrest, na.rm = T), event_vt = pmax(event_composite_v_arrhythmia, event_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         t2_hf = pmin(t2_lvsd,t2_htxvad, t2_composite_hf, t2_nyha_hf, t2_nyha_hf_systolic, na.rm = T), event_hf = pmax(event_lvsd,event_htxvad, event_composite_hf, event_nyha_hf, event_nyha_hf_systolic,na.rm=T)) 






########

# create a long dataset with relevant event-variables to rank the timing of events 
df_longneg <- dfneg %>%
  mutate(age_hcm = primary_diagnosis_age, # renaming variable
         age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         age_af = age_arrhythmia_a_fib, # renaming af var
         age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm)) %>%
  dplyr::select(pid,primary_diagnosis_age,
                age_hcm, age_htn,age_obstruction,age_srt,
                age_af , age_vt, age_syncope,  age_icd,
                age_hf,#age_lvsd, age_nyha_hf,
                age_stroke,age_htxvad, age_death,
                sarc_status,
  ) %>%
  pivot_longer(cols = starts_with("age_"),
               names_to = "outcome",
               values_to = "age") %>%
  mutate(outcome = case_when(str_detect(outcome, "lvef")~"age_lvef50",
                             str_detect(outcome, "age_vad")~"age_transplant",
                             T~outcome)) %>%
  drop_na() %>%
  mutate(age = case_when(age==0~primary_diagnosis_age,
                         T~age)) # remove rows with missing age values

# group the dataset by patient ID and outcome, and calculate the rank of age within each group
df_longneg <- df_longneg %>%
  group_by(pid) %>%
  mutate(rank = rank(age, ties.method = "first")) %>%
  ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>%
  group_by(pid) %>% arrange(pid,rank) %>%
  mutate(previous_event = ifelse(rank == 1, "share", lag(outcome))) %>%
  ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age)

edges_df_neg <- df_longneg %>% group_by(previous_event,rank, outcome) %>%
  summarise(n=n()) %>% ungroup() %>% arrange(rank,desc(n)) %>%
  mutate(from = paste(rank, previous_event, sep = "_"),
         to = paste(rank+1, outcome, sep = "_")
  )
nodes_df_neg <- edges_df_neg %>%  dplyr::select(from,to) %>%
  pivot_longer(cols = everything(), values_to = "label") %>%
  dplyr::select(label) %>% unique() %>%
  mutate(source = row_number()-1,
         name = str_remove(label, "[1234567890]"),
         name = str_remove(name, "[0123456789]"),
         name = str_remove(name, "_"))

edges_df_neg <- edges_df_neg %>%
  left_join(nodes_df, by = c("from" = "label")) %>%
  left_join(rename(nodes_df, "target"="source"), by = c("to" = "label"))

# #######
# 
# 
# 
# # create a long dataset with relevant event-variables to rank the timing of events 
# df_longpos <- dfpos %>% 
#   mutate(age_hcm = primary_diagnosis_age, # renaming variable
#          age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
#          age_af = age_arrhythmia_a_fib, # renaming af var
#          age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
#          age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
#          age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
#          age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm)) %>% 
#   dplyr::select(pid,primary_diagnosis_age,
#                 age_hcm, age_htn,age_obstruction,age_srt,
#                 age_af , age_vt, age_syncope,  age_icd,
#                 age_hf,#age_lvsd, age_nyha_hf, 
#                 age_stroke,age_htxvad, age_death,
#                 sarc_status,
#   ) %>% 
#   pivot_longer(cols = starts_with("age_"), 
#                names_to = "outcome",
#                values_to = "age") %>%
#   mutate(outcome = case_when(str_detect(outcome, "lvef")~"age_lvef50",
#                              str_detect(outcome, "age_vad")~"age_transplant",
#                              T~outcome)) %>% 
#   drop_na() %>% 
#   mutate(age = case_when(age==0~primary_diagnosis_age,
#                          T~age)) # remove rows with missing age values
# 
# # group the dataset by patient ID and outcome, and calculate the rank of age within each group
# df_longpos <- df_longpos %>% 
#   group_by(pid) %>% 
#   mutate(rank = rank(age, ties.method = "first")) %>%
#   ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>% 
#   group_by(pid) %>% arrange(pid,rank) %>% 
#   mutate(previous_event = ifelse(rank == 1, "share", lag(outcome))) %>% 
#   ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age)
# 
# edges_df_pos <- df_longpos %>% group_by(previous_event,rank, outcome) %>% 
#   summarise(n=n()) %>% ungroup() %>% arrange(rank,desc(n)) %>% 
#   mutate(from = paste(rank, previous_event, sep = "_"),
#          to = paste(rank+1, outcome, sep = "_")
#   )
# nodes_df_pos <- edges_df_pos %>%  dplyr::select(from,to) %>% 
#   pivot_longer(cols = everything(), values_to = "label") %>% 
#   dplyr::select(label) %>% unique() %>% 
#   mutate(source = row_number()-1,
#          name = str_remove(label, "[1234567890]"),
#          name = str_remove(name, "[0123456789]"),
#          name = str_remove(name, "_"))
# 
# edges_df_pos <- edges_df_pos %>% 
#   left_join(nodes_df, by = c("from" = "label")) %>% 
#   left_join(rename(nodes_df, "target"="source"), by = c("to" = "label"))



########
###
# create a long dataset with relevant event-variables to rank the timing of events 
df_long <- df %>%
  filter(sarc_status %in% c("SARC(+)","SARC(-)")) %>% 
  mutate(age_hcm = primary_diagnosis_age, # renaming variable
         age_htxvad = pmin(age_vad, age_transplant, na.rm = T), #merging vad and HTX
         age_af = age_arrhythmia_a_fib, # renaming af var
         age_lvsd = pmin(age_lvef50, age_lvef35, na.rm = T),
         age_vt = pmin(age_composite_v_arrhythmia, age_cardiac_arrest, na.rm = T), # a composite VT var including cardiac arrest
         age_hf = pmin(age_lvsd,age_htxvad, age_composite_hf, age_nyha_hf, age_nyha_hf_systolic, na.rm = T),
         age_hcm = case_when(is.na(age_hcm)~first_encounter_age,T~age_hcm),
         age_ablation = case_when(event_ablation==1~t2_ablation,T~NA_real_),
  ) %>%
  dplyr::select(pid,primary_diagnosis_age,
                age_hcm, 
                age_htn,age_obstruction,age_srt,
                age_af , age_vt, age_syncope,  age_icd,
                age_hf,
                age_lvsd, age_nyha_hf,
                age_ablation,
                age_stroke,age_htxvad, age_death,
                sarc_status,
  ) %>%
  pivot_longer(cols = starts_with("age_"),
               names_to = "outcome",
               values_to = "age") %>%
  mutate(outcome = case_when(str_detect(outcome, "lvef")~"age_lvef50",
                             str_detect(outcome, "age_vad")~"age_transplant",
                             T~outcome)) %>%
  drop_na() %>%
  mutate(age = case_when(age==0~primary_diagnosis_age,
                         T~age)) # remove rows with missing age values

# group the dataset by patient ID and outcome, and calculate the rank of age within each group
df_long <- df_long %>%
  group_by(pid) %>%
  mutate(rank = rank(age, ties.method = "first")) %>%
  ungroup() %>% mutate(outcome = str_replace(outcome, "age_", "")) %>%
  group_by(pid) %>% arrange(pid,rank) %>%
  mutate(previous_event = ifelse(rank == 1, "share", lag(outcome))) %>%
  ungroup() %>% select(sarc_status,pid,rank,previous_event,outcome,age)


edges_df <- df_long %>% group_by(previous_event,rank, outcome) %>% 
  summarise(n=n()) %>% ungroup() %>% arrange(rank,desc(n)) %>% 
  mutate(from = paste(rank, previous_event, sep = "_"),
         to = paste(rank+1, outcome, sep = "_")
  )
edges_df
nodes_df <- edges_df %>%  dplyr::select(from,to) %>% 
  pivot_longer(cols = everything(), values_to = "label") %>% 
  dplyr::select(label) %>% unique() %>% 
  mutate(source = row_number()-1,
         name = str_remove(label, "[1234567890]"),
         name = str_remove(name, "[0123456789]"),
         name = str_remove(name, "_"))

edges_df <- edges_df %>% 
  left_join(nodes_df, by = c("from" = "label")) %>% 
  left_join(rename(nodes_df, "target"="source"), by = c("to" = "label"))


###

trio <-
  df_long %>% 
  filter(sarc_status %in% c("SARC(+)", "SARC(-)")) %>% 
  select(!previous_event) %>% 
  pivot_wider(names_from = rank, values_from = c(outcome, age)) %>%
  mutate(vuf1 = paste(outcome_1,outcome_2,outcome_3,age_1,age_2,age_3, sep = ";"),
         vuf2 = paste(outcome_2,outcome_3,outcome_4,age_2,age_3,age_4, sep = ";"),
         vuf3 = paste(outcome_3,outcome_4,outcome_5,age_3,age_4,age_5, sep = ";"),
         vuf4 = paste(outcome_4,outcome_5,outcome_6,age_4,age_5,age_6, sep = ";"),
         vuf5 = paste(outcome_5,outcome_6,outcome_7,age_5,age_6,age_7, sep = ";"),
         vuf6 = paste(outcome_6,outcome_7,outcome_8,age_6,age_7,age_8, sep = ";"),
         vuf7 = paste(outcome_7,outcome_8,outcome_9,age_7,age_8,age_9, sep = ";"),
         #   vuf8 = paste(outcome_8,outcome_9,outcome_10,age_8,age_9,age_10, sep = ";"),
         #         vuf9 = paste(outcome_9,outcome_10,outcome_11,age_9,age_10,age_11, sep = ";")
         #vuf10 = paste(outcome_10,outcome_11,age_11)
  ) %>% #select(contains("vuf"))
  pivot_longer(cols = contains("vuf")) %>% select(pid,sarc_status,value) %>% 
  separate(col = value, into = c("source", "to", "destination", "age1", "age2", "age3"),sep = ";") %>% 
  mutate(across(c(age1, age2, age3), ~as.numeric(.x)),
         triplet = paste(source,to,destination),
         time_lag1 = age2-age1,
         time_lag2 = age3-age2) %>% drop_na() %>%
  # filter(time_lag1<10) %>% 
  #filter(time_lag2<10) %>% 
  group_by(triplet) %>% 
  summarise(n= n(), age1 = mean(age1), age2 = mean(age2), age3 = mean(age3), .groups = "drop") %>% 
  separate(triplet, into = c("source","to", "destination"), sep = " ") %>%
  mutate(triplet = paste(source,to,destination)) %>% 
  group_by(triplet) %>% mutate(triplet_frequency =sum(n)) %>% ungroup() %>% 
  arrange(desc(n))

##########
pairs <-
  df_long %>% 
  filter(sarc_status %in% c("SARC(+)", "SARC(-)")) %>% 
  
  select(!previous_event) %>% 
  pivot_wider(names_from = rank, values_from = c(outcome, age)) %>%
  mutate(vuf1 = paste(outcome_1,outcome_2,age_1,age_2, sep = ";"),
         vuf2 = paste(outcome_2,outcome_3,age_2,age_3, sep = ";"),
         vuf3 = paste(outcome_3,outcome_4,age_3,age_4, sep = ";"),
         vuf4 = paste(outcome_4,outcome_5,age_4,age_5, sep = ";"),
         vuf5 = paste(outcome_5,outcome_6,age_5,age_6, sep = ";"),
         vuf6 = paste(outcome_6,outcome_7,age_6,age_7, sep = ";"),
         vuf7 = paste(outcome_7,outcome_8,age_7,age_8, sep = ";"),
         vuf8 = paste(outcome_8,outcome_9,age_8,age_9, sep = ";"),
         #         vuf9 = paste(outcome_9,outcome_10,age_9,age_10, sep = ";"),
         #vuf10 = paste(outcome_10,outcome_11,age_11)
  ) %>% #select(contains("vuf"))
  pivot_longer(cols = contains("vuf")) %>% select(pid,sarc_status,value) %>% 
  separate(col = value, into = c("source", "destination", "age1", "age2"),sep = ";") %>% 
  mutate(across(c(age1, age2), ~as.numeric(.x)),
         pair = paste(source,destination),
         time_lag = age2-age1) %>% drop_na() %>%
  
  filter(time_lag<10) %>% 
  group_by(pair, sarc_status) %>% 
  summarise(n= n(), age1 = mean(age1), age2 = mean(age2), .groups = "drop") %>%
  group_by(pair) %>% mutate(pair_frequency =sum(n)) %>% ungroup() %>% 
  separate(pair, into = c("source", "destination"), sep = " ") %>% 
  mutate(pair = paste(source, destination)) %>% 
  arrange(desc(n)) %>% 
  group_by(sarc_status) %>% 
  mutate(r= row_number()) %>% 
  # slice_head(n=25) %>% 
  arrange(age1) %>% 
  
  pivot_longer(cols = c(age1,age2)) %>% 
  group_by(r, sarc_status) %>% 
  mutate(end = lead(value),
         start = lag(value)) %>% 
  mutate(name = case_when(name=="age1"~source,
                          T~destination)) %>% 
  group_by(pair) %>% mutate(min_start = min(value)) %>% ungroup() %>% 
  mutate(rank = dense_rank(desc(pair_frequency)),
         age_rank = dense_rank(desc(min_start))
  )


######################


bf_cor=1

pair_df<-
  rbind(
    calculate_rr_age(dfposneg, age_htn, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_htn, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_htxvad,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_obstruction, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_obstruction, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_af, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_af, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_nyha_hf, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_nyha_hf, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_lvsd, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_lvsd, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_vt, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_vt, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_stroke, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_stroke, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_syncope, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_syncope, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_icd, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_icd, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_srt, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_srt, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_ablation, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_af,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_htxvad,bf_cor, T),
    calculate_rr_age(dfposneg, age_ablation, age_arrhythmia_nsvt,bf_cor, T),
    
    calculate_rr_age(dfposneg, age_htxvad, age_htn,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_obstruction,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_nyha_hf,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_lvsd,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_vt,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_stroke,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_syncope,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_icd,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_srt,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_ablation,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_arrhythmia_nsvt,bf_cor, T),
    calculate_rr_age(dfposneg, age_htxvad, age_af,bf_cor, T)
  ) %>% filter(!str_detect(exposure, "(-)")) %>%
  # filter(exposure != "age_ablation (+)" &
  #         outcome != "age_ablation") %>%
  mutate(p = if_else(p>1,1,p),
         p_di = if_else(p<.05,1,0),
         vuffi = if_else(p<0.05, round(.est,2), NA),
         vuffi = case_when(vuffi>10~round(vuffi,0),
                           vuffi>2~round(vuffi,1),
                           vuffi>1~vuffi,
                           T~NA),
         exposure = str_replace(exposure, "age_",""),
         outcome = str_replace(outcome, "age_",""),
         sarc_status = "pos_neg") %>%
  
  bind_rows(
    rbind(
      calculate_rr_age(dfpos, age_htn, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_htn, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_arrhythmia_nsvt, age_htxvad,bf_cor, T),
      
      calculate_rr_age(dfpos, age_obstruction, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_obstruction, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_af, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_af, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_nyha_hf, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_nyha_hf, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_lvsd, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_lvsd, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_vt, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_vt, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_stroke, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_stroke, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_syncope, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_syncope, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_icd, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_icd, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_srt, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_srt, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_ablation, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_af,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_htxvad,bf_cor, T),
      calculate_rr_age(dfpos, age_ablation, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfpos, age_htxvad, age_htn,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_obstruction,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_lvsd,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_vt,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_stroke,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_syncope,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_icd,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_srt,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_ablation,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_arrhythmia_nsvt,bf_cor, T),
      calculate_rr_age(dfpos, age_htxvad, age_af,bf_cor, T)
    ) %>% filter(!str_detect(exposure, "(-)")) %>%
      # filter(exposure != "age_ablation (+)" &
      #         outcome != "age_ablation") %>%
      mutate(p = if_else(p>1,1,p),
             p_di = if_else(p<.05,1,0),
             vuffi = if_else(p<0.05, round(.est,2), NA),
             vuffi = case_when(vuffi>10~round(vuffi,0),
                               vuffi>2~round(vuffi,1),
                               vuffi>1~vuffi,
                               T~NA),
             exposure = str_replace(exposure, "age_",""),
             outcome = str_replace(outcome, "age_",""),
             sarc_status = "SARC(+)")) %>% 
  bind_rows(
    rbind(
      calculate_rr_age(dfneg, age_htn, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_htn, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_arrhythmia_nsvt, age_htxvad,bf_cor, T),
      
      calculate_rr_age(dfneg, age_obstruction, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_obstruction, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_af, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_af, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_nyha_hf, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_nyha_hf, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_lvsd, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_lvsd, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_vt, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_vt, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_stroke, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_stroke, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_syncope, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_syncope, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_icd, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_icd, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_srt, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_srt, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_ablation, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_af,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_htxvad,bf_cor, T),
      calculate_rr_age(dfneg, age_ablation, age_arrhythmia_nsvt,bf_cor, T),
      
      calculate_rr_age(dfneg, age_htxvad, age_htn,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_obstruction,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_nyha_hf,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_lvsd,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_vt,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_stroke,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_syncope,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_icd,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_srt,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_ablation,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_arrhythmia_nsvt,bf_cor, T),
      calculate_rr_age(dfneg, age_htxvad, age_af,bf_cor, T)
    ) %>% filter(!str_detect(exposure, "(-)")) %>%
      # filter(exposure != "age_ablation (+)" &
      #         outcome != "age_ablation") %>%
      mutate(p = if_else(p>1,1,p),
             p_di = if_else(p<.05,1,0),
             vuffi = if_else(p<0.05, round(.est,2), NA),
             vuffi = case_when(vuffi>10~round(vuffi,0),
                               vuffi>2~round(vuffi,1),
                               vuffi>1~vuffi,
                               T~NA),
             exposure = str_replace(exposure, "age_",""),
             outcome = str_replace(outcome, "age_",""),
             sarc_status = "SARC(-)")
  ) %>% 
  mutate(exposure = str_replace(exposure, '\\(', ""),
         exposure = str_replace(exposure, '\\+', ""),
         exposure = str_replace(exposure, '\\)', ""),
         pair = paste(exposure,outcome, sep = ""))%>% select(pair,sarc_status,.est,.lower,.upper,p)

pairs_df<-
  pairs %>% left_join(pair_df)
#############

