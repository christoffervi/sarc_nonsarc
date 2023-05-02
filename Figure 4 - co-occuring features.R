###################
library(epitools)

calculate_rr_age <- 
  function(data, event1, event2, n_cor = 1, identical =T) {
    event1 <- enquo(event1)
    event2 <- enquo(event2)
    name1 <- as_label(event1)
    name2 <- as_label(event2)
    if(identical ==T)
    {
      data <- {{data}} %>%  
        dplyr::select(event1 ={{event1}}, event2= {{event2}}) %>% 
        dplyr::mutate(event1= dplyr::case_when(event1<=event2~1,
                                               is.na(event2)&!is.na(event1)~1,
                                               T~0),
                      event2= dplyr::if_else(is.na(event2),0,1))}
    else{data <- {{data}} %>%  
      dplyr::select(event1 ={{event1}}, event2= {{event2}}) %>% 
      dplyr::mutate(event1= dplyr::case_when(event1<event2~1,
                                             is.na(event2)&!is.na(event1)~1,
                                             T~0),
                    event2= dplyr::if_else(is.na(event2),0,1))}
    
    epitools::riskratio(data$event1,data$event2, conf.level = 1-(.05/n_cor))$measure %>% 
      matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), paste(name1,"(+)")), c(".est", ".lower", ".upper"))) %>% 
      tibble::as_tibble(.,rownames = "rowname") %>% dplyr::mutate(term=name2) %>% 
      dplyr::bind_cols(epitools::riskratio(data$event1,data$event2)$p.value %>% 
                         matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), 
                                                            paste(name1,"(+)")), 
                                                          c("midp", "fisher", "chi"))) %>% 
                         tibble::as_tibble(.,rownames = "rowname") %>% 
                         dplyr::select(fisher)) %>% dplyr::mutate(p = fisher*n_cor) %>% 
      dplyr::select(rowname,.est,.lower,.upper,p,term)
  }


bf_cor <- 12^2
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
  # calculate_rr_age(dfposneg, age_htn, age_arrhythmia_nsvt,bf_cor, T),
  # 
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_obstruction,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_af,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_nyha_hf,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_lvsd,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_vt,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_stroke,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_syncope,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_icd,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_srt,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_ablation,bf_cor, T),
  # calculate_rr_age(dfposneg, age_arrhythmia_nsvt, age_htxvad,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_obstruction, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_af, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_nyha_hf, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_lvsd, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_vt, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_stroke, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_syncope, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_icd, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_srt, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_ablation, age_arrhythmia_nsvt,bf_cor, T),
  
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
  # calculate_rr_age(dfposneg, age_htxvad, age_arrhythmia_nsvt,bf_cor, T),
  calculate_rr_age(dfposneg, age_htxvad, age_af,bf_cor, T)
) %>% filter(!str_detect(rowname, "(-)")) %>% 
  # filter(rowname != "age_ablation (+)" &
  #         term != "age_ablation") %>% 
  mutate(p = if_else(p>1,1,p),
         p_di = if_else(p<.05,1,0),
         vuffi = if_else(p<0.05, round(.est,2), NA),
         vuffi = case_when(vuffi>10~round(vuffi,0),
                           vuffi>2~round(vuffi,1),
                           vuffi>1~vuffi,
                           T~NA),
         rowname = str_replace(rowname, "age_",""),
         term = str_replace(term, "age_","")) %>% 
  ggplot(aes(x=term, y=rowname, fill = log2(.est), label = vuffi))+
  geom_tile(show.legend = F, color = "white")+
  geom_text(family = "Roboto")+
  #  scale_fill_scico(palette = "nuuk")
  #scale_fill_gradient2(low = "#0C5DA5", mid = "white", high = "#CA2E2D", midpoint = 0)
  #scale_fill_gradientn(colors = ggsci::pal_gsea()(12), midpoint = 0)
  ggsci::scale_fill_gsea()+
  theme(panel.background = element_blank())+
  labs(x = "", y= "")

ggsave(filename = "Risk_map_dir.tiff", compression = "lzw", height = 16, width = 20, units = "cm", dpi =900)
