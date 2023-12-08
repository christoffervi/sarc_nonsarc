library(epitools)


calculate_rr <- function(data, event1, event2, n_cor = 1 ) {
  event1 <- enquo(event1)
  event2 <- enquo(event2)
  name1 <- as_label(event1)
  name2 <- as_label(event2)
  data <- {{data}} %>%  select(event1 ={{event1}}, event2= {{event2}})
epitools::riskratio(data$event1,data$event2, conf.level = 1-(.05/n_cor))$measure %>% 
    matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), paste(name1,"(+)")), c(".est", ".lower", ".upper"))) %>% 
    as_tibble(.,rownames = "rowname") %>% mutate(term=name2) %>% 
    bind_cols(epitools::riskratio(data$event1,data$event2)$p.value %>% 
                matrix(ncol = 3, dimnames = list(c(paste(as.character(name1),"(-)"), 
                                                   paste(name1,"(+)")), 
                                                 c("midp", "fisher", "chi"))) %>% 
                as_tibble(.,rownames = "rowname") %>% 
                select(fisher)) %>% mutate(p = fisher*n_cor) %>% 
  select(rowname,.est,.lower,.upper,p,term)
}

rbind(
calculate_rr(dfposneg, event_htn, event_obstruction,12^2),
calculate_rr(dfposneg, event_htn, event_af,12^2),
calculate_rr(dfposneg, event_htn, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_htn, event_lvsd,12^2),
calculate_rr(dfposneg, event_htn, event_vt,12^2),
calculate_rr(dfposneg, event_htn, event_stroke,12^2),
calculate_rr(dfposneg, event_htn, event_syncope,12^2),
calculate_rr(dfposneg, event_htn, event_icd,12^2),
calculate_rr(dfposneg, event_htn, event_srt,12^2),
calculate_rr(dfposneg, event_htn, event_ablation,12^2),
calculate_rr(dfposneg, event_htn, event_htxvad,12^2),

calculate_rr(dfposneg, event_obstruction, event_htn,12^2),
calculate_rr(dfposneg, event_obstruction, event_af,12^2),
calculate_rr(dfposneg, event_obstruction, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_obstruction, event_lvsd,12^2),
calculate_rr(dfposneg, event_obstruction, event_vt,12^2),
calculate_rr(dfposneg, event_obstruction, event_stroke,12^2),
calculate_rr(dfposneg, event_obstruction, event_syncope,12^2),
calculate_rr(dfposneg, event_obstruction, event_icd,12^2),
calculate_rr(dfposneg, event_obstruction, event_srt,12^2),
calculate_rr(dfposneg, event_obstruction, event_ablation,12^2),
calculate_rr(dfposneg, event_obstruction, event_htxvad,12^2),

calculate_rr(dfposneg, event_af, event_htn,12^2),
calculate_rr(dfposneg, event_af, event_obstruction,12^2),
calculate_rr(dfposneg, event_af, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_af, event_lvsd,12^2),
calculate_rr(dfposneg, event_af, event_vt,12^2),
calculate_rr(dfposneg, event_af, event_stroke,12^2),
calculate_rr(dfposneg, event_af, event_syncope,12^2),
calculate_rr(dfposneg, event_af, event_icd,12^2),
calculate_rr(dfposneg, event_af, event_srt,12^2),
calculate_rr(dfposneg, event_af, event_ablation,12^2),
calculate_rr(dfposneg, event_af, event_htxvad,12^2),

calculate_rr(dfposneg, event_nyha_hf, event_htn,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_obstruction,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_af,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_lvsd,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_vt,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_stroke,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_syncope,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_icd,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_srt,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_ablation,12^2),
calculate_rr(dfposneg, event_nyha_hf, event_htxvad,12^2),

calculate_rr(dfposneg, event_lvsd, event_htn,12^2),
calculate_rr(dfposneg, event_lvsd, event_obstruction,12^2),
calculate_rr(dfposneg, event_lvsd, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_lvsd, event_af,12^2),
calculate_rr(dfposneg, event_lvsd, event_vt,12^2),
calculate_rr(dfposneg, event_lvsd, event_stroke,12^2),
calculate_rr(dfposneg, event_lvsd, event_syncope,12^2),
calculate_rr(dfposneg, event_lvsd, event_icd,12^2),
calculate_rr(dfposneg, event_lvsd, event_srt,12^2),
calculate_rr(dfposneg, event_lvsd, event_ablation,12^2),
calculate_rr(dfposneg, event_lvsd, event_htxvad,12^2),

calculate_rr(dfposneg, event_vt, event_htn,12^2),
calculate_rr(dfposneg, event_vt, event_obstruction,12^2),
calculate_rr(dfposneg, event_vt, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_vt, event_lvsd,12^2),
calculate_rr(dfposneg, event_vt, event_af,12^2),
calculate_rr(dfposneg, event_vt, event_stroke,12^2),
calculate_rr(dfposneg, event_vt, event_syncope,12^2),
calculate_rr(dfposneg, event_vt, event_icd,12^2),
calculate_rr(dfposneg, event_vt, event_srt,12^2),
calculate_rr(dfposneg, event_vt, event_ablation,12^2),
calculate_rr(dfposneg, event_vt, event_htxvad,12^2),

calculate_rr(dfposneg, event_stroke, event_htn,12^2),
calculate_rr(dfposneg, event_stroke, event_obstruction,12^2),
calculate_rr(dfposneg, event_stroke, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_stroke, event_lvsd,12^2),
calculate_rr(dfposneg, event_stroke, event_vt,12^2),
calculate_rr(dfposneg, event_stroke, event_af,12^2),
calculate_rr(dfposneg, event_stroke, event_syncope,12^2),
calculate_rr(dfposneg, event_stroke, event_icd,12^2),
calculate_rr(dfposneg, event_stroke, event_srt,12^2),
calculate_rr(dfposneg, event_stroke, event_ablation,12^2),
calculate_rr(dfposneg, event_stroke, event_htxvad,12^2),

calculate_rr(dfposneg, event_syncope, event_htn,12^2),
calculate_rr(dfposneg, event_syncope, event_obstruction,12^2),
calculate_rr(dfposneg, event_syncope, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_syncope, event_lvsd,12^2),
calculate_rr(dfposneg, event_syncope, event_vt,12^2),
calculate_rr(dfposneg, event_syncope, event_stroke,12^2),
calculate_rr(dfposneg, event_syncope, event_af,12^2),
calculate_rr(dfposneg, event_syncope, event_icd,12^2),
calculate_rr(dfposneg, event_syncope, event_srt,12^2),
calculate_rr(dfposneg, event_syncope, event_ablation,12^2),
calculate_rr(dfposneg, event_syncope, event_htxvad,12^2),

calculate_rr(dfposneg, event_icd, event_htn,12^2),
calculate_rr(dfposneg, event_icd, event_obstruction,12^2),
calculate_rr(dfposneg, event_icd, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_icd, event_lvsd,12^2),
calculate_rr(dfposneg, event_icd, event_vt,12^2),
calculate_rr(dfposneg, event_icd, event_stroke,12^2),
calculate_rr(dfposneg, event_icd, event_syncope,12^2),
calculate_rr(dfposneg, event_icd, event_af,12^2),
calculate_rr(dfposneg, event_icd, event_srt,12^2),
calculate_rr(dfposneg, event_icd, event_ablation,12^2),
calculate_rr(dfposneg, event_icd, event_htxvad,12^2),

calculate_rr(dfposneg, event_srt, event_htn,12^2),
calculate_rr(dfposneg, event_srt, event_obstruction,12^2),
calculate_rr(dfposneg, event_srt, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_srt, event_lvsd,12^2),
calculate_rr(dfposneg, event_srt, event_vt,12^2),
calculate_rr(dfposneg, event_srt, event_stroke,12^2),
calculate_rr(dfposneg, event_srt, event_syncope,12^2),
calculate_rr(dfposneg, event_srt, event_icd,12^2),
calculate_rr(dfposneg, event_srt, event_af,12^2),
calculate_rr(dfposneg, event_srt, event_ablation,12^2),
calculate_rr(dfposneg, event_srt, event_htxvad,12^2),

calculate_rr(dfposneg, event_ablation, event_htn,12^2),
calculate_rr(dfposneg, event_ablation, event_obstruction,12^2),
calculate_rr(dfposneg, event_ablation, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_ablation, event_lvsd,12^2),
calculate_rr(dfposneg, event_ablation, event_vt,12^2),
calculate_rr(dfposneg, event_ablation, event_stroke,12^2),
calculate_rr(dfposneg, event_ablation, event_syncope,12^2),
calculate_rr(dfposneg, event_ablation, event_icd,12^2),
calculate_rr(dfposneg, event_ablation, event_srt,12^2),
calculate_rr(dfposneg, event_ablation, event_af,12^2),
calculate_rr(dfposneg, event_ablation, event_htxvad,12^2),

calculate_rr(dfposneg, event_htxvad, event_htn,12^2),
calculate_rr(dfposneg, event_htxvad, event_obstruction,12^2),
calculate_rr(dfposneg, event_htxvad, event_nyha_hf,12^2),
calculate_rr(dfposneg, event_htxvad, event_lvsd,12^2),
calculate_rr(dfposneg, event_htxvad, event_vt,12^2),
calculate_rr(dfposneg, event_htxvad, event_stroke,12^2),
calculate_rr(dfposneg, event_htxvad, event_syncope,12^2),
calculate_rr(dfposneg, event_htxvad, event_icd,12^2),
calculate_rr(dfposneg, event_htxvad, event_srt,12^2),
calculate_rr(dfposneg, event_htxvad, event_ablation,12^2),
calculate_rr(dfposneg, event_htxvad, event_af,12^2)
) %>% filter(!str_detect(rowname, "(-)")) %>% 
  filter(!(rowname %in% c("event_ablation (+)","event_htxvad (+)")),
         term != "event_ablation") %>% 
  #filter(rowname != "event_ablation (+)" &
  #         term != "event_ablation") %>% 
  mutate(p = if_else(p>1,1,p),
         p_di = if_else(p<.05,1,0),
         vuffi = if_else(p<0.05, round(.est,2), NA),
         vuffi = case_when(vuffi>10~round(vuffi,0),
                           vuffi>2~round(vuffi,1),
                           T~vuffi),
         rowname = str_replace(rowname, "event_",""),
         term = str_replace(term, "event_",""),
         fill_term = case_when(p<.0005~log2(.est),
                               T~NA)) %>% 
  ggplot(aes(x=term, y=rowname, fill = fill_term,#log2(.est), 
             label = vuffi))+
  geom_tile(show.legend = F, color = "white")+
  geom_text(family = "Roboto")+
#  scale_fill_scico(palette = "nuuk")
  #scale_fill_gradient2(low = "#0C5DA5", mid = "white", high = "#CA2E2D", midpoint = 0)
  #scale_fill_gradientn(colors = ggsci::pal_gsea()(12), midpoint = 0)
  ggsci::scale_fill_gsea()+
  theme(panel.background = element_blank())+
  labs(x = "", y= "")
ggsave(filename = "Risk_map.tiff", compression = "lzw", height = 16, width = 20, units = "cm", dpi =900)



#rr_df<-
  rbind(
    riskratio(dfposneg$event_htn,dfposneg$event_death, conf.level = .995)$measure %>% 
      matrix(ncol = 3, dimnames = list(c("htn(-)", "htn(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death") %>% 
      bind_cols(riskratio(dfposneg$event_htn,dfposneg$event_death, conf.level = .995)$p.value %>% 
      matrix(ncol = 3, dimnames = list(c("htn(-)", "htn(+)"), c("midp", "fisher", "chi"))) %>% 
      as_tibble(.,rownames = "rowname") %>% select(fisher)),
    riskratio(dfposneg$event_htn,dfposneg$event_death, conf.level = .995)$measure %>% 
      matrix(ncol = 3, dimnames = list(c("htn(-)", "htn(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death") %>% 
      bind_cols(riskratio(dfposneg$event_htn,dfposneg$event_death, conf.level = .995)$p.value %>% 
                  matrix(ncol = 3, dimnames = list(c("htn(-)", "htn(+)"), c("midp", "fisher", "chi"))) %>% 
                  as_tibble(.,rownames = "rowname") %>% select(fisher))
    
    riskratio(dfposneg$event_htn,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_htn,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_htn,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_htn,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_htn,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
  left_join( 
    bind_rows(
      
      
      riskratio(dfposneg$event_htn,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
        mutate(term="Death"),
      riskratio(dfposneg$event_htn,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
        mutate(term="Obesity"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Ablation"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Atrial Fibrillation"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="NSVT"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Cardiac Arrest"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Hypertension"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="ICD implantation"),
      
      # riskratio(dfposneg$event_htn,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
      #  mutate(term="LVEF ≤35"),
      # 
      # riskratio(dfposneg$event_htn,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
      #  mutate(term="LVEF ≤50"),
      # 
      riskratio(dfposneg$event_htn,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="LVSD"),
      
      # riskratio(dfposneg$event_htn,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
      #  mutate(term="Composite HF"),
      # 
      riskratio(dfposneg$event_htn,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Dyspnea"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        mutate(term="Obstruction"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
        mutate(term="SRT"),
      
      riskratio(dfposneg$event_htn,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
        mutate(term="Stroke"),
      riskratio(dfposneg$event_htn,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
        mutate(term="Cardiac transplantation")
      
    )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                  nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                  sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                  nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
) %>% 
  #  filter(term !="Cardiac transplantation") %>% 
  mutate(term = fct_reorder(term, desc(.est)),
         term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                            "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
         term = fct_rev(term)) %>% 
  ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
  
  # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
  #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
  geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
  geom_vline(aes(xintercept =1), color = "red", linetype =2)+
  geom_errorbar(width = .1)+
  geom_point(size = 4, shape= 21)+
  labs(x = "relative risk ratio", y="")+
  scale_fill_scico(palette = "berlin")+
  scale_y_discrete()+
  scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
  geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
  geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title = element_markdown(family = "Roboto", color = "black"),
        legend.position = "none"
  )+
  geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
  annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
  coord_cartesian(xlim = c(.4,12^2), ylim=c(.6,16), expand = F)
  
  
  
  
  
  
  ##############
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_obstruction,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_obstruction,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_obstruction,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_obstruction,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_obstruction,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_obstruction,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_obstruction,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_obstruction,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_obstruction,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_obstruction,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_obstruction,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_obstruction,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_obstruction,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  
  
  
  ################
  
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_af,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_af,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_af,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_af,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_af,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_af,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_af,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_af,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_af,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_af,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_af,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_af,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_af,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_af,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_af,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_af,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_af,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_af,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_af,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_af,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_af,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_af,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_af,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_af,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_af,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_af,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_af,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_af,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_af,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_af,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_af,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_af,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_af,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  
  
  
  #############
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_srt,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_srt,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_srt,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_srt,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_srt,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_srt,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_srt,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_srt,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_srt,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_srt,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_srt,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_srt,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_srt,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_srt,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  
  
  #######
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_vt,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_vt,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_vt,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_vt,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_vt,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_vt,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_vt,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_vt,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_vt,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_vt,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_vt,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_vt,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  
  
  
  #########
  
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_lvsd,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_lvsd,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_lvsd,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_lvsd,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_lvsd,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_lvsd,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_lvsd,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_lvsd,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_lvsd,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_lvsd,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_lvsd,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_lvsd,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_lvsd,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  
  
  ########
  
  library(epitools)
  #rr_df<-
  rbind(
    riskratio(dfposneg$event_vt,dfposneg$event_death, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Death"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_ablation, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Ablation"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_a_fib, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Atrial Fibrillation"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_nsvt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="NSVT"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_cardiac_arrest, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac Arrest"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_htn, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Hypertension"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_icd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="ICD implantation"),
    
    # riskratio(dfposneg$event_vt,dfposneg$event_lvef35)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤35"),
    # 
    # riskratio(dfposneg$event_vt,dfposneg$event_lvef50)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="LVEF ≤50"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_lvsd, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="LVSD"),
    
    # riskratio(dfposneg$event_vt,dfposneg$event_composite_hf)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
    #   as_tibble(.,rownames = "rowname") %>% mutate(term="Composite HF"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_nyha_hf, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>%
      as_tibble(.,rownames = "rowname") %>% mutate(term="Dyspnea"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_obstruction, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Obstruction"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_srt, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="SRT"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_stroke, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Stroke"),
    
    riskratio(dfposneg$event_vt,dfposneg$event_htxvad, conf.level = .995)$measure %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)"), c(".est", ".lower", ".upper"))) %>% 
      as_tibble(.,rownames = "rowname") %>% mutate(term="Cardiac transplantation")
  ) %>%
    left_join( 
      bind_rows(
        
        
        riskratio(dfposneg$event_vt,dfposneg$event_death)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Death"),
        riskratio(dfposneg$event_vt,dfposneg$obese)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Obesity"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_ablation)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Ablation"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_a_fib)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Atrial Fibrillation"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_arrhythmia_nsvt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="NSVT"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_cardiac_arrest)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Cardiac Arrest"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_htn)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Hypertension"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_icd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="ICD implantation"),
        
        # riskratio(dfposneg$event_vt,dfposneg$event_lvef35)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤35"),
        # 
        # riskratio(dfposneg$event_vt,dfposneg$event_lvef50)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="LVEF ≤50"),
        # 
        riskratio(dfposneg$event_vt,dfposneg$event_lvsd)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="LVSD"),
        
        # riskratio(dfposneg$event_vt,dfposneg$event_composite_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
        #  mutate(term="Composite HF"),
        # 
        riskratio(dfposneg$event_vt,dfposneg$event_nyha_hf)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Dyspnea"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_obstruction)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>%  
          mutate(term="Obstruction"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_srt)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="SRT"),
        
        riskratio(dfposneg$event_vt,dfposneg$event_stroke)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Stroke"),
        riskratio(dfposneg$event_vt,dfposneg$event_htxvad)$data %>% matrix(ncol = 3, dimnames = list(c("sarc(-)", "sarc(+)", "all"), c("no", "yes", "total"))) %>% as_tibble(.,rownames = "rowname") %>% filter(row_number()<3) %>% 
          mutate(term="Cardiac transplantation")
        
      )) %>% mutate(sarc_event_prevalence = if_else(rowname=="sarc(-)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    nonsarc_event_prevalence = if_else(rowname=="sarc(+)", NA, paste(yes,"/",total, " (",round(yes/total*100,0), "%)", sep = "")),
                    sarc_prevalence = if_else(rowname=="sarc(-)", NA, paste(round(yes/total*100,0), "%")),
                    nonsarc_prevalence = if_else(rowname=="sarc(+)", NA, paste(round(yes/total*100,0), "%"))) %>% 
    mutate(.est = if_else(rowname=="sarc(-)",NA,.est),
    ) %>% 
    #  filter(term !="Cardiac transplantation") %>% 
    mutate(term = fct_reorder(term, desc(.est)),
           term = fct_relevel(term, "Hypertension", "Obstruction", "Obesity", "SRT", "Dyspnea", "nyha_hf", "Stroke", "Composite HF", "Death", "Atrial Fibrillation",
                              "NSVT", "Cardiac Arrest", "Ablation", "ICD implantation", "LVEF ≤50", "LVEF ≤35", "LVSD"), 
           term = fct_rev(term)) %>% 
    ggplot(aes(y= term, x= .est, xmin = .lower, xmax = .upper, fill = log(.est^(1/3))))+
    
    # geom_rect(aes(xmin=1,xmax = 3,ymin=.8,ymax=7.2),fill = "gray89", alpha = .2)+
    #geom_rect(aes(xmin=1,xmax = .5,ymin=10.8,ymax=13.2),fill = "gray89", alpha = .2)+
    geom_segment(aes(x=.4, xend=4.8, y=term, yend= term), color = "gray", linetype = 3)+
    geom_vline(aes(xintercept =1), color = "red", linetype =2)+
    geom_errorbar(width = .1)+
    geom_point(size = 4, shape= 21)+
    labs(x = "relative risk ratio", y="")+
    scale_fill_scico(palette = "berlin")+
    scale_y_discrete()+
    scale_x_log10(breaks = c(.5,.75,1,1.5,2,2.5,3.5,4.7))+
    geom_text(aes(x= 10, y = term, label = sarc_prevalence), size = 3)+
    geom_text(aes(x= 6, y = term, label = nonsarc_prevalence), size = 3)+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_markdown(family = "Roboto", color = "black"),
          axis.title = element_markdown(family = "Roboto", color = "black"),
          legend.position = "none"
    )+
    geom_segment(aes(x= c(.9), xend = c(.6), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    geom_segment(aes(x= c(1.1), xend = c(1.7), y= 14.6, yend=14.6), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
    annotate("text", x= c(6,10), y= 15, label = c("non-sarc \n prevalence", "sarc\n prevalence"), family= "Roboto", size = 3.5, fontface = "bold")+
    annotate("text", x= c(.9,1.1), y= 15, label = c("Higher in \n non-sarcomeric HCM", "Higher in \n sarcomeric HCM"), family= "Roboto", size = 2.7, fontface = "bold", hjust = c(1,0), vjust =0)+
    coord_cartesian(xlim = c(.4,12), ylim=c(.6,16), expand = F)
  